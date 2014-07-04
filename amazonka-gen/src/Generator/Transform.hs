{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Generator.Transform
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Transform where

import           Control.Arrow
import           Control.Lens
import           Control.Monad
import           Data.Char
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Maybe
import           Data.Monoid         hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Util
import           Generator.AST
import           Text.EDE.Filters

transform :: [Service] -> [Service]
transform = map (\s -> s & svcOperations %~ map (operation s))

current :: [Service] -> [Service]
current = mapMaybe latest . groupBy identical
  where
    identical x y = EQ == comparing _svcName x y

    latest [] = Nothing
    latest xs = Just . head $ sortBy (comparing _svcVersion) xs

operation :: Service -> Operation -> Operation
operation Service{..} o = o
    & opService          .~ _svcName
    & opNamespace        .~ _svcVersionNamespace <> NS [_opName o]
    & opTypesNamespace   .~ _svcTypesNamespace
    & opVersionNamespace .~ _svcVersionNamespace
    & opRequestNamespace .~ "Network.AWS.Request" <> fromString (show _svcType)
    & opRequest          %~ request  _svcTimestamp o
    & opResponse         %~ response _svcTimestamp o
    & opPagination       %~ pagination o
    & opName             .~ fromMaybe (o ^. opName) (o ^. opAlias)

request :: Time -> Operation -> Request -> Request
request t o rq = rq
    & rqName     .~ (o ^. opName)
    & rqDefault  .~ lowerFirst (o ^. opName)
    & rqHttp     %~ http (rq ^. rqShape)
    & rqFields   .~ fs
    & rqPayload  .~ listToMaybe bdy
    & rqRequired .~ req
    & rqHeaders  .~ hs
  where
    bdy = filter ((== LBody) . view cmnLocation) fs
    req = filter (view cmnRequired) fs
    hs  = filter ((== LHeader) . view cmnLocation) fs

    fs  = map upd . sort . fields t $ rq ^. rqShape

    upd f | f ^. cmnLocation == LBody = f & cmnRequired .~ True
          | otherwise                 = f

http :: Shape -> HTTP -> HTTP
http p = hPath %~ map f
  where
    f (PVar v) = PVar (prefixed p v)
    f c        = c

response :: Time -> Operation -> Response -> Response
response t o rs = rs
    & rsName   .~ (o ^. opName) <> "Response"
    & rsFields .~ (sort . fields t $ rs ^. rsShape)

pagination :: Operation -> Maybe Pagination -> Maybe Pagination
pagination o = fmap go
  where
    go p = p
        & pgTokens %~ map (\t -> t & tokInput %~ rqPref & tokOutput %~ replace)
        & pgMore   %~ fmap rsPref

    -- S3 ListObjects
    replace "NextMarker || Contents[-1].Key" = "fmap oKey . listToMaybe $ looContents"
    replace x                                = rsPref x

    rqPref = pref (opRequest  . rqShape)
    rsPref = pref (opResponse . rsShape)

    pref s = prefixed (o ^. s)

rootNS :: NS -> NS
rootNS (NS []) = NS []
rootNS (NS xs) = NS (init xs)

typeNS :: NS -> NS
typeNS = (<> "Types")

fromName :: Shape -> Text
fromName = fromMaybe "Untyped" . view cmnName

shapeType :: Time -> Shape -> Type
shapeType t s = Type s (typeof t s) (ctorof s) (fields t s)

shapeEnums :: Maybe Text -> [Text] -> HashMap Text Text
shapeEnums n = Map.fromList . map trans . filter (not . Text.null)
  where
    trans = first (mappend (fromMaybe "" n) . rules) . join (,)

    rules x =
        let y  = Text.replace ":" "" . Text.replace "_" " " $ Text.replace "-" " " x
            zs = Text.words y

         in if | length zs > 1      -> Text.concat (map Text.toTitle zs)
               | Text.all isUpper y -> Text.toTitle y
               | otherwise          -> upcase y

    upcase x
        | Text.null x = x
        | otherwise   = toUpper (Text.head x) `Text.cons` Text.tail x

fields :: Time -> Shape -> [Field]
fields t s = case s of
    SStruct Struct{..} -> map f (Map.toList _sctFields)
    _                  -> []
  where
    f (k, v) = Field (typeof t v) (prefixed s k) (v ^. common)

prefixed :: Shape -> Text -> Text
prefixed p x = f (p ^. cmnName)
  where
    f (Just y) = prefix y <> x
    f Nothing  = "Prefixed"

typeof :: Time -> Shape -> Ann
typeof t s = Ann req (defaults s) (monoids s) typ
  where
    typ = case s of
        SStruct Struct {}   -> n
        SList   List   {..} -> "[" <> ann _lstItem <> "]"
        SMap    Map    {..} -> "HashMap " <> ann _mapKey <> " " <> ann _mapValue
        SSum    Sum    {}
            | swt, req          -> "Switch " <> n
            | swt               -> "(Switch " <> n <> ")"
            | otherwise         -> n
        SPrim   Prim   {..}
            | n `elem` reserved -> n
            | otherwise         -> fmt _prmType

    n   = fromName s
    ann = anType . typeof t

    req = required s
    swt = n `elem` switches

    fmt x = Text.pack $
        case x of
            PUTCTime -> show t
            _        -> drop 1 (show x)

required :: Shape -> Bool
required s = s ^. cmnRequired || s ^. cmnLocation == LBody

reserved :: [Text]
reserved =
    [ "BucketName"
    , "ObjectKey"
    , "ObjectVersionId"
    , "ETag"
    , "Region"
    , "AvailabilityZone"
    ]

switches :: [Text]
switches =
    [ "BucketVersioningStatus"
    , "ExpirationStatus"
    , "MFADelete"
    , "MFADeleteStatus"
    ]

ctorof :: Shape -> Ctor
ctorof s = case s of
    SStruct Struct{..}
        | Map.size _sctFields == 1   -> CNewtype
        | Map.null _sctFields        -> CNullary
    SSum{}
        | fromName s `elem` switches -> CWitness
        | otherwise                  -> CSum
    _                                -> CData

defaults :: Shape -> Bool
defaults s = case s of
    SStruct {} -> False
    SList   l  -> _lstMinLength l < 1
    SMap    {} -> False
    SSum    {} -> False
    SPrim   {} -> False

monoids :: Shape -> Bool
monoids s = case s of
    SStruct {} -> False
    SList   l  -> _lstMinLength l < 1
    SMap    {} -> True
    SSum    {} -> False
    SPrim   {} -> False

serviceTypes :: Service -> [Type]
serviceTypes Service{..} = sort
    . nub
    . map (shapeType _svcTimestamp . snd)
    . concatMap opfields
    $ _svcOperations
  where
    opfields o =
           descend (_rqShape $ _opRequest  o)
        ++ descend (_rsShape $ _opResponse o)

    descend (SStruct Struct{..}) =
        concatMap (\s -> flat (fromName s) s) (Map.elems _sctFields)
    descend _                   = []

    flat p s@SStruct     {}    = (p, s) : descend s
    flat _ s@(SList List {..}) = flat (fromName s) _lstItem
    flat _ s@(SMap  Map  {..}) = flat (fromName s) _mapKey ++ flat (fromName s) _mapValue
    flat p s@SSum        {}    = [(p, s)]
    flat _ _                   = []

serviceError :: Abbrev -> [Operation] -> Error
serviceError a os = Error (unAbbrev a <> "Error") ss ts
  where
    ts = Map.fromList $ map (\s -> (fromName s, shapeType def s)) ss
    ss = nub (concatMap _opErrors os)
