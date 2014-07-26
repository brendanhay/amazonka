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
import           Control.Monad.State
import           Data.Char
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.HashSet        (HashSet)
import qualified Data.HashSet        as Set
import           Data.List
import           Data.Maybe
import           Data.Monoid         hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Generator.AST
import           Text.EDE.Filters

-- FIXME: Provide the 'length' of the prefix so lenses can be derived.
-- FIXME: Fix ambiguous lens fields
-- FIXME: Add documentation about where the type in the 'Types' module is used

transform :: [Service] -> [Service]
transform = map eval . sort . nub
  where
    eval s = s
        & svcOperations .~ evalState (mapM (operation s) (_svcOperations s)) mempty

current :: [Service] -> [Service]
current = mapMaybe latest . groupBy identical
  where
    identical x y = EQ == comparing _svcName x y

    latest [] = Nothing
    latest xs = Just . head $ sortBy (comparing _svcVersion) xs

operation :: Service -> Operation -> State (HashSet Text) Operation
operation Service{..} o = do
    rq <- uniquify (o ^. opRequest  . rqShape)
    rs <- uniquify (o ^. opResponse . rsShape)

    return $ o
        & opService            .~ _svcName
        & opNamespace          .~ _svcVersionNamespace <> NS [_opName o]
        & opTypesNamespace     .~ _svcTypesNamespace
        & opVersionNamespace   .~ _svcVersionNamespace
        & opRequestNamespace   .~ "Network.AWS.Request" <> fromString (show _svcType)
        & opPagination         %~ pagination o
        & opRequest  . rqShape .~ rq
        & opRequest            %~ request  _svcTimestamp o
        & opResponse . rsShape .~ rs
        & opResponse           %~ response _svcTimestamp o

uniquify :: Shape -> State (HashSet Text) Shape
uniquify s = case s of
    SStruct c@Struct{..} -> do
        x <- go (c ^. cmnPrefix)
        return . SStruct $ (c & cmnPrefix .~ x) { _sctFields = _sctFields }
    SList l@List{..} -> do
        i <- uniquify _lstItem
        return . SList $ l { _lstItem = i }
    SMap m@Map{..} -> do
        k <- uniquify _mapKey
        v <- uniquify _mapValue
        return . SMap $ m { _mapKey = k, _mapValue = v }
    _ -> return s
  where
    go :: Text -> State (HashSet Text) Text
    go x = do
        p <- gets (Set.member x)
        if p
            then go (next x)
            else modify (Set.insert x) >> return x

    next x
        | Text.null x = "_a"
        | "_" <- x    = "_a"
        | otherwise   = Text.init x `Text.snoc` succ (Text.last x)

request :: Time -> Operation -> Request -> Request
request t o rq = rq
    & rqName     .~ (o ^. opName)
    & rqDefault  .~ lowerFirst (o ^. opName)
    & rqHttp     %~ http (rq ^. rqShape)
    & rqFields   .~ fs
    & rqPayload  .~ bdy
    & rqRequired .~ req
    & rqHeaders  .~ hs
  where
    bdy = listToMaybe $ filter ((== LBody) . view cmnLocation) fs
    req = filter (view cmnRequired) fs
    hs  = filter ((== LHeader) . view cmnLocation) fs

    fs  = map upd . sort . fields True t $ rq ^. rqShape

    upd f | f ^. cmnLocation == LBody = f & cmnRequired .~ True
          | otherwise                 = f

http :: Shape -> HTTP -> HTTP
http p = hPath %~ map f
  where
    f (PVar v) = PVar (prefixed p v)
    f c        = c

response :: Time -> Operation -> Response -> Response
response t o rs = rs
    & rsName    .~ (o ^. opName) <> "Response"
    & rsFields  .~ fs
    & rsPayload .~ bdy
    & rsHeaders .~ hs
    & rsType    .~ typ
  where
    bdy = listToMaybe $ filter ((== LBody) . view cmnLocation) fs
    hs  = filter ((== LHeader) . view cmnLocation) fs

    fs  = sort . fields False t $ rs ^. rsShape

    typ | maybe False (view cmnStreaming) bdy    = RBody
        | length hs == length fs                 = RHeaders
        | all ((== LBody) . view cmnLocation) fs = RXml
        | otherwise                              = def

pagination :: Operation -> Maybe Pagination -> Maybe Pagination
pagination o = fmap go
  where
    go p = p
        & pgTokens %~ map (\t -> t & tokInput %~ rqPref & tokOutput %~ replace)
        & pgMore   %~ fmap rsPref

    -- S3 ListObjects
    replace "NextMarker || Contents[-1].Key" = "fmap (toText . _oKey) . listToMaybe $ _looContents"
    replace x                                = rsPref x

    rqPref = pref (opRequest  . rqShape)
    rsPref = pref (opResponse . rsShape)

    pref s = prefixed (o ^. s)

rootNS :: NS -> NS
rootNS (NS []) = NS []
rootNS (NS xs) = NS (init xs)

typeNS :: NS -> NS
typeNS = (<> "Types")

lensNS :: NS -> NS
lensNS = (<> "Lenses")

serviceNamespaces :: Service -> [NS]
serviceNamespaces s = sort
    $ _svcTypesNamespace s
    : _svcLensNamespace s
    : map _opNamespace (_svcOperations s)

fromName :: HasCommon a => a -> Text
fromName = fromMaybe "Untyped" . view cmnName

shapeEnums :: Maybe Text -> [Text] -> HashMap Text Text
shapeEnums n = Map.fromList . map trans . filter (not . Text.null)
  where
    trans = first (mappend (reserve n) . rules) . join (,)

    reserve Nothing = ""
    reserve (Just x)
        | x `elem` unprefixed = ""
        | otherwise           = x

    unprefixed =
        [ "InstanceType"
        ]

    rules x =
        let y  = Text.replace ":" ""
               . Text.replace "." " "
               . Text.replace "/" " "
               . Text.replace "(" " "
               . Text.replace ")" " "
               . Text.replace "_" " "
               $ Text.replace "-" " " x
            zs = Text.words y

         in if | length zs > 1      -> Text.concat (map Text.toTitle zs)
               | Text.all isUpper y -> Text.toTitle y
               | otherwise          -> upcase y

    upcase x
        | Text.null x = x
        | otherwise   = toUpper (Text.head x) `Text.cons` Text.tail x

fields :: Bool -> Time -> Shape -> [Field]
fields rq t s = case s of
    SStruct Struct{..} -> map f (Map.toList _sctFields)
    _                  -> []
  where
    f :: (Text, Shape) -> Field
    f (k, v) =
        let fld = Field (typeof rq t v) (prefixed s k) (v ^. common)
         in if k == "IsTruncated"
                then fld & cmnRequired .~ True & fldType %~ (anRequired_ .~ True)
                else fld

prefixed :: Shape -> Text -> Text
prefixed p = mappend (p ^. cmnPrefix)

typeof :: Bool -> Time -> Shape -> Ann
typeof rq t s = Ann req (defaults s) (monoids s) typ
  where
    typ = case s of
        SStruct Struct {}       -> n
        SList   List   {..}     -> "[" <> ann _lstItem <> "]"
        SMap    Map    {..}     -> "HashMap " <> ann _mapKey <> " " <> ann _mapValue
        SSum    Sum    {}
            | swt, req          -> "Switch " <> n
            | swt               -> "(Switch " <> n <> ")"
            | otherwise         -> n
        SPrim   Prim   {..}
            | n `elem` reserved -> n
            | n == "Delimiter"
            , _prmType == PText -> "Char"
            | bdy, rq           -> "RqBody"
            | bdy               -> "RsBody"
            | otherwise         -> fmt _prmType

    n   = fromName s
    ann = _anType . typeof rq t

    req = bdy || required rq s
    swt = n `elem` switches
    bdy = body s

    fmt x = Text.pack $
        case x of
            PUTCTime -> show t
            _        -> drop 1 (show x)

required :: Bool -> Shape -> Bool
required True s = s ^. cmnRequired || s ^. cmnLocation == LBody
required _    s = s ^. cmnRequired

body :: Shape -> Bool
body s = s ^. cmnLocation == LBody && s ^. cmnStreaming

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
ctorof s =
    case s of
        SStruct Struct{..}
            | Map.size _sctFields == 1   -> CNewtype
            | Map.null _sctFields        -> CNullary
        SSum{}
            | fromName s `elem` switches -> CSwitch
            | otherwise                  -> CSum
        _                                -> CData

defaults :: Shape -> Bool
defaults s =
    case s of
        SStruct {} -> False
        SList   l  -> _lstMinLength l < 1
        SMap    {} -> False
        SSum    {} -> False
        SPrim   {} -> False

monoids :: Shape -> Bool
monoids s =
    case s of
        SStruct {} -> False
        SList   l  -> _lstMinLength l < 1
        SMap    {} -> True
        SSum    {} -> False
        SPrim   {} -> False

setDirection :: Direction -> Shape -> Shape
setDirection d s =
    case s of
        SStruct x@Struct{..} ->
            SStruct (dir x { _sctFields = Map.map (setDirection d) _sctFields })
        SList x@List{..} ->
            SList (dir x { _lstItem = dir _lstItem })
        SMap x@Map{..} ->
            SMap (dir x { _mapKey = dir _mapKey, _mapValue = dir _mapValue })
        SSum x ->
            SSum (dir x)
        SPrim x ->
            SPrim (dir x)
  where
    dir :: HasCommon a => a -> a
    dir = cmnDirection .~ d

serviceTypes :: Service -> [Type]
serviceTypes Service{..} = sort
    . Map.elems
    . (`execState` mempty)
    . mapM uniq
    . map (shapeType True _svcTimestamp . snd)
    . concatMap opfields
    $ _svcOperations
  where
    uniq :: Type -> State (HashMap Text Type) ()
    uniq x = modify $ \m ->
        let n = fromMaybe "Unknown" (x ^. cmnName)
            y = Map.lookup n m
            z = maybe x (cmnDirection <>~ (x ^. cmnDirection)) y
         in Map.insert n z m

    opfields o =
           descend (_rqShape $ _opRequest  o)
        ++ descend (_rsShape $ _opResponse o)

    descend (SStruct Struct{..}) =
        concatMap (\s -> flat (fromName s) s) (Map.elems _sctFields)
    descend _                   = []

    flat p s@SStruct {}         = (p, s) : descend s
    flat _ s@(SList  List {..}) = flat (fromName s) _lstItem
    flat _ s@(SMap   Map  {..}) = flat (fromName s) _mapKey ++ flat (fromName s) _mapValue
    flat p (SSum     x)         = [(p, SSum $ rename x)]
    flat _ _                    = []

    rename s
        | fromName s `notElem` switches = s
        | otherwise = s { _sumValues = f (_sumValues s) }
      where
        f = Map.fromList . map g . Map.toList

        g (_, "Enabled") = ("Enabled", "Enabled")
        g (_, v)         = ("Disabled", v)

serviceError :: Abbrev -> [Operation] -> Error
serviceError a os = Error (unAbbrev a <> "Error") ss ts
  where
    ts = Map.fromList $ map (\s -> (fromName s, shapeType True def s)) ss

    ss = except "Serializer" "String"
       : except "Client" "HttpException"
       : except "Service" "String"
       : nub (concatMap _opErrors os)

    except s t = SStruct $ Struct (Map.fromList [("", field)]) ctor
      where
        field  = SStruct . Struct mempty $ def & cmnName .~ Just t
        ctor   = def & cmnName .~ Just (unAbbrev a <> s)

shapeType :: Bool -> Time -> Shape -> Type
shapeType rq t s = Type s (typeof rq t s) (ctorof s) (fields rq t s)
