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
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.List
import           Data.Maybe
import           Data.Monoid                hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
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
operation s o = o
    & opService   .~ s ^. svcName
    & opNamespace .~ s ^. svcVersionNamespace <> NS [_opName o]
    & opImports  <>~ imports
    & opRequest   %~ request o
    & opResponse  %~ response o
  where
    imports = sort
        [ "Network.AWS.Data"
        , "Network.AWS.Types"
        , "Data.Monoid"
        , "GHC.Generics"
        , "Data.Time"
        , s ^. svcTypesNamespace
        , fromString $ "Network.AWS.Request." ++ show (s ^. svcType)
        ]

request :: Operation -> Request -> Request
request o rq = rq
    & rqName    .~ o ^. opName
    & rqDefault .~ lowerFirst (o ^. opName)
    & rqHttp    %~ http (rq ^. rqShape)

http :: Shape -> HTTP -> HTTP
http p = hPath %~ map f
  where
    f (PVar v) = PVar (prefixed p v)
    f c        = c

response :: Operation -> Response -> Response
response o = rsName .~ (o ^. opName) <> "Response"

rootNS :: NS -> NS
rootNS (NS []) = NS []
rootNS (NS xs) = NS (init xs)

typeNS :: NS -> NS
typeNS = (<> "Types")

fromName :: Shape -> Text
fromName = fromMaybe "Untyped" . view cmnName

shapeType :: Shape -> Type
shapeType s = Type s (typeof s) (ctorof s) (fields s)

shapeEnums :: [Text] -> HashMap Text Text
shapeEnums = Map.fromList . map trans . filter (not . Text.null)
  where
    trans = first rules . join (,)

    rules = Text.pack
        . upcase
        . recase Under Camel
        . Text.unpack
        . Text.replace "-" "_"

    -- ABC_ABC -> AbcAbc
    -- blah-blah -> BlahBlah
    -- prefix with type name

    upcase []       = []
    upcase (x : xs) = toUpper x : xs

fields :: Shape -> [Field]
fields s = case s of
    SStruct Struct{..} -> map f (Map.toList _sctFields)
    _                  -> []
  where
    f (k, v) = Field (typeof v) (prefixed s k) (v ^. common)

prefixed :: Shape -> Text -> Text
prefixed p x = f (p ^. cmnName)
  where
    f (Just y) = prefix y <> x
    f Nothing  = "Prefixed"

typeof :: Shape -> Ann
typeof s = Ann (required s) (defaults s) $
    case s of
        SStruct Struct {..} -> n
        SList   List   {..} -> "[" <> ann _lstItem <> "]"
        SMap    Map    {..} -> "HashMap " <> ann _mapKey <> " " <> ann _mapValue
        SSum    Sum    {..} -> n
        SPrim   Prim   {..}
            | n `elem` reserved -> n
            | otherwise         -> Text.pack . drop 1 $ show _prmType
  where
    n   = fromMaybe "Untyped" (s ^. cmnName)
    ann = anType . typeof

    reserved =
        [ "BucketName"
        , "ObjectKey"
        , "ObjectVersionId"
        , "ObjectCannedACL"
        , "ETag"
        , "Region"
        , "AvailabilityZone"
        ]

ctorof :: Shape -> Ctor
ctorof s = case s of
    SStruct Struct{..}
        | Map.size _sctFields == 1 -> CNewtype
        | Map.null _sctFields      -> CNullary
    SSum{}                         -> CSum
    _                              -> CData

defaults :: Shape -> Bool
defaults s = case s of
    SStruct {} -> False
    SList   l  -> _lstMinLength l < 1
    SMap    {} -> True
    SSum    {} -> False
    SPrim   {} -> False

required :: Shape -> Bool
required s =
    let Common{..} = s ^. common
     in _cmnRequired || _cmnLocation == LBody

serviceTypes :: Service -> [Type]
serviceTypes = sort
    . nub
    . map (shapeType . snd)
    . concatMap opfields
    . _svcOperations
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
    ts = Map.fromList $ map (\s -> (fromName s, shapeType s)) ss
    ss = nub (concatMap _opErrors os)
