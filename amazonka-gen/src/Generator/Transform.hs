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

import           Control.Applicative
import           Control.Lens
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Maybe
import           Data.Monoid
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

-- shape :: Shape -> Shape -> Shape
-- shape p x = x
--     & shpCommon %~ common (p ^. shpCommon)

-- common :: Common -> Common -> Common
-- common p x = x
--     & cmnName %~ (<|> _cmnName p)

rootNS :: NS -> NS
rootNS (NS []) = NS []
rootNS (NS xs) = NS (init xs)

typeNS :: NS -> NS
typeNS = (<> "Types")

shapeName :: Functor f => LensLike' f Shape (Maybe Text)
shapeName = shpCommon . cmnName

shapeType :: Shape -> Type
shapeType s = Type s (typeof s) (ctorof s) (fields s)

fields :: Shape -> [Field]
fields s = case s of
    SStruct{..} -> map f (Map.toList shpFields)
    _           -> []
  where
    f (k, v) = Field (typeof v) (prefixed s k) (_shpCommon v)

prefixed :: Shape -> Text -> Text
prefixed p x = f (p ^. shapeName)
  where
    f (Just y) = prefix y <> x
    f Nothing  = "Prefixed"

typeof :: Shape -> Ann
typeof s = Ann (required s) (defaults s) $
    case s of
        SStruct {..} -> name
        SList   {..} -> "[" <> ann shpItem <> "]"
        SMap    {..} -> "HashMap " <> ann shpKey <> " " <> ann shpValue
        SEnum   {..} -> name
        SPrim   {..}
            | name `elem` reserved -> name
            | otherwise            -> Text.pack . drop 1 $ show shpType
  where
    name = fromMaybe "Untyped" (s ^. shapeName)
    ann  = anType . typeof

    reserved =
        [ "BucketName"
        , "ObjectKey"
        , "ObjectVersionId"
        , "ObjectCannedACL"
        , "ETag"
        ]

ctorof :: Shape -> Ctor
ctorof s = case s of
    SStruct{..}
        | Map.size shpFields == 1 -> CNewtype
        | Map.null shpFields      -> CNullary
    SEnum{}                       -> CEnum
    _                             -> CData

defaults :: Shape -> Bool
defaults s = case s of
    SStruct {}   -> False
    SList   {..} -> shpMinLength < 1
    SMap    {}   -> True
    SEnum   {}   -> False
    SPrim   {}   -> False

required :: Shape -> Bool
required s =
    let Common{..} = _shpCommon s
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

    descend SStruct {..} = concatMap (\s -> flat (name s) s) (Map.elems shpFields)
    descend _            = []

    flat p s@SStruct {..} = (p, s) : descend s
    flat _ s@SList   {..} = flat (name s) shpItem
    flat _ s@SMap    {..} = flat (name s) shpKey ++ flat (name s) shpValue
    flat p s@SEnum   {}   = [(p, s)]
    flat _ _              = []

    name = fromMaybe "Untyped" . view shapeName
