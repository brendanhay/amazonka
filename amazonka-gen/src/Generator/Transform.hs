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
    f (PVar v) = PVar (fromJust $ prefixed p (Just v))
    f c        = c

response :: Operation -> Response -> Response
response o = rsName .~ (o ^. opName) <> "Response"

-- shape :: Shape -> Shape -> Shape
-- shape p x = x
--     & shpCommon %~ common (p ^. shpCommon)

-- common :: Common -> Common -> Common
-- common p x = x
--     & cmnName %~ (<|> _cmnName p)

shapeName :: Functor f => LensLike' f Shape (Maybe Text)
shapeName = shpCommon . cmnName

fields :: Shape -> [Field]
fields s = case s of
    SStruct{..} -> map f shpFields
    _           -> []
  where
    f x = Field (typeof x) (prefixed s $ x ^. shapeName) (_shpCommon x)

prefixed :: Shape -> Maybe Text -> Maybe Text
prefixed p x = f (p ^. shapeName)
  where
    f (Just y) = (prefix y <>) <$> x
    f Nothing  = Nothing

required :: Shape -> Bool
required s =
    let Common{..} = _shpCommon s
     in _cmnRequired || _cmnLocation == LBody

rename :: Text -> Shape -> Shape
rename k = shpCommon . cmnName .~ Just k

typeof :: Shape -> Ann
typeof s = Ann (required s) $
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
        [ "Bucket"
        , "Key"
        ]

