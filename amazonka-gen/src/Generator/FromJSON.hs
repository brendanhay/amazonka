{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- Module      : Generator.FromJSON
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.FromJSON where

import           Control.Applicative
import           Control.Arrow
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy       as LBS
import           Data.Char
import           Data.Default
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as Map
import           Data.Monoid
import           Data.Ord
import           Data.String.CaseConversion
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Unsafe           as Text
import           GHC.Generics
import           Generator.AST
import           Generator.Models
import           Text.EDE.Filters

parseModel :: Model -> Script Service
parseModel Model{..} = do
    r <- scriptIO $ do
        putStrLn $ "Parsing Service " ++ modPath
        LBS.readFile modPath
    hoistEither (eitherDecode r)

instance FromJSON Abbrev where
    parseJSON = withText "abbrev" (return . abbrev)

instance FromJSON Version where
    parseJSON = withText "version" (return . version)

instance FromJSON Doc where
    parseJSON = withText "documentation" (return . documentation)

instance FromJSON Time

instance FromJSON Checksum

instance FromJSON ServiceType where
    parseJSON = fromCtor (recase Camel Under)

instance FromJSON Signature where
    parseJSON = withText "signature" $ \t ->
        return $ case t of
            "v2"      -> V2
            "v3"      -> V3
            "v3https" -> V3
            _         -> V4

instance FromJSON JSONV where
    parseJSON (String t) = return (JSONV t)
    parseJSON (Number n) = return . JSONV . Text.pack $ show n
    parseJSON e          = fail $ "Unrecognised JSONV field: " ++ show e

instance FromJSON Service where
    parseJSON = withObject "service" $ \o -> do
        n <- o .: "service_full_name"
        a <- o .: "service_abbreviation" .!= abbrev n
        v <- o .: "api_version"

        Service a n (namespace a v) v
            <$> o .:? "type"             .!= def
            <*> o .:? "result_wrapped"   .!= False
            <*> o .:  "signature_version"
            <*> o .:? "documentation"    .!= def
            <*> o .:  "endpoint_prefix"
            <*> o .:? "global_endpoint"
            <*> o .:? "xmlnamespace"
            <*> o .:? "timestamp_format" .!= def
            <*> o .:? "checksum_format"  .!= def
            <*> o .:? "json_version"     .!= def
            <*> o .:? "target_prefix"
            <*> o .:  "operations"

instance FromJSON [Operation] where
    parseJSON = withObject "operations" (mapM parseJSON . Map.elems)

instance FromJSON Operation where
    parseJSON = withObject "operation" $ \o ->
        Operation <$> o .:  "name"
                  <*> o .:? "alias"
                  <*> pure def
                  <*> o .:  "documentation"
                  <*> o .:? "documentation_url"
                  <*> parseJSON (Object o)
                  <*> parseJSON (Object o)
                  <*> pure []
                  <*> o .:? "pagination"

instance FromJSON Request

instance FromJSON Response

instance FromJSON Pagination

instance FromJSON HTTP where
    parseJSON = withObject "http" $ \o ->
        HTTP <$> o .: "method"
             <*> o .: "uri"
             <*> o .: "uri"

instance FromJSON [PathPart] where
    parseJSON = withText "uri" $ return . path
       where
        path = filter (/= PConst "") . go . Text.takeWhile (/= '?')

        go x | Text.null s = [PConst l]
             | otherwise   = PConst l : PVar m : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

instance FromJSON [QueryPart] where
    parseJSON = withText "uri" $ return . query
      where
        query = map (uncurry QueryPart) . go . Text.dropWhile (/= '?')

        go x | Text.null s
             , Text.null l = []
             | Text.null s = [(Text.tail l, Nothing)]
             | otherwise   = brk : go (Text.unsafeTail t)
          where
            (m, t) = Text.span (/= '}') $ Text.unsafeTail s
            (l, s) = Text.span (/= '{') x

            brk | '=' <- Text.last l = (Text.init $ Text.tail l, Just m)
                | otherwise          = (Text.tail l, Nothing)

fromField :: (Generic a, GFromJSON (Rep a))
          => (String -> String)
          -> Value
          -> Parser a
fromField f = genericParseJSON $ defaultOptions { fieldLabelModifier = f }

fromCtor :: (Generic a, GFromJSON (Rep a))
         => (String -> String)
         -> Value
         -> Parser a
fromCtor f = genericParseJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }
