{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Generator.AST.Boto
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.AST.Boto where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.ByteString.Lazy        as LBS
import           Data.HashMap.Strict         (HashMap)
import           Data.String.Conversion
import           Data.Text                   (Text)
import           GHC.Generics
import           Network.AWS.Generator.Types

parse :: Model -> IO (Either String Service)
parse = fmap eitherDecode . LBS.readFile . mPath

data Type
    = RestXML
    | RestJSON
    | JSON
    | Query
      deriving (Show)

instance FromJSON Type where
    parseJSON (String "rest-xml")  = return RestXML
    parseJSON (String "rest-json") = return RestJSON
    parseJSON (String "json")      = return JSON
    parseJSON (String "query")     = return Query

    parseJSON o = fail $ "Unable to ctor Type from: " ++ show o

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Show, Generic)

instance FromJSON Signature where
    parseJSON = ctor lowered

data Time
    = RFC822
    | ISO8601
      deriving (Show, Generic)

instance FromJSON Time where
    parseJSON = ctor lowered

data Checksum
    = MD5
    | SHA256
      deriving (Show, Generic)

instance FromJSON Checksum where
    parseJSON = ctor lowered

data Operation = Operation
    { oName             :: Text
--    , oAlias            :: Maybe Text
    , oDocumentation    :: Maybe Text
    -- , oDocumentationUrl :: Maybe Text
    -- , oHttp             :: HTTP
    -- , oInput            :: Maybe Shape
    -- , oOutput           :: Maybe Shape
    -- , oErrors           :: [Shape]
    -- , oPagination       :: Maybe Pagination
    } deriving (Show, Generic)

instance FromJSON Operation where
    parseJSON = field (recase Camel Under . drop 1)

-- From S3:
data Service = Service
    { sApiVersion          :: String
    , sType                :: Type
    , sSignatureVersion    :: Signature
    , sTimestampFormat     :: Time
    , sChecksumFormat      :: Checksum
    , sServiceFullName     :: Text
    , sServiceAbbreviation :: Text
    , sGlobalEndpoint      :: Maybe Text
    , sEndpointPrefix      :: Text
    , sXmlnamespace        :: Text
    , sDocumentation       :: Maybe Text
    , sResultWrapped       :: Maybe Bool
    , sTargetPrefix        :: Maybe Text
    , sOperations          :: HashMap Text Operation
    } deriving (Show, Generic)

instance FromJSON Service where
    parseJSON = field (recase Camel Under . drop 1)

field :: (Generic a, GFromJSON (Rep a))
      => (String -> String)
      -> Value
      -> Parser a
field f = genericParseJSON $ defaultOptions { fieldLabelModifier = f }

ctor :: (Generic a, GFromJSON (Rep a))
     => (String -> String)
     -> Value
     -> Parser a
ctor f = genericParseJSON $ defaultOptions
    { constructorTagModifier = f
    , allNullaryToStringTag  = True
    }
