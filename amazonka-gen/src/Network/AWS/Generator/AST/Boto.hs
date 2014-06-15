{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.Types
import Data.Char
import Data.String.Conversion
import Data.Text                   (Text)
import GHC.Generics
import Network.AWS.Generator.Types

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

    parseJSON o = fail $ "Unable to generic Type from: " ++ show o

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Show, Generic)

instance FromJSON Signature where
    parseJSON = generic lowered

data Time
    = RFC822
      deriving (Show, Generic)

instance FromJSON Time where
    parseJSON = generic lowered

data Checksum
    = MD5
      deriving (Show, Generic)

instance FromJSON Checksum where
    parseJSON = generic lowered

-- From S3:
data BotoService = BotoService
    { _apiVersion          :: String
    , _type                :: Type
    , _signatureVersion    :: Signature
    , _timestampFormat     :: Time
    , _checksumFormat      :: Checksum
    , _serviceFullName     :: Text
    , _serviceAbbreviation :: Text
    , _globalEndpoint      :: Text
    , _endpointPrefix      :: Text
    , _xmlnamespace        :: Text
    , _documentation       :: Text
    } deriving (Show, Generic)

instance FromJSON BotoService where
    parseJSON = generic (recase Camel Under . drop 1)

generic :: (Generic a, GFromJSON (Rep a))
        => (String -> String)
        -> Value
        -> Parser a
generic f = genericParseJSON $ defaultOptions { constructorTagModifier = f }
