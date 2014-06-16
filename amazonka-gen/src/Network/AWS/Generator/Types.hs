{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Generator.Types
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Types where

import Data.Aeson
import Data.Aeson.Types
import Data.String.CaseConversion
import GHC.Generics
import System.FilePath

data Model = Model
    { mPath :: FilePath
    , mVers :: String
    } deriving (Show)

modelFromPath :: FilePath -> String -> Model
modelFromPath d f = Model (d </> f) (fst $ break (== '.') f)

data Type
    = RestXML
    | RestJSON
    | RestS3
    | JSON
    | Query
      deriving (Eq, Show, Generic)

instance FromJSON Type where
    parseJSON (String "rest-xml")  = return RestXML
    parseJSON (String "rest-json") = return RestJSON
    parseJSON (String "json")      = return JSON
    parseJSON (String "query")     = return Query

    parseJSON o = fail $ "Unable to ctor Type from: " ++ show o

instance ToJSON Type where
    toJSON = genericToJSON $ defaultOptions { fieldLabelModifier = lowered }

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show, Generic)

instance FromJSON Signature where
    parseJSON = fromCtor lowered

instance ToJSON Signature

data Time
    = RFC822
    | ISO8601
      deriving (Eq, Show, Generic)

instance FromJSON Time where
    parseJSON = fromCtor lowered

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show, Generic)

instance FromJSON Checksum where
    parseJSON = fromCtor lowered

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

toField :: (Generic a, GToJSON (Rep a))
        => (String -> String)
        -> a
        -> Value
toField f = genericToJSON $ defaultOptions { fieldLabelModifier = f }
