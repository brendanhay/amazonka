{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Generator.Stage1
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Stage1 where

import           Data.Aeson
import qualified Data.ByteString.Lazy        as LBS
import           Data.HashMap.Strict         (HashMap)
import           Data.String.CaseConversion
import           Data.Text                   (Text)
import           GHC.Generics
import           Network.AWS.Generator.Types

parse :: Model -> IO (Either String Service)
parse = fmap eitherDecode . LBS.readFile . mPath

data Operation = Operation
    { o1Name             :: Text
--    , oAlias            :: Maybe Text
    , o1Documentation    :: Maybe Text
    -- , oDocumentationUrl :: Maybe Text
    -- , oHttp             :: HTTP
    -- , oInput            :: Maybe Shape
    -- , oOutput           :: Maybe Shape
    -- , oErrors           :: [Shape]
    -- , oPagination       :: Maybe Pagination
    } deriving (Show, Generic)

instance FromJSON Operation where
    parseJSON = field (recase Camel Under . drop 2)

-- From S3:
data Service = Service
    { s1ApiVersion          :: Text
    , s1Type                :: Type
    , s1SignatureVersion    :: Signature
    , s1TimestampFormat     :: Time
    , s1ChecksumFormat      :: Checksum
    , s1ServiceFullName     :: Text
    , s1ServiceAbbreviation :: Text
    , s1GlobalEndpoint      :: Maybe Text
    , s1EndpointPrefix      :: Text
    , s1Xmlnamespace        :: Text
    , s1Documentation       :: Maybe Text
    , s1ResultWrapped       :: Maybe Bool
    , s1TargetPrefix        :: Maybe Text
    , s1Operations          :: HashMap Text Operation
    } deriving (Show, Generic)

instance FromJSON Service where
    parseJSON = field (recase Camel Under . drop 2)
