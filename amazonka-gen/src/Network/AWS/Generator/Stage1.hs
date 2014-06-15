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
import           Data.Text.Lazy              (Text)
import           GHC.Generics
import           Network.AWS.Generator.Types

parse :: Model -> IO (Either String Service)
parse = fmap eitherDecode . LBS.readFile . mPath

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
    { sApiVersion          :: Text
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
