{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

-- Module      : Compiler.AST
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla xtPublic License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST where

import           Compiler.TH
import           Compiler.Types
import           Control.Lens
import qualified Data.Aeson     as A
import           Data.Jason
import           Data.Monoid
import           Data.Text      (Text)
import qualified Data.Text      as Text

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show)

deriveJSON lower ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show)

deriveJSON spinal ''Protocol

data Timestamp
    = RFC822
    | ISO8601
    | POSIX
      deriving (Eq, Show)

instance FromJSON Timestamp where
    parseJSON = withText "timestamp" $ \case
        "rfc822"        -> pure RFC822
        "iso8601"       -> pure ISO8601
        "unixTimestamp" -> pure POSIX
        e               -> fail ("Unknown Timestamp: " ++ Text.unpack e)

deriveToJSON (aeson lower) ''Timestamp

defaultTimestamp :: Protocol -> Timestamp
defaultTimestamp = \case
    JSON     -> POSIX
    RestJSON -> POSIX
    XML      -> ISO8601
    RestXML  -> ISO8601
    Query    -> ISO8601
    EC2      -> ISO8601

data Checksum
    = MD5
    | SHA256
      deriving (Eq, Show)

deriveJSON lower ''Checksum

data Metadata = Metadata
    { _protocol            :: Protocol
    , _serviceAbbreviation :: Text
    , _serviceFullName     :: Text
    , _apiVersion          :: Text
    , _signatureVersion    :: Signature
    , _endpointPrefix      :: Text
    , _timestampFormat     :: Timestamp
    , _checksumFormat      :: Checksum
    , _jsonVersion         :: Text
    , _targetPrefix        :: Maybe Text
    } deriving (Eq, Show)

makeClassy ''Metadata

instance FromJSON Metadata where
    parseJSON = withObject "metadata" $ \o -> do
        p <- o .: "protocol"
        Metadata p
            <$> o .:  "serviceAbbreviation"
            <*> o .:  "serviceFullName"
            <*> o .:  "apiVersion"
            <*> o .:  "signatureVersion"
            <*> o .:  "endpointPrefix"
            <*> o .:? "timestampFormat" .!= defaultTimestamp p
            <*> o .:? "checksumFormat"  .!= SHA256
            <*> o .:? "jsonVersion"     .!= "1.0"
            <*> o .:? "targetPrefix"

deriveToJSON (aeson camel) ''Metadata

data API = API
    { _metadata'      :: Metadata
    , _referenceUrl   :: Text
    , _operationUrl   :: Text
    , _description    :: Text
    -- , Operations   :: HashMap Text Operation
    -- , Shapes       :: HashMap Text Shape
    , _libraryName    :: Text
    , _libraryVersion :: SemVer
    } deriving (Eq, Show)

makeLenses ''API

instance HasMetadata API where
    metadata = metadata'

instance FromJSON (SemVer -> API) where
    parseJSON = withObject "api" $ \o ->
         API <$> o .: "metadata"
             <*> o .: "referenceUrl"
             <*> o .: "operationUrl"
             <*> o .: "description"
             <*> o .: "libraryName"

instance A.ToJSON API where
    toJSON API{..} = A.Object (x <> y)
      where
        A.Object y = A.toJSON _metadata'
        A.Object x = A.object
            [ "referenceUrl"   A..= _referenceUrl
            , "operationUrl"   A..= _operationUrl
            , "description"    A..= _description
            , "libraryName"    A..= _libraryName
            , "libraryVersion" A..= _libraryVersion
            ]
