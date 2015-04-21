{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
import           Control.Lens
import           Data.Jason
import           Data.Text    (Text)
import qualified Data.Text    as Text

data Signature
    = V2
    | V3
    | V3HTTPS
    | V4
    | S3
      deriving (Eq, Show)

deriveFromJSON lower ''Signature

data Protocol
    = JSON
    | RestJSON
    | XML
    | RestXML
    | Query
    | EC2
      deriving (Eq, Show)

deriveFromJSON spinal ''Protocol

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

deriveFromJSON lower ''Checksum

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

data API = API
    { _libraryName  :: Text
    , _description  :: Text
    , _referenceUrl :: Text
    , _operationUrl :: Text
    , _metadata'    :: Metadata
    -- , Operations  :: HashMap Text Operation
    -- , Shapes      :: HashMap Text Shape
    } deriving (Eq, Show)

makeLenses ''API

instance HasMetadata API where
    metadata = metadata'

deriveFromJSON defaults ''API

-- data HTTP = HTTP
--     { _hMethod     :: !Method
--     , _hRequestUri :: URI
--     } deriving (Eq, Show)

-- data Ref = Ref
--     { _refShape         :: !Text
--     , _refDocumentation :: Maybe Text
--     , _refLocation      :: Maybe Location
--     , _refLocationName  :: Maybe Text
--     , _refStreaming     :: Maybe Bool
--     , _refException     :: Maybe Bool
--     , _refFault         :: Maybe Bool
--     , _refResultWrapper :: Maybe Text
--     , _refWrapper       :: Maybe Bool
--     , _refFlattened     :: Maybe Bool
--     } deriving (Eq, Show)

-- data Operation = Operation
--     { _oName             :: !Text
--     , _oDocumentation    :: Maybe Text
--     , _oDocumentationUrl :: Maybe Text
--     , _oHttp             :: HTTP
--     , _oInput            :: Maybe Ref
--     , _oOutput           :: Maybe Ref
--     , _oErrors           :: Maybe [Ref]
--     } deriving (Eq, Show)

-- data XmlNamespace = XmlNamespace
--     { _xnsPrefix :: !Text
--     , _xnsUri    :: !Text
--     } deriving (Eq, Show)

-- data Shape
--     = List'   SList
--     | Struct' SStruct
--     | Map'    SMap
--     | String' SString
--     | Int'    (SNum Int)
--     | Long'   (SNum Integer)
--     | Double' (SNum Double)
--     | Bool'   SBool
--     | Time'   STime
--     | Blob'   SBlob
--       deriving (Eq, Show)

-- data Metadata = Metadata
--     { _mServiceFullName     :: !Text
--     , _mServiceAbbreviation :: !Abbrev
--     , _mApiVersion          :: !Text
--     , _mEndpointPrefix      :: !Text
--     , _mGlobalEndpoint      :: Maybe Text
--     , _mSignatureVersion    :: !Signature
--     , _mXmlNamespace        :: Maybe Text
--     , _mTargetPrefix        :: Maybe Text
--     , _mJsonVersion         :: Maybe Text
--     , _mTimestampFormat     :: Maybe Timestamp
--     , _mChecksumFormat      :: Maybe Checksum
--     , _mProtocol            :: !Protocol
--     } deriving (Eq, Show)
