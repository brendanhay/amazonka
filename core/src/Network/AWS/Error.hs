{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Error
    (
    -- * Status Checks
      statusSuccess

    -- * Classes
    , AWSError     (..)
    , AWSErrorCode (..)

    -- * Types
    , ErrorCode    (..)
    , ErrorType    (..)
    , ServiceError (..)

    -- * REST Errors
    , RESTError
    , restRequestId
    , restType
    , restCode
    , restMessage
    , restError

    -- * JSON Errors
    , JSONError
    , jsonType
    , jsonCode
    , jsonMessage
    , jsonError
    ) where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import qualified Data.ByteString.Lazy       as LBS
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.String
import           Data.Text                  (Text)
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Types

statusSuccess :: Status -> Bool
statusSuccess (statusCode -> n) = n >= 200 && n < 400

class AWSError a where
    awsError :: a -> ServiceError String

instance Show a => AWSError (ServiceError a) where
    awsError = \case
        HttpError       e     -> HttpError e
        SerializerError a e   -> SerializerError a e
        ServiceError    a s x -> ServiceError a s (show x)
        Errors          xs    -> Errors (map awsError xs)

newtype ErrorCode = ErrorCode Text
    deriving (Eq, Ord, Show, FromXML, FromJSON, IsString, Generic)

class AWSErrorCode a where
    awsErrorCode :: a -> ErrorCode

data ErrorType
    = Receiver
    | Sender
      deriving (Eq, Ord, Enum, Show, Generic)

instance FromText ErrorType where
    parser = takeLowerText >>= \case
        "receiver" -> pure Receiver
        "sender"   -> pure Sender
        e          -> fail $ "Failure parsing ErrorType from " ++ show e

instance FromXML ErrorType where
    parseXML = parseXMLText "Type"

data RESTError = RESTError
    { _restRequestId :: Text
    , _restType      :: Maybe ErrorType
    , _restCode      :: ErrorCode
    , _restMessage   :: Text
    } deriving (Eq, Show, Generic)

makeLenses ''RESTError

instance AWSErrorCode RESTError where
    awsErrorCode = _restCode

instance FromXML RESTError where
    parseXML x = withElement "Error" f x <|> f x
      where
        f y = RESTError
            <$> x .@  "RequestId"
            <*> y .@? "Type"
            <*> y .@  "Code"
            <*> y .@  "Message"

restError :: FromXML (Er a)
          => (Status -> Bool)
          -> Service a
          -> Status
          -> Maybe (LBS.ByteString -> ServiceError (Er a))
restError f Service{..} s
    | f s       = Nothing
    | otherwise = Just go
  where
    go x = either failure success (decodeXML x >>= parseXML)
      where
        failure e = SerializerError _svcAbbrev (e ++ ":\n" ++ unpack x)
        success   = ServiceError _svcAbbrev s

data JSONError = JSONError
    { _jsonType    :: Maybe Text
    , _jsonCode    :: ErrorCode
    , _jsonMessage :: Text
    } deriving (Eq, Show, Generic)

makeLenses ''JSONError

instance AWSErrorCode JSONError where
    awsErrorCode = _jsonCode

instance FromJSON JSONError where
    parseJSON = withObject "JSONError" $ \o -> rest o <|> post o
      where
        rest o = JSONError
             <$> o .:? "Type"
             <*> o .:  "Code"
             <*> o .:  "Message"

        post o = JSONError
             <$> o .:? "__type"
             <*> o .:  "code"
             <*> o .:  "message"

jsonError :: FromJSON (Er a)
          => (Status -> Bool)
          -> Service a
          -> Status
          -> Maybe (LBS.ByteString -> ServiceError (Er a))
jsonError f Service{..} s
    | f s       = Nothing
    | otherwise = Just go
  where
    go x = either failure success (eitherDecode' x)
      where
        failure e = SerializerError _svcAbbrev (e ++ ":\n" ++ unpack x)
        success   = ServiceError _svcAbbrev s
