{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Network.AWS.Error
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Error where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Types
import           Network.HTTP.Types

statusSuccess :: Status -> Bool
statusSuccess (statusCode -> n) = n >= 200 && n < 400

data ErrorType
    = Receiver
    | Sender
      deriving (Eq, Ord, Enum, Show, Generic)

instance FromText ErrorType where
    parser = match "Receiver" Receiver
         <|> match "Sender"   Sender

instance FromXML ErrorType where
    parseXML = parseXMLText "Type"

data RESTMessage = RESTMessage
    { _msgType    :: !ErrorType
    , _msgCode    :: Text
    , _msgRESTMessage :: Text
    } deriving (Eq, Ord, Show, Generic)

instance FromXML RESTMessage where
    parseXML x = RESTMessage
        <$> x .@ "Type"
        <*> x .@ "Code"
        <*> x .@ "Message"

data RESTError = RESTError
    { _errError     :: RESTMessage
    , _errRequestId :: Text
    } deriving (Eq, Show, Generic)

instance FromXML RESTError where
    parseXML x = RESTError
        <$> x .@ "Error"
        <*> x .@ "RequestId"

restError :: FromXML (Er a)
          => (Status -> Bool)
          -> Service a
          -> Status
          -> Maybe (LBS.ByteString -> ServiceError (Er a))
restError f Service{..} s
    | f s       = Nothing
    | otherwise = Just (either failure success . (decodeXML >=> parseXML))
  where
    success = ServiceError _svcAbbrev s
    failure = SerializerError _svcAbbrev

-- cloudfront
-- autoscaling

-- <ErrorResponse xmlns="http://cloudfront.amazonaws.com/doc/2014-10-21/">
--    <Error>
--       <Type>Sender</Type>
--       <Code>InvalidURI</Code>
--       <Message>Could not parse the specified URI.</Message>
--    </Error>
--    <RequestId>410c2a4b-e435-49c9-8382-3770d80d7d4c</RequestId>
-- </ErrorResponse>

data JSONError = JSONError
    { _errType    :: Text
    , _errMessage :: Text
    } deriving (Eq, Show, Generic)

instance FromJSON JSONError where
    parseJSON = withObject "JSONError" $ \o ->
        JSONError
            <$> o .: "__type"
            <*> o .: "message"

jsonError :: FromJSON (Er a)
          => (Status -> Bool)
          -> Service a
          -> Status
          -> Maybe (LBS.ByteString -> ServiceError (Er a))
jsonError f Service{..} s
    | f s       = Nothing
    | otherwise = Just (either failure success . eitherDecode')
  where
    success = ServiceError _svcAbbrev s
    failure = SerializerError _svcAbbrev

-- {"__type":"ResourceNotFoundException","message":"Unable to find instance with ID 1"}
