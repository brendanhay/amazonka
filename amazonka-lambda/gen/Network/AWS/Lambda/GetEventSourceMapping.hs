{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Lambda.GetEventSourceMapping
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns configuration information for the specified event source mapping (see 'CreateEventSourceMapping').
--
-- This operation requires permission for the 'lambda:GetEventSourceMapping'
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_GetEventSourceMapping.html>
module Network.AWS.Lambda.GetEventSourceMapping
    (
    -- * Request
      GetEventSourceMapping
    -- ** Request constructor
    , getEventSourceMapping
    -- ** Request lenses
    , gesmUUID

    -- * Response
    , GetEventSourceMappingResponse
    -- ** Response constructor
    , getEventSourceMappingResponse
    -- ** Response lenses
    , gesmrBatchSize
    , gesmrEventSourceArn
    , gesmrFunctionArn
    , gesmrLastModified
    , gesmrLastProcessingResult
    , gesmrState
    , gesmrStateTransitionReason
    , gesmrUUID
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

newtype GetEventSourceMapping = GetEventSourceMapping
    { _gesmUUID :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

-- | 'GetEventSourceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesmUUID' @::@ 'Text'
--
getEventSourceMapping :: Text -- ^ 'gesmUUID'
                      -> GetEventSourceMapping
getEventSourceMapping p1 = GetEventSourceMapping
    { _gesmUUID = p1
    }

-- | The AWS Lambda assigned ID of the event source mapping.
gesmUUID :: Lens' GetEventSourceMapping Text
gesmUUID = lens _gesmUUID (\s a -> s { _gesmUUID = a })

data GetEventSourceMappingResponse = GetEventSourceMappingResponse
    { _gesmrBatchSize             :: Maybe Nat
    , _gesmrEventSourceArn        :: Maybe Text
    , _gesmrFunctionArn           :: Maybe Text
    , _gesmrLastModified          :: Maybe POSIX
    , _gesmrLastProcessingResult  :: Maybe Text
    , _gesmrState                 :: Maybe Text
    , _gesmrStateTransitionReason :: Maybe Text
    , _gesmrUUID                  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetEventSourceMappingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gesmrBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'gesmrEventSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'gesmrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'gesmrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'gesmrLastProcessingResult' @::@ 'Maybe' 'Text'
--
-- * 'gesmrState' @::@ 'Maybe' 'Text'
--
-- * 'gesmrStateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'gesmrUUID' @::@ 'Maybe' 'Text'
--
getEventSourceMappingResponse :: GetEventSourceMappingResponse
getEventSourceMappingResponse = GetEventSourceMappingResponse
    { _gesmrUUID                  = Nothing
    , _gesmrBatchSize             = Nothing
    , _gesmrEventSourceArn        = Nothing
    , _gesmrFunctionArn           = Nothing
    , _gesmrLastModified          = Nothing
    , _gesmrLastProcessingResult  = Nothing
    , _gesmrState                 = Nothing
    , _gesmrStateTransitionReason = Nothing
    }

-- | The largest number of records that AWS Lambda will retrieve from your event
-- source at the time of invoking your function. Your function receives an event
-- with all the retrieved records.
gesmrBatchSize :: Lens' GetEventSourceMappingResponse (Maybe Natural)
gesmrBatchSize = lens _gesmrBatchSize (\s a -> s { _gesmrBatchSize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
gesmrEventSourceArn :: Lens' GetEventSourceMappingResponse (Maybe Text)
gesmrEventSourceArn =
    lens _gesmrEventSourceArn (\s a -> s { _gesmrEventSourceArn = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
gesmrFunctionArn :: Lens' GetEventSourceMappingResponse (Maybe Text)
gesmrFunctionArn = lens _gesmrFunctionArn (\s a -> s { _gesmrFunctionArn = a })

-- | The UTC time string indicating the last time the event mapping was updated.
gesmrLastModified :: Lens' GetEventSourceMappingResponse (Maybe UTCTime)
gesmrLastModified =
    lens _gesmrLastModified (\s a -> s { _gesmrLastModified = a })
        . mapping _Time

-- | The result of the last AWS Lambda invocation of your Lambda function.
gesmrLastProcessingResult :: Lens' GetEventSourceMappingResponse (Maybe Text)
gesmrLastProcessingResult =
    lens _gesmrLastProcessingResult
        (\s a -> s { _gesmrLastProcessingResult = a })

-- | The state of the event source mapping. It can be "Creating", "Enabled",
-- "Disabled", "Enabling", "Disabling", "Updating", or "Deleting".
gesmrState :: Lens' GetEventSourceMappingResponse (Maybe Text)
gesmrState = lens _gesmrState (\s a -> s { _gesmrState = a })

-- | The reason the event source mapping is in its current state. It is either
-- user-requested or an AWS Lambda-initiated state transition.
gesmrStateTransitionReason :: Lens' GetEventSourceMappingResponse (Maybe Text)
gesmrStateTransitionReason =
    lens _gesmrStateTransitionReason
        (\s a -> s { _gesmrStateTransitionReason = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
gesmrUUID :: Lens' GetEventSourceMappingResponse (Maybe Text)
gesmrUUID = lens _gesmrUUID (\s a -> s { _gesmrUUID = a })

instance ToPath GetEventSourceMapping where
    toPath GetEventSourceMapping{..} = mconcat
        [ "/2015-03-31/event-source-mappings/"
        , toText _gesmUUID
        ]

instance ToQuery GetEventSourceMapping where
    toQuery = const mempty

instance ToHeaders GetEventSourceMapping

instance ToJSON GetEventSourceMapping where
    toJSON = const (toJSON Empty)

instance AWSRequest GetEventSourceMapping where
    type Sv GetEventSourceMapping = Lambda
    type Rs GetEventSourceMapping = GetEventSourceMappingResponse

    request  = get
    response = jsonResponse

instance FromJSON GetEventSourceMappingResponse where
    parseJSON = withObject "GetEventSourceMappingResponse" $ \o -> GetEventSourceMappingResponse
        <$> o .:? "BatchSize"
        <*> o .:? "EventSourceArn"
        <*> o .:? "FunctionArn"
        <*> o .:? "LastModified"
        <*> o .:? "LastProcessingResult"
        <*> o .:? "State"
        <*> o .:? "StateTransitionReason"
        <*> o .:? "UUID"
