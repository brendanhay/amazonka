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

-- Module      : Network.AWS.Lambda.CreateEventSourceMapping
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

-- | Identifies a stream as an event source for a Lambda function. It can be
-- either an Amazon Kinesis stream or an Amazon DynamoDB stream. AWS Lambda
-- invokes the specified function when records are posted to the stream.
--
-- This is the pull model, where AWS Lambda invokes the function. For more
-- information, go to <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works> in the /AWS Lambda Developer Guide/.
--
-- This association between an Amazon Kinesis stream and a Lambda function is
-- called the event source mapping. You provide the configuration information
-- (for example, which stream to read from and which Lambda function to invoke)
-- for the event source mapping in the request body.
--
-- Each event source, such as an Amazon Kinesis or a DynamoDB stream, can be
-- associated with multiple AWS Lambda function. A given Lambda function can be
-- associated with multiple AWS event sources.
--
-- This operation requires permission for the 'lambda:CreateEventSourceMapping'
-- action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_CreateEventSourceMapping.html>
module Network.AWS.Lambda.CreateEventSourceMapping
    (
    -- * Request
      CreateEventSourceMapping
    -- ** Request constructor
    , createEventSourceMapping
    -- ** Request lenses
    , cesmBatchSize
    , cesmEnabled
    , cesmEventSourceArn
    , cesmFunctionName
    , cesmStartingPosition

    -- * Response
    , CreateEventSourceMappingResponse
    -- ** Response constructor
    , createEventSourceMappingResponse
    -- ** Response lenses
    , cesmrBatchSize
    , cesmrEventSourceArn
    , cesmrFunctionArn
    , cesmrLastModified
    , cesmrLastProcessingResult
    , cesmrState
    , cesmrStateTransitionReason
    , cesmrUUID
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Lambda.Types
import qualified GHC.Exts

data CreateEventSourceMapping = CreateEventSourceMapping
    { _cesmBatchSize        :: Maybe Nat
    , _cesmEnabled          :: Maybe Bool
    , _cesmEventSourceArn   :: Text
    , _cesmFunctionName     :: Text
    , _cesmStartingPosition :: EventSourcePosition
    } deriving (Eq, Read, Show)

-- | 'CreateEventSourceMapping' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesmBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'cesmEnabled' @::@ 'Maybe' 'Bool'
--
-- * 'cesmEventSourceArn' @::@ 'Text'
--
-- * 'cesmFunctionName' @::@ 'Text'
--
-- * 'cesmStartingPosition' @::@ 'EventSourcePosition'
--
createEventSourceMapping :: Text -- ^ 'cesmEventSourceArn'
                         -> Text -- ^ 'cesmFunctionName'
                         -> EventSourcePosition -- ^ 'cesmStartingPosition'
                         -> CreateEventSourceMapping
createEventSourceMapping p1 p2 p3 = CreateEventSourceMapping
    { _cesmEventSourceArn   = p1
    , _cesmFunctionName     = p2
    , _cesmStartingPosition = p3
    , _cesmEnabled          = Nothing
    , _cesmBatchSize        = Nothing
    }

-- | The largest number of records that AWS Lambda will retrieve from your event
-- source at the time of invoking your function. Your function receives an event
-- with all the retrieved records. The default is 100 records.
cesmBatchSize :: Lens' CreateEventSourceMapping (Maybe Natural)
cesmBatchSize = lens _cesmBatchSize (\s a -> s { _cesmBatchSize = a }) . mapping _Nat

-- | Indicates whether AWS Lambda should begin polling the event source.
cesmEnabled :: Lens' CreateEventSourceMapping (Maybe Bool)
cesmEnabled = lens _cesmEnabled (\s a -> s { _cesmEnabled = a })

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the event
-- source. Any record added to this stream could cause AWS Lambda to invoke your
-- Lambda function, it depends on the 'BatchSize'. AWS Lambda POSTs the Amazon
-- Kinesis event, containing records, to your Lambda function as JSON.
cesmEventSourceArn :: Lens' CreateEventSourceMapping Text
cesmEventSourceArn =
    lens _cesmEventSourceArn (\s a -> s { _cesmEventSourceArn = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
--
-- You can specify an unqualified function name (for example, "Thumbnail") or
-- you can specify Amazon Resource Name (ARN) of the function (for example,
-- "arn:aws:lambda:us-west-2:account-id:function:ThumbNail"). AWS Lambda also
-- allows you to specify only the account ID qualifier (for example,
-- "account-id:Thumbnail"). Note that the length constraint applies only to the
-- ARN. If you specify only the function name, it is limited to 64 character in
-- length.
cesmFunctionName :: Lens' CreateEventSourceMapping Text
cesmFunctionName = lens _cesmFunctionName (\s a -> s { _cesmFunctionName = a })

-- | The position in the stream where AWS Lambda should start reading. For more
-- information, go to <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html#Kinesis-GetShardIterator-request-ShardIteratorType ShardIteratorType> in the /Amazon Kinesis API Reference/.
cesmStartingPosition :: Lens' CreateEventSourceMapping EventSourcePosition
cesmStartingPosition =
    lens _cesmStartingPosition (\s a -> s { _cesmStartingPosition = a })

data CreateEventSourceMappingResponse = CreateEventSourceMappingResponse
    { _cesmrBatchSize             :: Maybe Nat
    , _cesmrEventSourceArn        :: Maybe Text
    , _cesmrFunctionArn           :: Maybe Text
    , _cesmrLastModified          :: Maybe POSIX
    , _cesmrLastProcessingResult  :: Maybe Text
    , _cesmrState                 :: Maybe Text
    , _cesmrStateTransitionReason :: Maybe Text
    , _cesmrUUID                  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'CreateEventSourceMappingResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesmrBatchSize' @::@ 'Maybe' 'Natural'
--
-- * 'cesmrEventSourceArn' @::@ 'Maybe' 'Text'
--
-- * 'cesmrFunctionArn' @::@ 'Maybe' 'Text'
--
-- * 'cesmrLastModified' @::@ 'Maybe' 'UTCTime'
--
-- * 'cesmrLastProcessingResult' @::@ 'Maybe' 'Text'
--
-- * 'cesmrState' @::@ 'Maybe' 'Text'
--
-- * 'cesmrStateTransitionReason' @::@ 'Maybe' 'Text'
--
-- * 'cesmrUUID' @::@ 'Maybe' 'Text'
--
createEventSourceMappingResponse :: CreateEventSourceMappingResponse
createEventSourceMappingResponse = CreateEventSourceMappingResponse
    { _cesmrUUID                  = Nothing
    , _cesmrBatchSize             = Nothing
    , _cesmrEventSourceArn        = Nothing
    , _cesmrFunctionArn           = Nothing
    , _cesmrLastModified          = Nothing
    , _cesmrLastProcessingResult  = Nothing
    , _cesmrState                 = Nothing
    , _cesmrStateTransitionReason = Nothing
    }

-- | The largest number of records that AWS Lambda will retrieve from your event
-- source at the time of invoking your function. Your function receives an event
-- with all the retrieved records.
cesmrBatchSize :: Lens' CreateEventSourceMappingResponse (Maybe Natural)
cesmrBatchSize = lens _cesmrBatchSize (\s a -> s { _cesmrBatchSize = a }) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
cesmrEventSourceArn :: Lens' CreateEventSourceMappingResponse (Maybe Text)
cesmrEventSourceArn =
    lens _cesmrEventSourceArn (\s a -> s { _cesmrEventSourceArn = a })

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream.
cesmrFunctionArn :: Lens' CreateEventSourceMappingResponse (Maybe Text)
cesmrFunctionArn = lens _cesmrFunctionArn (\s a -> s { _cesmrFunctionArn = a })

-- | The UTC time string indicating the last time the event mapping was updated.
cesmrLastModified :: Lens' CreateEventSourceMappingResponse (Maybe UTCTime)
cesmrLastModified =
    lens _cesmrLastModified (\s a -> s { _cesmrLastModified = a })
        . mapping _Time

-- | The result of the last AWS Lambda invocation of your Lambda function.
cesmrLastProcessingResult :: Lens' CreateEventSourceMappingResponse (Maybe Text)
cesmrLastProcessingResult =
    lens _cesmrLastProcessingResult
        (\s a -> s { _cesmrLastProcessingResult = a })

-- | The state of the event source mapping. It can be "Creating", "Enabled",
-- "Disabled", "Enabling", "Disabling", "Updating", or "Deleting".
cesmrState :: Lens' CreateEventSourceMappingResponse (Maybe Text)
cesmrState = lens _cesmrState (\s a -> s { _cesmrState = a })

-- | The reason the event source mapping is in its current state. It is either
-- user-requested or an AWS Lambda-initiated state transition.
cesmrStateTransitionReason :: Lens' CreateEventSourceMappingResponse (Maybe Text)
cesmrStateTransitionReason =
    lens _cesmrStateTransitionReason
        (\s a -> s { _cesmrStateTransitionReason = a })

-- | The AWS Lambda assigned opaque identifier for the mapping.
cesmrUUID :: Lens' CreateEventSourceMappingResponse (Maybe Text)
cesmrUUID = lens _cesmrUUID (\s a -> s { _cesmrUUID = a })

instance ToPath CreateEventSourceMapping where
    toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery CreateEventSourceMapping where
    toQuery = const mempty

instance ToHeaders CreateEventSourceMapping

instance ToJSON CreateEventSourceMapping where
    toJSON CreateEventSourceMapping{..} = object
        [ "EventSourceArn"   .= _cesmEventSourceArn
        , "FunctionName"     .= _cesmFunctionName
        , "Enabled"          .= _cesmEnabled
        , "BatchSize"        .= _cesmBatchSize
        , "StartingPosition" .= _cesmStartingPosition
        ]

instance AWSRequest CreateEventSourceMapping where
    type Sv CreateEventSourceMapping = Lambda
    type Rs CreateEventSourceMapping = CreateEventSourceMappingResponse

    request  = post
    response = jsonResponse

instance FromJSON CreateEventSourceMappingResponse where
    parseJSON = withObject "CreateEventSourceMappingResponse" $ \o -> CreateEventSourceMappingResponse
        <$> o .:? "BatchSize"
        <*> o .:? "EventSourceArn"
        <*> o .:? "FunctionArn"
        <*> o .:? "LastModified"
        <*> o .:? "LastProcessingResult"
        <*> o .:? "State"
        <*> o .:? "StateTransitionReason"
        <*> o .:? "UUID"
