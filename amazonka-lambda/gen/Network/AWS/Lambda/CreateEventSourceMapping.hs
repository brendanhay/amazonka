{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Lambda.CreateEventSourceMapping
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
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
-- information, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/lambda-introduction.html AWS Lambda: How it Works>
-- in the /AWS Lambda Developer Guide/.
--
-- This association between an Amazon Kinesis stream and a Lambda function
-- is called the event source mapping. You provide the configuration
-- information (for example, which stream to read from and which Lambda
-- function to invoke) for the event source mapping in the request body.
--
-- Each event source, such as an Amazon Kinesis or a DynamoDB stream, can
-- be associated with multiple AWS Lambda function. A given Lambda function
-- can be associated with multiple AWS event sources.
--
-- This operation requires permission for the
-- @lambda:CreateEventSourceMapping@ action.
--
-- <http://docs.aws.amazon.com/lambda/latest/dg/API_CreateEventSourceMapping.html>
module Network.AWS.Lambda.CreateEventSourceMapping
    (
    -- * Request
      CreateEventSourceMapping
    -- ** Request constructor
    , createEventSourceMapping
    -- ** Request lenses
    , cesmEnabled
    , cesmEventSourceARN
    , cesmFunctionName
    , cesmStartingPosition
    , cesmBatchSize

    -- * Response
    , EventSourceMappingConfiguration
    -- ** Response constructor
    , eventSourceMappingConfiguration
    -- ** Response lenses
    , esmcEventSourceARN
    , esmcFunctionARN
    , esmcState
    , esmcUUID
    , esmcLastProcessingResult
    , esmcStateTransitionReason
    , esmcLastModified
    , esmcBatchSize
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Lambda.Types

-- | /See:/ 'createEventSourceMapping' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cesmEnabled'
--
-- * 'cesmEventSourceARN'
--
-- * 'cesmFunctionName'
--
-- * 'cesmStartingPosition'
--
-- * 'cesmBatchSize'
data CreateEventSourceMapping = CreateEventSourceMapping'{_cesmEnabled :: Maybe Bool, _cesmEventSourceARN :: Text, _cesmFunctionName :: Text, _cesmStartingPosition :: EventSourcePosition, _cesmBatchSize :: Nat} deriving (Eq, Read, Show)

-- | 'CreateEventSourceMapping' smart constructor.
createEventSourceMapping :: Text -> Text -> EventSourcePosition -> Natural -> CreateEventSourceMapping
createEventSourceMapping pEventSourceARN pFunctionName pStartingPosition pBatchSize = CreateEventSourceMapping'{_cesmEnabled = Nothing, _cesmEventSourceARN = pEventSourceARN, _cesmFunctionName = pFunctionName, _cesmStartingPosition = pStartingPosition, _cesmBatchSize = _Nat # pBatchSize};

-- | Indicates whether AWS Lambda should begin polling the event source.
cesmEnabled :: Lens' CreateEventSourceMapping (Maybe Bool)
cesmEnabled = lens _cesmEnabled (\ s a -> s{_cesmEnabled = a});

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis or the Amazon
-- DynamoDB stream that is the event source. Any record added to this
-- stream could cause AWS Lambda to invoke your Lambda function, it depends
-- on the @BatchSize@. AWS Lambda POSTs the Amazon Kinesis event,
-- containing records, to your Lambda function as JSON.
cesmEventSourceARN :: Lens' CreateEventSourceMapping Text
cesmEventSourceARN = lens _cesmEventSourceARN (\ s a -> s{_cesmEventSourceARN = a});

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
--
-- You can specify an unqualified function name (for example,
-- \"Thumbnail\") or you can specify Amazon Resource Name (ARN) of the
-- function (for example,
-- \"arn:aws:lambda:us-west-2:account-id:function:ThumbNail\"). AWS Lambda
-- also allows you to specify only the account ID qualifier (for example,
-- \"account-id:Thumbnail\"). Note that the length constraint applies only
-- to the ARN. If you specify only the function name, it is limited to 64
-- character in length.
cesmFunctionName :: Lens' CreateEventSourceMapping Text
cesmFunctionName = lens _cesmFunctionName (\ s a -> s{_cesmFunctionName = a});

-- | The position in the stream where AWS Lambda should start reading. For
-- more information, go to
-- <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html#Kinesis-GetShardIterator-request-ShardIteratorType ShardIteratorType>
-- in the /Amazon Kinesis API Reference/.
cesmStartingPosition :: Lens' CreateEventSourceMapping EventSourcePosition
cesmStartingPosition = lens _cesmStartingPosition (\ s a -> s{_cesmStartingPosition = a});

-- | The largest number of records that AWS Lambda will retrieve from your
-- event source at the time of invoking your function. Your function
-- receives an event with all the retrieved records. The default is 100
-- records.
cesmBatchSize :: Lens' CreateEventSourceMapping Natural
cesmBatchSize = lens _cesmBatchSize (\ s a -> s{_cesmBatchSize = a}) . _Nat;

instance AWSRequest CreateEventSourceMapping where
        type Sv CreateEventSourceMapping = Lambda
        type Rs CreateEventSourceMapping =
             EventSourceMappingConfiguration
        request = postJSON
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreateEventSourceMapping where
        toHeaders = const mempty

instance ToJSON CreateEventSourceMapping where
        toJSON CreateEventSourceMapping'{..}
          = object
              ["Enabled" .= _cesmEnabled,
               "EventSourceArn" .= _cesmEventSourceARN,
               "FunctionName" .= _cesmFunctionName,
               "StartingPosition" .= _cesmStartingPosition,
               "BatchSize" .= _cesmBatchSize]

instance ToPath CreateEventSourceMapping where
        toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery CreateEventSourceMapping where
        toQuery = const mempty
