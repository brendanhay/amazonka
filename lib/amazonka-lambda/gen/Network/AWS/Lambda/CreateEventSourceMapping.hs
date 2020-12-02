{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateEventSourceMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Identifies a stream as an event source for a Lambda function. It can be either an Amazon Kinesis stream or an Amazon DynamoDB stream. AWS Lambda invokes the specified function when records are posted to the stream.
--
--
-- This association between a stream source and a Lambda function is called the event source mapping.
--
-- You provide mapping information (for example, which stream to read from and which Lambda function to invoke) in the request body.
--
-- Each event source, such as an Amazon Kinesis or a DynamoDB stream, can be associated with multiple AWS Lambda functions. A given Lambda function can be associated with multiple AWS event sources.
--
-- If you are using versioning, you can specify a specific function version or an alias via the function name parameter. For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases> .
--
-- This operation requires permission for the @lambda:CreateEventSourceMapping@ action.
--
module Network.AWS.Lambda.CreateEventSourceMapping
    (
    -- * Creating a Request
      createEventSourceMapping
    , CreateEventSourceMapping
    -- * Request Lenses
    , cesmStartingPositionTimestamp
    , cesmEnabled
    , cesmBatchSize
    , cesmEventSourceARN
    , cesmFunctionName
    , cesmStartingPosition

    -- * Destructuring the Response
    , eventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    -- * Response Lenses
    , esmcEventSourceARN
    , esmcState
    , esmcFunctionARN
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createEventSourceMapping' smart constructor.
data CreateEventSourceMapping = CreateEventSourceMapping'
  { _cesmStartingPositionTimestamp :: !(Maybe POSIX)
  , _cesmEnabled                   :: !(Maybe Bool)
  , _cesmBatchSize                 :: !(Maybe Nat)
  , _cesmEventSourceARN            :: !Text
  , _cesmFunctionName              :: !Text
  , _cesmStartingPosition          :: !EventSourcePosition
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesmStartingPositionTimestamp' - The timestamp of the data record from which to start reading. Used with <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html#Kinesis-GetShardIterator-request-ShardIteratorType shard iterator type> AT_TIMESTAMP. If a record with this exact timestamp does not exist, the iterator returned is for the next (later) record. If the timestamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON). Valid only for <http://docs.aws.amazon.com/streams/latest/dev/amazon-kinesis-streams.html Kinesis streams> .
--
-- * 'cesmEnabled' - Indicates whether AWS Lambda should begin polling the event source. By default, @Enabled@ is true.
--
-- * 'cesmBatchSize' - The largest number of records that AWS Lambda will retrieve from your event source at the time of invoking your function. Your function receives an event with all the retrieved records. The default is 100 records.
--
-- * 'cesmEventSourceARN' - The Amazon Resource Name (ARN) of the Amazon Kinesis or the Amazon DynamoDB stream that is the event source. Any record added to this stream could cause AWS Lambda to invoke your Lambda function, it depends on the @BatchSize@ . AWS Lambda POSTs the Amazon Kinesis event, containing records, to your Lambda function as JSON.
--
-- * 'cesmFunctionName' - The Lambda function to invoke when AWS Lambda detects an event on the stream. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ).  If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases>  AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ).  Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'cesmStartingPosition' - The position in the DynamoDB or Kinesis stream where AWS Lambda should start reading. For more information, see <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html#Kinesis-GetShardIterator-request-ShardIteratorType GetShardIterator> in the /Amazon Kinesis API Reference Guide/ or <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_streams_GetShardIterator.html GetShardIterator> in the /Amazon DynamoDB API Reference Guide/ . The @AT_TIMESTAMP@ value is supported only for <http://docs.aws.amazon.com/streams/latest/dev/amazon-kinesis-streams.html Kinesis streams> .
createEventSourceMapping
    :: Text -- ^ 'cesmEventSourceARN'
    -> Text -- ^ 'cesmFunctionName'
    -> EventSourcePosition -- ^ 'cesmStartingPosition'
    -> CreateEventSourceMapping
createEventSourceMapping pEventSourceARN_ pFunctionName_ pStartingPosition_ =
  CreateEventSourceMapping'
    { _cesmStartingPositionTimestamp = Nothing
    , _cesmEnabled = Nothing
    , _cesmBatchSize = Nothing
    , _cesmEventSourceARN = pEventSourceARN_
    , _cesmFunctionName = pFunctionName_
    , _cesmStartingPosition = pStartingPosition_
    }


-- | The timestamp of the data record from which to start reading. Used with <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html#Kinesis-GetShardIterator-request-ShardIteratorType shard iterator type> AT_TIMESTAMP. If a record with this exact timestamp does not exist, the iterator returned is for the next (later) record. If the timestamp is older than the current trim horizon, the iterator returned is for the oldest untrimmed data record (TRIM_HORIZON). Valid only for <http://docs.aws.amazon.com/streams/latest/dev/amazon-kinesis-streams.html Kinesis streams> .
cesmStartingPositionTimestamp :: Lens' CreateEventSourceMapping (Maybe UTCTime)
cesmStartingPositionTimestamp = lens _cesmStartingPositionTimestamp (\ s a -> s{_cesmStartingPositionTimestamp = a}) . mapping _Time

-- | Indicates whether AWS Lambda should begin polling the event source. By default, @Enabled@ is true.
cesmEnabled :: Lens' CreateEventSourceMapping (Maybe Bool)
cesmEnabled = lens _cesmEnabled (\ s a -> s{_cesmEnabled = a})

-- | The largest number of records that AWS Lambda will retrieve from your event source at the time of invoking your function. Your function receives an event with all the retrieved records. The default is 100 records.
cesmBatchSize :: Lens' CreateEventSourceMapping (Maybe Natural)
cesmBatchSize = lens _cesmBatchSize (\ s a -> s{_cesmBatchSize = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis or the Amazon DynamoDB stream that is the event source. Any record added to this stream could cause AWS Lambda to invoke your Lambda function, it depends on the @BatchSize@ . AWS Lambda POSTs the Amazon Kinesis event, containing records, to your Lambda function as JSON.
cesmEventSourceARN :: Lens' CreateEventSourceMapping Text
cesmEventSourceARN = lens _cesmEventSourceARN (\ s a -> s{_cesmEventSourceARN = a})

-- | The Lambda function to invoke when AWS Lambda detects an event on the stream. You can specify the function name (for example, @Thumbnail@ ) or you can specify Amazon Resource Name (ARN) of the function (for example, @arn:aws:lambda:us-west-2:account-id:function:ThumbNail@ ).  If you are using versioning, you can also provide a qualified function ARN (ARN that is qualified with function version or alias name as suffix). For more information about versioning, see <http://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html AWS Lambda Function Versioning and Aliases>  AWS Lambda also allows you to specify only the function name with the account ID qualifier (for example, @account-id:Thumbnail@ ).  Note that the length constraint applies only to the ARN. If you specify only the function name, it is limited to 64 characters in length.
cesmFunctionName :: Lens' CreateEventSourceMapping Text
cesmFunctionName = lens _cesmFunctionName (\ s a -> s{_cesmFunctionName = a})

-- | The position in the DynamoDB or Kinesis stream where AWS Lambda should start reading. For more information, see <http://docs.aws.amazon.com/kinesis/latest/APIReference/API_GetShardIterator.html#Kinesis-GetShardIterator-request-ShardIteratorType GetShardIterator> in the /Amazon Kinesis API Reference Guide/ or <http://docs.aws.amazon.com/amazondynamodb/latest/APIReference/API_streams_GetShardIterator.html GetShardIterator> in the /Amazon DynamoDB API Reference Guide/ . The @AT_TIMESTAMP@ value is supported only for <http://docs.aws.amazon.com/streams/latest/dev/amazon-kinesis-streams.html Kinesis streams> .
cesmStartingPosition :: Lens' CreateEventSourceMapping EventSourcePosition
cesmStartingPosition = lens _cesmStartingPosition (\ s a -> s{_cesmStartingPosition = a})

instance AWSRequest CreateEventSourceMapping where
        type Rs CreateEventSourceMapping =
             EventSourceMappingConfiguration
        request = postJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable CreateEventSourceMapping where

instance NFData CreateEventSourceMapping where

instance ToHeaders CreateEventSourceMapping where
        toHeaders = const mempty

instance ToJSON CreateEventSourceMapping where
        toJSON CreateEventSourceMapping'{..}
          = object
              (catMaybes
                 [("StartingPositionTimestamp" .=) <$>
                    _cesmStartingPositionTimestamp,
                  ("Enabled" .=) <$> _cesmEnabled,
                  ("BatchSize" .=) <$> _cesmBatchSize,
                  Just ("EventSourceArn" .= _cesmEventSourceARN),
                  Just ("FunctionName" .= _cesmFunctionName),
                  Just ("StartingPosition" .= _cesmStartingPosition)])

instance ToPath CreateEventSourceMapping where
        toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery CreateEventSourceMapping where
        toQuery = const mempty
