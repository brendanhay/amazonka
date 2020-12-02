{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateEventSourceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mapping between an event source and an AWS Lambda function. Lambda reads items from the event source and triggers the function.
--
--
-- For details about each event source type, see the following topics.
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-ddb.html Using AWS Lambda with Amazon DynamoDB>
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-kinesis.html Using AWS Lambda with Amazon Kinesis>
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html Using AWS Lambda with Amazon SQS>
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-mq.html Using AWS Lambda with Amazon MQ>
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html Using AWS Lambda with Amazon MSK>
--
--
--
-- The following error handling options are only available for stream sources (DynamoDB and Kinesis):
--
--     * @BisectBatchOnFunctionError@ - If the function returns an error, split the batch in two and retry.
--
--     * @DestinationConfig@ - Send discarded records to an Amazon SQS queue or Amazon SNS topic.
--
--     * @MaximumRecordAgeInSeconds@ - Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires
--
--     * @MaximumRetryAttempts@ - Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
--     * @ParallelizationFactor@ - Process multiple batches from each shard concurrently.
module Network.AWS.Lambda.CreateEventSourceMapping
  ( -- * Creating a Request
    createEventSourceMapping,
    CreateEventSourceMapping,

    -- * Request Lenses
    cesmStartingPositionTimestamp,
    cesmTopics,
    cesmQueues,
    cesmEnabled,
    cesmBisectBatchOnFunctionError,
    cesmParallelizationFactor,
    cesmMaximumRetryAttempts,
    cesmBatchSize,
    cesmMaximumBatchingWindowInSeconds,
    cesmSourceAccessConfigurations,
    cesmMaximumRecordAgeInSeconds,
    cesmDestinationConfig,
    cesmStartingPosition,
    cesmEventSourceARN,
    cesmFunctionName,

    -- * Destructuring the Response
    eventSourceMappingConfiguration,
    EventSourceMappingConfiguration,

    -- * Response Lenses
    esmcEventSourceARN,
    esmcState,
    esmcStartingPositionTimestamp,
    esmcFunctionARN,
    esmcTopics,
    esmcQueues,
    esmcBisectBatchOnFunctionError,
    esmcUUId,
    esmcParallelizationFactor,
    esmcLastProcessingResult,
    esmcMaximumRetryAttempts,
    esmcBatchSize,
    esmcStateTransitionReason,
    esmcMaximumBatchingWindowInSeconds,
    esmcSourceAccessConfigurations,
    esmcMaximumRecordAgeInSeconds,
    esmcLastModified,
    esmcDestinationConfig,
    esmcStartingPosition,
  )
where

import Network.AWS.Lambda.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEventSourceMapping' smart constructor.
data CreateEventSourceMapping = CreateEventSourceMapping'
  { _cesmStartingPositionTimestamp ::
      !(Maybe POSIX),
    _cesmTopics :: !(Maybe (List1 Text)),
    _cesmQueues :: !(Maybe (List1 Text)),
    _cesmEnabled :: !(Maybe Bool),
    _cesmBisectBatchOnFunctionError ::
      !(Maybe Bool),
    _cesmParallelizationFactor ::
      !(Maybe Nat),
    _cesmMaximumRetryAttempts :: !(Maybe Int),
    _cesmBatchSize :: !(Maybe Nat),
    _cesmMaximumBatchingWindowInSeconds ::
      !(Maybe Nat),
    _cesmSourceAccessConfigurations ::
      !( Maybe
           (List1 SourceAccessConfiguration)
       ),
    _cesmMaximumRecordAgeInSeconds ::
      !(Maybe Int),
    _cesmDestinationConfig ::
      !(Maybe DestinationConfig),
    _cesmStartingPosition ::
      !(Maybe EventSourcePosition),
    _cesmEventSourceARN :: !Text,
    _cesmFunctionName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cesmStartingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
--
-- * 'cesmTopics' - (MSK) The name of the Kafka topic.
--
-- * 'cesmQueues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- * 'cesmEnabled' - If true, the event source mapping is active. Set to false to pause polling and invocation.
--
-- * 'cesmBisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and retry.
--
-- * 'cesmParallelizationFactor' - (Streams) The number of batches to process from each shard concurrently.
--
-- * 'cesmMaximumRetryAttempts' - (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
--
-- * 'cesmBatchSize' - The maximum number of items to retrieve in a single batch.     * __Amazon Kinesis__ - Default 100. Max 10,000.     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.     * __Amazon Simple Queue Service__ - Default 10. Max 10.     * __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max 10,000.
--
-- * 'cesmMaximumBatchingWindowInSeconds' - (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- * 'cesmSourceAccessConfigurations' - (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@  To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- * 'cesmMaximumRecordAgeInSeconds' - (Streams) Discard records older than the specified age. The default value is infinite (-1).
--
-- * 'cesmDestinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- * 'cesmStartingPosition' - The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- * 'cesmEventSourceARN' - The Amazon Resource Name (ARN) of the event source.     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.     * __Amazon DynamoDB Streams__ - The ARN of the stream.     * __Amazon Simple Queue Service__ - The ARN of the queue.     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
-- * 'cesmFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
createEventSourceMapping ::
  -- | 'cesmEventSourceARN'
  Text ->
  -- | 'cesmFunctionName'
  Text ->
  CreateEventSourceMapping
createEventSourceMapping pEventSourceARN_ pFunctionName_ =
  CreateEventSourceMapping'
    { _cesmStartingPositionTimestamp =
        Nothing,
      _cesmTopics = Nothing,
      _cesmQueues = Nothing,
      _cesmEnabled = Nothing,
      _cesmBisectBatchOnFunctionError = Nothing,
      _cesmParallelizationFactor = Nothing,
      _cesmMaximumRetryAttempts = Nothing,
      _cesmBatchSize = Nothing,
      _cesmMaximumBatchingWindowInSeconds = Nothing,
      _cesmSourceAccessConfigurations = Nothing,
      _cesmMaximumRecordAgeInSeconds = Nothing,
      _cesmDestinationConfig = Nothing,
      _cesmStartingPosition = Nothing,
      _cesmEventSourceARN = pEventSourceARN_,
      _cesmFunctionName = pFunctionName_
    }

-- | With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
cesmStartingPositionTimestamp :: Lens' CreateEventSourceMapping (Maybe UTCTime)
cesmStartingPositionTimestamp = lens _cesmStartingPositionTimestamp (\s a -> s {_cesmStartingPositionTimestamp = a}) . mapping _Time

-- | (MSK) The name of the Kafka topic.
cesmTopics :: Lens' CreateEventSourceMapping (Maybe (NonEmpty Text))
cesmTopics = lens _cesmTopics (\s a -> s {_cesmTopics = a}) . mapping _List1

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
cesmQueues :: Lens' CreateEventSourceMapping (Maybe (NonEmpty Text))
cesmQueues = lens _cesmQueues (\s a -> s {_cesmQueues = a}) . mapping _List1

-- | If true, the event source mapping is active. Set to false to pause polling and invocation.
cesmEnabled :: Lens' CreateEventSourceMapping (Maybe Bool)
cesmEnabled = lens _cesmEnabled (\s a -> s {_cesmEnabled = a})

-- | (Streams) If the function returns an error, split the batch in two and retry.
cesmBisectBatchOnFunctionError :: Lens' CreateEventSourceMapping (Maybe Bool)
cesmBisectBatchOnFunctionError = lens _cesmBisectBatchOnFunctionError (\s a -> s {_cesmBisectBatchOnFunctionError = a})

-- | (Streams) The number of batches to process from each shard concurrently.
cesmParallelizationFactor :: Lens' CreateEventSourceMapping (Maybe Natural)
cesmParallelizationFactor = lens _cesmParallelizationFactor (\s a -> s {_cesmParallelizationFactor = a}) . mapping _Nat

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
cesmMaximumRetryAttempts :: Lens' CreateEventSourceMapping (Maybe Int)
cesmMaximumRetryAttempts = lens _cesmMaximumRetryAttempts (\s a -> s {_cesmMaximumRetryAttempts = a})

-- | The maximum number of items to retrieve in a single batch.     * __Amazon Kinesis__ - Default 100. Max 10,000.     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.     * __Amazon Simple Queue Service__ - Default 10. Max 10.     * __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max 10,000.
cesmBatchSize :: Lens' CreateEventSourceMapping (Maybe Natural)
cesmBatchSize = lens _cesmBatchSize (\s a -> s {_cesmBatchSize = a}) . mapping _Nat

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
cesmMaximumBatchingWindowInSeconds :: Lens' CreateEventSourceMapping (Maybe Natural)
cesmMaximumBatchingWindowInSeconds = lens _cesmMaximumBatchingWindowInSeconds (\s a -> s {_cesmMaximumBatchingWindowInSeconds = a}) . mapping _Nat

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@  To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@  The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
cesmSourceAccessConfigurations :: Lens' CreateEventSourceMapping (Maybe (NonEmpty SourceAccessConfiguration))
cesmSourceAccessConfigurations = lens _cesmSourceAccessConfigurations (\s a -> s {_cesmSourceAccessConfigurations = a}) . mapping _List1

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1).
cesmMaximumRecordAgeInSeconds :: Lens' CreateEventSourceMapping (Maybe Int)
cesmMaximumRecordAgeInSeconds = lens _cesmMaximumRecordAgeInSeconds (\s a -> s {_cesmMaximumRecordAgeInSeconds = a})

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
cesmDestinationConfig :: Lens' CreateEventSourceMapping (Maybe DestinationConfig)
cesmDestinationConfig = lens _cesmDestinationConfig (\s a -> s {_cesmDestinationConfig = a})

-- | The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
cesmStartingPosition :: Lens' CreateEventSourceMapping (Maybe EventSourcePosition)
cesmStartingPosition = lens _cesmStartingPosition (\s a -> s {_cesmStartingPosition = a})

-- | The Amazon Resource Name (ARN) of the event source.     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.     * __Amazon DynamoDB Streams__ - The ARN of the stream.     * __Amazon Simple Queue Service__ - The ARN of the queue.     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
cesmEventSourceARN :: Lens' CreateEventSourceMapping Text
cesmEventSourceARN = lens _cesmEventSourceARN (\s a -> s {_cesmEventSourceARN = a})

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
cesmFunctionName :: Lens' CreateEventSourceMapping Text
cesmFunctionName = lens _cesmFunctionName (\s a -> s {_cesmFunctionName = a})

instance AWSRequest CreateEventSourceMapping where
  type Rs CreateEventSourceMapping = EventSourceMappingConfiguration
  request = postJSON lambda
  response = receiveJSON (\s h x -> eitherParseJSON x)

instance Hashable CreateEventSourceMapping

instance NFData CreateEventSourceMapping

instance ToHeaders CreateEventSourceMapping where
  toHeaders = const mempty

instance ToJSON CreateEventSourceMapping where
  toJSON CreateEventSourceMapping' {..} =
    object
      ( catMaybes
          [ ("StartingPositionTimestamp" .=)
              <$> _cesmStartingPositionTimestamp,
            ("Topics" .=) <$> _cesmTopics,
            ("Queues" .=) <$> _cesmQueues,
            ("Enabled" .=) <$> _cesmEnabled,
            ("BisectBatchOnFunctionError" .=)
              <$> _cesmBisectBatchOnFunctionError,
            ("ParallelizationFactor" .=) <$> _cesmParallelizationFactor,
            ("MaximumRetryAttempts" .=) <$> _cesmMaximumRetryAttempts,
            ("BatchSize" .=) <$> _cesmBatchSize,
            ("MaximumBatchingWindowInSeconds" .=)
              <$> _cesmMaximumBatchingWindowInSeconds,
            ("SourceAccessConfigurations" .=)
              <$> _cesmSourceAccessConfigurations,
            ("MaximumRecordAgeInSeconds" .=)
              <$> _cesmMaximumRecordAgeInSeconds,
            ("DestinationConfig" .=) <$> _cesmDestinationConfig,
            ("StartingPosition" .=) <$> _cesmStartingPosition,
            Just ("EventSourceArn" .= _cesmEventSourceARN),
            Just ("FunctionName" .= _cesmFunctionName)
          ]
      )

instance ToPath CreateEventSourceMapping where
  toPath = const "/2015-03-31/event-source-mappings/"

instance ToQuery CreateEventSourceMapping where
  toQuery = const mempty
