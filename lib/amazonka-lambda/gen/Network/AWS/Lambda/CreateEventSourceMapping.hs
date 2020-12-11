{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
-- For details about each event source type, see the following topics.
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-ddb.html Using AWS Lambda with Amazon DynamoDB>
--
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-kinesis.html Using AWS Lambda with Amazon Kinesis>
--
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html Using AWS Lambda with Amazon SQS>
--
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-mq.html Using AWS Lambda with Amazon MQ>
--
--
--     * <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html Using AWS Lambda with Amazon MSK>
--
--
-- The following error handling options are only available for stream sources (DynamoDB and Kinesis):
--
--     * @BisectBatchOnFunctionError@ - If the function returns an error, split the batch in two and retry.
--
--
--     * @DestinationConfig@ - Send discarded records to an Amazon SQS queue or Amazon SNS topic.
--
--
--     * @MaximumRecordAgeInSeconds@ - Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires
--
--
--     * @MaximumRetryAttempts@ - Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
--
--     * @ParallelizationFactor@ - Process multiple batches from each shard concurrently.
module Network.AWS.Lambda.CreateEventSourceMapping
  ( -- * Creating a request
    CreateEventSourceMapping (..),
    mkCreateEventSourceMapping,

    -- ** Request lenses
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

    -- * Destructuring the response
    EventSourceMappingConfiguration (..),
    mkEventSourceMappingConfiguration,

    -- ** Response lenses
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateEventSourceMapping' smart constructor.
data CreateEventSourceMapping = CreateEventSourceMapping'
  { startingPositionTimestamp ::
      Lude.Maybe Lude.Timestamp,
    topics ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    queues ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    enabled :: Lude.Maybe Lude.Bool,
    bisectBatchOnFunctionError ::
      Lude.Maybe Lude.Bool,
    parallelizationFactor ::
      Lude.Maybe Lude.Natural,
    maximumRetryAttempts ::
      Lude.Maybe Lude.Int,
    batchSize :: Lude.Maybe Lude.Natural,
    maximumBatchingWindowInSeconds ::
      Lude.Maybe Lude.Natural,
    sourceAccessConfigurations ::
      Lude.Maybe
        ( Lude.NonEmpty
            SourceAccessConfiguration
        ),
    maximumRecordAgeInSeconds ::
      Lude.Maybe Lude.Int,
    destinationConfig ::
      Lude.Maybe DestinationConfig,
    startingPosition ::
      Lude.Maybe EventSourcePosition,
    eventSourceARN :: Lude.Text,
    functionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateEventSourceMapping' with the minimum fields required to make a request.
--
-- * 'batchSize' - The maximum number of items to retrieve in a single batch.
--
--
--     * __Amazon Kinesis__ - Default 100. Max 10,000.
--
--
--     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.
--
--
--     * __Amazon Simple Queue Service__ - Default 10. Max 10.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max 10,000.
--
--
-- * 'bisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and retry.
-- * 'destinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
-- * 'enabled' - If true, the event source mapping is active. Set to false to pause polling and invocation.
-- * 'eventSourceARN' - The Amazon Resource Name (ARN) of the event source.
--
--
--     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
--
--
--     * __Amazon DynamoDB Streams__ - The ARN of the stream.
--
--
--     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
--
-- * 'functionName' - The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
-- * 'maximumBatchingWindowInSeconds' - (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
-- * 'maximumRecordAgeInSeconds' - (Streams) Discard records older than the specified age. The default value is infinite (-1).
-- * 'maximumRetryAttempts' - (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
-- * 'parallelizationFactor' - (Streams) The number of batches to process from each shard concurrently.
-- * 'queues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
-- * 'sourceAccessConfigurations' - (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
-- * 'startingPosition' - The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
-- * 'startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
-- * 'topics' - (MSK) The name of the Kafka topic.
mkCreateEventSourceMapping ::
  -- | 'eventSourceARN'
  Lude.Text ->
  -- | 'functionName'
  Lude.Text ->
  CreateEventSourceMapping
mkCreateEventSourceMapping pEventSourceARN_ pFunctionName_ =
  CreateEventSourceMapping'
    { startingPositionTimestamp =
        Lude.Nothing,
      topics = Lude.Nothing,
      queues = Lude.Nothing,
      enabled = Lude.Nothing,
      bisectBatchOnFunctionError = Lude.Nothing,
      parallelizationFactor = Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing,
      batchSize = Lude.Nothing,
      maximumBatchingWindowInSeconds = Lude.Nothing,
      sourceAccessConfigurations = Lude.Nothing,
      maximumRecordAgeInSeconds = Lude.Nothing,
      destinationConfig = Lude.Nothing,
      startingPosition = Lude.Nothing,
      eventSourceARN = pEventSourceARN_,
      functionName = pFunctionName_
    }

-- | With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
--
-- /Note:/ Consider using 'startingPositionTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmStartingPositionTimestamp :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Timestamp)
cesmStartingPositionTimestamp = Lens.lens (startingPositionTimestamp :: CreateEventSourceMapping -> Lude.Maybe Lude.Timestamp) (\s a -> s {startingPositionTimestamp = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmStartingPositionTimestamp "Use generic-lens or generic-optics with 'startingPositionTimestamp' instead." #-}

-- | (MSK) The name of the Kafka topic.
--
-- /Note:/ Consider using 'topics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmTopics :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe (Lude.NonEmpty Lude.Text))
cesmTopics = Lens.lens (topics :: CreateEventSourceMapping -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {topics = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmTopics "Use generic-lens or generic-optics with 'topics' instead." #-}

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmQueues :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe (Lude.NonEmpty Lude.Text))
cesmQueues = Lens.lens (queues :: CreateEventSourceMapping -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {queues = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

-- | If true, the event source mapping is active. Set to false to pause polling and invocation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmEnabled :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Bool)
cesmEnabled = Lens.lens (enabled :: CreateEventSourceMapping -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Streams) If the function returns an error, split the batch in two and retry.
--
-- /Note:/ Consider using 'bisectBatchOnFunctionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmBisectBatchOnFunctionError :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Bool)
cesmBisectBatchOnFunctionError = Lens.lens (bisectBatchOnFunctionError :: CreateEventSourceMapping -> Lude.Maybe Lude.Bool) (\s a -> s {bisectBatchOnFunctionError = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmBisectBatchOnFunctionError "Use generic-lens or generic-optics with 'bisectBatchOnFunctionError' instead." #-}

-- | (Streams) The number of batches to process from each shard concurrently.
--
-- /Note:/ Consider using 'parallelizationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmParallelizationFactor :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Natural)
cesmParallelizationFactor = Lens.lens (parallelizationFactor :: CreateEventSourceMapping -> Lude.Maybe Lude.Natural) (\s a -> s {parallelizationFactor = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmParallelizationFactor "Use generic-lens or generic-optics with 'parallelizationFactor' instead." #-}

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmMaximumRetryAttempts :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Int)
cesmMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: CreateEventSourceMapping -> Lude.Maybe Lude.Int) (\s a -> s {maximumRetryAttempts = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | The maximum number of items to retrieve in a single batch.
--
--
--     * __Amazon Kinesis__ - Default 100. Max 10,000.
--
--
--     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.
--
--
--     * __Amazon Simple Queue Service__ - Default 10. Max 10.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max 10,000.
--
--
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmBatchSize :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Natural)
cesmBatchSize = Lens.lens (batchSize :: CreateEventSourceMapping -> Lude.Maybe Lude.Natural) (\s a -> s {batchSize = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- /Note:/ Consider using 'maximumBatchingWindowInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmMaximumBatchingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Natural)
cesmMaximumBatchingWindowInSeconds = Lens.lens (maximumBatchingWindowInSeconds :: CreateEventSourceMapping -> Lude.Maybe Lude.Natural) (\s a -> s {maximumBatchingWindowInSeconds = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmMaximumBatchingWindowInSeconds "Use generic-lens or generic-optics with 'maximumBatchingWindowInSeconds' instead." #-}

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'sourceAccessConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmSourceAccessConfigurations :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe (Lude.NonEmpty SourceAccessConfiguration))
cesmSourceAccessConfigurations = Lens.lens (sourceAccessConfigurations :: CreateEventSourceMapping -> Lude.Maybe (Lude.NonEmpty SourceAccessConfiguration)) (\s a -> s {sourceAccessConfigurations = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmSourceAccessConfigurations "Use generic-lens or generic-optics with 'sourceAccessConfigurations' instead." #-}

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1).
--
-- /Note:/ Consider using 'maximumRecordAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmMaximumRecordAgeInSeconds :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe Lude.Int)
cesmMaximumRecordAgeInSeconds = Lens.lens (maximumRecordAgeInSeconds :: CreateEventSourceMapping -> Lude.Maybe Lude.Int) (\s a -> s {maximumRecordAgeInSeconds = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmMaximumRecordAgeInSeconds "Use generic-lens or generic-optics with 'maximumRecordAgeInSeconds' instead." #-}

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmDestinationConfig :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe DestinationConfig)
cesmDestinationConfig = Lens.lens (destinationConfig :: CreateEventSourceMapping -> Lude.Maybe DestinationConfig) (\s a -> s {destinationConfig = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- /Note:/ Consider using 'startingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmStartingPosition :: Lens.Lens' CreateEventSourceMapping (Lude.Maybe EventSourcePosition)
cesmStartingPosition = Lens.lens (startingPosition :: CreateEventSourceMapping -> Lude.Maybe EventSourcePosition) (\s a -> s {startingPosition = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmStartingPosition "Use generic-lens or generic-optics with 'startingPosition' instead." #-}

-- | The Amazon Resource Name (ARN) of the event source.
--
--
--     * __Amazon Kinesis__ - The ARN of the data stream or a stream consumer.
--
--
--     * __Amazon DynamoDB Streams__ - The ARN of the stream.
--
--
--     * __Amazon Simple Queue Service__ - The ARN of the queue.
--
--
--     * __Amazon Managed Streaming for Apache Kafka__ - The ARN of the cluster.
--
--
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmEventSourceARN :: Lens.Lens' CreateEventSourceMapping Lude.Text
cesmEventSourceARN = Lens.lens (eventSourceARN :: CreateEventSourceMapping -> Lude.Text) (\s a -> s {eventSourceARN = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The name of the Lambda function.
--
-- __Name formats__
--
--     * __Function name__ - @MyFunction@ .
--
--
--     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .
--
--
--     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .
--
--
--     * __Partial ARN__ - @123456789012:function:MyFunction@ .
--
--
-- The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
--
-- /Note:/ Consider using 'functionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmFunctionName :: Lens.Lens' CreateEventSourceMapping Lude.Text
cesmFunctionName = Lens.lens (functionName :: CreateEventSourceMapping -> Lude.Text) (\s a -> s {functionName = a} :: CreateEventSourceMapping)
{-# DEPRECATED cesmFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

instance Lude.AWSRequest CreateEventSourceMapping where
  type Rs CreateEventSourceMapping = EventSourceMappingConfiguration
  request = Req.postJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateEventSourceMapping where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateEventSourceMapping where
  toJSON CreateEventSourceMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("StartingPositionTimestamp" Lude..=)
              Lude.<$> startingPositionTimestamp,
            ("Topics" Lude..=) Lude.<$> topics,
            ("Queues" Lude..=) Lude.<$> queues,
            ("Enabled" Lude..=) Lude.<$> enabled,
            ("BisectBatchOnFunctionError" Lude..=)
              Lude.<$> bisectBatchOnFunctionError,
            ("ParallelizationFactor" Lude..=) Lude.<$> parallelizationFactor,
            ("MaximumRetryAttempts" Lude..=) Lude.<$> maximumRetryAttempts,
            ("BatchSize" Lude..=) Lude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Lude..=)
              Lude.<$> maximumBatchingWindowInSeconds,
            ("SourceAccessConfigurations" Lude..=)
              Lude.<$> sourceAccessConfigurations,
            ("MaximumRecordAgeInSeconds" Lude..=)
              Lude.<$> maximumRecordAgeInSeconds,
            ("DestinationConfig" Lude..=) Lude.<$> destinationConfig,
            ("StartingPosition" Lude..=) Lude.<$> startingPosition,
            Lude.Just ("EventSourceArn" Lude..= eventSourceARN),
            Lude.Just ("FunctionName" Lude..= functionName)
          ]
      )

instance Lude.ToPath CreateEventSourceMapping where
  toPath = Lude.const "/2015-03-31/event-source-mappings/"

instance Lude.ToQuery CreateEventSourceMapping where
  toQuery = Lude.const Lude.mempty
