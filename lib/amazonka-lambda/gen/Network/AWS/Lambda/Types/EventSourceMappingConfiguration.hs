-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourceMappingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EventSourceMappingConfiguration
  ( EventSourceMappingConfiguration (..),

    -- * Smart constructor
    mkEventSourceMappingConfiguration,

    -- * Lenses
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

import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lambda.Types.EventSourcePosition
import Network.AWS.Lambda.Types.SourceAccessConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A mapping between an AWS resource and an AWS Lambda function. See 'CreateEventSourceMapping' for details.
--
-- /See:/ 'mkEventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { eventSourceARN ::
      Lude.Maybe Lude.Text,
    state ::
      Lude.Maybe Lude.Text,
    startingPositionTimestamp ::
      Lude.Maybe Lude.Timestamp,
    functionARN ::
      Lude.Maybe Lude.Text,
    topics ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
    queues ::
      Lude.Maybe
        (Lude.NonEmpty Lude.Text),
    bisectBatchOnFunctionError ::
      Lude.Maybe Lude.Bool,
    uUId ::
      Lude.Maybe Lude.Text,
    parallelizationFactor ::
      Lude.Maybe Lude.Natural,
    lastProcessingResult ::
      Lude.Maybe Lude.Text,
    maximumRetryAttempts ::
      Lude.Maybe Lude.Int,
    batchSize ::
      Lude.Maybe Lude.Natural,
    stateTransitionReason ::
      Lude.Maybe Lude.Text,
    maximumBatchingWindowInSeconds ::
      Lude.Maybe Lude.Natural,
    sourceAccessConfigurations ::
      Lude.Maybe
        ( Lude.NonEmpty
            SourceAccessConfiguration
        ),
    maximumRecordAgeInSeconds ::
      Lude.Maybe Lude.Int,
    lastModified ::
      Lude.Maybe Lude.Timestamp,
    destinationConfig ::
      Lude.Maybe
        DestinationConfig,
    startingPosition ::
      Lude.Maybe
        EventSourcePosition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EventSourceMappingConfiguration' with the minimum fields required to make a request.
--
-- * 'batchSize' - The maximum number of items to retrieve in a single batch.
-- * 'bisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and retry. The default value is false.
-- * 'destinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
-- * 'eventSourceARN' - The Amazon Resource Name (ARN) of the event source.
-- * 'functionARN' - The ARN of the Lambda function.
-- * 'lastModified' - The date that the event source mapping was last updated, or its state changed.
-- * 'lastProcessingResult' - The result of the last AWS Lambda invocation of your Lambda function.
-- * 'maximumBatchingWindowInSeconds' - (Streams) The maximum amount of time to gather records before invoking the function, in seconds. The default value is zero.
-- * 'maximumRecordAgeInSeconds' - (Streams) Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
-- * 'maximumRetryAttempts' - (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
-- * 'parallelizationFactor' - (Streams) The number of batches to process from each shard concurrently. The default value is 1.
-- * 'queues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
-- * 'sourceAccessConfigurations' - (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
-- * 'startingPosition' - The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
-- * 'startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
-- * 'state' - The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
-- * 'stateTransitionReason' - Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
-- * 'topics' - (MSK) The name of the Kafka topic to consume.
-- * 'uUId' - The identifier of the event source mapping.
mkEventSourceMappingConfiguration ::
  EventSourceMappingConfiguration
mkEventSourceMappingConfiguration =
  EventSourceMappingConfiguration'
    { eventSourceARN = Lude.Nothing,
      state = Lude.Nothing,
      startingPositionTimestamp = Lude.Nothing,
      functionARN = Lude.Nothing,
      topics = Lude.Nothing,
      queues = Lude.Nothing,
      bisectBatchOnFunctionError = Lude.Nothing,
      uUId = Lude.Nothing,
      parallelizationFactor = Lude.Nothing,
      lastProcessingResult = Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing,
      batchSize = Lude.Nothing,
      stateTransitionReason = Lude.Nothing,
      maximumBatchingWindowInSeconds = Lude.Nothing,
      sourceAccessConfigurations = Lude.Nothing,
      maximumRecordAgeInSeconds = Lude.Nothing,
      lastModified = Lude.Nothing,
      destinationConfig = Lude.Nothing,
      startingPosition = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
--
-- /Note:/ Consider using 'eventSourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcEventSourceARN :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Text)
esmcEventSourceARN = Lens.lens (eventSourceARN :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {eventSourceARN = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcEventSourceARN "Use generic-lens or generic-optics with 'eventSourceARN' instead." #-}

-- | The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcState :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Text)
esmcState = Lens.lens (state :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
--
-- /Note:/ Consider using 'startingPositionTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcStartingPositionTimestamp :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Timestamp)
esmcStartingPositionTimestamp = Lens.lens (startingPositionTimestamp :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Timestamp) (\s a -> s {startingPositionTimestamp = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcStartingPositionTimestamp "Use generic-lens or generic-optics with 'startingPositionTimestamp' instead." #-}

-- | The ARN of the Lambda function.
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcFunctionARN :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Text)
esmcFunctionARN = Lens.lens (functionARN :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionARN = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

-- | (MSK) The name of the Kafka topic to consume.
--
-- /Note:/ Consider using 'topics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcTopics :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe (Lude.NonEmpty Lude.Text))
esmcTopics = Lens.lens (topics :: EventSourceMappingConfiguration -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {topics = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcTopics "Use generic-lens or generic-optics with 'topics' instead." #-}

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcQueues :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe (Lude.NonEmpty Lude.Text))
esmcQueues = Lens.lens (queues :: EventSourceMappingConfiguration -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {queues = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcQueues "Use generic-lens or generic-optics with 'queues' instead." #-}

-- | (Streams) If the function returns an error, split the batch in two and retry. The default value is false.
--
-- /Note:/ Consider using 'bisectBatchOnFunctionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcBisectBatchOnFunctionError :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Bool)
esmcBisectBatchOnFunctionError = Lens.lens (bisectBatchOnFunctionError :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Bool) (\s a -> s {bisectBatchOnFunctionError = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcBisectBatchOnFunctionError "Use generic-lens or generic-optics with 'bisectBatchOnFunctionError' instead." #-}

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uUId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcUUId :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Text)
esmcUUId = Lens.lens (uUId :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {uUId = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcUUId "Use generic-lens or generic-optics with 'uUId' instead." #-}

-- | (Streams) The number of batches to process from each shard concurrently. The default value is 1.
--
-- /Note:/ Consider using 'parallelizationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcParallelizationFactor :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Natural)
esmcParallelizationFactor = Lens.lens (parallelizationFactor :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {parallelizationFactor = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcParallelizationFactor "Use generic-lens or generic-optics with 'parallelizationFactor' instead." #-}

-- | The result of the last AWS Lambda invocation of your Lambda function.
--
-- /Note:/ Consider using 'lastProcessingResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcLastProcessingResult :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Text)
esmcLastProcessingResult = Lens.lens (lastProcessingResult :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {lastProcessingResult = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcLastProcessingResult "Use generic-lens or generic-optics with 'lastProcessingResult' instead." #-}

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcMaximumRetryAttempts :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Int)
esmcMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {maximumRetryAttempts = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | The maximum number of items to retrieve in a single batch.
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcBatchSize :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Natural)
esmcBatchSize = Lens.lens (batchSize :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {batchSize = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcStateTransitionReason :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Text)
esmcStateTransitionReason = Lens.lens (stateTransitionReason :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {stateTransitionReason = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcStateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead." #-}

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds. The default value is zero.
--
-- /Note:/ Consider using 'maximumBatchingWindowInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcMaximumBatchingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Natural)
esmcMaximumBatchingWindowInSeconds = Lens.lens (maximumBatchingWindowInSeconds :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Natural) (\s a -> s {maximumBatchingWindowInSeconds = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcMaximumBatchingWindowInSeconds "Use generic-lens or generic-optics with 'maximumBatchingWindowInSeconds' instead." #-}

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'sourceAccessConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcSourceAccessConfigurations :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe (Lude.NonEmpty SourceAccessConfiguration))
esmcSourceAccessConfigurations = Lens.lens (sourceAccessConfigurations :: EventSourceMappingConfiguration -> Lude.Maybe (Lude.NonEmpty SourceAccessConfiguration)) (\s a -> s {sourceAccessConfigurations = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcSourceAccessConfigurations "Use generic-lens or generic-optics with 'sourceAccessConfigurations' instead." #-}

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
-- /Note:/ Consider using 'maximumRecordAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcMaximumRecordAgeInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Int)
esmcMaximumRecordAgeInSeconds = Lens.lens (maximumRecordAgeInSeconds :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {maximumRecordAgeInSeconds = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcMaximumRecordAgeInSeconds "Use generic-lens or generic-optics with 'maximumRecordAgeInSeconds' instead." #-}

-- | The date that the event source mapping was last updated, or its state changed.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcLastModified :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe Lude.Timestamp)
esmcLastModified = Lens.lens (lastModified :: EventSourceMappingConfiguration -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModified = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcDestinationConfig :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe DestinationConfig)
esmcDestinationConfig = Lens.lens (destinationConfig :: EventSourceMappingConfiguration -> Lude.Maybe DestinationConfig) (\s a -> s {destinationConfig = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- /Note:/ Consider using 'startingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcStartingPosition :: Lens.Lens' EventSourceMappingConfiguration (Lude.Maybe EventSourcePosition)
esmcStartingPosition = Lens.lens (startingPosition :: EventSourceMappingConfiguration -> Lude.Maybe EventSourcePosition) (\s a -> s {startingPosition = a} :: EventSourceMappingConfiguration)
{-# DEPRECATED esmcStartingPosition "Use generic-lens or generic-optics with 'startingPosition' instead." #-}

instance Lude.FromJSON EventSourceMappingConfiguration where
  parseJSON =
    Lude.withObject
      "EventSourceMappingConfiguration"
      ( \x ->
          EventSourceMappingConfiguration'
            Lude.<$> (x Lude..:? "EventSourceArn")
            Lude.<*> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "StartingPositionTimestamp")
            Lude.<*> (x Lude..:? "FunctionArn")
            Lude.<*> (x Lude..:? "Topics")
            Lude.<*> (x Lude..:? "Queues")
            Lude.<*> (x Lude..:? "BisectBatchOnFunctionError")
            Lude.<*> (x Lude..:? "UUID")
            Lude.<*> (x Lude..:? "ParallelizationFactor")
            Lude.<*> (x Lude..:? "LastProcessingResult")
            Lude.<*> (x Lude..:? "MaximumRetryAttempts")
            Lude.<*> (x Lude..:? "BatchSize")
            Lude.<*> (x Lude..:? "StateTransitionReason")
            Lude.<*> (x Lude..:? "MaximumBatchingWindowInSeconds")
            Lude.<*> (x Lude..:? "SourceAccessConfigurations")
            Lude.<*> (x Lude..:? "MaximumRecordAgeInSeconds")
            Lude.<*> (x Lude..:? "LastModified")
            Lude.<*> (x Lude..:? "DestinationConfig")
            Lude.<*> (x Lude..:? "StartingPosition")
      )
