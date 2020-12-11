{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event source mapping. You can change the function that AWS Lambda invokes, or pause invocation and resume later from the same location.
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
module Network.AWS.Lambda.UpdateEventSourceMapping
  ( -- * Creating a request
    UpdateEventSourceMapping (..),
    mkUpdateEventSourceMapping,

    -- ** Request lenses
    uesmEnabled,
    uesmBisectBatchOnFunctionError,
    uesmParallelizationFactor,
    uesmMaximumRetryAttempts,
    uesmBatchSize,
    uesmMaximumBatchingWindowInSeconds,
    uesmSourceAccessConfigurations,
    uesmMaximumRecordAgeInSeconds,
    uesmFunctionName,
    uesmDestinationConfig,
    uesmUUId,

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

-- | /See:/ 'mkUpdateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
  { enabled ::
      Lude.Maybe Lude.Bool,
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
    functionName :: Lude.Maybe Lude.Text,
    destinationConfig ::
      Lude.Maybe DestinationConfig,
    uUId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateEventSourceMapping' with the minimum fields required to make a request.
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
-- * 'sourceAccessConfigurations' - (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
-- * 'uUId' - The identifier of the event source mapping.
mkUpdateEventSourceMapping ::
  -- | 'uUId'
  Lude.Text ->
  UpdateEventSourceMapping
mkUpdateEventSourceMapping pUUId_ =
  UpdateEventSourceMapping'
    { enabled = Lude.Nothing,
      bisectBatchOnFunctionError = Lude.Nothing,
      parallelizationFactor = Lude.Nothing,
      maximumRetryAttempts = Lude.Nothing,
      batchSize = Lude.Nothing,
      maximumBatchingWindowInSeconds = Lude.Nothing,
      sourceAccessConfigurations = Lude.Nothing,
      maximumRecordAgeInSeconds = Lude.Nothing,
      functionName = Lude.Nothing,
      destinationConfig = Lude.Nothing,
      uUId = pUUId_
    }

-- | If true, the event source mapping is active. Set to false to pause polling and invocation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmEnabled :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Bool)
uesmEnabled = Lens.lens (enabled :: UpdateEventSourceMapping -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | (Streams) If the function returns an error, split the batch in two and retry.
--
-- /Note:/ Consider using 'bisectBatchOnFunctionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmBisectBatchOnFunctionError :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Bool)
uesmBisectBatchOnFunctionError = Lens.lens (bisectBatchOnFunctionError :: UpdateEventSourceMapping -> Lude.Maybe Lude.Bool) (\s a -> s {bisectBatchOnFunctionError = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmBisectBatchOnFunctionError "Use generic-lens or generic-optics with 'bisectBatchOnFunctionError' instead." #-}

-- | (Streams) The number of batches to process from each shard concurrently.
--
-- /Note:/ Consider using 'parallelizationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmParallelizationFactor :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Natural)
uesmParallelizationFactor = Lens.lens (parallelizationFactor :: UpdateEventSourceMapping -> Lude.Maybe Lude.Natural) (\s a -> s {parallelizationFactor = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmParallelizationFactor "Use generic-lens or generic-optics with 'parallelizationFactor' instead." #-}

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmMaximumRetryAttempts :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Int)
uesmMaximumRetryAttempts = Lens.lens (maximumRetryAttempts :: UpdateEventSourceMapping -> Lude.Maybe Lude.Int) (\s a -> s {maximumRetryAttempts = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

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
uesmBatchSize :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Natural)
uesmBatchSize = Lens.lens (batchSize :: UpdateEventSourceMapping -> Lude.Maybe Lude.Natural) (\s a -> s {batchSize = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- /Note:/ Consider using 'maximumBatchingWindowInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmMaximumBatchingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Natural)
uesmMaximumBatchingWindowInSeconds = Lens.lens (maximumBatchingWindowInSeconds :: UpdateEventSourceMapping -> Lude.Maybe Lude.Natural) (\s a -> s {maximumBatchingWindowInSeconds = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmMaximumBatchingWindowInSeconds "Use generic-lens or generic-optics with 'maximumBatchingWindowInSeconds' instead." #-}

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'sourceAccessConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmSourceAccessConfigurations :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe (Lude.NonEmpty SourceAccessConfiguration))
uesmSourceAccessConfigurations = Lens.lens (sourceAccessConfigurations :: UpdateEventSourceMapping -> Lude.Maybe (Lude.NonEmpty SourceAccessConfiguration)) (\s a -> s {sourceAccessConfigurations = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmSourceAccessConfigurations "Use generic-lens or generic-optics with 'sourceAccessConfigurations' instead." #-}

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1).
--
-- /Note:/ Consider using 'maximumRecordAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmMaximumRecordAgeInSeconds :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Int)
uesmMaximumRecordAgeInSeconds = Lens.lens (maximumRecordAgeInSeconds :: UpdateEventSourceMapping -> Lude.Maybe Lude.Int) (\s a -> s {maximumRecordAgeInSeconds = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmMaximumRecordAgeInSeconds "Use generic-lens or generic-optics with 'maximumRecordAgeInSeconds' instead." #-}

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
uesmFunctionName :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe Lude.Text)
uesmFunctionName = Lens.lens (functionName :: UpdateEventSourceMapping -> Lude.Maybe Lude.Text) (\s a -> s {functionName = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmDestinationConfig :: Lens.Lens' UpdateEventSourceMapping (Lude.Maybe DestinationConfig)
uesmDestinationConfig = Lens.lens (destinationConfig :: UpdateEventSourceMapping -> Lude.Maybe DestinationConfig) (\s a -> s {destinationConfig = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uUId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmUUId :: Lens.Lens' UpdateEventSourceMapping Lude.Text
uesmUUId = Lens.lens (uUId :: UpdateEventSourceMapping -> Lude.Text) (\s a -> s {uUId = a} :: UpdateEventSourceMapping)
{-# DEPRECATED uesmUUId "Use generic-lens or generic-optics with 'uUId' instead." #-}

instance Lude.AWSRequest UpdateEventSourceMapping where
  type Rs UpdateEventSourceMapping = EventSourceMappingConfiguration
  request = Req.putJSON lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders UpdateEventSourceMapping where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateEventSourceMapping where
  toJSON UpdateEventSourceMapping' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
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
            ("FunctionName" Lude..=) Lude.<$> functionName,
            ("DestinationConfig" Lude..=) Lude.<$> destinationConfig
          ]
      )

instance Lude.ToPath UpdateEventSourceMapping where
  toPath UpdateEventSourceMapping' {..} =
    Lude.mconcat
      ["/2015-03-31/event-source-mappings/", Lude.toBS uUId]

instance Lude.ToQuery UpdateEventSourceMapping where
  toQuery = Lude.const Lude.mempty
