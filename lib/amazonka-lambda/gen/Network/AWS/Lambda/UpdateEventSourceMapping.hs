{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    uesmUUID,
    uesmBatchSize,
    uesmBisectBatchOnFunctionError,
    uesmDestinationConfig,
    uesmEnabled,
    uesmFunctionName,
    uesmMaximumBatchingWindowInSeconds,
    uesmMaximumRecordAgeInSeconds,
    uesmMaximumRetryAttempts,
    uesmParallelizationFactor,
    uesmSourceAccessConfigurations,

    -- * Destructuring the response
    Types.EventSourceMappingConfiguration (..),
    Types.mkEventSourceMappingConfiguration,

    -- ** Response lenses
    Types.esmcBatchSize,
    Types.esmcBisectBatchOnFunctionError,
    Types.esmcDestinationConfig,
    Types.esmcEventSourceArn,
    Types.esmcFunctionArn,
    Types.esmcLastModified,
    Types.esmcLastProcessingResult,
    Types.esmcMaximumBatchingWindowInSeconds,
    Types.esmcMaximumRecordAgeInSeconds,
    Types.esmcMaximumRetryAttempts,
    Types.esmcParallelizationFactor,
    Types.esmcQueues,
    Types.esmcSourceAccessConfigurations,
    Types.esmcStartingPosition,
    Types.esmcStartingPositionTimestamp,
    Types.esmcState,
    Types.esmcStateTransitionReason,
    Types.esmcTopics,
    Types.esmcUUID,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
  { -- | The identifier of the event source mapping.
    uuid :: Types.String,
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
    batchSize :: Core.Maybe Core.Natural,
    -- | (Streams) If the function returns an error, split the batch in two and retry.
    bisectBatchOnFunctionError :: Core.Maybe Core.Bool,
    -- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
    destinationConfig :: Core.Maybe Types.DestinationConfig,
    -- | If true, the event source mapping is active. Set to false to pause polling and invocation.
    enabled :: Core.Maybe Core.Bool,
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
    functionName :: Core.Maybe Types.FunctionName,
    -- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
    maximumBatchingWindowInSeconds :: Core.Maybe Core.Natural,
    -- | (Streams) Discard records older than the specified age. The default value is infinite (-1).
    maximumRecordAgeInSeconds :: Core.Maybe Core.Int,
    -- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
    maximumRetryAttempts :: Core.Maybe Core.Int,
    -- | (Streams) The number of batches to process from each shard concurrently.
    parallelizationFactor :: Core.Maybe Core.Natural,
    -- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
    --
    -- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
    -- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
    sourceAccessConfigurations :: Core.Maybe (Core.NonEmpty Types.SourceAccessConfiguration)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateEventSourceMapping' value with any optional fields omitted.
mkUpdateEventSourceMapping ::
  -- | 'uuid'
  Types.String ->
  UpdateEventSourceMapping
mkUpdateEventSourceMapping uuid =
  UpdateEventSourceMapping'
    { uuid,
      batchSize = Core.Nothing,
      bisectBatchOnFunctionError = Core.Nothing,
      destinationConfig = Core.Nothing,
      enabled = Core.Nothing,
      functionName = Core.Nothing,
      maximumBatchingWindowInSeconds = Core.Nothing,
      maximumRecordAgeInSeconds = Core.Nothing,
      maximumRetryAttempts = Core.Nothing,
      parallelizationFactor = Core.Nothing,
      sourceAccessConfigurations = Core.Nothing
    }

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uuid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmUUID :: Lens.Lens' UpdateEventSourceMapping Types.String
uesmUUID = Lens.field @"uuid"
{-# DEPRECATED uesmUUID "Use generic-lens or generic-optics with 'uuid' instead." #-}

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
uesmBatchSize :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Natural)
uesmBatchSize = Lens.field @"batchSize"
{-# DEPRECATED uesmBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | (Streams) If the function returns an error, split the batch in two and retry.
--
-- /Note:/ Consider using 'bisectBatchOnFunctionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmBisectBatchOnFunctionError :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Bool)
uesmBisectBatchOnFunctionError = Lens.field @"bisectBatchOnFunctionError"
{-# DEPRECATED uesmBisectBatchOnFunctionError "Use generic-lens or generic-optics with 'bisectBatchOnFunctionError' instead." #-}

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmDestinationConfig :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Types.DestinationConfig)
uesmDestinationConfig = Lens.field @"destinationConfig"
{-# DEPRECATED uesmDestinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead." #-}

-- | If true, the event source mapping is active. Set to false to pause polling and invocation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmEnabled :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Bool)
uesmEnabled = Lens.field @"enabled"
{-# DEPRECATED uesmEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

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
uesmFunctionName :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Types.FunctionName)
uesmFunctionName = Lens.field @"functionName"
{-# DEPRECATED uesmFunctionName "Use generic-lens or generic-optics with 'functionName' instead." #-}

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- /Note:/ Consider using 'maximumBatchingWindowInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmMaximumBatchingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Natural)
uesmMaximumBatchingWindowInSeconds = Lens.field @"maximumBatchingWindowInSeconds"
{-# DEPRECATED uesmMaximumBatchingWindowInSeconds "Use generic-lens or generic-optics with 'maximumBatchingWindowInSeconds' instead." #-}

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1).
--
-- /Note:/ Consider using 'maximumRecordAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmMaximumRecordAgeInSeconds :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Int)
uesmMaximumRecordAgeInSeconds = Lens.field @"maximumRecordAgeInSeconds"
{-# DEPRECATED uesmMaximumRecordAgeInSeconds "Use generic-lens or generic-optics with 'maximumRecordAgeInSeconds' instead." #-}

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmMaximumRetryAttempts :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Int)
uesmMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# DEPRECATED uesmMaximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead." #-}

-- | (Streams) The number of batches to process from each shard concurrently.
--
-- /Note:/ Consider using 'parallelizationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmParallelizationFactor :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe Core.Natural)
uesmParallelizationFactor = Lens.field @"parallelizationFactor"
{-# DEPRECATED uesmParallelizationFactor "Use generic-lens or generic-optics with 'parallelizationFactor' instead." #-}

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'sourceAccessConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uesmSourceAccessConfigurations :: Lens.Lens' UpdateEventSourceMapping (Core.Maybe (Core.NonEmpty Types.SourceAccessConfiguration))
uesmSourceAccessConfigurations = Lens.field @"sourceAccessConfigurations"
{-# DEPRECATED uesmSourceAccessConfigurations "Use generic-lens or generic-optics with 'sourceAccessConfigurations' instead." #-}

instance Core.FromJSON UpdateEventSourceMapping where
  toJSON UpdateEventSourceMapping {..} =
    Core.object
      ( Core.catMaybes
          [ ("BatchSize" Core..=) Core.<$> batchSize,
            ("BisectBatchOnFunctionError" Core..=)
              Core.<$> bisectBatchOnFunctionError,
            ("DestinationConfig" Core..=) Core.<$> destinationConfig,
            ("Enabled" Core..=) Core.<$> enabled,
            ("FunctionName" Core..=) Core.<$> functionName,
            ("MaximumBatchingWindowInSeconds" Core..=)
              Core.<$> maximumBatchingWindowInSeconds,
            ("MaximumRecordAgeInSeconds" Core..=)
              Core.<$> maximumRecordAgeInSeconds,
            ("MaximumRetryAttempts" Core..=) Core.<$> maximumRetryAttempts,
            ("ParallelizationFactor" Core..=) Core.<$> parallelizationFactor,
            ("SourceAccessConfigurations" Core..=)
              Core.<$> sourceAccessConfigurations
          ]
      )

instance Core.AWSRequest UpdateEventSourceMapping where
  type
    Rs UpdateEventSourceMapping =
      Types.EventSourceMappingConfiguration
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ("/2015-03-31/event-source-mappings/" Core.<> (Core.toText uuid)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveJSON (\s h x -> Core.eitherParseJSON x)
