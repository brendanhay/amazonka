{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
--
--
module Network.AWS.Lambda.CreateEventSourceMapping
    (
    -- * Creating a request
      CreateEventSourceMapping (..)
    , mkCreateEventSourceMapping
    -- ** Request lenses
    , cesmEventSourceArn
    , cesmFunctionName
    , cesmBatchSize
    , cesmBisectBatchOnFunctionError
    , cesmDestinationConfig
    , cesmEnabled
    , cesmMaximumBatchingWindowInSeconds
    , cesmMaximumRecordAgeInSeconds
    , cesmMaximumRetryAttempts
    , cesmParallelizationFactor
    , cesmQueues
    , cesmSourceAccessConfigurations
    , cesmStartingPosition
    , cesmStartingPositionTimestamp
    , cesmTopics

     -- * Destructuring the response
    , Types.EventSourceMappingConfiguration (..)
    , Types.mkEventSourceMappingConfiguration
    -- ** Response lenses
    , Types.esmcBatchSize
    , Types.esmcBisectBatchOnFunctionError
    , Types.esmcDestinationConfig
    , Types.esmcEventSourceArn
    , Types.esmcFunctionArn
    , Types.esmcLastModified
    , Types.esmcLastProcessingResult
    , Types.esmcMaximumBatchingWindowInSeconds
    , Types.esmcMaximumRecordAgeInSeconds
    , Types.esmcMaximumRetryAttempts
    , Types.esmcParallelizationFactor
    , Types.esmcQueues
    , Types.esmcSourceAccessConfigurations
    , Types.esmcStartingPosition
    , Types.esmcStartingPositionTimestamp
    , Types.esmcState
    , Types.esmcStateTransitionReason
    , Types.esmcTopics
    , Types.esmcUUID
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateEventSourceMapping' smart constructor.
data CreateEventSourceMapping = CreateEventSourceMapping'
  { eventSourceArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the event source.
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
  , functionName :: Types.FunctionName
    -- ^ The name of the Lambda function.
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
  , batchSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to retrieve in a single batch.
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
  , bisectBatchOnFunctionError :: Core.Maybe Core.Bool
    -- ^ (Streams) If the function returns an error, split the batch in two and retry.
  , destinationConfig :: Core.Maybe Types.DestinationConfig
    -- ^ (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
  , enabled :: Core.Maybe Core.Bool
    -- ^ If true, the event source mapping is active. Set to false to pause polling and invocation.
  , maximumBatchingWindowInSeconds :: Core.Maybe Core.Natural
    -- ^ (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
  , maximumRecordAgeInSeconds :: Core.Maybe Core.Int
    -- ^ (Streams) Discard records older than the specified age. The default value is infinite (-1).
  , maximumRetryAttempts :: Core.Maybe Core.Int
    -- ^ (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
  , parallelizationFactor :: Core.Maybe Core.Natural
    -- ^ (Streams) The number of batches to process from each shard concurrently.
  , queues :: Core.Maybe (Core.NonEmpty Types.Queue)
    -- ^ (MQ) The name of the Amazon MQ broker destination queue to consume. 
  , sourceAccessConfigurations :: Core.Maybe (Core.NonEmpty Types.SourceAccessConfiguration)
    -- ^ (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@ 
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
  , startingPosition :: Core.Maybe Types.EventSourcePosition
    -- ^ The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
  , startingPositionTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
  , topics :: Core.Maybe (Core.NonEmpty Types.Topic)
    -- ^ (MSK) The name of the Kafka topic. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateEventSourceMapping' value with any optional fields omitted.
mkCreateEventSourceMapping
    :: Types.Arn -- ^ 'eventSourceArn'
    -> Types.FunctionName -- ^ 'functionName'
    -> CreateEventSourceMapping
mkCreateEventSourceMapping eventSourceArn functionName
  = CreateEventSourceMapping'{eventSourceArn, functionName,
                              batchSize = Core.Nothing,
                              bisectBatchOnFunctionError = Core.Nothing,
                              destinationConfig = Core.Nothing, enabled = Core.Nothing,
                              maximumBatchingWindowInSeconds = Core.Nothing,
                              maximumRecordAgeInSeconds = Core.Nothing,
                              maximumRetryAttempts = Core.Nothing,
                              parallelizationFactor = Core.Nothing, queues = Core.Nothing,
                              sourceAccessConfigurations = Core.Nothing,
                              startingPosition = Core.Nothing,
                              startingPositionTimestamp = Core.Nothing, topics = Core.Nothing}

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
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmEventSourceArn :: Lens.Lens' CreateEventSourceMapping Types.Arn
cesmEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE cesmEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

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
cesmFunctionName :: Lens.Lens' CreateEventSourceMapping Types.FunctionName
cesmFunctionName = Lens.field @"functionName"
{-# INLINEABLE cesmFunctionName #-}
{-# DEPRECATED functionName "Use generic-lens or generic-optics with 'functionName' instead"  #-}

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
cesmBatchSize :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Natural)
cesmBatchSize = Lens.field @"batchSize"
{-# INLINEABLE cesmBatchSize #-}
{-# DEPRECATED batchSize "Use generic-lens or generic-optics with 'batchSize' instead"  #-}

-- | (Streams) If the function returns an error, split the batch in two and retry.
--
-- /Note:/ Consider using 'bisectBatchOnFunctionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmBisectBatchOnFunctionError :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Bool)
cesmBisectBatchOnFunctionError = Lens.field @"bisectBatchOnFunctionError"
{-# INLINEABLE cesmBisectBatchOnFunctionError #-}
{-# DEPRECATED bisectBatchOnFunctionError "Use generic-lens or generic-optics with 'bisectBatchOnFunctionError' instead"  #-}

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmDestinationConfig :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Types.DestinationConfig)
cesmDestinationConfig = Lens.field @"destinationConfig"
{-# INLINEABLE cesmDestinationConfig #-}
{-# DEPRECATED destinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead"  #-}

-- | If true, the event source mapping is active. Set to false to pause polling and invocation.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmEnabled :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Bool)
cesmEnabled = Lens.field @"enabled"
{-# INLINEABLE cesmEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds.
--
-- /Note:/ Consider using 'maximumBatchingWindowInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmMaximumBatchingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Natural)
cesmMaximumBatchingWindowInSeconds = Lens.field @"maximumBatchingWindowInSeconds"
{-# INLINEABLE cesmMaximumBatchingWindowInSeconds #-}
{-# DEPRECATED maximumBatchingWindowInSeconds "Use generic-lens or generic-optics with 'maximumBatchingWindowInSeconds' instead"  #-}

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1).
--
-- /Note:/ Consider using 'maximumRecordAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmMaximumRecordAgeInSeconds :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Int)
cesmMaximumRecordAgeInSeconds = Lens.field @"maximumRecordAgeInSeconds"
{-# INLINEABLE cesmMaximumRecordAgeInSeconds #-}
{-# DEPRECATED maximumRecordAgeInSeconds "Use generic-lens or generic-optics with 'maximumRecordAgeInSeconds' instead"  #-}

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records will be retried until the record expires.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmMaximumRetryAttempts :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Int)
cesmMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# INLINEABLE cesmMaximumRetryAttempts #-}
{-# DEPRECATED maximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead"  #-}

-- | (Streams) The number of batches to process from each shard concurrently.
--
-- /Note:/ Consider using 'parallelizationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmParallelizationFactor :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.Natural)
cesmParallelizationFactor = Lens.field @"parallelizationFactor"
{-# INLINEABLE cesmParallelizationFactor #-}
{-# DEPRECATED parallelizationFactor "Use generic-lens or generic-optics with 'parallelizationFactor' instead"  #-}

-- | (MQ) The name of the Amazon MQ broker destination queue to consume. 
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmQueues :: Lens.Lens' CreateEventSourceMapping (Core.Maybe (Core.NonEmpty Types.Queue))
cesmQueues = Lens.field @"queues"
{-# INLINEABLE cesmQueues #-}
{-# DEPRECATED queues "Use generic-lens or generic-optics with 'queues' instead"  #-}

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@ 
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'sourceAccessConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmSourceAccessConfigurations :: Lens.Lens' CreateEventSourceMapping (Core.Maybe (Core.NonEmpty Types.SourceAccessConfiguration))
cesmSourceAccessConfigurations = Lens.field @"sourceAccessConfigurations"
{-# INLINEABLE cesmSourceAccessConfigurations #-}
{-# DEPRECATED sourceAccessConfigurations "Use generic-lens or generic-optics with 'sourceAccessConfigurations' instead"  #-}

-- | The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- /Note:/ Consider using 'startingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmStartingPosition :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Types.EventSourcePosition)
cesmStartingPosition = Lens.field @"startingPosition"
{-# INLINEABLE cesmStartingPosition #-}
{-# DEPRECATED startingPosition "Use generic-lens or generic-optics with 'startingPosition' instead"  #-}

-- | With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
--
-- /Note:/ Consider using 'startingPositionTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmStartingPositionTimestamp :: Lens.Lens' CreateEventSourceMapping (Core.Maybe Core.NominalDiffTime)
cesmStartingPositionTimestamp = Lens.field @"startingPositionTimestamp"
{-# INLINEABLE cesmStartingPositionTimestamp #-}
{-# DEPRECATED startingPositionTimestamp "Use generic-lens or generic-optics with 'startingPositionTimestamp' instead"  #-}

-- | (MSK) The name of the Kafka topic. 
--
-- /Note:/ Consider using 'topics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cesmTopics :: Lens.Lens' CreateEventSourceMapping (Core.Maybe (Core.NonEmpty Types.Topic))
cesmTopics = Lens.field @"topics"
{-# INLINEABLE cesmTopics #-}
{-# DEPRECATED topics "Use generic-lens or generic-optics with 'topics' instead"  #-}

instance Core.ToQuery CreateEventSourceMapping where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateEventSourceMapping where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON CreateEventSourceMapping where
        toJSON CreateEventSourceMapping{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("EventSourceArn" Core..= eventSourceArn),
                  Core.Just ("FunctionName" Core..= functionName),
                  ("BatchSize" Core..=) Core.<$> batchSize,
                  ("BisectBatchOnFunctionError" Core..=) Core.<$>
                    bisectBatchOnFunctionError,
                  ("DestinationConfig" Core..=) Core.<$> destinationConfig,
                  ("Enabled" Core..=) Core.<$> enabled,
                  ("MaximumBatchingWindowInSeconds" Core..=) Core.<$>
                    maximumBatchingWindowInSeconds,
                  ("MaximumRecordAgeInSeconds" Core..=) Core.<$>
                    maximumRecordAgeInSeconds,
                  ("MaximumRetryAttempts" Core..=) Core.<$> maximumRetryAttempts,
                  ("ParallelizationFactor" Core..=) Core.<$> parallelizationFactor,
                  ("Queues" Core..=) Core.<$> queues,
                  ("SourceAccessConfigurations" Core..=) Core.<$>
                    sourceAccessConfigurations,
                  ("StartingPosition" Core..=) Core.<$> startingPosition,
                  ("StartingPositionTimestamp" Core..=) Core.<$>
                    startingPositionTimestamp,
                  ("Topics" Core..=) Core.<$> topics])

instance Core.AWSRequest CreateEventSourceMapping where
        type Rs CreateEventSourceMapping =
             Types.EventSourceMappingConfiguration
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath = "/2015-03-31/event-source-mappings/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON (\ s h x -> Core.eitherParseJSON x)
        
        {-# INLINE parseResponse #-}
