{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourceMappingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.EventSourceMappingConfiguration
  ( EventSourceMappingConfiguration (..)
  -- * Smart constructor
  , mkEventSourceMappingConfiguration
  -- * Lenses
  , esmcBatchSize
  , esmcBisectBatchOnFunctionError
  , esmcDestinationConfig
  , esmcEventSourceArn
  , esmcFunctionArn
  , esmcLastModified
  , esmcLastProcessingResult
  , esmcMaximumBatchingWindowInSeconds
  , esmcMaximumRecordAgeInSeconds
  , esmcMaximumRetryAttempts
  , esmcParallelizationFactor
  , esmcQueues
  , esmcSourceAccessConfigurations
  , esmcStartingPosition
  , esmcStartingPositionTimestamp
  , esmcState
  , esmcStateTransitionReason
  , esmcTopics
  , esmcUUID
  ) where

import qualified Network.AWS.Lambda.Types.Arn as Types
import qualified Network.AWS.Lambda.Types.DestinationConfig as Types
import qualified Network.AWS.Lambda.Types.EventSourcePosition as Types
import qualified Network.AWS.Lambda.Types.FunctionArn as Types
import qualified Network.AWS.Lambda.Types.Queue as Types
import qualified Network.AWS.Lambda.Types.SourceAccessConfiguration as Types
import qualified Network.AWS.Lambda.Types.Topic as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A mapping between an AWS resource and an AWS Lambda function. See 'CreateEventSourceMapping' for details.
--
-- /See:/ 'mkEventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { batchSize :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to retrieve in a single batch.
  , bisectBatchOnFunctionError :: Core.Maybe Core.Bool
    -- ^ (Streams) If the function returns an error, split the batch in two and retry. The default value is false.
  , destinationConfig :: Core.Maybe Types.DestinationConfig
    -- ^ (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
  , eventSourceArn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the event source.
  , functionArn :: Core.Maybe Types.FunctionArn
    -- ^ The ARN of the Lambda function.
  , lastModified :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the event source mapping was last updated, or its state changed.
  , lastProcessingResult :: Core.Maybe Core.Text
    -- ^ The result of the last AWS Lambda invocation of your Lambda function.
  , maximumBatchingWindowInSeconds :: Core.Maybe Core.Natural
    -- ^ (Streams) The maximum amount of time to gather records before invoking the function, in seconds. The default value is zero.
  , maximumRecordAgeInSeconds :: Core.Maybe Core.Int
    -- ^ (Streams) Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
  , maximumRetryAttempts :: Core.Maybe Core.Int
    -- ^ (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
  , parallelizationFactor :: Core.Maybe Core.Natural
    -- ^ (Streams) The number of batches to process from each shard concurrently. The default value is 1.
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
  , state :: Core.Maybe Core.Text
    -- ^ The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
  , stateTransitionReason :: Core.Maybe Core.Text
    -- ^ Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
  , topics :: Core.Maybe (Core.NonEmpty Types.Topic)
    -- ^ (MSK) The name of the Kafka topic to consume. 
  , uuid :: Core.Maybe Core.Text
    -- ^ The identifier of the event source mapping.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EventSourceMappingConfiguration' value with any optional fields omitted.
mkEventSourceMappingConfiguration
    :: EventSourceMappingConfiguration
mkEventSourceMappingConfiguration
  = EventSourceMappingConfiguration'{batchSize = Core.Nothing,
                                     bisectBatchOnFunctionError = Core.Nothing,
                                     destinationConfig = Core.Nothing,
                                     eventSourceArn = Core.Nothing, functionArn = Core.Nothing,
                                     lastModified = Core.Nothing,
                                     lastProcessingResult = Core.Nothing,
                                     maximumBatchingWindowInSeconds = Core.Nothing,
                                     maximumRecordAgeInSeconds = Core.Nothing,
                                     maximumRetryAttempts = Core.Nothing,
                                     parallelizationFactor = Core.Nothing, queues = Core.Nothing,
                                     sourceAccessConfigurations = Core.Nothing,
                                     startingPosition = Core.Nothing,
                                     startingPositionTimestamp = Core.Nothing, state = Core.Nothing,
                                     stateTransitionReason = Core.Nothing, topics = Core.Nothing,
                                     uuid = Core.Nothing}

-- | The maximum number of items to retrieve in a single batch.
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcBatchSize :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
esmcBatchSize = Lens.field @"batchSize"
{-# INLINEABLE esmcBatchSize #-}
{-# DEPRECATED batchSize "Use generic-lens or generic-optics with 'batchSize' instead"  #-}

-- | (Streams) If the function returns an error, split the batch in two and retry. The default value is false.
--
-- /Note:/ Consider using 'bisectBatchOnFunctionError' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcBisectBatchOnFunctionError :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Bool)
esmcBisectBatchOnFunctionError = Lens.field @"bisectBatchOnFunctionError"
{-# INLINEABLE esmcBisectBatchOnFunctionError #-}
{-# DEPRECATED bisectBatchOnFunctionError "Use generic-lens or generic-optics with 'bisectBatchOnFunctionError' instead"  #-}

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for discarded records.
--
-- /Note:/ Consider using 'destinationConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcDestinationConfig :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Types.DestinationConfig)
esmcDestinationConfig = Lens.field @"destinationConfig"
{-# INLINEABLE esmcDestinationConfig #-}
{-# DEPRECATED destinationConfig "Use generic-lens or generic-optics with 'destinationConfig' instead"  #-}

-- | The Amazon Resource Name (ARN) of the event source.
--
-- /Note:/ Consider using 'eventSourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcEventSourceArn :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Types.Arn)
esmcEventSourceArn = Lens.field @"eventSourceArn"
{-# INLINEABLE esmcEventSourceArn #-}
{-# DEPRECATED eventSourceArn "Use generic-lens or generic-optics with 'eventSourceArn' instead"  #-}

-- | The ARN of the Lambda function.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcFunctionArn :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Types.FunctionArn)
esmcFunctionArn = Lens.field @"functionArn"
{-# INLINEABLE esmcFunctionArn #-}
{-# DEPRECATED functionArn "Use generic-lens or generic-optics with 'functionArn' instead"  #-}

-- | The date that the event source mapping was last updated, or its state changed.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcLastModified :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.NominalDiffTime)
esmcLastModified = Lens.field @"lastModified"
{-# INLINEABLE esmcLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | The result of the last AWS Lambda invocation of your Lambda function.
--
-- /Note:/ Consider using 'lastProcessingResult' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcLastProcessingResult :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
esmcLastProcessingResult = Lens.field @"lastProcessingResult"
{-# INLINEABLE esmcLastProcessingResult #-}
{-# DEPRECATED lastProcessingResult "Use generic-lens or generic-optics with 'lastProcessingResult' instead"  #-}

-- | (Streams) The maximum amount of time to gather records before invoking the function, in seconds. The default value is zero.
--
-- /Note:/ Consider using 'maximumBatchingWindowInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcMaximumBatchingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
esmcMaximumBatchingWindowInSeconds = Lens.field @"maximumBatchingWindowInSeconds"
{-# INLINEABLE esmcMaximumBatchingWindowInSeconds #-}
{-# DEPRECATED maximumBatchingWindowInSeconds "Use generic-lens or generic-optics with 'maximumBatchingWindowInSeconds' instead"  #-}

-- | (Streams) Discard records older than the specified age. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
-- /Note:/ Consider using 'maximumRecordAgeInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcMaximumRecordAgeInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Int)
esmcMaximumRecordAgeInSeconds = Lens.field @"maximumRecordAgeInSeconds"
{-# INLINEABLE esmcMaximumRecordAgeInSeconds #-}
{-# DEPRECATED maximumRecordAgeInSeconds "Use generic-lens or generic-optics with 'maximumRecordAgeInSeconds' instead"  #-}

-- | (Streams) Discard records after the specified number of retries. The default value is infinite (-1). When set to infinite (-1), failed records are retried until the record expires.
--
-- /Note:/ Consider using 'maximumRetryAttempts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcMaximumRetryAttempts :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Int)
esmcMaximumRetryAttempts = Lens.field @"maximumRetryAttempts"
{-# INLINEABLE esmcMaximumRetryAttempts #-}
{-# DEPRECATED maximumRetryAttempts "Use generic-lens or generic-optics with 'maximumRetryAttempts' instead"  #-}

-- | (Streams) The number of batches to process from each shard concurrently. The default value is 1.
--
-- /Note:/ Consider using 'parallelizationFactor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcParallelizationFactor :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
esmcParallelizationFactor = Lens.field @"parallelizationFactor"
{-# INLINEABLE esmcParallelizationFactor #-}
{-# DEPRECATED parallelizationFactor "Use generic-lens or generic-optics with 'parallelizationFactor' instead"  #-}

-- | (MQ) The name of the Amazon MQ broker destination queue to consume. 
--
-- /Note:/ Consider using 'queues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcQueues :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe (Core.NonEmpty Types.Queue))
esmcQueues = Lens.field @"queues"
{-# INLINEABLE esmcQueues #-}
{-# DEPRECATED queues "Use generic-lens or generic-optics with 'queues' instead"  #-}

-- | (MQ) The Secrets Manager secret that stores your broker credentials. To store your secret, use the following format: @{ "username": "your username", "password": "your password" }@ 
--
-- To reference the secret, use the following format: @[ { "Type": "BASIC_AUTH", "URI": "secretARN" } ]@ 
-- The value of @Type@ is always @BASIC_AUTH@ . To encrypt the secret, you can use customer or service managed keys. When using a customer managed KMS key, the Lambda execution role requires @kms:Decrypt@ permissions.
--
-- /Note:/ Consider using 'sourceAccessConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcSourceAccessConfigurations :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe (Core.NonEmpty Types.SourceAccessConfiguration))
esmcSourceAccessConfigurations = Lens.field @"sourceAccessConfigurations"
{-# INLINEABLE esmcSourceAccessConfigurations #-}
{-# DEPRECATED sourceAccessConfigurations "Use generic-lens or generic-optics with 'sourceAccessConfigurations' instead"  #-}

-- | The position in a stream from which to start reading. Required for Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources. @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- /Note:/ Consider using 'startingPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcStartingPosition :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Types.EventSourcePosition)
esmcStartingPosition = Lens.field @"startingPosition"
{-# INLINEABLE esmcStartingPosition #-}
{-# DEPRECATED startingPosition "Use generic-lens or generic-optics with 'startingPosition' instead"  #-}

-- | With @StartingPosition@ set to @AT_TIMESTAMP@ , the time from which to start reading.
--
-- /Note:/ Consider using 'startingPositionTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcStartingPositionTimestamp :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.NominalDiffTime)
esmcStartingPositionTimestamp = Lens.field @"startingPositionTimestamp"
{-# INLINEABLE esmcStartingPositionTimestamp #-}
{-# DEPRECATED startingPositionTimestamp "Use generic-lens or generic-optics with 'startingPositionTimestamp' instead"  #-}

-- | The state of the event source mapping. It can be one of the following: @Creating@ , @Enabling@ , @Enabled@ , @Disabling@ , @Disabled@ , @Updating@ , or @Deleting@ .
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcState :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
esmcState = Lens.field @"state"
{-# INLINEABLE esmcState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | Indicates whether the last change to the event source mapping was made by a user, or by the Lambda service.
--
-- /Note:/ Consider using 'stateTransitionReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcStateTransitionReason :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
esmcStateTransitionReason = Lens.field @"stateTransitionReason"
{-# INLINEABLE esmcStateTransitionReason #-}
{-# DEPRECATED stateTransitionReason "Use generic-lens or generic-optics with 'stateTransitionReason' instead"  #-}

-- | (MSK) The name of the Kafka topic to consume. 
--
-- /Note:/ Consider using 'topics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcTopics :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe (Core.NonEmpty Types.Topic))
esmcTopics = Lens.field @"topics"
{-# INLINEABLE esmcTopics #-}
{-# DEPRECATED topics "Use generic-lens or generic-optics with 'topics' instead"  #-}

-- | The identifier of the event source mapping.
--
-- /Note:/ Consider using 'uuid' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esmcUUID :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
esmcUUID = Lens.field @"uuid"
{-# INLINEABLE esmcUUID #-}
{-# DEPRECATED uuid "Use generic-lens or generic-optics with 'uuid' instead"  #-}

instance Core.FromJSON EventSourceMappingConfiguration where
        parseJSON
          = Core.withObject "EventSourceMappingConfiguration" Core.$
              \ x ->
                EventSourceMappingConfiguration' Core.<$>
                  (x Core..:? "BatchSize") Core.<*>
                    x Core..:? "BisectBatchOnFunctionError"
                    Core.<*> x Core..:? "DestinationConfig"
                    Core.<*> x Core..:? "EventSourceArn"
                    Core.<*> x Core..:? "FunctionArn"
                    Core.<*> x Core..:? "LastModified"
                    Core.<*> x Core..:? "LastProcessingResult"
                    Core.<*> x Core..:? "MaximumBatchingWindowInSeconds"
                    Core.<*> x Core..:? "MaximumRecordAgeInSeconds"
                    Core.<*> x Core..:? "MaximumRetryAttempts"
                    Core.<*> x Core..:? "ParallelizationFactor"
                    Core.<*> x Core..:? "Queues"
                    Core.<*> x Core..:? "SourceAccessConfigurations"
                    Core.<*> x Core..:? "StartingPosition"
                    Core.<*> x Core..:? "StartingPositionTimestamp"
                    Core.<*> x Core..:? "State"
                    Core.<*> x Core..:? "StateTransitionReason"
                    Core.<*> x Core..:? "Topics"
                    Core.<*> x Core..:? "UUID"
