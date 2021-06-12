{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourceMappingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EventSourceMappingConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types.DestinationConfig
import Network.AWS.Lambda.Types.EventSourcePosition
import Network.AWS.Lambda.Types.FunctionResponseType
import Network.AWS.Lambda.Types.SelfManagedEventSource
import Network.AWS.Lambda.Types.SourceAccessConfiguration
import qualified Network.AWS.Lens as Lens

-- | A mapping between an AWS resource and an AWS Lambda function. See
-- CreateEventSourceMapping for details.
--
-- /See:/ 'newEventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the event source.
    eventSourceArn :: Core.Maybe Core.Text,
    -- | Indicates whether the last change to the event source mapping was made
    -- by a user, or by the Lambda service.
    stateTransitionReason :: Core.Maybe Core.Text,
    -- | The result of the last AWS Lambda invocation of your Lambda function.
    lastProcessingResult :: Core.Maybe Core.Text,
    -- | The name of the Kafka topic.
    topics :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | (Streams) Discard records older than the specified age. The default
    -- value is infinite (-1). When set to infinite (-1), failed records are
    -- retried until the record expires.
    maximumRecordAgeInSeconds :: Core.Maybe Core.Int,
    -- | (Streams) A list of current response type enums applied to the event
    -- source mapping.
    functionResponseTypes :: Core.Maybe (Core.NonEmpty FunctionResponseType),
    -- | (MQ) The name of the Amazon MQ broker destination queue to consume.
    queues :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | (Streams) The duration in seconds of a processing window. The range is
    -- between 1 second up to 900 seconds.
    tumblingWindowInSeconds :: Core.Maybe Core.Natural,
    -- | The ARN of the Lambda function.
    functionArn :: Core.Maybe Core.Text,
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading.
    startingPositionTimestamp :: Core.Maybe Core.POSIX,
    -- | The state of the event source mapping. It can be one of the following:
    -- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
    -- or @Deleting@.
    state :: Core.Maybe Core.Text,
    -- | (Streams and SQS standard queues) The maximum amount of time to gather
    -- records before invoking the function, in seconds. The default value is
    -- zero.
    maximumBatchingWindowInSeconds :: Core.Maybe Core.Natural,
    -- | The maximum number of items to retrieve in a single batch.
    batchSize :: Core.Maybe Core.Natural,
    -- | The position in a stream from which to start reading. Required for
    -- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
    -- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
    startingPosition :: Core.Maybe EventSourcePosition,
    -- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Core.Maybe DestinationConfig,
    -- | (Streams) Discard records after the specified number of retries. The
    -- default value is infinite (-1). When set to infinite (-1), failed
    -- records are retried until the record expires.
    maximumRetryAttempts :: Core.Maybe Core.Int,
    -- | The date that the event source mapping was last updated, or its state
    -- changed.
    lastModified :: Core.Maybe Core.POSIX,
    -- | (Streams) The number of batches to process from each shard concurrently.
    -- The default value is 1.
    parallelizationFactor :: Core.Maybe Core.Natural,
    -- | The Self-Managed Apache Kafka cluster for your event source.
    selfManagedEventSource :: Core.Maybe SelfManagedEventSource,
    -- | The identifier of the event source mapping.
    uuid :: Core.Maybe Core.Text,
    -- | (Streams) If the function returns an error, split the batch in two and
    -- retry. The default value is false.
    bisectBatchOnFunctionError :: Core.Maybe Core.Bool,
    -- | An array of the authentication protocol, or the VPC components to secure
    -- your event source.
    sourceAccessConfigurations :: Core.Maybe [SourceAccessConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EventSourceMappingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'eventSourceMappingConfiguration_eventSourceArn' - The Amazon Resource Name (ARN) of the event source.
--
-- 'stateTransitionReason', 'eventSourceMappingConfiguration_stateTransitionReason' - Indicates whether the last change to the event source mapping was made
-- by a user, or by the Lambda service.
--
-- 'lastProcessingResult', 'eventSourceMappingConfiguration_lastProcessingResult' - The result of the last AWS Lambda invocation of your Lambda function.
--
-- 'topics', 'eventSourceMappingConfiguration_topics' - The name of the Kafka topic.
--
-- 'maximumRecordAgeInSeconds', 'eventSourceMappingConfiguration_maximumRecordAgeInSeconds' - (Streams) Discard records older than the specified age. The default
-- value is infinite (-1). When set to infinite (-1), failed records are
-- retried until the record expires.
--
-- 'functionResponseTypes', 'eventSourceMappingConfiguration_functionResponseTypes' - (Streams) A list of current response type enums applied to the event
-- source mapping.
--
-- 'queues', 'eventSourceMappingConfiguration_queues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- 'tumblingWindowInSeconds', 'eventSourceMappingConfiguration_tumblingWindowInSeconds' - (Streams) The duration in seconds of a processing window. The range is
-- between 1 second up to 900 seconds.
--
-- 'functionArn', 'eventSourceMappingConfiguration_functionArn' - The ARN of the Lambda function.
--
-- 'startingPositionTimestamp', 'eventSourceMappingConfiguration_startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
--
-- 'state', 'eventSourceMappingConfiguration_state' - The state of the event source mapping. It can be one of the following:
-- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
-- or @Deleting@.
--
-- 'maximumBatchingWindowInSeconds', 'eventSourceMappingConfiguration_maximumBatchingWindowInSeconds' - (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds. The default value is
-- zero.
--
-- 'batchSize', 'eventSourceMappingConfiguration_batchSize' - The maximum number of items to retrieve in a single batch.
--
-- 'startingPosition', 'eventSourceMappingConfiguration_startingPosition' - The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- 'destinationConfig', 'eventSourceMappingConfiguration_destinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'maximumRetryAttempts', 'eventSourceMappingConfiguration_maximumRetryAttempts' - (Streams) Discard records after the specified number of retries. The
-- default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
--
-- 'lastModified', 'eventSourceMappingConfiguration_lastModified' - The date that the event source mapping was last updated, or its state
-- changed.
--
-- 'parallelizationFactor', 'eventSourceMappingConfiguration_parallelizationFactor' - (Streams) The number of batches to process from each shard concurrently.
-- The default value is 1.
--
-- 'selfManagedEventSource', 'eventSourceMappingConfiguration_selfManagedEventSource' - The Self-Managed Apache Kafka cluster for your event source.
--
-- 'uuid', 'eventSourceMappingConfiguration_uuid' - The identifier of the event source mapping.
--
-- 'bisectBatchOnFunctionError', 'eventSourceMappingConfiguration_bisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and
-- retry. The default value is false.
--
-- 'sourceAccessConfigurations', 'eventSourceMappingConfiguration_sourceAccessConfigurations' - An array of the authentication protocol, or the VPC components to secure
-- your event source.
newEventSourceMappingConfiguration ::
  EventSourceMappingConfiguration
newEventSourceMappingConfiguration =
  EventSourceMappingConfiguration'
    { eventSourceArn =
        Core.Nothing,
      stateTransitionReason = Core.Nothing,
      lastProcessingResult = Core.Nothing,
      topics = Core.Nothing,
      maximumRecordAgeInSeconds = Core.Nothing,
      functionResponseTypes = Core.Nothing,
      queues = Core.Nothing,
      tumblingWindowInSeconds = Core.Nothing,
      functionArn = Core.Nothing,
      startingPositionTimestamp = Core.Nothing,
      state = Core.Nothing,
      maximumBatchingWindowInSeconds =
        Core.Nothing,
      batchSize = Core.Nothing,
      startingPosition = Core.Nothing,
      destinationConfig = Core.Nothing,
      maximumRetryAttempts = Core.Nothing,
      lastModified = Core.Nothing,
      parallelizationFactor = Core.Nothing,
      selfManagedEventSource = Core.Nothing,
      uuid = Core.Nothing,
      bisectBatchOnFunctionError = Core.Nothing,
      sourceAccessConfigurations = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
eventSourceMappingConfiguration_eventSourceArn :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
eventSourceMappingConfiguration_eventSourceArn = Lens.lens (\EventSourceMappingConfiguration' {eventSourceArn} -> eventSourceArn) (\s@EventSourceMappingConfiguration' {} a -> s {eventSourceArn = a} :: EventSourceMappingConfiguration)

-- | Indicates whether the last change to the event source mapping was made
-- by a user, or by the Lambda service.
eventSourceMappingConfiguration_stateTransitionReason :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
eventSourceMappingConfiguration_stateTransitionReason = Lens.lens (\EventSourceMappingConfiguration' {stateTransitionReason} -> stateTransitionReason) (\s@EventSourceMappingConfiguration' {} a -> s {stateTransitionReason = a} :: EventSourceMappingConfiguration)

-- | The result of the last AWS Lambda invocation of your Lambda function.
eventSourceMappingConfiguration_lastProcessingResult :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
eventSourceMappingConfiguration_lastProcessingResult = Lens.lens (\EventSourceMappingConfiguration' {lastProcessingResult} -> lastProcessingResult) (\s@EventSourceMappingConfiguration' {} a -> s {lastProcessingResult = a} :: EventSourceMappingConfiguration)

-- | The name of the Kafka topic.
eventSourceMappingConfiguration_topics :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe (Core.NonEmpty Core.Text))
eventSourceMappingConfiguration_topics = Lens.lens (\EventSourceMappingConfiguration' {topics} -> topics) (\s@EventSourceMappingConfiguration' {} a -> s {topics = a} :: EventSourceMappingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | (Streams) Discard records older than the specified age. The default
-- value is infinite (-1). When set to infinite (-1), failed records are
-- retried until the record expires.
eventSourceMappingConfiguration_maximumRecordAgeInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Int)
eventSourceMappingConfiguration_maximumRecordAgeInSeconds = Lens.lens (\EventSourceMappingConfiguration' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {maximumRecordAgeInSeconds = a} :: EventSourceMappingConfiguration)

-- | (Streams) A list of current response type enums applied to the event
-- source mapping.
eventSourceMappingConfiguration_functionResponseTypes :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe (Core.NonEmpty FunctionResponseType))
eventSourceMappingConfiguration_functionResponseTypes = Lens.lens (\EventSourceMappingConfiguration' {functionResponseTypes} -> functionResponseTypes) (\s@EventSourceMappingConfiguration' {} a -> s {functionResponseTypes = a} :: EventSourceMappingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
eventSourceMappingConfiguration_queues :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe (Core.NonEmpty Core.Text))
eventSourceMappingConfiguration_queues = Lens.lens (\EventSourceMappingConfiguration' {queues} -> queues) (\s@EventSourceMappingConfiguration' {} a -> s {queues = a} :: EventSourceMappingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | (Streams) The duration in seconds of a processing window. The range is
-- between 1 second up to 900 seconds.
eventSourceMappingConfiguration_tumblingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
eventSourceMappingConfiguration_tumblingWindowInSeconds = Lens.lens (\EventSourceMappingConfiguration' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {tumblingWindowInSeconds = a} :: EventSourceMappingConfiguration)

-- | The ARN of the Lambda function.
eventSourceMappingConfiguration_functionArn :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
eventSourceMappingConfiguration_functionArn = Lens.lens (\EventSourceMappingConfiguration' {functionArn} -> functionArn) (\s@EventSourceMappingConfiguration' {} a -> s {functionArn = a} :: EventSourceMappingConfiguration)

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
eventSourceMappingConfiguration_startingPositionTimestamp :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.UTCTime)
eventSourceMappingConfiguration_startingPositionTimestamp = Lens.lens (\EventSourceMappingConfiguration' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@EventSourceMappingConfiguration' {} a -> s {startingPositionTimestamp = a} :: EventSourceMappingConfiguration) Core.. Lens.mapping Core._Time

-- | The state of the event source mapping. It can be one of the following:
-- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
-- or @Deleting@.
eventSourceMappingConfiguration_state :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
eventSourceMappingConfiguration_state = Lens.lens (\EventSourceMappingConfiguration' {state} -> state) (\s@EventSourceMappingConfiguration' {} a -> s {state = a} :: EventSourceMappingConfiguration)

-- | (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds. The default value is
-- zero.
eventSourceMappingConfiguration_maximumBatchingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
eventSourceMappingConfiguration_maximumBatchingWindowInSeconds = Lens.lens (\EventSourceMappingConfiguration' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {maximumBatchingWindowInSeconds = a} :: EventSourceMappingConfiguration)

-- | The maximum number of items to retrieve in a single batch.
eventSourceMappingConfiguration_batchSize :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
eventSourceMappingConfiguration_batchSize = Lens.lens (\EventSourceMappingConfiguration' {batchSize} -> batchSize) (\s@EventSourceMappingConfiguration' {} a -> s {batchSize = a} :: EventSourceMappingConfiguration)

-- | The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
eventSourceMappingConfiguration_startingPosition :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe EventSourcePosition)
eventSourceMappingConfiguration_startingPosition = Lens.lens (\EventSourceMappingConfiguration' {startingPosition} -> startingPosition) (\s@EventSourceMappingConfiguration' {} a -> s {startingPosition = a} :: EventSourceMappingConfiguration)

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
eventSourceMappingConfiguration_destinationConfig :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe DestinationConfig)
eventSourceMappingConfiguration_destinationConfig = Lens.lens (\EventSourceMappingConfiguration' {destinationConfig} -> destinationConfig) (\s@EventSourceMappingConfiguration' {} a -> s {destinationConfig = a} :: EventSourceMappingConfiguration)

-- | (Streams) Discard records after the specified number of retries. The
-- default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
eventSourceMappingConfiguration_maximumRetryAttempts :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Int)
eventSourceMappingConfiguration_maximumRetryAttempts = Lens.lens (\EventSourceMappingConfiguration' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@EventSourceMappingConfiguration' {} a -> s {maximumRetryAttempts = a} :: EventSourceMappingConfiguration)

-- | The date that the event source mapping was last updated, or its state
-- changed.
eventSourceMappingConfiguration_lastModified :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.UTCTime)
eventSourceMappingConfiguration_lastModified = Lens.lens (\EventSourceMappingConfiguration' {lastModified} -> lastModified) (\s@EventSourceMappingConfiguration' {} a -> s {lastModified = a} :: EventSourceMappingConfiguration) Core.. Lens.mapping Core._Time

-- | (Streams) The number of batches to process from each shard concurrently.
-- The default value is 1.
eventSourceMappingConfiguration_parallelizationFactor :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Natural)
eventSourceMappingConfiguration_parallelizationFactor = Lens.lens (\EventSourceMappingConfiguration' {parallelizationFactor} -> parallelizationFactor) (\s@EventSourceMappingConfiguration' {} a -> s {parallelizationFactor = a} :: EventSourceMappingConfiguration)

-- | The Self-Managed Apache Kafka cluster for your event source.
eventSourceMappingConfiguration_selfManagedEventSource :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe SelfManagedEventSource)
eventSourceMappingConfiguration_selfManagedEventSource = Lens.lens (\EventSourceMappingConfiguration' {selfManagedEventSource} -> selfManagedEventSource) (\s@EventSourceMappingConfiguration' {} a -> s {selfManagedEventSource = a} :: EventSourceMappingConfiguration)

-- | The identifier of the event source mapping.
eventSourceMappingConfiguration_uuid :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Text)
eventSourceMappingConfiguration_uuid = Lens.lens (\EventSourceMappingConfiguration' {uuid} -> uuid) (\s@EventSourceMappingConfiguration' {} a -> s {uuid = a} :: EventSourceMappingConfiguration)

-- | (Streams) If the function returns an error, split the batch in two and
-- retry. The default value is false.
eventSourceMappingConfiguration_bisectBatchOnFunctionError :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe Core.Bool)
eventSourceMappingConfiguration_bisectBatchOnFunctionError = Lens.lens (\EventSourceMappingConfiguration' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@EventSourceMappingConfiguration' {} a -> s {bisectBatchOnFunctionError = a} :: EventSourceMappingConfiguration)

-- | An array of the authentication protocol, or the VPC components to secure
-- your event source.
eventSourceMappingConfiguration_sourceAccessConfigurations :: Lens.Lens' EventSourceMappingConfiguration (Core.Maybe [SourceAccessConfiguration])
eventSourceMappingConfiguration_sourceAccessConfigurations = Lens.lens (\EventSourceMappingConfiguration' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@EventSourceMappingConfiguration' {} a -> s {sourceAccessConfigurations = a} :: EventSourceMappingConfiguration) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    EventSourceMappingConfiguration
  where
  parseJSON =
    Core.withObject
      "EventSourceMappingConfiguration"
      ( \x ->
          EventSourceMappingConfiguration'
            Core.<$> (x Core..:? "EventSourceArn")
            Core.<*> (x Core..:? "StateTransitionReason")
            Core.<*> (x Core..:? "LastProcessingResult")
            Core.<*> (x Core..:? "Topics")
            Core.<*> (x Core..:? "MaximumRecordAgeInSeconds")
            Core.<*> (x Core..:? "FunctionResponseTypes")
            Core.<*> (x Core..:? "Queues")
            Core.<*> (x Core..:? "TumblingWindowInSeconds")
            Core.<*> (x Core..:? "FunctionArn")
            Core.<*> (x Core..:? "StartingPositionTimestamp")
            Core.<*> (x Core..:? "State")
            Core.<*> (x Core..:? "MaximumBatchingWindowInSeconds")
            Core.<*> (x Core..:? "BatchSize")
            Core.<*> (x Core..:? "StartingPosition")
            Core.<*> (x Core..:? "DestinationConfig")
            Core.<*> (x Core..:? "MaximumRetryAttempts")
            Core.<*> (x Core..:? "LastModified")
            Core.<*> (x Core..:? "ParallelizationFactor")
            Core.<*> (x Core..:? "SelfManagedEventSource")
            Core.<*> (x Core..:? "UUID")
            Core.<*> (x Core..:? "BisectBatchOnFunctionError")
            Core.<*> ( x Core..:? "SourceAccessConfigurations"
                         Core..!= Core.mempty
                     )
      )

instance
  Core.Hashable
    EventSourceMappingConfiguration

instance Core.NFData EventSourceMappingConfiguration
