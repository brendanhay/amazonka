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
import qualified Network.AWS.Prelude as Prelude

-- | A mapping between an AWS resource and an AWS Lambda function. See
-- CreateEventSourceMapping for details.
--
-- /See:/ 'newEventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the event source.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the last change to the event source mapping was made
    -- by a user, or by the Lambda service.
    stateTransitionReason :: Prelude.Maybe Prelude.Text,
    -- | The result of the last AWS Lambda invocation of your Lambda function.
    lastProcessingResult :: Prelude.Maybe Prelude.Text,
    -- | The name of the Kafka topic.
    topics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams) Discard records older than the specified age. The default
    -- value is infinite (-1). When set to infinite (-1), failed records are
    -- retried until the record expires.
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | (Streams) A list of current response type enums applied to the event
    -- source mapping.
    functionResponseTypes :: Prelude.Maybe (Prelude.NonEmpty FunctionResponseType),
    -- | (MQ) The name of the Amazon MQ broker destination queue to consume.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams) The duration in seconds of a processing window. The range is
    -- between 1 second up to 900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the Lambda function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading.
    startingPositionTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The state of the event source mapping. It can be one of the following:
    -- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
    -- or @Deleting@.
    state :: Prelude.Maybe Prelude.Text,
    -- | (Streams and SQS standard queues) The maximum amount of time to gather
    -- records before invoking the function, in seconds. The default value is
    -- zero.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of items to retrieve in a single batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The position in a stream from which to start reading. Required for
    -- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
    -- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
    startingPosition :: Prelude.Maybe EventSourcePosition,
    -- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | (Streams) Discard records after the specified number of retries. The
    -- default value is infinite (-1). When set to infinite (-1), failed
    -- records are retried until the record expires.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | The date that the event source mapping was last updated, or its state
    -- changed.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | (Streams) The number of batches to process from each shard concurrently.
    -- The default value is 1.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | The Self-Managed Apache Kafka cluster for your event source.
    selfManagedEventSource :: Prelude.Maybe SelfManagedEventSource,
    -- | The identifier of the event source mapping.
    uuid :: Prelude.Maybe Prelude.Text,
    -- | (Streams) If the function returns an error, split the batch in two and
    -- retry. The default value is false.
    bisectBatchOnFunctionError :: Prelude.Maybe Prelude.Bool,
    -- | An array of the authentication protocol, or the VPC components to secure
    -- your event source.
    sourceAccessConfigurations :: Prelude.Maybe [SourceAccessConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing,
      stateTransitionReason = Prelude.Nothing,
      lastProcessingResult = Prelude.Nothing,
      topics = Prelude.Nothing,
      maximumRecordAgeInSeconds =
        Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      queues = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      startingPositionTimestamp =
        Prelude.Nothing,
      state = Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing,
      batchSize = Prelude.Nothing,
      startingPosition = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      selfManagedEventSource = Prelude.Nothing,
      uuid = Prelude.Nothing,
      bisectBatchOnFunctionError =
        Prelude.Nothing,
      sourceAccessConfigurations =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
eventSourceMappingConfiguration_eventSourceArn :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_eventSourceArn = Lens.lens (\EventSourceMappingConfiguration' {eventSourceArn} -> eventSourceArn) (\s@EventSourceMappingConfiguration' {} a -> s {eventSourceArn = a} :: EventSourceMappingConfiguration)

-- | Indicates whether the last change to the event source mapping was made
-- by a user, or by the Lambda service.
eventSourceMappingConfiguration_stateTransitionReason :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_stateTransitionReason = Lens.lens (\EventSourceMappingConfiguration' {stateTransitionReason} -> stateTransitionReason) (\s@EventSourceMappingConfiguration' {} a -> s {stateTransitionReason = a} :: EventSourceMappingConfiguration)

-- | The result of the last AWS Lambda invocation of your Lambda function.
eventSourceMappingConfiguration_lastProcessingResult :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_lastProcessingResult = Lens.lens (\EventSourceMappingConfiguration' {lastProcessingResult} -> lastProcessingResult) (\s@EventSourceMappingConfiguration' {} a -> s {lastProcessingResult = a} :: EventSourceMappingConfiguration)

-- | The name of the Kafka topic.
eventSourceMappingConfiguration_topics :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventSourceMappingConfiguration_topics = Lens.lens (\EventSourceMappingConfiguration' {topics} -> topics) (\s@EventSourceMappingConfiguration' {} a -> s {topics = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams) Discard records older than the specified age. The default
-- value is infinite (-1). When set to infinite (-1), failed records are
-- retried until the record expires.
eventSourceMappingConfiguration_maximumRecordAgeInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Int)
eventSourceMappingConfiguration_maximumRecordAgeInSeconds = Lens.lens (\EventSourceMappingConfiguration' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {maximumRecordAgeInSeconds = a} :: EventSourceMappingConfiguration)

-- | (Streams) A list of current response type enums applied to the event
-- source mapping.
eventSourceMappingConfiguration_functionResponseTypes :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe (Prelude.NonEmpty FunctionResponseType))
eventSourceMappingConfiguration_functionResponseTypes = Lens.lens (\EventSourceMappingConfiguration' {functionResponseTypes} -> functionResponseTypes) (\s@EventSourceMappingConfiguration' {} a -> s {functionResponseTypes = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
eventSourceMappingConfiguration_queues :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventSourceMappingConfiguration_queues = Lens.lens (\EventSourceMappingConfiguration' {queues} -> queues) (\s@EventSourceMappingConfiguration' {} a -> s {queues = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams) The duration in seconds of a processing window. The range is
-- between 1 second up to 900 seconds.
eventSourceMappingConfiguration_tumblingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_tumblingWindowInSeconds = Lens.lens (\EventSourceMappingConfiguration' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {tumblingWindowInSeconds = a} :: EventSourceMappingConfiguration)

-- | The ARN of the Lambda function.
eventSourceMappingConfiguration_functionArn :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_functionArn = Lens.lens (\EventSourceMappingConfiguration' {functionArn} -> functionArn) (\s@EventSourceMappingConfiguration' {} a -> s {functionArn = a} :: EventSourceMappingConfiguration)

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
eventSourceMappingConfiguration_startingPositionTimestamp :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.UTCTime)
eventSourceMappingConfiguration_startingPositionTimestamp = Lens.lens (\EventSourceMappingConfiguration' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@EventSourceMappingConfiguration' {} a -> s {startingPositionTimestamp = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Core._Time

-- | The state of the event source mapping. It can be one of the following:
-- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
-- or @Deleting@.
eventSourceMappingConfiguration_state :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_state = Lens.lens (\EventSourceMappingConfiguration' {state} -> state) (\s@EventSourceMappingConfiguration' {} a -> s {state = a} :: EventSourceMappingConfiguration)

-- | (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds. The default value is
-- zero.
eventSourceMappingConfiguration_maximumBatchingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_maximumBatchingWindowInSeconds = Lens.lens (\EventSourceMappingConfiguration' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {maximumBatchingWindowInSeconds = a} :: EventSourceMappingConfiguration)

-- | The maximum number of items to retrieve in a single batch.
eventSourceMappingConfiguration_batchSize :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_batchSize = Lens.lens (\EventSourceMappingConfiguration' {batchSize} -> batchSize) (\s@EventSourceMappingConfiguration' {} a -> s {batchSize = a} :: EventSourceMappingConfiguration)

-- | The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
eventSourceMappingConfiguration_startingPosition :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe EventSourcePosition)
eventSourceMappingConfiguration_startingPosition = Lens.lens (\EventSourceMappingConfiguration' {startingPosition} -> startingPosition) (\s@EventSourceMappingConfiguration' {} a -> s {startingPosition = a} :: EventSourceMappingConfiguration)

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
eventSourceMappingConfiguration_destinationConfig :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe DestinationConfig)
eventSourceMappingConfiguration_destinationConfig = Lens.lens (\EventSourceMappingConfiguration' {destinationConfig} -> destinationConfig) (\s@EventSourceMappingConfiguration' {} a -> s {destinationConfig = a} :: EventSourceMappingConfiguration)

-- | (Streams) Discard records after the specified number of retries. The
-- default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
eventSourceMappingConfiguration_maximumRetryAttempts :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Int)
eventSourceMappingConfiguration_maximumRetryAttempts = Lens.lens (\EventSourceMappingConfiguration' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@EventSourceMappingConfiguration' {} a -> s {maximumRetryAttempts = a} :: EventSourceMappingConfiguration)

-- | The date that the event source mapping was last updated, or its state
-- changed.
eventSourceMappingConfiguration_lastModified :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.UTCTime)
eventSourceMappingConfiguration_lastModified = Lens.lens (\EventSourceMappingConfiguration' {lastModified} -> lastModified) (\s@EventSourceMappingConfiguration' {} a -> s {lastModified = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Core._Time

-- | (Streams) The number of batches to process from each shard concurrently.
-- The default value is 1.
eventSourceMappingConfiguration_parallelizationFactor :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_parallelizationFactor = Lens.lens (\EventSourceMappingConfiguration' {parallelizationFactor} -> parallelizationFactor) (\s@EventSourceMappingConfiguration' {} a -> s {parallelizationFactor = a} :: EventSourceMappingConfiguration)

-- | The Self-Managed Apache Kafka cluster for your event source.
eventSourceMappingConfiguration_selfManagedEventSource :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe SelfManagedEventSource)
eventSourceMappingConfiguration_selfManagedEventSource = Lens.lens (\EventSourceMappingConfiguration' {selfManagedEventSource} -> selfManagedEventSource) (\s@EventSourceMappingConfiguration' {} a -> s {selfManagedEventSource = a} :: EventSourceMappingConfiguration)

-- | The identifier of the event source mapping.
eventSourceMappingConfiguration_uuid :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_uuid = Lens.lens (\EventSourceMappingConfiguration' {uuid} -> uuid) (\s@EventSourceMappingConfiguration' {} a -> s {uuid = a} :: EventSourceMappingConfiguration)

-- | (Streams) If the function returns an error, split the batch in two and
-- retry. The default value is false.
eventSourceMappingConfiguration_bisectBatchOnFunctionError :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Bool)
eventSourceMappingConfiguration_bisectBatchOnFunctionError = Lens.lens (\EventSourceMappingConfiguration' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@EventSourceMappingConfiguration' {} a -> s {bisectBatchOnFunctionError = a} :: EventSourceMappingConfiguration)

-- | An array of the authentication protocol, or the VPC components to secure
-- your event source.
eventSourceMappingConfiguration_sourceAccessConfigurations :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe [SourceAccessConfiguration])
eventSourceMappingConfiguration_sourceAccessConfigurations = Lens.lens (\EventSourceMappingConfiguration' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@EventSourceMappingConfiguration' {} a -> s {sourceAccessConfigurations = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    EventSourceMappingConfiguration
  where
  parseJSON =
    Core.withObject
      "EventSourceMappingConfiguration"
      ( \x ->
          EventSourceMappingConfiguration'
            Prelude.<$> (x Core..:? "EventSourceArn")
            Prelude.<*> (x Core..:? "StateTransitionReason")
            Prelude.<*> (x Core..:? "LastProcessingResult")
            Prelude.<*> (x Core..:? "Topics")
            Prelude.<*> (x Core..:? "MaximumRecordAgeInSeconds")
            Prelude.<*> (x Core..:? "FunctionResponseTypes")
            Prelude.<*> (x Core..:? "Queues")
            Prelude.<*> (x Core..:? "TumblingWindowInSeconds")
            Prelude.<*> (x Core..:? "FunctionArn")
            Prelude.<*> (x Core..:? "StartingPositionTimestamp")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Core..:? "BatchSize")
            Prelude.<*> (x Core..:? "StartingPosition")
            Prelude.<*> (x Core..:? "DestinationConfig")
            Prelude.<*> (x Core..:? "MaximumRetryAttempts")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "ParallelizationFactor")
            Prelude.<*> (x Core..:? "SelfManagedEventSource")
            Prelude.<*> (x Core..:? "UUID")
            Prelude.<*> (x Core..:? "BisectBatchOnFunctionError")
            Prelude.<*> ( x Core..:? "SourceAccessConfigurations"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    EventSourceMappingConfiguration

instance
  Prelude.NFData
    EventSourceMappingConfiguration
