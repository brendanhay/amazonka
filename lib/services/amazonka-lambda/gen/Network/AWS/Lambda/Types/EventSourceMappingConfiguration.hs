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

-- | A mapping between an Amazon Web Services resource and a Lambda function.
-- For details, see CreateEventSourceMapping.
--
-- /See:/ 'newEventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
  { -- | The Amazon Resource Name (ARN) of the event source.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a user or Lambda made the last change to the event
    -- source mapping.
    stateTransitionReason :: Prelude.Maybe Prelude.Text,
    -- | The result of the last Lambda invocation of your function.
    lastProcessingResult :: Prelude.Maybe Prelude.Text,
    -- | (Amazon MQ) The name of the Amazon MQ broker destination queue to
    -- consume.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams only) Discard records older than the specified age. The default
    -- value is -1, which sets the maximum age to infinite. When the value is
    -- set to infinite, Lambda never discards old records.
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of the Kafka topic.
    topics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams only) A list of current response type enums applied to the
    -- event source mapping.
    functionResponseTypes :: Prelude.Maybe [FunctionResponseType],
    -- | (Streams only) The duration in seconds of a processing window. The range
    -- is 1–900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading.
    startingPositionTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The state of the event source mapping. It can be one of the following:
    -- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
    -- or @Deleting@.
    state :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Lambda function.
    functionArn :: Prelude.Maybe Prelude.Text,
    -- | (Streams and Amazon SQS standard queues) The maximum amount of time to
    -- gather records before invoking the function, in seconds. The default
    -- value is zero.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of items to retrieve in a single batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The position in a stream from which to start reading. Required for
    -- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK stream sources.
    -- @AT_TIMESTAMP@ is supported only for Amazon Kinesis streams.
    startingPosition :: Prelude.Maybe EventSourcePosition,
    -- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | (Streams only) Discard records after the specified number of retries.
    -- The default value is -1, which sets the maximum number of retries to
    -- infinite. When MaximumRetryAttempts is infinite, Lambda retries failed
    -- records until the record expires in the event source.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | The date that the event source mapping was last updated or that its
    -- state changed.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | The self-managed Apache Kafka cluster for your event source.
    selfManagedEventSource :: Prelude.Maybe SelfManagedEventSource,
    -- | (Streams only) The number of batches to process concurrently from each
    -- shard. The default value is 1.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) If the function returns an error, split the batch in two
    -- and retry. The default value is false.
    bisectBatchOnFunctionError :: Prelude.Maybe Prelude.Bool,
    -- | The identifier of the event source mapping.
    uuid :: Prelude.Maybe Prelude.Text,
    -- | An array of the authentication protocol, VPC components, or virtual host
    -- to secure and define your event source.
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
-- 'stateTransitionReason', 'eventSourceMappingConfiguration_stateTransitionReason' - Indicates whether a user or Lambda made the last change to the event
-- source mapping.
--
-- 'lastProcessingResult', 'eventSourceMappingConfiguration_lastProcessingResult' - The result of the last Lambda invocation of your function.
--
-- 'queues', 'eventSourceMappingConfiguration_queues' - (Amazon MQ) The name of the Amazon MQ broker destination queue to
-- consume.
--
-- 'maximumRecordAgeInSeconds', 'eventSourceMappingConfiguration_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, Lambda never discards old records.
--
-- 'topics', 'eventSourceMappingConfiguration_topics' - The name of the Kafka topic.
--
-- 'functionResponseTypes', 'eventSourceMappingConfiguration_functionResponseTypes' - (Streams only) A list of current response type enums applied to the
-- event source mapping.
--
-- 'tumblingWindowInSeconds', 'eventSourceMappingConfiguration_tumblingWindowInSeconds' - (Streams only) The duration in seconds of a processing window. The range
-- is 1–900 seconds.
--
-- 'startingPositionTimestamp', 'eventSourceMappingConfiguration_startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
--
-- 'state', 'eventSourceMappingConfiguration_state' - The state of the event source mapping. It can be one of the following:
-- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
-- or @Deleting@.
--
-- 'functionArn', 'eventSourceMappingConfiguration_functionArn' - The ARN of the Lambda function.
--
-- 'maximumBatchingWindowInSeconds', 'eventSourceMappingConfiguration_maximumBatchingWindowInSeconds' - (Streams and Amazon SQS standard queues) The maximum amount of time to
-- gather records before invoking the function, in seconds. The default
-- value is zero.
--
-- 'batchSize', 'eventSourceMappingConfiguration_batchSize' - The maximum number of items to retrieve in a single batch.
--
-- 'startingPosition', 'eventSourceMappingConfiguration_startingPosition' - The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK stream sources.
-- @AT_TIMESTAMP@ is supported only for Amazon Kinesis streams.
--
-- 'destinationConfig', 'eventSourceMappingConfiguration_destinationConfig' - (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'maximumRetryAttempts', 'eventSourceMappingConfiguration_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, Lambda retries failed
-- records until the record expires in the event source.
--
-- 'lastModified', 'eventSourceMappingConfiguration_lastModified' - The date that the event source mapping was last updated or that its
-- state changed.
--
-- 'selfManagedEventSource', 'eventSourceMappingConfiguration_selfManagedEventSource' - The self-managed Apache Kafka cluster for your event source.
--
-- 'parallelizationFactor', 'eventSourceMappingConfiguration_parallelizationFactor' - (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
--
-- 'bisectBatchOnFunctionError', 'eventSourceMappingConfiguration_bisectBatchOnFunctionError' - (Streams only) If the function returns an error, split the batch in two
-- and retry. The default value is false.
--
-- 'uuid', 'eventSourceMappingConfiguration_uuid' - The identifier of the event source mapping.
--
-- 'sourceAccessConfigurations', 'eventSourceMappingConfiguration_sourceAccessConfigurations' - An array of the authentication protocol, VPC components, or virtual host
-- to secure and define your event source.
newEventSourceMappingConfiguration ::
  EventSourceMappingConfiguration
newEventSourceMappingConfiguration =
  EventSourceMappingConfiguration'
    { eventSourceArn =
        Prelude.Nothing,
      stateTransitionReason = Prelude.Nothing,
      lastProcessingResult = Prelude.Nothing,
      queues = Prelude.Nothing,
      maximumRecordAgeInSeconds =
        Prelude.Nothing,
      topics = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      startingPositionTimestamp =
        Prelude.Nothing,
      state = Prelude.Nothing,
      functionArn = Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing,
      batchSize = Prelude.Nothing,
      startingPosition = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      selfManagedEventSource = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      bisectBatchOnFunctionError =
        Prelude.Nothing,
      uuid = Prelude.Nothing,
      sourceAccessConfigurations =
        Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the event source.
eventSourceMappingConfiguration_eventSourceArn :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_eventSourceArn = Lens.lens (\EventSourceMappingConfiguration' {eventSourceArn} -> eventSourceArn) (\s@EventSourceMappingConfiguration' {} a -> s {eventSourceArn = a} :: EventSourceMappingConfiguration)

-- | Indicates whether a user or Lambda made the last change to the event
-- source mapping.
eventSourceMappingConfiguration_stateTransitionReason :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_stateTransitionReason = Lens.lens (\EventSourceMappingConfiguration' {stateTransitionReason} -> stateTransitionReason) (\s@EventSourceMappingConfiguration' {} a -> s {stateTransitionReason = a} :: EventSourceMappingConfiguration)

-- | The result of the last Lambda invocation of your function.
eventSourceMappingConfiguration_lastProcessingResult :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_lastProcessingResult = Lens.lens (\EventSourceMappingConfiguration' {lastProcessingResult} -> lastProcessingResult) (\s@EventSourceMappingConfiguration' {} a -> s {lastProcessingResult = a} :: EventSourceMappingConfiguration)

-- | (Amazon MQ) The name of the Amazon MQ broker destination queue to
-- consume.
eventSourceMappingConfiguration_queues :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventSourceMappingConfiguration_queues = Lens.lens (\EventSourceMappingConfiguration' {queues} -> queues) (\s@EventSourceMappingConfiguration' {} a -> s {queues = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, Lambda never discards old records.
eventSourceMappingConfiguration_maximumRecordAgeInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Int)
eventSourceMappingConfiguration_maximumRecordAgeInSeconds = Lens.lens (\EventSourceMappingConfiguration' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {maximumRecordAgeInSeconds = a} :: EventSourceMappingConfiguration)

-- | The name of the Kafka topic.
eventSourceMappingConfiguration_topics :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
eventSourceMappingConfiguration_topics = Lens.lens (\EventSourceMappingConfiguration' {topics} -> topics) (\s@EventSourceMappingConfiguration' {} a -> s {topics = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams only) A list of current response type enums applied to the
-- event source mapping.
eventSourceMappingConfiguration_functionResponseTypes :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe [FunctionResponseType])
eventSourceMappingConfiguration_functionResponseTypes = Lens.lens (\EventSourceMappingConfiguration' {functionResponseTypes} -> functionResponseTypes) (\s@EventSourceMappingConfiguration' {} a -> s {functionResponseTypes = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams only) The duration in seconds of a processing window. The range
-- is 1–900 seconds.
eventSourceMappingConfiguration_tumblingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_tumblingWindowInSeconds = Lens.lens (\EventSourceMappingConfiguration' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {tumblingWindowInSeconds = a} :: EventSourceMappingConfiguration)

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
eventSourceMappingConfiguration_startingPositionTimestamp :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.UTCTime)
eventSourceMappingConfiguration_startingPositionTimestamp = Lens.lens (\EventSourceMappingConfiguration' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@EventSourceMappingConfiguration' {} a -> s {startingPositionTimestamp = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Core._Time

-- | The state of the event source mapping. It can be one of the following:
-- @Creating@, @Enabling@, @Enabled@, @Disabling@, @Disabled@, @Updating@,
-- or @Deleting@.
eventSourceMappingConfiguration_state :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_state = Lens.lens (\EventSourceMappingConfiguration' {state} -> state) (\s@EventSourceMappingConfiguration' {} a -> s {state = a} :: EventSourceMappingConfiguration)

-- | The ARN of the Lambda function.
eventSourceMappingConfiguration_functionArn :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_functionArn = Lens.lens (\EventSourceMappingConfiguration' {functionArn} -> functionArn) (\s@EventSourceMappingConfiguration' {} a -> s {functionArn = a} :: EventSourceMappingConfiguration)

-- | (Streams and Amazon SQS standard queues) The maximum amount of time to
-- gather records before invoking the function, in seconds. The default
-- value is zero.
eventSourceMappingConfiguration_maximumBatchingWindowInSeconds :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_maximumBatchingWindowInSeconds = Lens.lens (\EventSourceMappingConfiguration' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@EventSourceMappingConfiguration' {} a -> s {maximumBatchingWindowInSeconds = a} :: EventSourceMappingConfiguration)

-- | The maximum number of items to retrieve in a single batch.
eventSourceMappingConfiguration_batchSize :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_batchSize = Lens.lens (\EventSourceMappingConfiguration' {batchSize} -> batchSize) (\s@EventSourceMappingConfiguration' {} a -> s {batchSize = a} :: EventSourceMappingConfiguration)

-- | The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK stream sources.
-- @AT_TIMESTAMP@ is supported only for Amazon Kinesis streams.
eventSourceMappingConfiguration_startingPosition :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe EventSourcePosition)
eventSourceMappingConfiguration_startingPosition = Lens.lens (\EventSourceMappingConfiguration' {startingPosition} -> startingPosition) (\s@EventSourceMappingConfiguration' {} a -> s {startingPosition = a} :: EventSourceMappingConfiguration)

-- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
eventSourceMappingConfiguration_destinationConfig :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe DestinationConfig)
eventSourceMappingConfiguration_destinationConfig = Lens.lens (\EventSourceMappingConfiguration' {destinationConfig} -> destinationConfig) (\s@EventSourceMappingConfiguration' {} a -> s {destinationConfig = a} :: EventSourceMappingConfiguration)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, Lambda retries failed
-- records until the record expires in the event source.
eventSourceMappingConfiguration_maximumRetryAttempts :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Int)
eventSourceMappingConfiguration_maximumRetryAttempts = Lens.lens (\EventSourceMappingConfiguration' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@EventSourceMappingConfiguration' {} a -> s {maximumRetryAttempts = a} :: EventSourceMappingConfiguration)

-- | The date that the event source mapping was last updated or that its
-- state changed.
eventSourceMappingConfiguration_lastModified :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.UTCTime)
eventSourceMappingConfiguration_lastModified = Lens.lens (\EventSourceMappingConfiguration' {lastModified} -> lastModified) (\s@EventSourceMappingConfiguration' {} a -> s {lastModified = a} :: EventSourceMappingConfiguration) Prelude.. Lens.mapping Core._Time

-- | The self-managed Apache Kafka cluster for your event source.
eventSourceMappingConfiguration_selfManagedEventSource :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe SelfManagedEventSource)
eventSourceMappingConfiguration_selfManagedEventSource = Lens.lens (\EventSourceMappingConfiguration' {selfManagedEventSource} -> selfManagedEventSource) (\s@EventSourceMappingConfiguration' {} a -> s {selfManagedEventSource = a} :: EventSourceMappingConfiguration)

-- | (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
eventSourceMappingConfiguration_parallelizationFactor :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Natural)
eventSourceMappingConfiguration_parallelizationFactor = Lens.lens (\EventSourceMappingConfiguration' {parallelizationFactor} -> parallelizationFactor) (\s@EventSourceMappingConfiguration' {} a -> s {parallelizationFactor = a} :: EventSourceMappingConfiguration)

-- | (Streams only) If the function returns an error, split the batch in two
-- and retry. The default value is false.
eventSourceMappingConfiguration_bisectBatchOnFunctionError :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Bool)
eventSourceMappingConfiguration_bisectBatchOnFunctionError = Lens.lens (\EventSourceMappingConfiguration' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@EventSourceMappingConfiguration' {} a -> s {bisectBatchOnFunctionError = a} :: EventSourceMappingConfiguration)

-- | The identifier of the event source mapping.
eventSourceMappingConfiguration_uuid :: Lens.Lens' EventSourceMappingConfiguration (Prelude.Maybe Prelude.Text)
eventSourceMappingConfiguration_uuid = Lens.lens (\EventSourceMappingConfiguration' {uuid} -> uuid) (\s@EventSourceMappingConfiguration' {} a -> s {uuid = a} :: EventSourceMappingConfiguration)

-- | An array of the authentication protocol, VPC components, or virtual host
-- to secure and define your event source.
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
            Prelude.<*> (x Core..:? "Queues")
            Prelude.<*> (x Core..:? "MaximumRecordAgeInSeconds")
            Prelude.<*> (x Core..:? "Topics")
            Prelude.<*> ( x Core..:? "FunctionResponseTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TumblingWindowInSeconds")
            Prelude.<*> (x Core..:? "StartingPositionTimestamp")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "FunctionArn")
            Prelude.<*> (x Core..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Core..:? "BatchSize")
            Prelude.<*> (x Core..:? "StartingPosition")
            Prelude.<*> (x Core..:? "DestinationConfig")
            Prelude.<*> (x Core..:? "MaximumRetryAttempts")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "SelfManagedEventSource")
            Prelude.<*> (x Core..:? "ParallelizationFactor")
            Prelude.<*> (x Core..:? "BisectBatchOnFunctionError")
            Prelude.<*> (x Core..:? "UUID")
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
