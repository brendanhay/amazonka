{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.CreateEventSourceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mapping between an event source and an Lambda function. Lambda
-- reads items from the event source and triggers the function.
--
-- For details about each event source type, see the following topics. In
-- particular, each of the topics describes the required and optional
-- parameters for the specific event source.
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-ddb.html#services-dynamodb-eventsourcemapping Configuring a Dynamo DB stream as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-kinesis.html#services-kinesis-eventsourcemapping Configuring a Kinesis stream as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html#events-sqs-eventsource Configuring an SQS queue as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-mq.html#services-mq-eventsourcemapping Configuring an MQ broker as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html Configuring MSK as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/kafka-smaa.html Configuring Self-Managed Apache Kafka as an event source>
--
-- The following error handling options are only available for stream
-- sources (DynamoDB and Kinesis):
--
-- -   @BisectBatchOnFunctionError@ - If the function returns an error,
--     split the batch in two and retry.
--
-- -   @DestinationConfig@ - Send discarded records to an Amazon SQS queue
--     or Amazon SNS topic.
--
-- -   @MaximumRecordAgeInSeconds@ - Discard records older than the
--     specified age. The default value is infinite (-1). When set to
--     infinite (-1), failed records are retried until the record expires
--
-- -   @MaximumRetryAttempts@ - Discard records after the specified number
--     of retries. The default value is infinite (-1). When set to infinite
--     (-1), failed records are retried until the record expires.
--
-- -   @ParallelizationFactor@ - Process multiple batches from each shard
--     concurrently.
module Network.AWS.Lambda.CreateEventSourceMapping
  ( -- * Creating a Request
    CreateEventSourceMapping (..),
    newCreateEventSourceMapping,

    -- * Request Lenses
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_enabled,
    createEventSourceMapping_queues,
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_topics,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_functionName,

    -- * Destructuring the Response
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,

    -- * Response Lenses
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateEventSourceMapping' smart constructor.
data CreateEventSourceMapping = CreateEventSourceMapping'
  { -- | The Amazon Resource Name (ARN) of the event source.
    --
    -- -   __Amazon Kinesis__ - The ARN of the data stream or a stream
    --     consumer.
    --
    -- -   __Amazon DynamoDB Streams__ - The ARN of the stream.
    --
    -- -   __Amazon Simple Queue Service__ - The ARN of the queue.
    --
    -- -   __Amazon Managed Streaming for Apache Kafka__ - The ARN of the
    --     cluster.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | If true, the event source mapping is active. Set to false to pause
    -- polling and invocation.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (MQ) The name of the Amazon MQ broker destination queue to consume.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams only) Discard records older than the specified age. The default
    -- value is infinite (-1).
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | The name of the Kafka topic.
    topics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams only) A list of current response type enums applied to the
    -- event source mapping.
    functionResponseTypes :: Prelude.Maybe [FunctionResponseType],
    -- | (Streams only) The duration in seconds of a processing window. The range
    -- is between 1 second up to 900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading.
    startingPositionTimestamp :: Prelude.Maybe Core.POSIX,
    -- | (Streams and SQS standard queues) The maximum amount of time to gather
    -- records before invoking the function, in seconds.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of items to retrieve in a single batch.
    --
    -- -   __Amazon Kinesis__ - Default 100. Max 10,000.
    --
    -- -   __Amazon DynamoDB Streams__ - Default 100. Max 1,000.
    --
    -- -   __Amazon Simple Queue Service__ - Default 10. For standard queues
    --     the max is 10,000. For FIFO queues the max is 10.
    --
    -- -   __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max
    --     10,000.
    --
    -- -   __Self-Managed Apache Kafka__ - Default 100. Max 10,000.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | The position in a stream from which to start reading. Required for
    -- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
    -- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
    startingPosition :: Prelude.Maybe EventSourcePosition,
    -- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | (Streams only) Discard records after the specified number of retries.
    -- The default value is infinite (-1). When set to infinite (-1), failed
    -- records will be retried until the record expires.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | The Self-Managed Apache Kafka cluster to send records.
    selfManagedEventSource :: Prelude.Maybe SelfManagedEventSource,
    -- | (Streams only) The number of batches to process from each shard
    -- concurrently.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) If the function returns an error, split the batch in two
    -- and retry.
    bisectBatchOnFunctionError :: Prelude.Maybe Prelude.Bool,
    -- | An array of authentication protocols or VPC components required to
    -- secure your event source.
    sourceAccessConfigurations :: Prelude.Maybe [SourceAccessConfiguration],
    -- | The name of the Lambda function.
    --
    -- __Name formats__
    --
    -- -   __Function name__ - @MyFunction@.
    --
    -- -   __Function ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
    --
    -- -   __Version or Alias ARN__ -
    --     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@.
    --
    -- -   __Partial ARN__ - @123456789012:function:MyFunction@.
    --
    -- The length constraint applies only to the full ARN. If you specify only
    -- the function name, it\'s limited to 64 characters in length.
    functionName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateEventSourceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'createEventSourceMapping_eventSourceArn' - The Amazon Resource Name (ARN) of the event source.
--
-- -   __Amazon Kinesis__ - The ARN of the data stream or a stream
--     consumer.
--
-- -   __Amazon DynamoDB Streams__ - The ARN of the stream.
--
-- -   __Amazon Simple Queue Service__ - The ARN of the queue.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - The ARN of the
--     cluster.
--
-- 'enabled', 'createEventSourceMapping_enabled' - If true, the event source mapping is active. Set to false to pause
-- polling and invocation.
--
-- 'queues', 'createEventSourceMapping_queues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- 'maximumRecordAgeInSeconds', 'createEventSourceMapping_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
--
-- 'topics', 'createEventSourceMapping_topics' - The name of the Kafka topic.
--
-- 'functionResponseTypes', 'createEventSourceMapping_functionResponseTypes' - (Streams only) A list of current response type enums applied to the
-- event source mapping.
--
-- 'tumblingWindowInSeconds', 'createEventSourceMapping_tumblingWindowInSeconds' - (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second up to 900 seconds.
--
-- 'startingPositionTimestamp', 'createEventSourceMapping_startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
--
-- 'maximumBatchingWindowInSeconds', 'createEventSourceMapping_maximumBatchingWindowInSeconds' - (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds.
--
-- 'batchSize', 'createEventSourceMapping_batchSize' - The maximum number of items to retrieve in a single batch.
--
-- -   __Amazon Kinesis__ - Default 100. Max 10,000.
--
-- -   __Amazon DynamoDB Streams__ - Default 100. Max 1,000.
--
-- -   __Amazon Simple Queue Service__ - Default 10. For standard queues
--     the max is 10,000. For FIFO queues the max is 10.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max
--     10,000.
--
-- -   __Self-Managed Apache Kafka__ - Default 100. Max 10,000.
--
-- 'startingPosition', 'createEventSourceMapping_startingPosition' - The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
--
-- 'destinationConfig', 'createEventSourceMapping_destinationConfig' - (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'maximumRetryAttempts', 'createEventSourceMapping_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
--
-- 'selfManagedEventSource', 'createEventSourceMapping_selfManagedEventSource' - The Self-Managed Apache Kafka cluster to send records.
--
-- 'parallelizationFactor', 'createEventSourceMapping_parallelizationFactor' - (Streams only) The number of batches to process from each shard
-- concurrently.
--
-- 'bisectBatchOnFunctionError', 'createEventSourceMapping_bisectBatchOnFunctionError' - (Streams only) If the function returns an error, split the batch in two
-- and retry.
--
-- 'sourceAccessConfigurations', 'createEventSourceMapping_sourceAccessConfigurations' - An array of authentication protocols or VPC components required to
-- secure your event source.
--
-- 'functionName', 'createEventSourceMapping_functionName' - The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Version or Alias ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it\'s limited to 64 characters in length.
newCreateEventSourceMapping ::
  -- | 'functionName'
  Prelude.Text ->
  CreateEventSourceMapping
newCreateEventSourceMapping pFunctionName_ =
  CreateEventSourceMapping'
    { eventSourceArn =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      queues = Prelude.Nothing,
      maximumRecordAgeInSeconds = Prelude.Nothing,
      topics = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      startingPositionTimestamp = Prelude.Nothing,
      maximumBatchingWindowInSeconds = Prelude.Nothing,
      batchSize = Prelude.Nothing,
      startingPosition = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      selfManagedEventSource = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      bisectBatchOnFunctionError = Prelude.Nothing,
      sourceAccessConfigurations = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | The Amazon Resource Name (ARN) of the event source.
--
-- -   __Amazon Kinesis__ - The ARN of the data stream or a stream
--     consumer.
--
-- -   __Amazon DynamoDB Streams__ - The ARN of the stream.
--
-- -   __Amazon Simple Queue Service__ - The ARN of the queue.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - The ARN of the
--     cluster.
createEventSourceMapping_eventSourceArn :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Text)
createEventSourceMapping_eventSourceArn = Lens.lens (\CreateEventSourceMapping' {eventSourceArn} -> eventSourceArn) (\s@CreateEventSourceMapping' {} a -> s {eventSourceArn = a} :: CreateEventSourceMapping)

-- | If true, the event source mapping is active. Set to false to pause
-- polling and invocation.
createEventSourceMapping_enabled :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Bool)
createEventSourceMapping_enabled = Lens.lens (\CreateEventSourceMapping' {enabled} -> enabled) (\s@CreateEventSourceMapping' {} a -> s {enabled = a} :: CreateEventSourceMapping)

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
createEventSourceMapping_queues :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createEventSourceMapping_queues = Lens.lens (\CreateEventSourceMapping' {queues} -> queues) (\s@CreateEventSourceMapping' {} a -> s {queues = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
createEventSourceMapping_maximumRecordAgeInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Int)
createEventSourceMapping_maximumRecordAgeInSeconds = Lens.lens (\CreateEventSourceMapping' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@CreateEventSourceMapping' {} a -> s {maximumRecordAgeInSeconds = a} :: CreateEventSourceMapping)

-- | The name of the Kafka topic.
createEventSourceMapping_topics :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createEventSourceMapping_topics = Lens.lens (\CreateEventSourceMapping' {topics} -> topics) (\s@CreateEventSourceMapping' {} a -> s {topics = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams only) A list of current response type enums applied to the
-- event source mapping.
createEventSourceMapping_functionResponseTypes :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe [FunctionResponseType])
createEventSourceMapping_functionResponseTypes = Lens.lens (\CreateEventSourceMapping' {functionResponseTypes} -> functionResponseTypes) (\s@CreateEventSourceMapping' {} a -> s {functionResponseTypes = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens._Coerce

-- | (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second up to 900 seconds.
createEventSourceMapping_tumblingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_tumblingWindowInSeconds = Lens.lens (\CreateEventSourceMapping' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@CreateEventSourceMapping' {} a -> s {tumblingWindowInSeconds = a} :: CreateEventSourceMapping)

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
createEventSourceMapping_startingPositionTimestamp :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.UTCTime)
createEventSourceMapping_startingPositionTimestamp = Lens.lens (\CreateEventSourceMapping' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@CreateEventSourceMapping' {} a -> s {startingPositionTimestamp = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Core._Time

-- | (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds.
createEventSourceMapping_maximumBatchingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_maximumBatchingWindowInSeconds = Lens.lens (\CreateEventSourceMapping' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@CreateEventSourceMapping' {} a -> s {maximumBatchingWindowInSeconds = a} :: CreateEventSourceMapping)

-- | The maximum number of items to retrieve in a single batch.
--
-- -   __Amazon Kinesis__ - Default 100. Max 10,000.
--
-- -   __Amazon DynamoDB Streams__ - Default 100. Max 1,000.
--
-- -   __Amazon Simple Queue Service__ - Default 10. For standard queues
--     the max is 10,000. For FIFO queues the max is 10.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max
--     10,000.
--
-- -   __Self-Managed Apache Kafka__ - Default 100. Max 10,000.
createEventSourceMapping_batchSize :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_batchSize = Lens.lens (\CreateEventSourceMapping' {batchSize} -> batchSize) (\s@CreateEventSourceMapping' {} a -> s {batchSize = a} :: CreateEventSourceMapping)

-- | The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
createEventSourceMapping_startingPosition :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe EventSourcePosition)
createEventSourceMapping_startingPosition = Lens.lens (\CreateEventSourceMapping' {startingPosition} -> startingPosition) (\s@CreateEventSourceMapping' {} a -> s {startingPosition = a} :: CreateEventSourceMapping)

-- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
createEventSourceMapping_destinationConfig :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe DestinationConfig)
createEventSourceMapping_destinationConfig = Lens.lens (\CreateEventSourceMapping' {destinationConfig} -> destinationConfig) (\s@CreateEventSourceMapping' {} a -> s {destinationConfig = a} :: CreateEventSourceMapping)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
createEventSourceMapping_maximumRetryAttempts :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Int)
createEventSourceMapping_maximumRetryAttempts = Lens.lens (\CreateEventSourceMapping' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@CreateEventSourceMapping' {} a -> s {maximumRetryAttempts = a} :: CreateEventSourceMapping)

-- | The Self-Managed Apache Kafka cluster to send records.
createEventSourceMapping_selfManagedEventSource :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe SelfManagedEventSource)
createEventSourceMapping_selfManagedEventSource = Lens.lens (\CreateEventSourceMapping' {selfManagedEventSource} -> selfManagedEventSource) (\s@CreateEventSourceMapping' {} a -> s {selfManagedEventSource = a} :: CreateEventSourceMapping)

-- | (Streams only) The number of batches to process from each shard
-- concurrently.
createEventSourceMapping_parallelizationFactor :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_parallelizationFactor = Lens.lens (\CreateEventSourceMapping' {parallelizationFactor} -> parallelizationFactor) (\s@CreateEventSourceMapping' {} a -> s {parallelizationFactor = a} :: CreateEventSourceMapping)

-- | (Streams only) If the function returns an error, split the batch in two
-- and retry.
createEventSourceMapping_bisectBatchOnFunctionError :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Bool)
createEventSourceMapping_bisectBatchOnFunctionError = Lens.lens (\CreateEventSourceMapping' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@CreateEventSourceMapping' {} a -> s {bisectBatchOnFunctionError = a} :: CreateEventSourceMapping)

-- | An array of authentication protocols or VPC components required to
-- secure your event source.
createEventSourceMapping_sourceAccessConfigurations :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe [SourceAccessConfiguration])
createEventSourceMapping_sourceAccessConfigurations = Lens.lens (\CreateEventSourceMapping' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@CreateEventSourceMapping' {} a -> s {sourceAccessConfigurations = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the Lambda function.
--
-- __Name formats__
--
-- -   __Function name__ - @MyFunction@.
--
-- -   __Function ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@.
--
-- -   __Version or Alias ARN__ -
--     @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@.
--
-- -   __Partial ARN__ - @123456789012:function:MyFunction@.
--
-- The length constraint applies only to the full ARN. If you specify only
-- the function name, it\'s limited to 64 characters in length.
createEventSourceMapping_functionName :: Lens.Lens' CreateEventSourceMapping Prelude.Text
createEventSourceMapping_functionName = Lens.lens (\CreateEventSourceMapping' {functionName} -> functionName) (\s@CreateEventSourceMapping' {} a -> s {functionName = a} :: CreateEventSourceMapping)

instance Core.AWSRequest CreateEventSourceMapping where
  type
    AWSResponse CreateEventSourceMapping =
      EventSourceMappingConfiguration
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable CreateEventSourceMapping

instance Prelude.NFData CreateEventSourceMapping

instance Core.ToHeaders CreateEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateEventSourceMapping where
  toJSON CreateEventSourceMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EventSourceArn" Core..=)
              Prelude.<$> eventSourceArn,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("Queues" Core..=) Prelude.<$> queues,
            ("MaximumRecordAgeInSeconds" Core..=)
              Prelude.<$> maximumRecordAgeInSeconds,
            ("Topics" Core..=) Prelude.<$> topics,
            ("FunctionResponseTypes" Core..=)
              Prelude.<$> functionResponseTypes,
            ("TumblingWindowInSeconds" Core..=)
              Prelude.<$> tumblingWindowInSeconds,
            ("StartingPositionTimestamp" Core..=)
              Prelude.<$> startingPositionTimestamp,
            ("MaximumBatchingWindowInSeconds" Core..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("BatchSize" Core..=) Prelude.<$> batchSize,
            ("StartingPosition" Core..=)
              Prelude.<$> startingPosition,
            ("DestinationConfig" Core..=)
              Prelude.<$> destinationConfig,
            ("MaximumRetryAttempts" Core..=)
              Prelude.<$> maximumRetryAttempts,
            ("SelfManagedEventSource" Core..=)
              Prelude.<$> selfManagedEventSource,
            ("ParallelizationFactor" Core..=)
              Prelude.<$> parallelizationFactor,
            ("BisectBatchOnFunctionError" Core..=)
              Prelude.<$> bisectBatchOnFunctionError,
            ("SourceAccessConfigurations" Core..=)
              Prelude.<$> sourceAccessConfigurations,
            Prelude.Just ("FunctionName" Core..= functionName)
          ]
      )

instance Core.ToPath CreateEventSourceMapping where
  toPath =
    Prelude.const "/2015-03-31/event-source-mappings/"

instance Core.ToQuery CreateEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
