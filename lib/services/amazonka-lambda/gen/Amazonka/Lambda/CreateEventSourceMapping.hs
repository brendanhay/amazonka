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
-- Module      : Amazonka.Lambda.CreateEventSourceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mapping between an event source and an Lambda function. Lambda
-- reads items from the event source and triggers the function.
--
-- For details about each event source type, see the following topics.
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-ddb.html#services-dynamodb-eventsourcemapping Configuring a Dynamo DB stream as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-kinesis.html#services-kinesis-eventsourcemapping Configuring a Kinesis stream as an event source>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html#events-sqs-eventsource Configuring an Amazon SQS queue as an event source>
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
module Amazonka.Lambda.CreateEventSourceMapping
  ( -- * Creating a Request
    CreateEventSourceMapping (..),
    newCreateEventSourceMapping,

    -- * Request Lenses
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_topics,
    createEventSourceMapping_queues,
    createEventSourceMapping_enabled,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_functionName,

    -- * Destructuring the Response
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,

    -- * Response Lenses
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_startingPosition,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Lambda.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading.
    startingPositionTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name of the Kafka topic.
    topics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (MQ) The name of the Amazon MQ broker destination queue to consume.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | When true, the event source mapping is active. When false, Lambda pauses
    -- polling and invocation.
    --
    -- Default: True
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (Streams only) If the function returns an error, split the batch in two
    -- and retry.
    bisectBatchOnFunctionError :: Prelude.Maybe Prelude.Bool,
    -- | (Streams only) The number of batches to process from each shard
    -- concurrently.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) Discard records after the specified number of retries.
    -- The default value is infinite (-1). When set to infinite (-1), failed
    -- records will be retried until the record expires.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of records in each batch that Lambda pulls from your
    -- stream or queue and sends to your function. Lambda passes all of the
    -- records in the batch to the function in a single call, up to the payload
    -- limit for synchronous invocation (6 MB).
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
    -- | (Streams and Amazon SQS standard queues) The maximum amount of time, in
    -- seconds, that Lambda spends gathering records before invoking the
    -- function.
    --
    -- Default: 0
    --
    -- Related setting: When you set @BatchSize@ to a value greater than 10,
    -- you must set @MaximumBatchingWindowInSeconds@ to at least 1.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | An array of authentication protocols or VPC components required to
    -- secure your event source.
    sourceAccessConfigurations :: Prelude.Maybe [SourceAccessConfiguration],
    -- | (Streams only) Discard records older than the specified age. The default
    -- value is infinite (-1).
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | (Streams only) A list of current response type enums applied to the
    -- event source mapping.
    functionResponseTypes :: Prelude.Maybe [FunctionResponseType],
    -- | (Streams only) The duration in seconds of a processing window. The range
    -- is between 1 second up to 900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Self-Managed Apache Kafka cluster to send records.
    selfManagedEventSource :: Prelude.Maybe SelfManagedEventSource,
    -- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | The position in a stream from which to start reading. Required for
    -- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
    -- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
    startingPosition :: Prelude.Maybe EventSourcePosition,
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
-- 'startingPositionTimestamp', 'createEventSourceMapping_startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
--
-- 'topics', 'createEventSourceMapping_topics' - The name of the Kafka topic.
--
-- 'queues', 'createEventSourceMapping_queues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- 'enabled', 'createEventSourceMapping_enabled' - When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
--
-- 'bisectBatchOnFunctionError', 'createEventSourceMapping_bisectBatchOnFunctionError' - (Streams only) If the function returns an error, split the batch in two
-- and retry.
--
-- 'parallelizationFactor', 'createEventSourceMapping_parallelizationFactor' - (Streams only) The number of batches to process from each shard
-- concurrently.
--
-- 'maximumRetryAttempts', 'createEventSourceMapping_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
--
-- 'batchSize', 'createEventSourceMapping_batchSize' - The maximum number of records in each batch that Lambda pulls from your
-- stream or queue and sends to your function. Lambda passes all of the
-- records in the batch to the function in a single call, up to the payload
-- limit for synchronous invocation (6 MB).
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
-- 'maximumBatchingWindowInSeconds', 'createEventSourceMapping_maximumBatchingWindowInSeconds' - (Streams and Amazon SQS standard queues) The maximum amount of time, in
-- seconds, that Lambda spends gathering records before invoking the
-- function.
--
-- Default: 0
--
-- Related setting: When you set @BatchSize@ to a value greater than 10,
-- you must set @MaximumBatchingWindowInSeconds@ to at least 1.
--
-- 'sourceAccessConfigurations', 'createEventSourceMapping_sourceAccessConfigurations' - An array of authentication protocols or VPC components required to
-- secure your event source.
--
-- 'maximumRecordAgeInSeconds', 'createEventSourceMapping_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
--
-- 'functionResponseTypes', 'createEventSourceMapping_functionResponseTypes' - (Streams only) A list of current response type enums applied to the
-- event source mapping.
--
-- 'tumblingWindowInSeconds', 'createEventSourceMapping_tumblingWindowInSeconds' - (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second up to 900 seconds.
--
-- 'selfManagedEventSource', 'createEventSourceMapping_selfManagedEventSource' - The Self-Managed Apache Kafka cluster to send records.
--
-- 'destinationConfig', 'createEventSourceMapping_destinationConfig' - (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'startingPosition', 'createEventSourceMapping_startingPosition' - The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
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
      startingPositionTimestamp = Prelude.Nothing,
      topics = Prelude.Nothing,
      queues = Prelude.Nothing,
      enabled = Prelude.Nothing,
      bisectBatchOnFunctionError = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      batchSize = Prelude.Nothing,
      maximumBatchingWindowInSeconds = Prelude.Nothing,
      sourceAccessConfigurations = Prelude.Nothing,
      maximumRecordAgeInSeconds = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      selfManagedEventSource = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      startingPosition = Prelude.Nothing,
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

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
createEventSourceMapping_startingPositionTimestamp :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.UTCTime)
createEventSourceMapping_startingPositionTimestamp = Lens.lens (\CreateEventSourceMapping' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@CreateEventSourceMapping' {} a -> s {startingPositionTimestamp = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Core._Time

-- | The name of the Kafka topic.
createEventSourceMapping_topics :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createEventSourceMapping_topics = Lens.lens (\CreateEventSourceMapping' {topics} -> topics) (\s@CreateEventSourceMapping' {} a -> s {topics = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
createEventSourceMapping_queues :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createEventSourceMapping_queues = Lens.lens (\CreateEventSourceMapping' {queues} -> queues) (\s@CreateEventSourceMapping' {} a -> s {queues = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
createEventSourceMapping_enabled :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Bool)
createEventSourceMapping_enabled = Lens.lens (\CreateEventSourceMapping' {enabled} -> enabled) (\s@CreateEventSourceMapping' {} a -> s {enabled = a} :: CreateEventSourceMapping)

-- | (Streams only) If the function returns an error, split the batch in two
-- and retry.
createEventSourceMapping_bisectBatchOnFunctionError :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Bool)
createEventSourceMapping_bisectBatchOnFunctionError = Lens.lens (\CreateEventSourceMapping' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@CreateEventSourceMapping' {} a -> s {bisectBatchOnFunctionError = a} :: CreateEventSourceMapping)

-- | (Streams only) The number of batches to process from each shard
-- concurrently.
createEventSourceMapping_parallelizationFactor :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_parallelizationFactor = Lens.lens (\CreateEventSourceMapping' {parallelizationFactor} -> parallelizationFactor) (\s@CreateEventSourceMapping' {} a -> s {parallelizationFactor = a} :: CreateEventSourceMapping)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
createEventSourceMapping_maximumRetryAttempts :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Int)
createEventSourceMapping_maximumRetryAttempts = Lens.lens (\CreateEventSourceMapping' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@CreateEventSourceMapping' {} a -> s {maximumRetryAttempts = a} :: CreateEventSourceMapping)

-- | The maximum number of records in each batch that Lambda pulls from your
-- stream or queue and sends to your function. Lambda passes all of the
-- records in the batch to the function in a single call, up to the payload
-- limit for synchronous invocation (6 MB).
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

-- | (Streams and Amazon SQS standard queues) The maximum amount of time, in
-- seconds, that Lambda spends gathering records before invoking the
-- function.
--
-- Default: 0
--
-- Related setting: When you set @BatchSize@ to a value greater than 10,
-- you must set @MaximumBatchingWindowInSeconds@ to at least 1.
createEventSourceMapping_maximumBatchingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_maximumBatchingWindowInSeconds = Lens.lens (\CreateEventSourceMapping' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@CreateEventSourceMapping' {} a -> s {maximumBatchingWindowInSeconds = a} :: CreateEventSourceMapping)

-- | An array of authentication protocols or VPC components required to
-- secure your event source.
createEventSourceMapping_sourceAccessConfigurations :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe [SourceAccessConfiguration])
createEventSourceMapping_sourceAccessConfigurations = Lens.lens (\CreateEventSourceMapping' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@CreateEventSourceMapping' {} a -> s {sourceAccessConfigurations = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
createEventSourceMapping_maximumRecordAgeInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Int)
createEventSourceMapping_maximumRecordAgeInSeconds = Lens.lens (\CreateEventSourceMapping' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@CreateEventSourceMapping' {} a -> s {maximumRecordAgeInSeconds = a} :: CreateEventSourceMapping)

-- | (Streams only) A list of current response type enums applied to the
-- event source mapping.
createEventSourceMapping_functionResponseTypes :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe [FunctionResponseType])
createEventSourceMapping_functionResponseTypes = Lens.lens (\CreateEventSourceMapping' {functionResponseTypes} -> functionResponseTypes) (\s@CreateEventSourceMapping' {} a -> s {functionResponseTypes = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second up to 900 seconds.
createEventSourceMapping_tumblingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_tumblingWindowInSeconds = Lens.lens (\CreateEventSourceMapping' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@CreateEventSourceMapping' {} a -> s {tumblingWindowInSeconds = a} :: CreateEventSourceMapping)

-- | The Self-Managed Apache Kafka cluster to send records.
createEventSourceMapping_selfManagedEventSource :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe SelfManagedEventSource)
createEventSourceMapping_selfManagedEventSource = Lens.lens (\CreateEventSourceMapping' {selfManagedEventSource} -> selfManagedEventSource) (\s@CreateEventSourceMapping' {} a -> s {selfManagedEventSource = a} :: CreateEventSourceMapping)

-- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
createEventSourceMapping_destinationConfig :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe DestinationConfig)
createEventSourceMapping_destinationConfig = Lens.lens (\CreateEventSourceMapping' {destinationConfig} -> destinationConfig) (\s@CreateEventSourceMapping' {} a -> s {destinationConfig = a} :: CreateEventSourceMapping)

-- | The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is only supported for Amazon Kinesis streams.
createEventSourceMapping_startingPosition :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe EventSourcePosition)
createEventSourceMapping_startingPosition = Lens.lens (\CreateEventSourceMapping' {startingPosition} -> startingPosition) (\s@CreateEventSourceMapping' {} a -> s {startingPosition = a} :: CreateEventSourceMapping)

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

instance Prelude.Hashable CreateEventSourceMapping where
  hashWithSalt _salt CreateEventSourceMapping' {..} =
    _salt `Prelude.hashWithSalt` eventSourceArn
      `Prelude.hashWithSalt` startingPositionTimestamp
      `Prelude.hashWithSalt` topics
      `Prelude.hashWithSalt` queues
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` bisectBatchOnFunctionError
      `Prelude.hashWithSalt` parallelizationFactor
      `Prelude.hashWithSalt` maximumRetryAttempts
      `Prelude.hashWithSalt` batchSize
      `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
      `Prelude.hashWithSalt` sourceAccessConfigurations
      `Prelude.hashWithSalt` maximumRecordAgeInSeconds
      `Prelude.hashWithSalt` functionResponseTypes
      `Prelude.hashWithSalt` tumblingWindowInSeconds
      `Prelude.hashWithSalt` selfManagedEventSource
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` startingPosition
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData CreateEventSourceMapping where
  rnf CreateEventSourceMapping' {..} =
    Prelude.rnf eventSourceArn
      `Prelude.seq` Prelude.rnf startingPositionTimestamp
      `Prelude.seq` Prelude.rnf topics
      `Prelude.seq` Prelude.rnf queues
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf bisectBatchOnFunctionError
      `Prelude.seq` Prelude.rnf parallelizationFactor
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf sourceAccessConfigurations
      `Prelude.seq` Prelude.rnf maximumRecordAgeInSeconds
      `Prelude.seq` Prelude.rnf functionResponseTypes
      `Prelude.seq` Prelude.rnf tumblingWindowInSeconds
      `Prelude.seq` Prelude.rnf selfManagedEventSource
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf startingPosition
      `Prelude.seq` Prelude.rnf functionName

instance Core.ToHeaders CreateEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateEventSourceMapping where
  toJSON CreateEventSourceMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EventSourceArn" Core..=)
              Prelude.<$> eventSourceArn,
            ("StartingPositionTimestamp" Core..=)
              Prelude.<$> startingPositionTimestamp,
            ("Topics" Core..=) Prelude.<$> topics,
            ("Queues" Core..=) Prelude.<$> queues,
            ("Enabled" Core..=) Prelude.<$> enabled,
            ("BisectBatchOnFunctionError" Core..=)
              Prelude.<$> bisectBatchOnFunctionError,
            ("ParallelizationFactor" Core..=)
              Prelude.<$> parallelizationFactor,
            ("MaximumRetryAttempts" Core..=)
              Prelude.<$> maximumRetryAttempts,
            ("BatchSize" Core..=) Prelude.<$> batchSize,
            ("MaximumBatchingWindowInSeconds" Core..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("SourceAccessConfigurations" Core..=)
              Prelude.<$> sourceAccessConfigurations,
            ("MaximumRecordAgeInSeconds" Core..=)
              Prelude.<$> maximumRecordAgeInSeconds,
            ("FunctionResponseTypes" Core..=)
              Prelude.<$> functionResponseTypes,
            ("TumblingWindowInSeconds" Core..=)
              Prelude.<$> tumblingWindowInSeconds,
            ("SelfManagedEventSource" Core..=)
              Prelude.<$> selfManagedEventSource,
            ("DestinationConfig" Core..=)
              Prelude.<$> destinationConfig,
            ("StartingPosition" Core..=)
              Prelude.<$> startingPosition,
            Prelude.Just ("FunctionName" Core..= functionName)
          ]
      )

instance Core.ToPath CreateEventSourceMapping where
  toPath =
    Prelude.const "/2015-03-31/event-source-mappings/"

instance Core.ToQuery CreateEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
