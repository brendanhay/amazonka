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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a mapping between an event source and an Lambda function. Lambda
-- reads items from the event source and invokes the function.
--
-- For details about how to configure different event sources, see the
-- following topics.
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-ddb.html#services-dynamodb-eventsourcemapping Amazon DynamoDB Streams>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-kinesis.html#services-kinesis-eventsourcemapping Amazon Kinesis>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html#events-sqs-eventsource Amazon SQS>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-mq.html#services-mq-eventsourcemapping Amazon MQ and RabbitMQ>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html Amazon MSK>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/kafka-smaa.html Apache Kafka>
--
-- The following error handling options are available only for stream
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
--
-- For information about which configuration parameters apply to each event
-- source, see the following topics.
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-ddb.html#services-ddb-params Amazon DynamoDB Streams>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-kinesis.html#services-kinesis-params Amazon Kinesis>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-sqs.html#services-sqs-params Amazon SQS>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-mq.html#services-mq-params Amazon MQ and RabbitMQ>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-msk.html#services-msk-parms Amazon MSK>
--
-- -   <https://docs.aws.amazon.com/lambda/latest/dg/with-kafka.html#services-kafka-parms Apache Kafka>
module Amazonka.Lambda.CreateEventSourceMapping
  ( -- * Creating a Request
    CreateEventSourceMapping (..),
    newCreateEventSourceMapping,

    -- * Request Lenses
    createEventSourceMapping_amazonManagedKafkaEventSourceConfig,
    createEventSourceMapping_batchSize,
    createEventSourceMapping_bisectBatchOnFunctionError,
    createEventSourceMapping_destinationConfig,
    createEventSourceMapping_enabled,
    createEventSourceMapping_eventSourceArn,
    createEventSourceMapping_filterCriteria,
    createEventSourceMapping_functionResponseTypes,
    createEventSourceMapping_maximumBatchingWindowInSeconds,
    createEventSourceMapping_maximumRecordAgeInSeconds,
    createEventSourceMapping_maximumRetryAttempts,
    createEventSourceMapping_parallelizationFactor,
    createEventSourceMapping_queues,
    createEventSourceMapping_selfManagedEventSource,
    createEventSourceMapping_selfManagedKafkaEventSourceConfig,
    createEventSourceMapping_sourceAccessConfigurations,
    createEventSourceMapping_startingPosition,
    createEventSourceMapping_startingPositionTimestamp,
    createEventSourceMapping_topics,
    createEventSourceMapping_tumblingWindowInSeconds,
    createEventSourceMapping_functionName,

    -- * Destructuring the Response
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,

    -- * Response Lenses
    eventSourceMappingConfiguration_amazonManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_filterCriteria,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_selfManagedKafkaEventSourceConfig,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_uuid,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lambda.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateEventSourceMapping' smart constructor.
data CreateEventSourceMapping = CreateEventSourceMapping'
  { -- | Specific configuration settings for an Amazon Managed Streaming for
    -- Apache Kafka (Amazon MSK) event source.
    amazonManagedKafkaEventSourceConfig :: Prelude.Maybe AmazonManagedKafkaEventSourceConfig,
    -- | The maximum number of records in each batch that Lambda pulls from your
    -- stream or queue and sends to your function. Lambda passes all of the
    -- records in the batch to the function in a single call, up to the payload
    -- limit for synchronous invocation (6 MB).
    --
    -- -   __Amazon Kinesis__ - Default 100. Max 10,000.
    --
    -- -   __Amazon DynamoDB Streams__ - Default 100. Max 10,000.
    --
    -- -   __Amazon Simple Queue Service__ - Default 10. For standard queues
    --     the max is 10,000. For FIFO queues the max is 10.
    --
    -- -   __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max
    --     10,000.
    --
    -- -   __Self-managed Apache Kafka__ - Default 100. Max 10,000.
    --
    -- -   __Amazon MQ (ActiveMQ and RabbitMQ)__ - Default 100. Max 10,000.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) If the function returns an error, split the batch in two
    -- and retry.
    bisectBatchOnFunctionError :: Prelude.Maybe Prelude.Bool,
    -- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | When true, the event source mapping is active. When false, Lambda pauses
    -- polling and invocation.
    --
    -- Default: True
    enabled :: Prelude.Maybe Prelude.Bool,
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
    --
    -- -   __Amazon MQ__ - The ARN of the broker.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | An object that defines the filter criteria that determine whether Lambda
    -- should process an event. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html Lambda event filtering>.
    filterCriteria :: Prelude.Maybe FilterCriteria,
    -- | (Streams and Amazon SQS) A list of current response type enums applied
    -- to the event source mapping.
    functionResponseTypes :: Prelude.Maybe [FunctionResponseType],
    -- | The maximum amount of time, in seconds, that Lambda spends gathering
    -- records before invoking the function. You can configure
    -- @MaximumBatchingWindowInSeconds@ to any value from 0 seconds to 300
    -- seconds in increments of seconds.
    --
    -- For streams and Amazon SQS event sources, the default batching window is
    -- 0 seconds. For Amazon MSK, Self-managed Apache Kafka, and Amazon MQ
    -- event sources, the default batching window is 500 ms. Note that because
    -- you can only change @MaximumBatchingWindowInSeconds@ in increments of
    -- seconds, you cannot revert back to the 500 ms default batching window
    -- after you have changed it. To restore the default batching window, you
    -- must create a new event source mapping.
    --
    -- Related setting: For streams and Amazon SQS event sources, when you set
    -- @BatchSize@ to a value greater than 10, you must set
    -- @MaximumBatchingWindowInSeconds@ to at least 1.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) Discard records older than the specified age. The default
    -- value is infinite (-1).
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | (Streams only) Discard records after the specified number of retries.
    -- The default value is infinite (-1). When set to infinite (-1), failed
    -- records are retried until the record expires.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | (Streams only) The number of batches to process from each shard
    -- concurrently.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | (MQ) The name of the Amazon MQ broker destination queue to consume.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The self-managed Apache Kafka cluster to receive records from.
    selfManagedEventSource :: Prelude.Maybe SelfManagedEventSource,
    -- | Specific configuration settings for a self-managed Apache Kafka event
    -- source.
    selfManagedKafkaEventSourceConfig :: Prelude.Maybe SelfManagedKafkaEventSourceConfig,
    -- | An array of authentication protocols or VPC components required to
    -- secure your event source.
    sourceAccessConfigurations :: Prelude.Maybe [SourceAccessConfiguration],
    -- | The position in a stream from which to start reading. Required for
    -- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
    -- @AT_TIMESTAMP@ is supported only for Amazon Kinesis streams.
    startingPosition :: Prelude.Maybe EventSourcePosition,
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading.
    startingPositionTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the Kafka topic.
    topics :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | (Streams only) The duration in seconds of a processing window. The range
    -- is between 1 second and 900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
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
-- 'amazonManagedKafkaEventSourceConfig', 'createEventSourceMapping_amazonManagedKafkaEventSourceConfig' - Specific configuration settings for an Amazon Managed Streaming for
-- Apache Kafka (Amazon MSK) event source.
--
-- 'batchSize', 'createEventSourceMapping_batchSize' - The maximum number of records in each batch that Lambda pulls from your
-- stream or queue and sends to your function. Lambda passes all of the
-- records in the batch to the function in a single call, up to the payload
-- limit for synchronous invocation (6 MB).
--
-- -   __Amazon Kinesis__ - Default 100. Max 10,000.
--
-- -   __Amazon DynamoDB Streams__ - Default 100. Max 10,000.
--
-- -   __Amazon Simple Queue Service__ - Default 10. For standard queues
--     the max is 10,000. For FIFO queues the max is 10.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max
--     10,000.
--
-- -   __Self-managed Apache Kafka__ - Default 100. Max 10,000.
--
-- -   __Amazon MQ (ActiveMQ and RabbitMQ)__ - Default 100. Max 10,000.
--
-- 'bisectBatchOnFunctionError', 'createEventSourceMapping_bisectBatchOnFunctionError' - (Streams only) If the function returns an error, split the batch in two
-- and retry.
--
-- 'destinationConfig', 'createEventSourceMapping_destinationConfig' - (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'enabled', 'createEventSourceMapping_enabled' - When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
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
-- -   __Amazon MQ__ - The ARN of the broker.
--
-- 'filterCriteria', 'createEventSourceMapping_filterCriteria' - An object that defines the filter criteria that determine whether Lambda
-- should process an event. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html Lambda event filtering>.
--
-- 'functionResponseTypes', 'createEventSourceMapping_functionResponseTypes' - (Streams and Amazon SQS) A list of current response type enums applied
-- to the event source mapping.
--
-- 'maximumBatchingWindowInSeconds', 'createEventSourceMapping_maximumBatchingWindowInSeconds' - The maximum amount of time, in seconds, that Lambda spends gathering
-- records before invoking the function. You can configure
-- @MaximumBatchingWindowInSeconds@ to any value from 0 seconds to 300
-- seconds in increments of seconds.
--
-- For streams and Amazon SQS event sources, the default batching window is
-- 0 seconds. For Amazon MSK, Self-managed Apache Kafka, and Amazon MQ
-- event sources, the default batching window is 500 ms. Note that because
-- you can only change @MaximumBatchingWindowInSeconds@ in increments of
-- seconds, you cannot revert back to the 500 ms default batching window
-- after you have changed it. To restore the default batching window, you
-- must create a new event source mapping.
--
-- Related setting: For streams and Amazon SQS event sources, when you set
-- @BatchSize@ to a value greater than 10, you must set
-- @MaximumBatchingWindowInSeconds@ to at least 1.
--
-- 'maximumRecordAgeInSeconds', 'createEventSourceMapping_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
--
-- 'maximumRetryAttempts', 'createEventSourceMapping_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
--
-- 'parallelizationFactor', 'createEventSourceMapping_parallelizationFactor' - (Streams only) The number of batches to process from each shard
-- concurrently.
--
-- 'queues', 'createEventSourceMapping_queues' - (MQ) The name of the Amazon MQ broker destination queue to consume.
--
-- 'selfManagedEventSource', 'createEventSourceMapping_selfManagedEventSource' - The self-managed Apache Kafka cluster to receive records from.
--
-- 'selfManagedKafkaEventSourceConfig', 'createEventSourceMapping_selfManagedKafkaEventSourceConfig' - Specific configuration settings for a self-managed Apache Kafka event
-- source.
--
-- 'sourceAccessConfigurations', 'createEventSourceMapping_sourceAccessConfigurations' - An array of authentication protocols or VPC components required to
-- secure your event source.
--
-- 'startingPosition', 'createEventSourceMapping_startingPosition' - The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is supported only for Amazon Kinesis streams.
--
-- 'startingPositionTimestamp', 'createEventSourceMapping_startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
--
-- 'topics', 'createEventSourceMapping_topics' - The name of the Kafka topic.
--
-- 'tumblingWindowInSeconds', 'createEventSourceMapping_tumblingWindowInSeconds' - (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second and 900 seconds.
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
    { amazonManagedKafkaEventSourceConfig =
        Prelude.Nothing,
      batchSize = Prelude.Nothing,
      bisectBatchOnFunctionError = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      enabled = Prelude.Nothing,
      eventSourceArn = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      maximumBatchingWindowInSeconds = Prelude.Nothing,
      maximumRecordAgeInSeconds = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      queues = Prelude.Nothing,
      selfManagedEventSource = Prelude.Nothing,
      selfManagedKafkaEventSourceConfig =
        Prelude.Nothing,
      sourceAccessConfigurations = Prelude.Nothing,
      startingPosition = Prelude.Nothing,
      startingPositionTimestamp = Prelude.Nothing,
      topics = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      functionName = pFunctionName_
    }

-- | Specific configuration settings for an Amazon Managed Streaming for
-- Apache Kafka (Amazon MSK) event source.
createEventSourceMapping_amazonManagedKafkaEventSourceConfig :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe AmazonManagedKafkaEventSourceConfig)
createEventSourceMapping_amazonManagedKafkaEventSourceConfig = Lens.lens (\CreateEventSourceMapping' {amazonManagedKafkaEventSourceConfig} -> amazonManagedKafkaEventSourceConfig) (\s@CreateEventSourceMapping' {} a -> s {amazonManagedKafkaEventSourceConfig = a} :: CreateEventSourceMapping)

-- | The maximum number of records in each batch that Lambda pulls from your
-- stream or queue and sends to your function. Lambda passes all of the
-- records in the batch to the function in a single call, up to the payload
-- limit for synchronous invocation (6 MB).
--
-- -   __Amazon Kinesis__ - Default 100. Max 10,000.
--
-- -   __Amazon DynamoDB Streams__ - Default 100. Max 10,000.
--
-- -   __Amazon Simple Queue Service__ - Default 10. For standard queues
--     the max is 10,000. For FIFO queues the max is 10.
--
-- -   __Amazon Managed Streaming for Apache Kafka__ - Default 100. Max
--     10,000.
--
-- -   __Self-managed Apache Kafka__ - Default 100. Max 10,000.
--
-- -   __Amazon MQ (ActiveMQ and RabbitMQ)__ - Default 100. Max 10,000.
createEventSourceMapping_batchSize :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_batchSize = Lens.lens (\CreateEventSourceMapping' {batchSize} -> batchSize) (\s@CreateEventSourceMapping' {} a -> s {batchSize = a} :: CreateEventSourceMapping)

-- | (Streams only) If the function returns an error, split the batch in two
-- and retry.
createEventSourceMapping_bisectBatchOnFunctionError :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Bool)
createEventSourceMapping_bisectBatchOnFunctionError = Lens.lens (\CreateEventSourceMapping' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@CreateEventSourceMapping' {} a -> s {bisectBatchOnFunctionError = a} :: CreateEventSourceMapping)

-- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
createEventSourceMapping_destinationConfig :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe DestinationConfig)
createEventSourceMapping_destinationConfig = Lens.lens (\CreateEventSourceMapping' {destinationConfig} -> destinationConfig) (\s@CreateEventSourceMapping' {} a -> s {destinationConfig = a} :: CreateEventSourceMapping)

-- | When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
createEventSourceMapping_enabled :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Bool)
createEventSourceMapping_enabled = Lens.lens (\CreateEventSourceMapping' {enabled} -> enabled) (\s@CreateEventSourceMapping' {} a -> s {enabled = a} :: CreateEventSourceMapping)

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
--
-- -   __Amazon MQ__ - The ARN of the broker.
createEventSourceMapping_eventSourceArn :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Text)
createEventSourceMapping_eventSourceArn = Lens.lens (\CreateEventSourceMapping' {eventSourceArn} -> eventSourceArn) (\s@CreateEventSourceMapping' {} a -> s {eventSourceArn = a} :: CreateEventSourceMapping)

-- | An object that defines the filter criteria that determine whether Lambda
-- should process an event. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html Lambda event filtering>.
createEventSourceMapping_filterCriteria :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe FilterCriteria)
createEventSourceMapping_filterCriteria = Lens.lens (\CreateEventSourceMapping' {filterCriteria} -> filterCriteria) (\s@CreateEventSourceMapping' {} a -> s {filterCriteria = a} :: CreateEventSourceMapping)

-- | (Streams and Amazon SQS) A list of current response type enums applied
-- to the event source mapping.
createEventSourceMapping_functionResponseTypes :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe [FunctionResponseType])
createEventSourceMapping_functionResponseTypes = Lens.lens (\CreateEventSourceMapping' {functionResponseTypes} -> functionResponseTypes) (\s@CreateEventSourceMapping' {} a -> s {functionResponseTypes = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | The maximum amount of time, in seconds, that Lambda spends gathering
-- records before invoking the function. You can configure
-- @MaximumBatchingWindowInSeconds@ to any value from 0 seconds to 300
-- seconds in increments of seconds.
--
-- For streams and Amazon SQS event sources, the default batching window is
-- 0 seconds. For Amazon MSK, Self-managed Apache Kafka, and Amazon MQ
-- event sources, the default batching window is 500 ms. Note that because
-- you can only change @MaximumBatchingWindowInSeconds@ in increments of
-- seconds, you cannot revert back to the 500 ms default batching window
-- after you have changed it. To restore the default batching window, you
-- must create a new event source mapping.
--
-- Related setting: For streams and Amazon SQS event sources, when you set
-- @BatchSize@ to a value greater than 10, you must set
-- @MaximumBatchingWindowInSeconds@ to at least 1.
createEventSourceMapping_maximumBatchingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_maximumBatchingWindowInSeconds = Lens.lens (\CreateEventSourceMapping' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@CreateEventSourceMapping' {} a -> s {maximumBatchingWindowInSeconds = a} :: CreateEventSourceMapping)

-- | (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
createEventSourceMapping_maximumRecordAgeInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Int)
createEventSourceMapping_maximumRecordAgeInSeconds = Lens.lens (\CreateEventSourceMapping' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@CreateEventSourceMapping' {} a -> s {maximumRecordAgeInSeconds = a} :: CreateEventSourceMapping)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
createEventSourceMapping_maximumRetryAttempts :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Int)
createEventSourceMapping_maximumRetryAttempts = Lens.lens (\CreateEventSourceMapping' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@CreateEventSourceMapping' {} a -> s {maximumRetryAttempts = a} :: CreateEventSourceMapping)

-- | (Streams only) The number of batches to process from each shard
-- concurrently.
createEventSourceMapping_parallelizationFactor :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_parallelizationFactor = Lens.lens (\CreateEventSourceMapping' {parallelizationFactor} -> parallelizationFactor) (\s@CreateEventSourceMapping' {} a -> s {parallelizationFactor = a} :: CreateEventSourceMapping)

-- | (MQ) The name of the Amazon MQ broker destination queue to consume.
createEventSourceMapping_queues :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createEventSourceMapping_queues = Lens.lens (\CreateEventSourceMapping' {queues} -> queues) (\s@CreateEventSourceMapping' {} a -> s {queues = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | The self-managed Apache Kafka cluster to receive records from.
createEventSourceMapping_selfManagedEventSource :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe SelfManagedEventSource)
createEventSourceMapping_selfManagedEventSource = Lens.lens (\CreateEventSourceMapping' {selfManagedEventSource} -> selfManagedEventSource) (\s@CreateEventSourceMapping' {} a -> s {selfManagedEventSource = a} :: CreateEventSourceMapping)

-- | Specific configuration settings for a self-managed Apache Kafka event
-- source.
createEventSourceMapping_selfManagedKafkaEventSourceConfig :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe SelfManagedKafkaEventSourceConfig)
createEventSourceMapping_selfManagedKafkaEventSourceConfig = Lens.lens (\CreateEventSourceMapping' {selfManagedKafkaEventSourceConfig} -> selfManagedKafkaEventSourceConfig) (\s@CreateEventSourceMapping' {} a -> s {selfManagedKafkaEventSourceConfig = a} :: CreateEventSourceMapping)

-- | An array of authentication protocols or VPC components required to
-- secure your event source.
createEventSourceMapping_sourceAccessConfigurations :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe [SourceAccessConfiguration])
createEventSourceMapping_sourceAccessConfigurations = Lens.lens (\CreateEventSourceMapping' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@CreateEventSourceMapping' {} a -> s {sourceAccessConfigurations = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | The position in a stream from which to start reading. Required for
-- Amazon Kinesis, Amazon DynamoDB, and Amazon MSK Streams sources.
-- @AT_TIMESTAMP@ is supported only for Amazon Kinesis streams.
createEventSourceMapping_startingPosition :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe EventSourcePosition)
createEventSourceMapping_startingPosition = Lens.lens (\CreateEventSourceMapping' {startingPosition} -> startingPosition) (\s@CreateEventSourceMapping' {} a -> s {startingPosition = a} :: CreateEventSourceMapping)

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading.
createEventSourceMapping_startingPositionTimestamp :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.UTCTime)
createEventSourceMapping_startingPositionTimestamp = Lens.lens (\CreateEventSourceMapping' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@CreateEventSourceMapping' {} a -> s {startingPositionTimestamp = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Data._Time

-- | The name of the Kafka topic.
createEventSourceMapping_topics :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
createEventSourceMapping_topics = Lens.lens (\CreateEventSourceMapping' {topics} -> topics) (\s@CreateEventSourceMapping' {} a -> s {topics = a} :: CreateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second and 900 seconds.
createEventSourceMapping_tumblingWindowInSeconds :: Lens.Lens' CreateEventSourceMapping (Prelude.Maybe Prelude.Natural)
createEventSourceMapping_tumblingWindowInSeconds = Lens.lens (\CreateEventSourceMapping' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@CreateEventSourceMapping' {} a -> s {tumblingWindowInSeconds = a} :: CreateEventSourceMapping)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateEventSourceMapping where
  hashWithSalt _salt CreateEventSourceMapping' {..} =
    _salt
      `Prelude.hashWithSalt` amazonManagedKafkaEventSourceConfig
      `Prelude.hashWithSalt` batchSize
      `Prelude.hashWithSalt` bisectBatchOnFunctionError
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` eventSourceArn
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` functionResponseTypes
      `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
      `Prelude.hashWithSalt` maximumRecordAgeInSeconds
      `Prelude.hashWithSalt` maximumRetryAttempts
      `Prelude.hashWithSalt` parallelizationFactor
      `Prelude.hashWithSalt` queues
      `Prelude.hashWithSalt` selfManagedEventSource
      `Prelude.hashWithSalt` selfManagedKafkaEventSourceConfig
      `Prelude.hashWithSalt` sourceAccessConfigurations
      `Prelude.hashWithSalt` startingPosition
      `Prelude.hashWithSalt` startingPositionTimestamp
      `Prelude.hashWithSalt` topics
      `Prelude.hashWithSalt` tumblingWindowInSeconds
      `Prelude.hashWithSalt` functionName

instance Prelude.NFData CreateEventSourceMapping where
  rnf CreateEventSourceMapping' {..} =
    Prelude.rnf amazonManagedKafkaEventSourceConfig
      `Prelude.seq` Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf bisectBatchOnFunctionError
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf eventSourceArn
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf functionResponseTypes
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf maximumRecordAgeInSeconds
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf parallelizationFactor
      `Prelude.seq` Prelude.rnf queues
      `Prelude.seq` Prelude.rnf selfManagedEventSource
      `Prelude.seq` Prelude.rnf
        selfManagedKafkaEventSourceConfig
      `Prelude.seq` Prelude.rnf sourceAccessConfigurations
      `Prelude.seq` Prelude.rnf startingPosition
      `Prelude.seq` Prelude.rnf
        startingPositionTimestamp
      `Prelude.seq` Prelude.rnf topics
      `Prelude.seq` Prelude.rnf
        tumblingWindowInSeconds
      `Prelude.seq` Prelude.rnf functionName

instance Data.ToHeaders CreateEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateEventSourceMapping where
  toJSON CreateEventSourceMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AmazonManagedKafkaEventSourceConfig" Data..=)
              Prelude.<$> amazonManagedKafkaEventSourceConfig,
            ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("BisectBatchOnFunctionError" Data..=)
              Prelude.<$> bisectBatchOnFunctionError,
            ("DestinationConfig" Data..=)
              Prelude.<$> destinationConfig,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("EventSourceArn" Data..=)
              Prelude.<$> eventSourceArn,
            ("FilterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("FunctionResponseTypes" Data..=)
              Prelude.<$> functionResponseTypes,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("MaximumRecordAgeInSeconds" Data..=)
              Prelude.<$> maximumRecordAgeInSeconds,
            ("MaximumRetryAttempts" Data..=)
              Prelude.<$> maximumRetryAttempts,
            ("ParallelizationFactor" Data..=)
              Prelude.<$> parallelizationFactor,
            ("Queues" Data..=) Prelude.<$> queues,
            ("SelfManagedEventSource" Data..=)
              Prelude.<$> selfManagedEventSource,
            ("SelfManagedKafkaEventSourceConfig" Data..=)
              Prelude.<$> selfManagedKafkaEventSourceConfig,
            ("SourceAccessConfigurations" Data..=)
              Prelude.<$> sourceAccessConfigurations,
            ("StartingPosition" Data..=)
              Prelude.<$> startingPosition,
            ("StartingPositionTimestamp" Data..=)
              Prelude.<$> startingPositionTimestamp,
            ("Topics" Data..=) Prelude.<$> topics,
            ("TumblingWindowInSeconds" Data..=)
              Prelude.<$> tumblingWindowInSeconds,
            Prelude.Just ("FunctionName" Data..= functionName)
          ]
      )

instance Data.ToPath CreateEventSourceMapping where
  toPath =
    Prelude.const "/2015-03-31/event-source-mappings/"

instance Data.ToQuery CreateEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
