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
-- Module      : Amazonka.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event source mapping. You can change the function that Lambda
-- invokes, or pause invocation and resume later from the same location.
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
module Amazonka.Lambda.UpdateEventSourceMapping
  ( -- * Creating a Request
    UpdateEventSourceMapping (..),
    newUpdateEventSourceMapping,

    -- * Request Lenses
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_filterCriteria,
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_uuid,

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

-- | /See:/ 'newUpdateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
  { -- | The maximum number of records in each batch that Lambda pulls from your
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
    -- | An object that defines the filter criteria that determine whether Lambda
    -- should process an event. For more information, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html Lambda event filtering>.
    filterCriteria :: Prelude.Maybe FilterCriteria,
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
    functionName :: Prelude.Maybe Prelude.Text,
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
    -- | An array of authentication protocols or VPC components required to
    -- secure your event source.
    sourceAccessConfigurations :: Prelude.Maybe [SourceAccessConfiguration],
    -- | (Streams only) The duration in seconds of a processing window. The range
    -- is between 1 second and 900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the event source mapping.
    uuid :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventSourceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updateEventSourceMapping_batchSize' - The maximum number of records in each batch that Lambda pulls from your
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
-- 'bisectBatchOnFunctionError', 'updateEventSourceMapping_bisectBatchOnFunctionError' - (Streams only) If the function returns an error, split the batch in two
-- and retry.
--
-- 'destinationConfig', 'updateEventSourceMapping_destinationConfig' - (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'enabled', 'updateEventSourceMapping_enabled' - When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
--
-- 'filterCriteria', 'updateEventSourceMapping_filterCriteria' - An object that defines the filter criteria that determine whether Lambda
-- should process an event. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html Lambda event filtering>.
--
-- 'functionName', 'updateEventSourceMapping_functionName' - The name of the Lambda function.
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
--
-- 'functionResponseTypes', 'updateEventSourceMapping_functionResponseTypes' - (Streams and Amazon SQS) A list of current response type enums applied
-- to the event source mapping.
--
-- 'maximumBatchingWindowInSeconds', 'updateEventSourceMapping_maximumBatchingWindowInSeconds' - The maximum amount of time, in seconds, that Lambda spends gathering
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
-- 'maximumRecordAgeInSeconds', 'updateEventSourceMapping_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
--
-- 'maximumRetryAttempts', 'updateEventSourceMapping_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
--
-- 'parallelizationFactor', 'updateEventSourceMapping_parallelizationFactor' - (Streams only) The number of batches to process from each shard
-- concurrently.
--
-- 'sourceAccessConfigurations', 'updateEventSourceMapping_sourceAccessConfigurations' - An array of authentication protocols or VPC components required to
-- secure your event source.
--
-- 'tumblingWindowInSeconds', 'updateEventSourceMapping_tumblingWindowInSeconds' - (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second and 900 seconds.
--
-- 'uuid', 'updateEventSourceMapping_uuid' - The identifier of the event source mapping.
newUpdateEventSourceMapping ::
  -- | 'uuid'
  Prelude.Text ->
  UpdateEventSourceMapping
newUpdateEventSourceMapping pUUID_ =
  UpdateEventSourceMapping'
    { batchSize =
        Prelude.Nothing,
      bisectBatchOnFunctionError = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      enabled = Prelude.Nothing,
      filterCriteria = Prelude.Nothing,
      functionName = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      maximumBatchingWindowInSeconds = Prelude.Nothing,
      maximumRecordAgeInSeconds = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      sourceAccessConfigurations = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      uuid = pUUID_
    }

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
updateEventSourceMapping_batchSize :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_batchSize = Lens.lens (\UpdateEventSourceMapping' {batchSize} -> batchSize) (\s@UpdateEventSourceMapping' {} a -> s {batchSize = a} :: UpdateEventSourceMapping)

-- | (Streams only) If the function returns an error, split the batch in two
-- and retry.
updateEventSourceMapping_bisectBatchOnFunctionError :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Bool)
updateEventSourceMapping_bisectBatchOnFunctionError = Lens.lens (\UpdateEventSourceMapping' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@UpdateEventSourceMapping' {} a -> s {bisectBatchOnFunctionError = a} :: UpdateEventSourceMapping)

-- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
updateEventSourceMapping_destinationConfig :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe DestinationConfig)
updateEventSourceMapping_destinationConfig = Lens.lens (\UpdateEventSourceMapping' {destinationConfig} -> destinationConfig) (\s@UpdateEventSourceMapping' {} a -> s {destinationConfig = a} :: UpdateEventSourceMapping)

-- | When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
updateEventSourceMapping_enabled :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Bool)
updateEventSourceMapping_enabled = Lens.lens (\UpdateEventSourceMapping' {enabled} -> enabled) (\s@UpdateEventSourceMapping' {} a -> s {enabled = a} :: UpdateEventSourceMapping)

-- | An object that defines the filter criteria that determine whether Lambda
-- should process an event. For more information, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/invocation-eventfiltering.html Lambda event filtering>.
updateEventSourceMapping_filterCriteria :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe FilterCriteria)
updateEventSourceMapping_filterCriteria = Lens.lens (\UpdateEventSourceMapping' {filterCriteria} -> filterCriteria) (\s@UpdateEventSourceMapping' {} a -> s {filterCriteria = a} :: UpdateEventSourceMapping)

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
updateEventSourceMapping_functionName :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Text)
updateEventSourceMapping_functionName = Lens.lens (\UpdateEventSourceMapping' {functionName} -> functionName) (\s@UpdateEventSourceMapping' {} a -> s {functionName = a} :: UpdateEventSourceMapping)

-- | (Streams and Amazon SQS) A list of current response type enums applied
-- to the event source mapping.
updateEventSourceMapping_functionResponseTypes :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe [FunctionResponseType])
updateEventSourceMapping_functionResponseTypes = Lens.lens (\UpdateEventSourceMapping' {functionResponseTypes} -> functionResponseTypes) (\s@UpdateEventSourceMapping' {} a -> s {functionResponseTypes = a} :: UpdateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

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
updateEventSourceMapping_maximumBatchingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_maximumBatchingWindowInSeconds = Lens.lens (\UpdateEventSourceMapping' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdateEventSourceMapping)

-- | (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
updateEventSourceMapping_maximumRecordAgeInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Int)
updateEventSourceMapping_maximumRecordAgeInSeconds = Lens.lens (\UpdateEventSourceMapping' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {maximumRecordAgeInSeconds = a} :: UpdateEventSourceMapping)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records are retried until the record expires.
updateEventSourceMapping_maximumRetryAttempts :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Int)
updateEventSourceMapping_maximumRetryAttempts = Lens.lens (\UpdateEventSourceMapping' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@UpdateEventSourceMapping' {} a -> s {maximumRetryAttempts = a} :: UpdateEventSourceMapping)

-- | (Streams only) The number of batches to process from each shard
-- concurrently.
updateEventSourceMapping_parallelizationFactor :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_parallelizationFactor = Lens.lens (\UpdateEventSourceMapping' {parallelizationFactor} -> parallelizationFactor) (\s@UpdateEventSourceMapping' {} a -> s {parallelizationFactor = a} :: UpdateEventSourceMapping)

-- | An array of authentication protocols or VPC components required to
-- secure your event source.
updateEventSourceMapping_sourceAccessConfigurations :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe [SourceAccessConfiguration])
updateEventSourceMapping_sourceAccessConfigurations = Lens.lens (\UpdateEventSourceMapping' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@UpdateEventSourceMapping' {} a -> s {sourceAccessConfigurations = a} :: UpdateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second and 900 seconds.
updateEventSourceMapping_tumblingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_tumblingWindowInSeconds = Lens.lens (\UpdateEventSourceMapping' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {tumblingWindowInSeconds = a} :: UpdateEventSourceMapping)

-- | The identifier of the event source mapping.
updateEventSourceMapping_uuid :: Lens.Lens' UpdateEventSourceMapping Prelude.Text
updateEventSourceMapping_uuid = Lens.lens (\UpdateEventSourceMapping' {uuid} -> uuid) (\s@UpdateEventSourceMapping' {} a -> s {uuid = a} :: UpdateEventSourceMapping)

instance Core.AWSRequest UpdateEventSourceMapping where
  type
    AWSResponse UpdateEventSourceMapping =
      EventSourceMappingConfiguration
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateEventSourceMapping where
  hashWithSalt _salt UpdateEventSourceMapping' {..} =
    _salt
      `Prelude.hashWithSalt` batchSize
      `Prelude.hashWithSalt` bisectBatchOnFunctionError
      `Prelude.hashWithSalt` destinationConfig
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` functionName
      `Prelude.hashWithSalt` functionResponseTypes
      `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
      `Prelude.hashWithSalt` maximumRecordAgeInSeconds
      `Prelude.hashWithSalt` maximumRetryAttempts
      `Prelude.hashWithSalt` parallelizationFactor
      `Prelude.hashWithSalt` sourceAccessConfigurations
      `Prelude.hashWithSalt` tumblingWindowInSeconds
      `Prelude.hashWithSalt` uuid

instance Prelude.NFData UpdateEventSourceMapping where
  rnf UpdateEventSourceMapping' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf bisectBatchOnFunctionError
      `Prelude.seq` Prelude.rnf destinationConfig
      `Prelude.seq` Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf functionName
      `Prelude.seq` Prelude.rnf functionResponseTypes
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf maximumRecordAgeInSeconds
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf parallelizationFactor
      `Prelude.seq` Prelude.rnf sourceAccessConfigurations
      `Prelude.seq` Prelude.rnf tumblingWindowInSeconds
      `Prelude.seq` Prelude.rnf uuid

instance Data.ToHeaders UpdateEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateEventSourceMapping where
  toJSON UpdateEventSourceMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("BisectBatchOnFunctionError" Data..=)
              Prelude.<$> bisectBatchOnFunctionError,
            ("DestinationConfig" Data..=)
              Prelude.<$> destinationConfig,
            ("Enabled" Data..=) Prelude.<$> enabled,
            ("FilterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("FunctionName" Data..=) Prelude.<$> functionName,
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
            ("SourceAccessConfigurations" Data..=)
              Prelude.<$> sourceAccessConfigurations,
            ("TumblingWindowInSeconds" Data..=)
              Prelude.<$> tumblingWindowInSeconds
          ]
      )

instance Data.ToPath UpdateEventSourceMapping where
  toPath UpdateEventSourceMapping' {..} =
    Prelude.mconcat
      [ "/2015-03-31/event-source-mappings/",
        Data.toBS uuid
      ]

instance Data.ToQuery UpdateEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
