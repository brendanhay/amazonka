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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event source mapping. You can change the function that Lambda
-- invokes, or pause invocation and resume later from the same location.
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
module Amazonka.Lambda.UpdateEventSourceMapping
  ( -- * Creating a Request
    UpdateEventSourceMapping (..),
    newUpdateEventSourceMapping,

    -- * Request Lenses
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_uuid,

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

-- | /See:/ 'newUpdateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
  { -- | When true, the event source mapping is active. When false, Lambda pauses
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
    -- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
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
-- 'enabled', 'updateEventSourceMapping_enabled' - When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
--
-- 'bisectBatchOnFunctionError', 'updateEventSourceMapping_bisectBatchOnFunctionError' - (Streams only) If the function returns an error, split the batch in two
-- and retry.
--
-- 'parallelizationFactor', 'updateEventSourceMapping_parallelizationFactor' - (Streams only) The number of batches to process from each shard
-- concurrently.
--
-- 'maximumRetryAttempts', 'updateEventSourceMapping_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
--
-- 'batchSize', 'updateEventSourceMapping_batchSize' - The maximum number of records in each batch that Lambda pulls from your
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
-- 'maximumBatchingWindowInSeconds', 'updateEventSourceMapping_maximumBatchingWindowInSeconds' - (Streams and Amazon SQS standard queues) The maximum amount of time, in
-- seconds, that Lambda spends gathering records before invoking the
-- function.
--
-- Default: 0
--
-- Related setting: When you set @BatchSize@ to a value greater than 10,
-- you must set @MaximumBatchingWindowInSeconds@ to at least 1.
--
-- 'sourceAccessConfigurations', 'updateEventSourceMapping_sourceAccessConfigurations' - An array of authentication protocols or VPC components required to
-- secure your event source.
--
-- 'maximumRecordAgeInSeconds', 'updateEventSourceMapping_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
--
-- 'functionResponseTypes', 'updateEventSourceMapping_functionResponseTypes' - (Streams only) A list of current response type enums applied to the
-- event source mapping.
--
-- 'tumblingWindowInSeconds', 'updateEventSourceMapping_tumblingWindowInSeconds' - (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second up to 900 seconds.
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
-- 'destinationConfig', 'updateEventSourceMapping_destinationConfig' - (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'uuid', 'updateEventSourceMapping_uuid' - The identifier of the event source mapping.
newUpdateEventSourceMapping ::
  -- | 'uuid'
  Prelude.Text ->
  UpdateEventSourceMapping
newUpdateEventSourceMapping pUUID_ =
  UpdateEventSourceMapping'
    { enabled =
        Prelude.Nothing,
      bisectBatchOnFunctionError = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      batchSize = Prelude.Nothing,
      maximumBatchingWindowInSeconds = Prelude.Nothing,
      sourceAccessConfigurations = Prelude.Nothing,
      maximumRecordAgeInSeconds = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      functionName = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      uuid = pUUID_
    }

-- | When true, the event source mapping is active. When false, Lambda pauses
-- polling and invocation.
--
-- Default: True
updateEventSourceMapping_enabled :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Bool)
updateEventSourceMapping_enabled = Lens.lens (\UpdateEventSourceMapping' {enabled} -> enabled) (\s@UpdateEventSourceMapping' {} a -> s {enabled = a} :: UpdateEventSourceMapping)

-- | (Streams only) If the function returns an error, split the batch in two
-- and retry.
updateEventSourceMapping_bisectBatchOnFunctionError :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Bool)
updateEventSourceMapping_bisectBatchOnFunctionError = Lens.lens (\UpdateEventSourceMapping' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@UpdateEventSourceMapping' {} a -> s {bisectBatchOnFunctionError = a} :: UpdateEventSourceMapping)

-- | (Streams only) The number of batches to process from each shard
-- concurrently.
updateEventSourceMapping_parallelizationFactor :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_parallelizationFactor = Lens.lens (\UpdateEventSourceMapping' {parallelizationFactor} -> parallelizationFactor) (\s@UpdateEventSourceMapping' {} a -> s {parallelizationFactor = a} :: UpdateEventSourceMapping)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
updateEventSourceMapping_maximumRetryAttempts :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Int)
updateEventSourceMapping_maximumRetryAttempts = Lens.lens (\UpdateEventSourceMapping' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@UpdateEventSourceMapping' {} a -> s {maximumRetryAttempts = a} :: UpdateEventSourceMapping)

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
updateEventSourceMapping_batchSize :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_batchSize = Lens.lens (\UpdateEventSourceMapping' {batchSize} -> batchSize) (\s@UpdateEventSourceMapping' {} a -> s {batchSize = a} :: UpdateEventSourceMapping)

-- | (Streams and Amazon SQS standard queues) The maximum amount of time, in
-- seconds, that Lambda spends gathering records before invoking the
-- function.
--
-- Default: 0
--
-- Related setting: When you set @BatchSize@ to a value greater than 10,
-- you must set @MaximumBatchingWindowInSeconds@ to at least 1.
updateEventSourceMapping_maximumBatchingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_maximumBatchingWindowInSeconds = Lens.lens (\UpdateEventSourceMapping' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdateEventSourceMapping)

-- | An array of authentication protocols or VPC components required to
-- secure your event source.
updateEventSourceMapping_sourceAccessConfigurations :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe [SourceAccessConfiguration])
updateEventSourceMapping_sourceAccessConfigurations = Lens.lens (\UpdateEventSourceMapping' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@UpdateEventSourceMapping' {} a -> s {sourceAccessConfigurations = a} :: UpdateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (Streams only) Discard records older than the specified age. The default
-- value is infinite (-1).
updateEventSourceMapping_maximumRecordAgeInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Int)
updateEventSourceMapping_maximumRecordAgeInSeconds = Lens.lens (\UpdateEventSourceMapping' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {maximumRecordAgeInSeconds = a} :: UpdateEventSourceMapping)

-- | (Streams only) A list of current response type enums applied to the
-- event source mapping.
updateEventSourceMapping_functionResponseTypes :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe [FunctionResponseType])
updateEventSourceMapping_functionResponseTypes = Lens.lens (\UpdateEventSourceMapping' {functionResponseTypes} -> functionResponseTypes) (\s@UpdateEventSourceMapping' {} a -> s {functionResponseTypes = a} :: UpdateEventSourceMapping) Prelude.. Lens.mapping Lens.coerced

-- | (Streams only) The duration in seconds of a processing window. The range
-- is between 1 second up to 900 seconds.
updateEventSourceMapping_tumblingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_tumblingWindowInSeconds = Lens.lens (\UpdateEventSourceMapping' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {tumblingWindowInSeconds = a} :: UpdateEventSourceMapping)

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

-- | (Streams only) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
updateEventSourceMapping_destinationConfig :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe DestinationConfig)
updateEventSourceMapping_destinationConfig = Lens.lens (\UpdateEventSourceMapping' {destinationConfig} -> destinationConfig) (\s@UpdateEventSourceMapping' {} a -> s {destinationConfig = a} :: UpdateEventSourceMapping)

-- | The identifier of the event source mapping.
updateEventSourceMapping_uuid :: Lens.Lens' UpdateEventSourceMapping Prelude.Text
updateEventSourceMapping_uuid = Lens.lens (\UpdateEventSourceMapping' {uuid} -> uuid) (\s@UpdateEventSourceMapping' {} a -> s {uuid = a} :: UpdateEventSourceMapping)

instance Core.AWSRequest UpdateEventSourceMapping where
  type
    AWSResponse UpdateEventSourceMapping =
      EventSourceMappingConfiguration
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateEventSourceMapping

instance Prelude.NFData UpdateEventSourceMapping

instance Core.ToHeaders UpdateEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON UpdateEventSourceMapping where
  toJSON UpdateEventSourceMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Enabled" Core..=) Prelude.<$> enabled,
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
            ("FunctionName" Core..=) Prelude.<$> functionName,
            ("DestinationConfig" Core..=)
              Prelude.<$> destinationConfig
          ]
      )

instance Core.ToPath UpdateEventSourceMapping where
  toPath UpdateEventSourceMapping' {..} =
    Prelude.mconcat
      [ "/2015-03-31/event-source-mappings/",
        Core.toBS uuid
      ]

instance Core.ToQuery UpdateEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
