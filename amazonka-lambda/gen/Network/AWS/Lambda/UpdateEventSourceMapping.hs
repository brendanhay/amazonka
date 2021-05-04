{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event source mapping. You can change the function that AWS
-- Lambda invokes, or pause invocation and resume later from the same
-- location.
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
module Network.AWS.Lambda.UpdateEventSourceMapping
  ( -- * Creating a Request
    UpdateEventSourceMapping (..),
    newUpdateEventSourceMapping,

    -- * Request Lenses
    updateEventSourceMapping_functionName,
    updateEventSourceMapping_enabled,
    updateEventSourceMapping_maximumRecordAgeInSeconds,
    updateEventSourceMapping_functionResponseTypes,
    updateEventSourceMapping_tumblingWindowInSeconds,
    updateEventSourceMapping_maximumBatchingWindowInSeconds,
    updateEventSourceMapping_batchSize,
    updateEventSourceMapping_destinationConfig,
    updateEventSourceMapping_maximumRetryAttempts,
    updateEventSourceMapping_parallelizationFactor,
    updateEventSourceMapping_bisectBatchOnFunctionError,
    updateEventSourceMapping_sourceAccessConfigurations,
    updateEventSourceMapping_uuid,

    -- * Destructuring the Response
    EventSourceMappingConfiguration (..),
    newEventSourceMappingConfiguration,

    -- * Response Lenses
    eventSourceMappingConfiguration_eventSourceArn,
    eventSourceMappingConfiguration_stateTransitionReason,
    eventSourceMappingConfiguration_lastProcessingResult,
    eventSourceMappingConfiguration_topics,
    eventSourceMappingConfiguration_maximumRecordAgeInSeconds,
    eventSourceMappingConfiguration_functionResponseTypes,
    eventSourceMappingConfiguration_queues,
    eventSourceMappingConfiguration_tumblingWindowInSeconds,
    eventSourceMappingConfiguration_functionArn,
    eventSourceMappingConfiguration_startingPositionTimestamp,
    eventSourceMappingConfiguration_state,
    eventSourceMappingConfiguration_maximumBatchingWindowInSeconds,
    eventSourceMappingConfiguration_batchSize,
    eventSourceMappingConfiguration_startingPosition,
    eventSourceMappingConfiguration_destinationConfig,
    eventSourceMappingConfiguration_maximumRetryAttempts,
    eventSourceMappingConfiguration_lastModified,
    eventSourceMappingConfiguration_parallelizationFactor,
    eventSourceMappingConfiguration_selfManagedEventSource,
    eventSourceMappingConfiguration_uuid,
    eventSourceMappingConfiguration_bisectBatchOnFunctionError,
    eventSourceMappingConfiguration_sourceAccessConfigurations,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
  { -- | The name of the Lambda function.
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
    -- | If true, the event source mapping is active. Set to false to pause
    -- polling and invocation.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | (Streams) Discard records older than the specified age. The default
    -- value is infinite (-1).
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | (Streams) A list of current response type enums applied to the event
    -- source mapping.
    functionResponseTypes :: Prelude.Maybe (Prelude.NonEmpty FunctionResponseType),
    -- | (Streams) The duration in seconds of a processing window. The range is
    -- between 1 second up to 900 seconds.
    tumblingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
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
    -- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for
    -- discarded records.
    destinationConfig :: Prelude.Maybe DestinationConfig,
    -- | (Streams) Discard records after the specified number of retries. The
    -- default value is infinite (-1). When set to infinite (-1), failed
    -- records will be retried until the record expires.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | (Streams) The number of batches to process from each shard concurrently.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | (Streams) If the function returns an error, split the batch in two and
    -- retry.
    bisectBatchOnFunctionError :: Prelude.Maybe Prelude.Bool,
    -- | An array of the authentication protocol, or the VPC components to secure
    -- your event source.
    sourceAccessConfigurations :: Prelude.Maybe [SourceAccessConfiguration],
    -- | The identifier of the event source mapping.
    uuid :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateEventSourceMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'enabled', 'updateEventSourceMapping_enabled' - If true, the event source mapping is active. Set to false to pause
-- polling and invocation.
--
-- 'maximumRecordAgeInSeconds', 'updateEventSourceMapping_maximumRecordAgeInSeconds' - (Streams) Discard records older than the specified age. The default
-- value is infinite (-1).
--
-- 'functionResponseTypes', 'updateEventSourceMapping_functionResponseTypes' - (Streams) A list of current response type enums applied to the event
-- source mapping.
--
-- 'tumblingWindowInSeconds', 'updateEventSourceMapping_tumblingWindowInSeconds' - (Streams) The duration in seconds of a processing window. The range is
-- between 1 second up to 900 seconds.
--
-- 'maximumBatchingWindowInSeconds', 'updateEventSourceMapping_maximumBatchingWindowInSeconds' - (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds.
--
-- 'batchSize', 'updateEventSourceMapping_batchSize' - The maximum number of items to retrieve in a single batch.
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
-- 'destinationConfig', 'updateEventSourceMapping_destinationConfig' - (Streams) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
--
-- 'maximumRetryAttempts', 'updateEventSourceMapping_maximumRetryAttempts' - (Streams) Discard records after the specified number of retries. The
-- default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
--
-- 'parallelizationFactor', 'updateEventSourceMapping_parallelizationFactor' - (Streams) The number of batches to process from each shard concurrently.
--
-- 'bisectBatchOnFunctionError', 'updateEventSourceMapping_bisectBatchOnFunctionError' - (Streams) If the function returns an error, split the batch in two and
-- retry.
--
-- 'sourceAccessConfigurations', 'updateEventSourceMapping_sourceAccessConfigurations' - An array of the authentication protocol, or the VPC components to secure
-- your event source.
--
-- 'uuid', 'updateEventSourceMapping_uuid' - The identifier of the event source mapping.
newUpdateEventSourceMapping ::
  -- | 'uuid'
  Prelude.Text ->
  UpdateEventSourceMapping
newUpdateEventSourceMapping pUUID_ =
  UpdateEventSourceMapping'
    { functionName =
        Prelude.Nothing,
      enabled = Prelude.Nothing,
      maximumRecordAgeInSeconds = Prelude.Nothing,
      functionResponseTypes = Prelude.Nothing,
      tumblingWindowInSeconds = Prelude.Nothing,
      maximumBatchingWindowInSeconds = Prelude.Nothing,
      batchSize = Prelude.Nothing,
      destinationConfig = Prelude.Nothing,
      maximumRetryAttempts = Prelude.Nothing,
      parallelizationFactor = Prelude.Nothing,
      bisectBatchOnFunctionError = Prelude.Nothing,
      sourceAccessConfigurations = Prelude.Nothing,
      uuid = pUUID_
    }

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

-- | If true, the event source mapping is active. Set to false to pause
-- polling and invocation.
updateEventSourceMapping_enabled :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Bool)
updateEventSourceMapping_enabled = Lens.lens (\UpdateEventSourceMapping' {enabled} -> enabled) (\s@UpdateEventSourceMapping' {} a -> s {enabled = a} :: UpdateEventSourceMapping)

-- | (Streams) Discard records older than the specified age. The default
-- value is infinite (-1).
updateEventSourceMapping_maximumRecordAgeInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Int)
updateEventSourceMapping_maximumRecordAgeInSeconds = Lens.lens (\UpdateEventSourceMapping' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {maximumRecordAgeInSeconds = a} :: UpdateEventSourceMapping)

-- | (Streams) A list of current response type enums applied to the event
-- source mapping.
updateEventSourceMapping_functionResponseTypes :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe (Prelude.NonEmpty FunctionResponseType))
updateEventSourceMapping_functionResponseTypes = Lens.lens (\UpdateEventSourceMapping' {functionResponseTypes} -> functionResponseTypes) (\s@UpdateEventSourceMapping' {} a -> s {functionResponseTypes = a} :: UpdateEventSourceMapping) Prelude.. Lens.mapping Prelude._Coerce

-- | (Streams) The duration in seconds of a processing window. The range is
-- between 1 second up to 900 seconds.
updateEventSourceMapping_tumblingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_tumblingWindowInSeconds = Lens.lens (\UpdateEventSourceMapping' {tumblingWindowInSeconds} -> tumblingWindowInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {tumblingWindowInSeconds = a} :: UpdateEventSourceMapping)

-- | (Streams and SQS standard queues) The maximum amount of time to gather
-- records before invoking the function, in seconds.
updateEventSourceMapping_maximumBatchingWindowInSeconds :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_maximumBatchingWindowInSeconds = Lens.lens (\UpdateEventSourceMapping' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdateEventSourceMapping' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdateEventSourceMapping)

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
updateEventSourceMapping_batchSize :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_batchSize = Lens.lens (\UpdateEventSourceMapping' {batchSize} -> batchSize) (\s@UpdateEventSourceMapping' {} a -> s {batchSize = a} :: UpdateEventSourceMapping)

-- | (Streams) An Amazon SQS queue or Amazon SNS topic destination for
-- discarded records.
updateEventSourceMapping_destinationConfig :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe DestinationConfig)
updateEventSourceMapping_destinationConfig = Lens.lens (\UpdateEventSourceMapping' {destinationConfig} -> destinationConfig) (\s@UpdateEventSourceMapping' {} a -> s {destinationConfig = a} :: UpdateEventSourceMapping)

-- | (Streams) Discard records after the specified number of retries. The
-- default value is infinite (-1). When set to infinite (-1), failed
-- records will be retried until the record expires.
updateEventSourceMapping_maximumRetryAttempts :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Int)
updateEventSourceMapping_maximumRetryAttempts = Lens.lens (\UpdateEventSourceMapping' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@UpdateEventSourceMapping' {} a -> s {maximumRetryAttempts = a} :: UpdateEventSourceMapping)

-- | (Streams) The number of batches to process from each shard concurrently.
updateEventSourceMapping_parallelizationFactor :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Natural)
updateEventSourceMapping_parallelizationFactor = Lens.lens (\UpdateEventSourceMapping' {parallelizationFactor} -> parallelizationFactor) (\s@UpdateEventSourceMapping' {} a -> s {parallelizationFactor = a} :: UpdateEventSourceMapping)

-- | (Streams) If the function returns an error, split the batch in two and
-- retry.
updateEventSourceMapping_bisectBatchOnFunctionError :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe Prelude.Bool)
updateEventSourceMapping_bisectBatchOnFunctionError = Lens.lens (\UpdateEventSourceMapping' {bisectBatchOnFunctionError} -> bisectBatchOnFunctionError) (\s@UpdateEventSourceMapping' {} a -> s {bisectBatchOnFunctionError = a} :: UpdateEventSourceMapping)

-- | An array of the authentication protocol, or the VPC components to secure
-- your event source.
updateEventSourceMapping_sourceAccessConfigurations :: Lens.Lens' UpdateEventSourceMapping (Prelude.Maybe [SourceAccessConfiguration])
updateEventSourceMapping_sourceAccessConfigurations = Lens.lens (\UpdateEventSourceMapping' {sourceAccessConfigurations} -> sourceAccessConfigurations) (\s@UpdateEventSourceMapping' {} a -> s {sourceAccessConfigurations = a} :: UpdateEventSourceMapping) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier of the event source mapping.
updateEventSourceMapping_uuid :: Lens.Lens' UpdateEventSourceMapping Prelude.Text
updateEventSourceMapping_uuid = Lens.lens (\UpdateEventSourceMapping' {uuid} -> uuid) (\s@UpdateEventSourceMapping' {} a -> s {uuid = a} :: UpdateEventSourceMapping)

instance Prelude.AWSRequest UpdateEventSourceMapping where
  type
    Rs UpdateEventSourceMapping =
      EventSourceMappingConfiguration
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable UpdateEventSourceMapping

instance Prelude.NFData UpdateEventSourceMapping

instance Prelude.ToHeaders UpdateEventSourceMapping where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToJSON UpdateEventSourceMapping where
  toJSON UpdateEventSourceMapping' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("FunctionName" Prelude..=)
              Prelude.<$> functionName,
            ("Enabled" Prelude..=) Prelude.<$> enabled,
            ("MaximumRecordAgeInSeconds" Prelude..=)
              Prelude.<$> maximumRecordAgeInSeconds,
            ("FunctionResponseTypes" Prelude..=)
              Prelude.<$> functionResponseTypes,
            ("TumblingWindowInSeconds" Prelude..=)
              Prelude.<$> tumblingWindowInSeconds,
            ("MaximumBatchingWindowInSeconds" Prelude..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("BatchSize" Prelude..=) Prelude.<$> batchSize,
            ("DestinationConfig" Prelude..=)
              Prelude.<$> destinationConfig,
            ("MaximumRetryAttempts" Prelude..=)
              Prelude.<$> maximumRetryAttempts,
            ("ParallelizationFactor" Prelude..=)
              Prelude.<$> parallelizationFactor,
            ("BisectBatchOnFunctionError" Prelude..=)
              Prelude.<$> bisectBatchOnFunctionError,
            ("SourceAccessConfigurations" Prelude..=)
              Prelude.<$> sourceAccessConfigurations
          ]
      )

instance Prelude.ToPath UpdateEventSourceMapping where
  toPath UpdateEventSourceMapping' {..} =
    Prelude.mconcat
      [ "/2015-03-31/event-source-mappings/",
        Prelude.toBS uuid
      ]

instance Prelude.ToQuery UpdateEventSourceMapping where
  toQuery = Prelude.const Prelude.mempty
