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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceDynamoDBStreamParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceDynamoDBStreamParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.DeadLetterConfig
import Amazonka.Pipes.Types.OnPartialBatchItemFailureStreams
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a DynamoDB stream as a source.
--
-- /See:/ 'newUpdatePipeSourceDynamoDBStreamParameters' smart constructor.
data UpdatePipeSourceDynamoDBStreamParameters = UpdatePipeSourceDynamoDBStreamParameters'
  { -- | The maximum number of records to include in each batch.
    batchSize :: Prelude.Maybe Prelude.Natural,
    -- | Define the target queue to send dead-letter queue events to.
    deadLetterConfig :: Prelude.Maybe DeadLetterConfig,
    -- | The maximum length of a time to wait for events.
    maximumBatchingWindowInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | (Streams only) Discard records older than the specified age. The default
    -- value is -1, which sets the maximum age to infinite. When the value is
    -- set to infinite, EventBridge never discards old records.
    maximumRecordAgeInSeconds :: Prelude.Maybe Prelude.Int,
    -- | (Streams only) Discard records after the specified number of retries.
    -- The default value is -1, which sets the maximum number of retries to
    -- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
    -- failed records until the record expires in the event source.
    maximumRetryAttempts :: Prelude.Maybe Prelude.Int,
    -- | (Streams only) Define how to handle item process failures.
    -- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
    -- records are processed or there is one failed message left in the batch.
    onPartialBatchItemFailure :: Prelude.Maybe OnPartialBatchItemFailureStreams,
    -- | (Streams only) The number of batches to process concurrently from each
    -- shard. The default value is 1.
    parallelizationFactor :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeSourceDynamoDBStreamParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceDynamoDBStreamParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'deadLetterConfig', 'updatePipeSourceDynamoDBStreamParameters_deadLetterConfig' - Define the target queue to send dead-letter queue events to.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'maximumRecordAgeInSeconds', 'updatePipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
--
-- 'maximumRetryAttempts', 'updatePipeSourceDynamoDBStreamParameters_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
--
-- 'onPartialBatchItemFailure', 'updatePipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure' - (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
--
-- 'parallelizationFactor', 'updatePipeSourceDynamoDBStreamParameters_parallelizationFactor' - (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
newUpdatePipeSourceDynamoDBStreamParameters ::
  UpdatePipeSourceDynamoDBStreamParameters
newUpdatePipeSourceDynamoDBStreamParameters =
  UpdatePipeSourceDynamoDBStreamParameters'
    { batchSize =
        Prelude.Nothing,
      deadLetterConfig =
        Prelude.Nothing,
      maximumBatchingWindowInSeconds =
        Prelude.Nothing,
      maximumRecordAgeInSeconds =
        Prelude.Nothing,
      maximumRetryAttempts =
        Prelude.Nothing,
      onPartialBatchItemFailure =
        Prelude.Nothing,
      parallelizationFactor =
        Prelude.Nothing
    }

-- | The maximum number of records to include in each batch.
updatePipeSourceDynamoDBStreamParameters_batchSize :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceDynamoDBStreamParameters_batchSize = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceDynamoDBStreamParameters)

-- | Define the target queue to send dead-letter queue events to.
updatePipeSourceDynamoDBStreamParameters_deadLetterConfig :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe DeadLetterConfig)
updatePipeSourceDynamoDBStreamParameters_deadLetterConfig = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {deadLetterConfig} -> deadLetterConfig) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {deadLetterConfig = a} :: UpdatePipeSourceDynamoDBStreamParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceDynamoDBStreamParameters)

-- | (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
updatePipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Int)
updatePipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {maximumRecordAgeInSeconds = a} :: UpdatePipeSourceDynamoDBStreamParameters)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
updatePipeSourceDynamoDBStreamParameters_maximumRetryAttempts :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Int)
updatePipeSourceDynamoDBStreamParameters_maximumRetryAttempts = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {maximumRetryAttempts = a} :: UpdatePipeSourceDynamoDBStreamParameters)

-- | (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
updatePipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe OnPartialBatchItemFailureStreams)
updatePipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {onPartialBatchItemFailure} -> onPartialBatchItemFailure) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {onPartialBatchItemFailure = a} :: UpdatePipeSourceDynamoDBStreamParameters)

-- | (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
updatePipeSourceDynamoDBStreamParameters_parallelizationFactor :: Lens.Lens' UpdatePipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceDynamoDBStreamParameters_parallelizationFactor = Lens.lens (\UpdatePipeSourceDynamoDBStreamParameters' {parallelizationFactor} -> parallelizationFactor) (\s@UpdatePipeSourceDynamoDBStreamParameters' {} a -> s {parallelizationFactor = a} :: UpdatePipeSourceDynamoDBStreamParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceDynamoDBStreamParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceDynamoDBStreamParameters' {..} =
      _salt
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` deadLetterConfig
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` maximumRecordAgeInSeconds
        `Prelude.hashWithSalt` maximumRetryAttempts
        `Prelude.hashWithSalt` onPartialBatchItemFailure
        `Prelude.hashWithSalt` parallelizationFactor

instance
  Prelude.NFData
    UpdatePipeSourceDynamoDBStreamParameters
  where
  rnf UpdatePipeSourceDynamoDBStreamParameters' {..} =
    Prelude.rnf batchSize `Prelude.seq`
      Prelude.rnf deadLetterConfig `Prelude.seq`
        Prelude.rnf maximumBatchingWindowInSeconds `Prelude.seq`
          Prelude.rnf maximumRecordAgeInSeconds `Prelude.seq`
            Prelude.rnf maximumRetryAttempts `Prelude.seq`
              Prelude.rnf onPartialBatchItemFailure `Prelude.seq`
                Prelude.rnf parallelizationFactor

instance
  Data.ToJSON
    UpdatePipeSourceDynamoDBStreamParameters
  where
  toJSON UpdatePipeSourceDynamoDBStreamParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BatchSize" Data..=) Prelude.<$> batchSize,
            ("DeadLetterConfig" Data..=)
              Prelude.<$> deadLetterConfig,
            ("MaximumBatchingWindowInSeconds" Data..=)
              Prelude.<$> maximumBatchingWindowInSeconds,
            ("MaximumRecordAgeInSeconds" Data..=)
              Prelude.<$> maximumRecordAgeInSeconds,
            ("MaximumRetryAttempts" Data..=)
              Prelude.<$> maximumRetryAttempts,
            ("OnPartialBatchItemFailure" Data..=)
              Prelude.<$> onPartialBatchItemFailure,
            ("ParallelizationFactor" Data..=)
              Prelude.<$> parallelizationFactor
          ]
      )
