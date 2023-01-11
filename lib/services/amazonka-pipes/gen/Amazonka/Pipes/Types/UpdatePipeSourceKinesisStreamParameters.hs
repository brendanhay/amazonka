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
-- Module      : Amazonka.Pipes.Types.UpdatePipeSourceKinesisStreamParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.UpdatePipeSourceKinesisStreamParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.DeadLetterConfig
import Amazonka.Pipes.Types.OnPartialBatchItemFailureStreams
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Kinesis stream as a source.
--
-- /See:/ 'newUpdatePipeSourceKinesisStreamParameters' smart constructor.
data UpdatePipeSourceKinesisStreamParameters = UpdatePipeSourceKinesisStreamParameters'
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
-- Create a value of 'UpdatePipeSourceKinesisStreamParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'updatePipeSourceKinesisStreamParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'deadLetterConfig', 'updatePipeSourceKinesisStreamParameters_deadLetterConfig' - Define the target queue to send dead-letter queue events to.
--
-- 'maximumBatchingWindowInSeconds', 'updatePipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'maximumRecordAgeInSeconds', 'updatePipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
--
-- 'maximumRetryAttempts', 'updatePipeSourceKinesisStreamParameters_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
--
-- 'onPartialBatchItemFailure', 'updatePipeSourceKinesisStreamParameters_onPartialBatchItemFailure' - (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
--
-- 'parallelizationFactor', 'updatePipeSourceKinesisStreamParameters_parallelizationFactor' - (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
newUpdatePipeSourceKinesisStreamParameters ::
  UpdatePipeSourceKinesisStreamParameters
newUpdatePipeSourceKinesisStreamParameters =
  UpdatePipeSourceKinesisStreamParameters'
    { batchSize =
        Prelude.Nothing,
      deadLetterConfig = Prelude.Nothing,
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
updatePipeSourceKinesisStreamParameters_batchSize :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceKinesisStreamParameters_batchSize = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {batchSize} -> batchSize) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {batchSize = a} :: UpdatePipeSourceKinesisStreamParameters)

-- | Define the target queue to send dead-letter queue events to.
updatePipeSourceKinesisStreamParameters_deadLetterConfig :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe DeadLetterConfig)
updatePipeSourceKinesisStreamParameters_deadLetterConfig = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {deadLetterConfig} -> deadLetterConfig) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {deadLetterConfig = a} :: UpdatePipeSourceKinesisStreamParameters)

-- | The maximum length of a time to wait for events.
updatePipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: UpdatePipeSourceKinesisStreamParameters)

-- | (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
updatePipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Int)
updatePipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {maximumRecordAgeInSeconds = a} :: UpdatePipeSourceKinesisStreamParameters)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
updatePipeSourceKinesisStreamParameters_maximumRetryAttempts :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Int)
updatePipeSourceKinesisStreamParameters_maximumRetryAttempts = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {maximumRetryAttempts = a} :: UpdatePipeSourceKinesisStreamParameters)

-- | (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
updatePipeSourceKinesisStreamParameters_onPartialBatchItemFailure :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe OnPartialBatchItemFailureStreams)
updatePipeSourceKinesisStreamParameters_onPartialBatchItemFailure = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {onPartialBatchItemFailure} -> onPartialBatchItemFailure) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {onPartialBatchItemFailure = a} :: UpdatePipeSourceKinesisStreamParameters)

-- | (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
updatePipeSourceKinesisStreamParameters_parallelizationFactor :: Lens.Lens' UpdatePipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Natural)
updatePipeSourceKinesisStreamParameters_parallelizationFactor = Lens.lens (\UpdatePipeSourceKinesisStreamParameters' {parallelizationFactor} -> parallelizationFactor) (\s@UpdatePipeSourceKinesisStreamParameters' {} a -> s {parallelizationFactor = a} :: UpdatePipeSourceKinesisStreamParameters)

instance
  Prelude.Hashable
    UpdatePipeSourceKinesisStreamParameters
  where
  hashWithSalt
    _salt
    UpdatePipeSourceKinesisStreamParameters' {..} =
      _salt `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` deadLetterConfig
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` maximumRecordAgeInSeconds
        `Prelude.hashWithSalt` maximumRetryAttempts
        `Prelude.hashWithSalt` onPartialBatchItemFailure
        `Prelude.hashWithSalt` parallelizationFactor

instance
  Prelude.NFData
    UpdatePipeSourceKinesisStreamParameters
  where
  rnf UpdatePipeSourceKinesisStreamParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf maximumRecordAgeInSeconds
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf onPartialBatchItemFailure
      `Prelude.seq` Prelude.rnf parallelizationFactor

instance
  Data.ToJSON
    UpdatePipeSourceKinesisStreamParameters
  where
  toJSON UpdatePipeSourceKinesisStreamParameters' {..} =
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
