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
-- Module      : Amazonka.Pipes.Types.PipeSourceKinesisStreamParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceKinesisStreamParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.DeadLetterConfig
import Amazonka.Pipes.Types.KinesisStreamStartPosition
import Amazonka.Pipes.Types.OnPartialBatchItemFailureStreams
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a Kinesis stream as a source.
--
-- /See:/ 'newPipeSourceKinesisStreamParameters' smart constructor.
data PipeSourceKinesisStreamParameters = PipeSourceKinesisStreamParameters'
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
    parallelizationFactor :: Prelude.Maybe Prelude.Natural,
    -- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
    -- start reading, in Unix time seconds.
    startingPositionTimestamp :: Prelude.Maybe Data.POSIX,
    -- | (Streams only) The position in a stream from which to start reading.
    startingPosition :: KinesisStreamStartPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceKinesisStreamParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'pipeSourceKinesisStreamParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'deadLetterConfig', 'pipeSourceKinesisStreamParameters_deadLetterConfig' - Define the target queue to send dead-letter queue events to.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'maximumRecordAgeInSeconds', 'pipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
--
-- 'maximumRetryAttempts', 'pipeSourceKinesisStreamParameters_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
--
-- 'onPartialBatchItemFailure', 'pipeSourceKinesisStreamParameters_onPartialBatchItemFailure' - (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
--
-- 'parallelizationFactor', 'pipeSourceKinesisStreamParameters_parallelizationFactor' - (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
--
-- 'startingPositionTimestamp', 'pipeSourceKinesisStreamParameters_startingPositionTimestamp' - With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading, in Unix time seconds.
--
-- 'startingPosition', 'pipeSourceKinesisStreamParameters_startingPosition' - (Streams only) The position in a stream from which to start reading.
newPipeSourceKinesisStreamParameters ::
  -- | 'startingPosition'
  KinesisStreamStartPosition ->
  PipeSourceKinesisStreamParameters
newPipeSourceKinesisStreamParameters
  pStartingPosition_ =
    PipeSourceKinesisStreamParameters'
      { batchSize =
          Prelude.Nothing,
        deadLetterConfig = Prelude.Nothing,
        maximumBatchingWindowInSeconds =
          Prelude.Nothing,
        maximumRecordAgeInSeconds =
          Prelude.Nothing,
        maximumRetryAttempts = Prelude.Nothing,
        onPartialBatchItemFailure =
          Prelude.Nothing,
        parallelizationFactor = Prelude.Nothing,
        startingPositionTimestamp =
          Prelude.Nothing,
        startingPosition = pStartingPosition_
      }

-- | The maximum number of records to include in each batch.
pipeSourceKinesisStreamParameters_batchSize :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Natural)
pipeSourceKinesisStreamParameters_batchSize = Lens.lens (\PipeSourceKinesisStreamParameters' {batchSize} -> batchSize) (\s@PipeSourceKinesisStreamParameters' {} a -> s {batchSize = a} :: PipeSourceKinesisStreamParameters)

-- | Define the target queue to send dead-letter queue events to.
pipeSourceKinesisStreamParameters_deadLetterConfig :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe DeadLetterConfig)
pipeSourceKinesisStreamParameters_deadLetterConfig = Lens.lens (\PipeSourceKinesisStreamParameters' {deadLetterConfig} -> deadLetterConfig) (\s@PipeSourceKinesisStreamParameters' {} a -> s {deadLetterConfig = a} :: PipeSourceKinesisStreamParameters)

-- | The maximum length of a time to wait for events.
pipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Natural)
pipeSourceKinesisStreamParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceKinesisStreamParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceKinesisStreamParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceKinesisStreamParameters)

-- | (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
pipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Int)
pipeSourceKinesisStreamParameters_maximumRecordAgeInSeconds = Lens.lens (\PipeSourceKinesisStreamParameters' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@PipeSourceKinesisStreamParameters' {} a -> s {maximumRecordAgeInSeconds = a} :: PipeSourceKinesisStreamParameters)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
pipeSourceKinesisStreamParameters_maximumRetryAttempts :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Int)
pipeSourceKinesisStreamParameters_maximumRetryAttempts = Lens.lens (\PipeSourceKinesisStreamParameters' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@PipeSourceKinesisStreamParameters' {} a -> s {maximumRetryAttempts = a} :: PipeSourceKinesisStreamParameters)

-- | (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
pipeSourceKinesisStreamParameters_onPartialBatchItemFailure :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe OnPartialBatchItemFailureStreams)
pipeSourceKinesisStreamParameters_onPartialBatchItemFailure = Lens.lens (\PipeSourceKinesisStreamParameters' {onPartialBatchItemFailure} -> onPartialBatchItemFailure) (\s@PipeSourceKinesisStreamParameters' {} a -> s {onPartialBatchItemFailure = a} :: PipeSourceKinesisStreamParameters)

-- | (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
pipeSourceKinesisStreamParameters_parallelizationFactor :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.Natural)
pipeSourceKinesisStreamParameters_parallelizationFactor = Lens.lens (\PipeSourceKinesisStreamParameters' {parallelizationFactor} -> parallelizationFactor) (\s@PipeSourceKinesisStreamParameters' {} a -> s {parallelizationFactor = a} :: PipeSourceKinesisStreamParameters)

-- | With @StartingPosition@ set to @AT_TIMESTAMP@, the time from which to
-- start reading, in Unix time seconds.
pipeSourceKinesisStreamParameters_startingPositionTimestamp :: Lens.Lens' PipeSourceKinesisStreamParameters (Prelude.Maybe Prelude.UTCTime)
pipeSourceKinesisStreamParameters_startingPositionTimestamp = Lens.lens (\PipeSourceKinesisStreamParameters' {startingPositionTimestamp} -> startingPositionTimestamp) (\s@PipeSourceKinesisStreamParameters' {} a -> s {startingPositionTimestamp = a} :: PipeSourceKinesisStreamParameters) Prelude.. Lens.mapping Data._Time

-- | (Streams only) The position in a stream from which to start reading.
pipeSourceKinesisStreamParameters_startingPosition :: Lens.Lens' PipeSourceKinesisStreamParameters KinesisStreamStartPosition
pipeSourceKinesisStreamParameters_startingPosition = Lens.lens (\PipeSourceKinesisStreamParameters' {startingPosition} -> startingPosition) (\s@PipeSourceKinesisStreamParameters' {} a -> s {startingPosition = a} :: PipeSourceKinesisStreamParameters)

instance
  Data.FromJSON
    PipeSourceKinesisStreamParameters
  where
  parseJSON =
    Data.withObject
      "PipeSourceKinesisStreamParameters"
      ( \x ->
          PipeSourceKinesisStreamParameters'
            Prelude.<$> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "DeadLetterConfig")
            Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Data..:? "MaximumRecordAgeInSeconds")
            Prelude.<*> (x Data..:? "MaximumRetryAttempts")
            Prelude.<*> (x Data..:? "OnPartialBatchItemFailure")
            Prelude.<*> (x Data..:? "ParallelizationFactor")
            Prelude.<*> (x Data..:? "StartingPositionTimestamp")
            Prelude.<*> (x Data..: "StartingPosition")
      )

instance
  Prelude.Hashable
    PipeSourceKinesisStreamParameters
  where
  hashWithSalt
    _salt
    PipeSourceKinesisStreamParameters' {..} =
      _salt
        `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` deadLetterConfig
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` maximumRecordAgeInSeconds
        `Prelude.hashWithSalt` maximumRetryAttempts
        `Prelude.hashWithSalt` onPartialBatchItemFailure
        `Prelude.hashWithSalt` parallelizationFactor
        `Prelude.hashWithSalt` startingPositionTimestamp
        `Prelude.hashWithSalt` startingPosition

instance
  Prelude.NFData
    PipeSourceKinesisStreamParameters
  where
  rnf PipeSourceKinesisStreamParameters' {..} =
    Prelude.rnf batchSize `Prelude.seq`
      Prelude.rnf deadLetterConfig `Prelude.seq`
        Prelude.rnf maximumBatchingWindowInSeconds `Prelude.seq`
          Prelude.rnf maximumRecordAgeInSeconds `Prelude.seq`
            Prelude.rnf maximumRetryAttempts `Prelude.seq`
              Prelude.rnf onPartialBatchItemFailure `Prelude.seq`
                Prelude.rnf parallelizationFactor `Prelude.seq`
                  Prelude.rnf startingPositionTimestamp `Prelude.seq`
                    Prelude.rnf startingPosition

instance
  Data.ToJSON
    PipeSourceKinesisStreamParameters
  where
  toJSON PipeSourceKinesisStreamParameters' {..} =
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
              Prelude.<$> parallelizationFactor,
            ("StartingPositionTimestamp" Data..=)
              Prelude.<$> startingPositionTimestamp,
            Prelude.Just
              ("StartingPosition" Data..= startingPosition)
          ]
      )
