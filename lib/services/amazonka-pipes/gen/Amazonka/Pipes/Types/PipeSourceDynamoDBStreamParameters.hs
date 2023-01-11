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
-- Module      : Amazonka.Pipes.Types.PipeSourceDynamoDBStreamParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.PipeSourceDynamoDBStreamParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.DeadLetterConfig
import Amazonka.Pipes.Types.DynamoDBStreamStartPosition
import Amazonka.Pipes.Types.OnPartialBatchItemFailureStreams
import qualified Amazonka.Prelude as Prelude

-- | The parameters for using a DynamoDB stream as a source.
--
-- /See:/ 'newPipeSourceDynamoDBStreamParameters' smart constructor.
data PipeSourceDynamoDBStreamParameters = PipeSourceDynamoDBStreamParameters'
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
    -- | (Streams only) The position in a stream from which to start reading.
    startingPosition :: DynamoDBStreamStartPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipeSourceDynamoDBStreamParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchSize', 'pipeSourceDynamoDBStreamParameters_batchSize' - The maximum number of records to include in each batch.
--
-- 'deadLetterConfig', 'pipeSourceDynamoDBStreamParameters_deadLetterConfig' - Define the target queue to send dead-letter queue events to.
--
-- 'maximumBatchingWindowInSeconds', 'pipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds' - The maximum length of a time to wait for events.
--
-- 'maximumRecordAgeInSeconds', 'pipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds' - (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
--
-- 'maximumRetryAttempts', 'pipeSourceDynamoDBStreamParameters_maximumRetryAttempts' - (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
--
-- 'onPartialBatchItemFailure', 'pipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure' - (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
--
-- 'parallelizationFactor', 'pipeSourceDynamoDBStreamParameters_parallelizationFactor' - (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
--
-- 'startingPosition', 'pipeSourceDynamoDBStreamParameters_startingPosition' - (Streams only) The position in a stream from which to start reading.
newPipeSourceDynamoDBStreamParameters ::
  -- | 'startingPosition'
  DynamoDBStreamStartPosition ->
  PipeSourceDynamoDBStreamParameters
newPipeSourceDynamoDBStreamParameters
  pStartingPosition_ =
    PipeSourceDynamoDBStreamParameters'
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
        startingPosition = pStartingPosition_
      }

-- | The maximum number of records to include in each batch.
pipeSourceDynamoDBStreamParameters_batchSize :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Natural)
pipeSourceDynamoDBStreamParameters_batchSize = Lens.lens (\PipeSourceDynamoDBStreamParameters' {batchSize} -> batchSize) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {batchSize = a} :: PipeSourceDynamoDBStreamParameters)

-- | Define the target queue to send dead-letter queue events to.
pipeSourceDynamoDBStreamParameters_deadLetterConfig :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe DeadLetterConfig)
pipeSourceDynamoDBStreamParameters_deadLetterConfig = Lens.lens (\PipeSourceDynamoDBStreamParameters' {deadLetterConfig} -> deadLetterConfig) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {deadLetterConfig = a} :: PipeSourceDynamoDBStreamParameters)

-- | The maximum length of a time to wait for events.
pipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Natural)
pipeSourceDynamoDBStreamParameters_maximumBatchingWindowInSeconds = Lens.lens (\PipeSourceDynamoDBStreamParameters' {maximumBatchingWindowInSeconds} -> maximumBatchingWindowInSeconds) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {maximumBatchingWindowInSeconds = a} :: PipeSourceDynamoDBStreamParameters)

-- | (Streams only) Discard records older than the specified age. The default
-- value is -1, which sets the maximum age to infinite. When the value is
-- set to infinite, EventBridge never discards old records.
pipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Int)
pipeSourceDynamoDBStreamParameters_maximumRecordAgeInSeconds = Lens.lens (\PipeSourceDynamoDBStreamParameters' {maximumRecordAgeInSeconds} -> maximumRecordAgeInSeconds) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {maximumRecordAgeInSeconds = a} :: PipeSourceDynamoDBStreamParameters)

-- | (Streams only) Discard records after the specified number of retries.
-- The default value is -1, which sets the maximum number of retries to
-- infinite. When MaximumRetryAttempts is infinite, EventBridge retries
-- failed records until the record expires in the event source.
pipeSourceDynamoDBStreamParameters_maximumRetryAttempts :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Int)
pipeSourceDynamoDBStreamParameters_maximumRetryAttempts = Lens.lens (\PipeSourceDynamoDBStreamParameters' {maximumRetryAttempts} -> maximumRetryAttempts) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {maximumRetryAttempts = a} :: PipeSourceDynamoDBStreamParameters)

-- | (Streams only) Define how to handle item process failures.
-- @AUTOMATIC_BISECT@ halves each batch and retry each half until all the
-- records are processed or there is one failed message left in the batch.
pipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe OnPartialBatchItemFailureStreams)
pipeSourceDynamoDBStreamParameters_onPartialBatchItemFailure = Lens.lens (\PipeSourceDynamoDBStreamParameters' {onPartialBatchItemFailure} -> onPartialBatchItemFailure) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {onPartialBatchItemFailure = a} :: PipeSourceDynamoDBStreamParameters)

-- | (Streams only) The number of batches to process concurrently from each
-- shard. The default value is 1.
pipeSourceDynamoDBStreamParameters_parallelizationFactor :: Lens.Lens' PipeSourceDynamoDBStreamParameters (Prelude.Maybe Prelude.Natural)
pipeSourceDynamoDBStreamParameters_parallelizationFactor = Lens.lens (\PipeSourceDynamoDBStreamParameters' {parallelizationFactor} -> parallelizationFactor) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {parallelizationFactor = a} :: PipeSourceDynamoDBStreamParameters)

-- | (Streams only) The position in a stream from which to start reading.
pipeSourceDynamoDBStreamParameters_startingPosition :: Lens.Lens' PipeSourceDynamoDBStreamParameters DynamoDBStreamStartPosition
pipeSourceDynamoDBStreamParameters_startingPosition = Lens.lens (\PipeSourceDynamoDBStreamParameters' {startingPosition} -> startingPosition) (\s@PipeSourceDynamoDBStreamParameters' {} a -> s {startingPosition = a} :: PipeSourceDynamoDBStreamParameters)

instance
  Data.FromJSON
    PipeSourceDynamoDBStreamParameters
  where
  parseJSON =
    Data.withObject
      "PipeSourceDynamoDBStreamParameters"
      ( \x ->
          PipeSourceDynamoDBStreamParameters'
            Prelude.<$> (x Data..:? "BatchSize")
            Prelude.<*> (x Data..:? "DeadLetterConfig")
            Prelude.<*> (x Data..:? "MaximumBatchingWindowInSeconds")
            Prelude.<*> (x Data..:? "MaximumRecordAgeInSeconds")
            Prelude.<*> (x Data..:? "MaximumRetryAttempts")
            Prelude.<*> (x Data..:? "OnPartialBatchItemFailure")
            Prelude.<*> (x Data..:? "ParallelizationFactor")
            Prelude.<*> (x Data..: "StartingPosition")
      )

instance
  Prelude.Hashable
    PipeSourceDynamoDBStreamParameters
  where
  hashWithSalt
    _salt
    PipeSourceDynamoDBStreamParameters' {..} =
      _salt `Prelude.hashWithSalt` batchSize
        `Prelude.hashWithSalt` deadLetterConfig
        `Prelude.hashWithSalt` maximumBatchingWindowInSeconds
        `Prelude.hashWithSalt` maximumRecordAgeInSeconds
        `Prelude.hashWithSalt` maximumRetryAttempts
        `Prelude.hashWithSalt` onPartialBatchItemFailure
        `Prelude.hashWithSalt` parallelizationFactor
        `Prelude.hashWithSalt` startingPosition

instance
  Prelude.NFData
    PipeSourceDynamoDBStreamParameters
  where
  rnf PipeSourceDynamoDBStreamParameters' {..} =
    Prelude.rnf batchSize
      `Prelude.seq` Prelude.rnf deadLetterConfig
      `Prelude.seq` Prelude.rnf maximumBatchingWindowInSeconds
      `Prelude.seq` Prelude.rnf maximumRecordAgeInSeconds
      `Prelude.seq` Prelude.rnf maximumRetryAttempts
      `Prelude.seq` Prelude.rnf onPartialBatchItemFailure
      `Prelude.seq` Prelude.rnf parallelizationFactor
      `Prelude.seq` Prelude.rnf startingPosition

instance
  Data.ToJSON
    PipeSourceDynamoDBStreamParameters
  where
  toJSON PipeSourceDynamoDBStreamParameters' {..} =
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
            Prelude.Just
              ("StartingPosition" Data..= startingPosition)
          ]
      )
