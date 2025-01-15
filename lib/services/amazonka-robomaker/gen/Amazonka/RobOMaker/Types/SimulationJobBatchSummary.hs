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
-- Module      : Amazonka.RobOMaker.Types.SimulationJobBatchSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.SimulationJobBatchSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.SimulationJobBatchStatus

-- | Information about a simulation job batch.
--
-- /See:/ 'newSimulationJobBatchSummary' smart constructor.
data SimulationJobBatchSummary = SimulationJobBatchSummary'
  { -- | The Amazon Resource Name (ARN) of the batch.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the simulation job batch
    -- was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The number of created simulation job requests.
    createdRequestCount :: Prelude.Maybe Prelude.Int,
    -- | The number of failed simulation job requests.
    failedRequestCount :: Prelude.Maybe Prelude.Int,
    -- | The time, in milliseconds since the epoch, when the simulation job batch
    -- was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The number of pending simulation job requests.
    pendingRequestCount :: Prelude.Maybe Prelude.Int,
    -- | The status of the simulation job batch.
    --
    -- [Pending]
    --     The simulation job batch request is pending.
    --
    -- [InProgress]
    --     The simulation job batch is in progress.
    --
    -- [Failed]
    --     The simulation job batch failed. One or more simulation job requests
    --     could not be completed due to an internal failure (like
    --     @InternalServiceError@). See @failureCode@ and @failureReason@ for
    --     more information.
    --
    -- [Completed]
    --     The simulation batch job completed. A batch is complete when (1)
    --     there are no pending simulation job requests in the batch and none
    --     of the failed simulation job requests are due to
    --     @InternalServiceError@ and (2) when all created simulation jobs have
    --     reached a terminal state (for example, @Completed@ or @Failed@).
    --
    -- [Canceled]
    --     The simulation batch job was cancelled.
    --
    -- [Canceling]
    --     The simulation batch job is being cancelled.
    --
    -- [Completing]
    --     The simulation batch job is completing.
    --
    -- [TimingOut]
    --     The simulation job batch is timing out.
    --
    --     If a batch timing out, and there are pending requests that were
    --     failing due to an internal failure (like @InternalServiceError@),
    --     the batch status will be @Failed@. If there are no such failing
    --     request, the batch status will be @TimedOut@.
    --
    -- [TimedOut]
    --     The simulation batch job timed out.
    status :: Prelude.Maybe SimulationJobBatchStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimulationJobBatchSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'simulationJobBatchSummary_arn' - The Amazon Resource Name (ARN) of the batch.
--
-- 'createdAt', 'simulationJobBatchSummary_createdAt' - The time, in milliseconds since the epoch, when the simulation job batch
-- was created.
--
-- 'createdRequestCount', 'simulationJobBatchSummary_createdRequestCount' - The number of created simulation job requests.
--
-- 'failedRequestCount', 'simulationJobBatchSummary_failedRequestCount' - The number of failed simulation job requests.
--
-- 'lastUpdatedAt', 'simulationJobBatchSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the simulation job batch
-- was last updated.
--
-- 'pendingRequestCount', 'simulationJobBatchSummary_pendingRequestCount' - The number of pending simulation job requests.
--
-- 'status', 'simulationJobBatchSummary_status' - The status of the simulation job batch.
--
-- [Pending]
--     The simulation job batch request is pending.
--
-- [InProgress]
--     The simulation job batch is in progress.
--
-- [Failed]
--     The simulation job batch failed. One or more simulation job requests
--     could not be completed due to an internal failure (like
--     @InternalServiceError@). See @failureCode@ and @failureReason@ for
--     more information.
--
-- [Completed]
--     The simulation batch job completed. A batch is complete when (1)
--     there are no pending simulation job requests in the batch and none
--     of the failed simulation job requests are due to
--     @InternalServiceError@ and (2) when all created simulation jobs have
--     reached a terminal state (for example, @Completed@ or @Failed@).
--
-- [Canceled]
--     The simulation batch job was cancelled.
--
-- [Canceling]
--     The simulation batch job is being cancelled.
--
-- [Completing]
--     The simulation batch job is completing.
--
-- [TimingOut]
--     The simulation job batch is timing out.
--
--     If a batch timing out, and there are pending requests that were
--     failing due to an internal failure (like @InternalServiceError@),
--     the batch status will be @Failed@. If there are no such failing
--     request, the batch status will be @TimedOut@.
--
-- [TimedOut]
--     The simulation batch job timed out.
newSimulationJobBatchSummary ::
  SimulationJobBatchSummary
newSimulationJobBatchSummary =
  SimulationJobBatchSummary'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      createdRequestCount = Prelude.Nothing,
      failedRequestCount = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      pendingRequestCount = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the batch.
simulationJobBatchSummary_arn :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe Prelude.Text)
simulationJobBatchSummary_arn = Lens.lens (\SimulationJobBatchSummary' {arn} -> arn) (\s@SimulationJobBatchSummary' {} a -> s {arn = a} :: SimulationJobBatchSummary)

-- | The time, in milliseconds since the epoch, when the simulation job batch
-- was created.
simulationJobBatchSummary_createdAt :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe Prelude.UTCTime)
simulationJobBatchSummary_createdAt = Lens.lens (\SimulationJobBatchSummary' {createdAt} -> createdAt) (\s@SimulationJobBatchSummary' {} a -> s {createdAt = a} :: SimulationJobBatchSummary) Prelude.. Lens.mapping Data._Time

-- | The number of created simulation job requests.
simulationJobBatchSummary_createdRequestCount :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe Prelude.Int)
simulationJobBatchSummary_createdRequestCount = Lens.lens (\SimulationJobBatchSummary' {createdRequestCount} -> createdRequestCount) (\s@SimulationJobBatchSummary' {} a -> s {createdRequestCount = a} :: SimulationJobBatchSummary)

-- | The number of failed simulation job requests.
simulationJobBatchSummary_failedRequestCount :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe Prelude.Int)
simulationJobBatchSummary_failedRequestCount = Lens.lens (\SimulationJobBatchSummary' {failedRequestCount} -> failedRequestCount) (\s@SimulationJobBatchSummary' {} a -> s {failedRequestCount = a} :: SimulationJobBatchSummary)

-- | The time, in milliseconds since the epoch, when the simulation job batch
-- was last updated.
simulationJobBatchSummary_lastUpdatedAt :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe Prelude.UTCTime)
simulationJobBatchSummary_lastUpdatedAt = Lens.lens (\SimulationJobBatchSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@SimulationJobBatchSummary' {} a -> s {lastUpdatedAt = a} :: SimulationJobBatchSummary) Prelude.. Lens.mapping Data._Time

-- | The number of pending simulation job requests.
simulationJobBatchSummary_pendingRequestCount :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe Prelude.Int)
simulationJobBatchSummary_pendingRequestCount = Lens.lens (\SimulationJobBatchSummary' {pendingRequestCount} -> pendingRequestCount) (\s@SimulationJobBatchSummary' {} a -> s {pendingRequestCount = a} :: SimulationJobBatchSummary)

-- | The status of the simulation job batch.
--
-- [Pending]
--     The simulation job batch request is pending.
--
-- [InProgress]
--     The simulation job batch is in progress.
--
-- [Failed]
--     The simulation job batch failed. One or more simulation job requests
--     could not be completed due to an internal failure (like
--     @InternalServiceError@). See @failureCode@ and @failureReason@ for
--     more information.
--
-- [Completed]
--     The simulation batch job completed. A batch is complete when (1)
--     there are no pending simulation job requests in the batch and none
--     of the failed simulation job requests are due to
--     @InternalServiceError@ and (2) when all created simulation jobs have
--     reached a terminal state (for example, @Completed@ or @Failed@).
--
-- [Canceled]
--     The simulation batch job was cancelled.
--
-- [Canceling]
--     The simulation batch job is being cancelled.
--
-- [Completing]
--     The simulation batch job is completing.
--
-- [TimingOut]
--     The simulation job batch is timing out.
--
--     If a batch timing out, and there are pending requests that were
--     failing due to an internal failure (like @InternalServiceError@),
--     the batch status will be @Failed@. If there are no such failing
--     request, the batch status will be @TimedOut@.
--
-- [TimedOut]
--     The simulation batch job timed out.
simulationJobBatchSummary_status :: Lens.Lens' SimulationJobBatchSummary (Prelude.Maybe SimulationJobBatchStatus)
simulationJobBatchSummary_status = Lens.lens (\SimulationJobBatchSummary' {status} -> status) (\s@SimulationJobBatchSummary' {} a -> s {status = a} :: SimulationJobBatchSummary)

instance Data.FromJSON SimulationJobBatchSummary where
  parseJSON =
    Data.withObject
      "SimulationJobBatchSummary"
      ( \x ->
          SimulationJobBatchSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "createdRequestCount")
            Prelude.<*> (x Data..:? "failedRequestCount")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "pendingRequestCount")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable SimulationJobBatchSummary where
  hashWithSalt _salt SimulationJobBatchSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` createdRequestCount
      `Prelude.hashWithSalt` failedRequestCount
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` pendingRequestCount
      `Prelude.hashWithSalt` status

instance Prelude.NFData SimulationJobBatchSummary where
  rnf SimulationJobBatchSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdAt `Prelude.seq`
        Prelude.rnf createdRequestCount `Prelude.seq`
          Prelude.rnf failedRequestCount `Prelude.seq`
            Prelude.rnf lastUpdatedAt `Prelude.seq`
              Prelude.rnf pendingRequestCount `Prelude.seq`
                Prelude.rnf status
