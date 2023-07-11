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
-- Module      : Amazonka.IoT.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.JobExecutionStatus
import qualified Amazonka.Prelude as Prelude

-- | The job execution summary.
--
-- /See:/ 'newJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { -- | A string (consisting of the digits \"0\" through \"9\") which identifies
    -- this particular job execution on this particular device. It can be used
    -- later in commands which return or update job execution information.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The time, in seconds since the epoch, when the job execution was last
    -- updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The time, in seconds since the epoch, when the job execution was queued.
    queuedAt :: Prelude.Maybe Data.POSIX,
    -- | The number that indicates how many retry attempts have been completed
    -- for this job on this device.
    retryAttempt :: Prelude.Maybe Prelude.Int,
    -- | The time, in seconds since the epoch, when the job execution started.
    startedAt :: Prelude.Maybe Data.POSIX,
    -- | The status of the job execution.
    status :: Prelude.Maybe JobExecutionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionNumber', 'jobExecutionSummary_executionNumber' - A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- later in commands which return or update job execution information.
--
-- 'lastUpdatedAt', 'jobExecutionSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last
-- updated.
--
-- 'queuedAt', 'jobExecutionSummary_queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
--
-- 'retryAttempt', 'jobExecutionSummary_retryAttempt' - The number that indicates how many retry attempts have been completed
-- for this job on this device.
--
-- 'startedAt', 'jobExecutionSummary_startedAt' - The time, in seconds since the epoch, when the job execution started.
--
-- 'status', 'jobExecutionSummary_status' - The status of the job execution.
newJobExecutionSummary ::
  JobExecutionSummary
newJobExecutionSummary =
  JobExecutionSummary'
    { executionNumber =
        Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      retryAttempt = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- later in commands which return or update job execution information.
jobExecutionSummary_executionNumber :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_executionNumber = Lens.lens (\JobExecutionSummary' {executionNumber} -> executionNumber) (\s@JobExecutionSummary' {} a -> s {executionNumber = a} :: JobExecutionSummary)

-- | The time, in seconds since the epoch, when the job execution was last
-- updated.
jobExecutionSummary_lastUpdatedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
jobExecutionSummary_lastUpdatedAt = Lens.lens (\JobExecutionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecutionSummary' {} a -> s {lastUpdatedAt = a} :: JobExecutionSummary) Prelude.. Lens.mapping Data._Time

-- | The time, in seconds since the epoch, when the job execution was queued.
jobExecutionSummary_queuedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
jobExecutionSummary_queuedAt = Lens.lens (\JobExecutionSummary' {queuedAt} -> queuedAt) (\s@JobExecutionSummary' {} a -> s {queuedAt = a} :: JobExecutionSummary) Prelude.. Lens.mapping Data._Time

-- | The number that indicates how many retry attempts have been completed
-- for this job on this device.
jobExecutionSummary_retryAttempt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Int)
jobExecutionSummary_retryAttempt = Lens.lens (\JobExecutionSummary' {retryAttempt} -> retryAttempt) (\s@JobExecutionSummary' {} a -> s {retryAttempt = a} :: JobExecutionSummary)

-- | The time, in seconds since the epoch, when the job execution started.
jobExecutionSummary_startedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
jobExecutionSummary_startedAt = Lens.lens (\JobExecutionSummary' {startedAt} -> startedAt) (\s@JobExecutionSummary' {} a -> s {startedAt = a} :: JobExecutionSummary) Prelude.. Lens.mapping Data._Time

-- | The status of the job execution.
jobExecutionSummary_status :: Lens.Lens' JobExecutionSummary (Prelude.Maybe JobExecutionStatus)
jobExecutionSummary_status = Lens.lens (\JobExecutionSummary' {status} -> status) (\s@JobExecutionSummary' {} a -> s {status = a} :: JobExecutionSummary)

instance Data.FromJSON JobExecutionSummary where
  parseJSON =
    Data.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Prelude.<$> (x Data..:? "executionNumber")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "queuedAt")
            Prelude.<*> (x Data..:? "retryAttempt")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable JobExecutionSummary where
  hashWithSalt _salt JobExecutionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` executionNumber
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` queuedAt
      `Prelude.hashWithSalt` retryAttempt
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData JobExecutionSummary where
  rnf JobExecutionSummary' {..} =
    Prelude.rnf executionNumber
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf queuedAt
      `Prelude.seq` Prelude.rnf retryAttempt
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf status
