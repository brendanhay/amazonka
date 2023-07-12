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
-- Module      : Amazonka.IoTJobsData.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTJobsData.Types.JobExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains a subset of information about a job execution.
--
-- /See:/ 'newJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { -- | A number that identifies a particular job execution on a particular
    -- device.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- enqueued.
    queuedAt :: Prelude.Maybe Prelude.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution
    -- started.
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time AWS IoT Jobs receives an update from a device.
    versionNumber :: Prelude.Maybe Prelude.Integer
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
-- 'executionNumber', 'jobExecutionSummary_executionNumber' - A number that identifies a particular job execution on a particular
-- device.
--
-- 'jobId', 'jobExecutionSummary_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'lastUpdatedAt', 'jobExecutionSummary_lastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was
-- last updated.
--
-- 'queuedAt', 'jobExecutionSummary_queuedAt' - The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
--
-- 'startedAt', 'jobExecutionSummary_startedAt' - The time, in milliseconds since the epoch, when the job execution
-- started.
--
-- 'versionNumber', 'jobExecutionSummary_versionNumber' - The version of the job execution. Job execution versions are incremented
-- each time AWS IoT Jobs receives an update from a device.
newJobExecutionSummary ::
  JobExecutionSummary
newJobExecutionSummary =
  JobExecutionSummary'
    { executionNumber =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      versionNumber = Prelude.Nothing
    }

-- | A number that identifies a particular job execution on a particular
-- device.
jobExecutionSummary_executionNumber :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_executionNumber = Lens.lens (\JobExecutionSummary' {executionNumber} -> executionNumber) (\s@JobExecutionSummary' {} a -> s {executionNumber = a} :: JobExecutionSummary)

-- | The unique identifier you assigned to this job when it was created.
jobExecutionSummary_jobId :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Text)
jobExecutionSummary_jobId = Lens.lens (\JobExecutionSummary' {jobId} -> jobId) (\s@JobExecutionSummary' {} a -> s {jobId = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution was
-- last updated.
jobExecutionSummary_lastUpdatedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_lastUpdatedAt = Lens.lens (\JobExecutionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecutionSummary' {} a -> s {lastUpdatedAt = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
jobExecutionSummary_queuedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_queuedAt = Lens.lens (\JobExecutionSummary' {queuedAt} -> queuedAt) (\s@JobExecutionSummary' {} a -> s {queuedAt = a} :: JobExecutionSummary)

-- | The time, in milliseconds since the epoch, when the job execution
-- started.
jobExecutionSummary_startedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_startedAt = Lens.lens (\JobExecutionSummary' {startedAt} -> startedAt) (\s@JobExecutionSummary' {} a -> s {startedAt = a} :: JobExecutionSummary)

-- | The version of the job execution. Job execution versions are incremented
-- each time AWS IoT Jobs receives an update from a device.
jobExecutionSummary_versionNumber :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_versionNumber = Lens.lens (\JobExecutionSummary' {versionNumber} -> versionNumber) (\s@JobExecutionSummary' {} a -> s {versionNumber = a} :: JobExecutionSummary)

instance Data.FromJSON JobExecutionSummary where
  parseJSON =
    Data.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Prelude.<$> (x Data..:? "executionNumber")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "queuedAt")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "versionNumber")
      )

instance Prelude.Hashable JobExecutionSummary where
  hashWithSalt _salt JobExecutionSummary' {..} =
    _salt
      `Prelude.hashWithSalt` executionNumber
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` queuedAt
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData JobExecutionSummary where
  rnf JobExecutionSummary' {..} =
    Prelude.rnf executionNumber
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf queuedAt
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf versionNumber
