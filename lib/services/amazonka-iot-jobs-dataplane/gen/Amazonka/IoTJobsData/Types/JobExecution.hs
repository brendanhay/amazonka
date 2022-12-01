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
-- Module      : Amazonka.IoTJobsData.Types.JobExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTJobsData.Types.JobExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTJobsData.Types.JobExecutionStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains data about a job execution.
--
-- /See:/ 'newJobExecution' smart constructor.
data JobExecution = JobExecution'
  { -- | The name of the thing that is executing the job.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | A number that identifies a particular job execution on a particular
    -- device. It can be used later in commands that return or update job
    -- execution information.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The content of the job document.
    jobDocument :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.Integer,
    -- | A collection of name\/value pairs that describe the status of the job
    -- execution.
    statusDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The status of the job execution. Can be one of: \"QUEUED\",
    -- \"IN_PROGRESS\", \"FAILED\", \"SUCCESS\", \"CANCELED\", \"REJECTED\", or
    -- \"REMOVED\".
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- started.
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time they are updated by a device.
    versionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- enqueued.
    queuedAt :: Prelude.Maybe Prelude.Integer,
    -- | The estimated number of seconds that remain before the job execution
    -- status will be changed to @TIMED_OUT@.
    approximateSecondsBeforeTimedOut :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingName', 'jobExecution_thingName' - The name of the thing that is executing the job.
--
-- 'executionNumber', 'jobExecution_executionNumber' - A number that identifies a particular job execution on a particular
-- device. It can be used later in commands that return or update job
-- execution information.
--
-- 'jobDocument', 'jobExecution_jobDocument' - The content of the job document.
--
-- 'lastUpdatedAt', 'jobExecution_lastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was
-- last updated.
--
-- 'statusDetails', 'jobExecution_statusDetails' - A collection of name\/value pairs that describe the status of the job
-- execution.
--
-- 'jobId', 'jobExecution_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'status', 'jobExecution_status' - The status of the job execution. Can be one of: \"QUEUED\",
-- \"IN_PROGRESS\", \"FAILED\", \"SUCCESS\", \"CANCELED\", \"REJECTED\", or
-- \"REMOVED\".
--
-- 'startedAt', 'jobExecution_startedAt' - The time, in milliseconds since the epoch, when the job execution was
-- started.
--
-- 'versionNumber', 'jobExecution_versionNumber' - The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
--
-- 'queuedAt', 'jobExecution_queuedAt' - The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
--
-- 'approximateSecondsBeforeTimedOut', 'jobExecution_approximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@.
newJobExecution ::
  JobExecution
newJobExecution =
  JobExecution'
    { thingName = Prelude.Nothing,
      executionNumber = Prelude.Nothing,
      jobDocument = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      approximateSecondsBeforeTimedOut = Prelude.Nothing
    }

-- | The name of the thing that is executing the job.
jobExecution_thingName :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_thingName = Lens.lens (\JobExecution' {thingName} -> thingName) (\s@JobExecution' {} a -> s {thingName = a} :: JobExecution)

-- | A number that identifies a particular job execution on a particular
-- device. It can be used later in commands that return or update job
-- execution information.
jobExecution_executionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_executionNumber = Lens.lens (\JobExecution' {executionNumber} -> executionNumber) (\s@JobExecution' {} a -> s {executionNumber = a} :: JobExecution)

-- | The content of the job document.
jobExecution_jobDocument :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_jobDocument = Lens.lens (\JobExecution' {jobDocument} -> jobDocument) (\s@JobExecution' {} a -> s {jobDocument = a} :: JobExecution)

-- | The time, in milliseconds since the epoch, when the job execution was
-- last updated.
jobExecution_lastUpdatedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_lastUpdatedAt = Lens.lens (\JobExecution' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecution' {} a -> s {lastUpdatedAt = a} :: JobExecution)

-- | A collection of name\/value pairs that describe the status of the job
-- execution.
jobExecution_statusDetails :: Lens.Lens' JobExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobExecution_statusDetails = Lens.lens (\JobExecution' {statusDetails} -> statusDetails) (\s@JobExecution' {} a -> s {statusDetails = a} :: JobExecution) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier you assigned to this job when it was created.
jobExecution_jobId :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_jobId = Lens.lens (\JobExecution' {jobId} -> jobId) (\s@JobExecution' {} a -> s {jobId = a} :: JobExecution)

-- | The status of the job execution. Can be one of: \"QUEUED\",
-- \"IN_PROGRESS\", \"FAILED\", \"SUCCESS\", \"CANCELED\", \"REJECTED\", or
-- \"REMOVED\".
jobExecution_status :: Lens.Lens' JobExecution (Prelude.Maybe JobExecutionStatus)
jobExecution_status = Lens.lens (\JobExecution' {status} -> status) (\s@JobExecution' {} a -> s {status = a} :: JobExecution)

-- | The time, in milliseconds since the epoch, when the job execution was
-- started.
jobExecution_startedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_startedAt = Lens.lens (\JobExecution' {startedAt} -> startedAt) (\s@JobExecution' {} a -> s {startedAt = a} :: JobExecution)

-- | The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
jobExecution_versionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_versionNumber = Lens.lens (\JobExecution' {versionNumber} -> versionNumber) (\s@JobExecution' {} a -> s {versionNumber = a} :: JobExecution)

-- | The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
jobExecution_queuedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_queuedAt = Lens.lens (\JobExecution' {queuedAt} -> queuedAt) (\s@JobExecution' {} a -> s {queuedAt = a} :: JobExecution)

-- | The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@.
jobExecution_approximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_approximateSecondsBeforeTimedOut = Lens.lens (\JobExecution' {approximateSecondsBeforeTimedOut} -> approximateSecondsBeforeTimedOut) (\s@JobExecution' {} a -> s {approximateSecondsBeforeTimedOut = a} :: JobExecution)

instance Core.FromJSON JobExecution where
  parseJSON =
    Core.withObject
      "JobExecution"
      ( \x ->
          JobExecution'
            Prelude.<$> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "executionNumber")
            Prelude.<*> (x Core..:? "jobDocument")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "statusDetails" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "startedAt")
            Prelude.<*> (x Core..:? "versionNumber")
            Prelude.<*> (x Core..:? "queuedAt")
            Prelude.<*> (x Core..:? "approximateSecondsBeforeTimedOut")
      )

instance Prelude.Hashable JobExecution where
  hashWithSalt _salt JobExecution' {..} =
    _salt `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` executionNumber
      `Prelude.hashWithSalt` jobDocument
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` queuedAt
      `Prelude.hashWithSalt` approximateSecondsBeforeTimedOut

instance Prelude.NFData JobExecution where
  rnf JobExecution' {..} =
    Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf executionNumber
      `Prelude.seq` Prelude.rnf jobDocument
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf queuedAt
      `Prelude.seq` Prelude.rnf approximateSecondsBeforeTimedOut
