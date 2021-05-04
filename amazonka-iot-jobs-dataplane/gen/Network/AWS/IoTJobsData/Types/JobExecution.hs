{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTJobsData.Types.JobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecution where

import Network.AWS.IoTJobsData.Types.JobExecutionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains data about a job execution.
--
-- /See:/ 'newJobExecution' smart constructor.
data JobExecution = JobExecution'
  { -- | The time, in milliseconds since the epoch, when the job execution was
    -- started.
    startedAt :: Prelude.Maybe Prelude.Integer,
    -- | The status of the job execution. Can be one of: \"QUEUED\",
    -- \"IN_PROGRESS\", \"FAILED\", \"SUCCESS\", \"CANCELED\", \"REJECTED\", or
    -- \"REMOVED\".
    status :: Prelude.Maybe JobExecutionStatus,
    -- | A collection of name\/value pairs that describe the status of the job
    -- execution.
    statusDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the thing that is executing the job.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- enqueued.
    queuedAt :: Prelude.Maybe Prelude.Integer,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time they are updated by a device.
    versionNumber :: Prelude.Maybe Prelude.Integer,
    -- | A number that identifies a particular job execution on a particular
    -- device. It can be used later in commands that return or update job
    -- execution information.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The content of the job document.
    jobDocument :: Prelude.Maybe Prelude.Text,
    -- | The estimated number of seconds that remain before the job execution
    -- status will be changed to @TIMED_OUT@.
    approximateSecondsBeforeTimedOut :: Prelude.Maybe Prelude.Integer,
    -- | The time, in milliseconds since the epoch, when the job execution was
    -- last updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.Integer,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JobExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startedAt', 'jobExecution_startedAt' - The time, in milliseconds since the epoch, when the job execution was
-- started.
--
-- 'status', 'jobExecution_status' - The status of the job execution. Can be one of: \"QUEUED\",
-- \"IN_PROGRESS\", \"FAILED\", \"SUCCESS\", \"CANCELED\", \"REJECTED\", or
-- \"REMOVED\".
--
-- 'statusDetails', 'jobExecution_statusDetails' - A collection of name\/value pairs that describe the status of the job
-- execution.
--
-- 'thingName', 'jobExecution_thingName' - The name of the thing that is executing the job.
--
-- 'queuedAt', 'jobExecution_queuedAt' - The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
--
-- 'versionNumber', 'jobExecution_versionNumber' - The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
--
-- 'executionNumber', 'jobExecution_executionNumber' - A number that identifies a particular job execution on a particular
-- device. It can be used later in commands that return or update job
-- execution information.
--
-- 'jobDocument', 'jobExecution_jobDocument' - The content of the job document.
--
-- 'approximateSecondsBeforeTimedOut', 'jobExecution_approximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@.
--
-- 'lastUpdatedAt', 'jobExecution_lastUpdatedAt' - The time, in milliseconds since the epoch, when the job execution was
-- last updated.
--
-- 'jobId', 'jobExecution_jobId' - The unique identifier you assigned to this job when it was created.
newJobExecution ::
  JobExecution
newJobExecution =
  JobExecution'
    { startedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      thingName = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      executionNumber = Prelude.Nothing,
      jobDocument = Prelude.Nothing,
      approximateSecondsBeforeTimedOut = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The time, in milliseconds since the epoch, when the job execution was
-- started.
jobExecution_startedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_startedAt = Lens.lens (\JobExecution' {startedAt} -> startedAt) (\s@JobExecution' {} a -> s {startedAt = a} :: JobExecution)

-- | The status of the job execution. Can be one of: \"QUEUED\",
-- \"IN_PROGRESS\", \"FAILED\", \"SUCCESS\", \"CANCELED\", \"REJECTED\", or
-- \"REMOVED\".
jobExecution_status :: Lens.Lens' JobExecution (Prelude.Maybe JobExecutionStatus)
jobExecution_status = Lens.lens (\JobExecution' {status} -> status) (\s@JobExecution' {} a -> s {status = a} :: JobExecution)

-- | A collection of name\/value pairs that describe the status of the job
-- execution.
jobExecution_statusDetails :: Lens.Lens' JobExecution (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
jobExecution_statusDetails = Lens.lens (\JobExecution' {statusDetails} -> statusDetails) (\s@JobExecution' {} a -> s {statusDetails = a} :: JobExecution) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the thing that is executing the job.
jobExecution_thingName :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_thingName = Lens.lens (\JobExecution' {thingName} -> thingName) (\s@JobExecution' {} a -> s {thingName = a} :: JobExecution)

-- | The time, in milliseconds since the epoch, when the job execution was
-- enqueued.
jobExecution_queuedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_queuedAt = Lens.lens (\JobExecution' {queuedAt} -> queuedAt) (\s@JobExecution' {} a -> s {queuedAt = a} :: JobExecution)

-- | The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
jobExecution_versionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_versionNumber = Lens.lens (\JobExecution' {versionNumber} -> versionNumber) (\s@JobExecution' {} a -> s {versionNumber = a} :: JobExecution)

-- | A number that identifies a particular job execution on a particular
-- device. It can be used later in commands that return or update job
-- execution information.
jobExecution_executionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_executionNumber = Lens.lens (\JobExecution' {executionNumber} -> executionNumber) (\s@JobExecution' {} a -> s {executionNumber = a} :: JobExecution)

-- | The content of the job document.
jobExecution_jobDocument :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_jobDocument = Lens.lens (\JobExecution' {jobDocument} -> jobDocument) (\s@JobExecution' {} a -> s {jobDocument = a} :: JobExecution)

-- | The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@.
jobExecution_approximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_approximateSecondsBeforeTimedOut = Lens.lens (\JobExecution' {approximateSecondsBeforeTimedOut} -> approximateSecondsBeforeTimedOut) (\s@JobExecution' {} a -> s {approximateSecondsBeforeTimedOut = a} :: JobExecution)

-- | The time, in milliseconds since the epoch, when the job execution was
-- last updated.
jobExecution_lastUpdatedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_lastUpdatedAt = Lens.lens (\JobExecution' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecution' {} a -> s {lastUpdatedAt = a} :: JobExecution)

-- | The unique identifier you assigned to this job when it was created.
jobExecution_jobId :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_jobId = Lens.lens (\JobExecution' {jobId} -> jobId) (\s@JobExecution' {} a -> s {jobId = a} :: JobExecution)

instance Prelude.FromJSON JobExecution where
  parseJSON =
    Prelude.withObject
      "JobExecution"
      ( \x ->
          JobExecution'
            Prelude.<$> (x Prelude..:? "startedAt")
            Prelude.<*> (x Prelude..:? "status")
            Prelude.<*> ( x Prelude..:? "statusDetails"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "thingName")
            Prelude.<*> (x Prelude..:? "queuedAt")
            Prelude.<*> (x Prelude..:? "versionNumber")
            Prelude.<*> (x Prelude..:? "executionNumber")
            Prelude.<*> (x Prelude..:? "jobDocument")
            Prelude.<*> (x Prelude..:? "approximateSecondsBeforeTimedOut")
            Prelude.<*> (x Prelude..:? "lastUpdatedAt")
            Prelude.<*> (x Prelude..:? "jobId")
      )

instance Prelude.Hashable JobExecution

instance Prelude.NFData JobExecution
