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
-- Module      : Amazonka.IoT.Types.JobExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.JobExecutionStatus
import Amazonka.IoT.Types.JobExecutionStatusDetails
import qualified Amazonka.Prelude as Prelude

-- | The job execution object represents the execution of a job on a
-- particular device.
--
-- /See:/ 'newJobExecution' smart constructor.
data JobExecution = JobExecution'
  { -- | A string (consisting of the digits \"0\" through \"9\") which identifies
    -- this particular job execution on this particular device. It can be used
    -- in commands which return or update job execution information.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The ARN of the thing on which the job execution is running.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job execution was last
    -- updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | A collection of name\/value pairs that describe the status of the job
    -- execution.
    statusDetails :: Prelude.Maybe JobExecutionStatusDetails,
    -- | The unique identifier you assigned to the job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | Will be @true@ if the job execution was canceled with the optional
    -- @force@ parameter set to @true@.
    forceCanceled :: Prelude.Maybe Prelude.Bool,
    -- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED,
    -- TIMED_OUT, CANCELED, or REJECTED).
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The time, in seconds since the epoch, when the job execution started.
    startedAt :: Prelude.Maybe Data.POSIX,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time they are updated by a device.
    versionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The time, in seconds since the epoch, when the job execution was queued.
    queuedAt :: Prelude.Maybe Data.POSIX,
    -- | The estimated number of seconds that remain before the job execution
    -- status will be changed to @TIMED_OUT@. The timeout interval can be
    -- anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual
    -- job execution timeout can occur up to 60 seconds later than the
    -- estimated duration. This value will not be included if the job execution
    -- has reached a terminal status.
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
-- 'executionNumber', 'jobExecution_executionNumber' - A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- in commands which return or update job execution information.
--
-- 'thingArn', 'jobExecution_thingArn' - The ARN of the thing on which the job execution is running.
--
-- 'lastUpdatedAt', 'jobExecution_lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last
-- updated.
--
-- 'statusDetails', 'jobExecution_statusDetails' - A collection of name\/value pairs that describe the status of the job
-- execution.
--
-- 'jobId', 'jobExecution_jobId' - The unique identifier you assigned to the job when it was created.
--
-- 'forceCanceled', 'jobExecution_forceCanceled' - Will be @true@ if the job execution was canceled with the optional
-- @force@ parameter set to @true@.
--
-- 'status', 'jobExecution_status' - The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED,
-- TIMED_OUT, CANCELED, or REJECTED).
--
-- 'startedAt', 'jobExecution_startedAt' - The time, in seconds since the epoch, when the job execution started.
--
-- 'versionNumber', 'jobExecution_versionNumber' - The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
--
-- 'queuedAt', 'jobExecution_queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
--
-- 'approximateSecondsBeforeTimedOut', 'jobExecution_approximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@. The timeout interval can be
-- anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual
-- job execution timeout can occur up to 60 seconds later than the
-- estimated duration. This value will not be included if the job execution
-- has reached a terminal status.
newJobExecution ::
  JobExecution
newJobExecution =
  JobExecution'
    { executionNumber = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      jobId = Prelude.Nothing,
      forceCanceled = Prelude.Nothing,
      status = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      approximateSecondsBeforeTimedOut = Prelude.Nothing
    }

-- | A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- in commands which return or update job execution information.
jobExecution_executionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_executionNumber = Lens.lens (\JobExecution' {executionNumber} -> executionNumber) (\s@JobExecution' {} a -> s {executionNumber = a} :: JobExecution)

-- | The ARN of the thing on which the job execution is running.
jobExecution_thingArn :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_thingArn = Lens.lens (\JobExecution' {thingArn} -> thingArn) (\s@JobExecution' {} a -> s {thingArn = a} :: JobExecution)

-- | The time, in seconds since the epoch, when the job execution was last
-- updated.
jobExecution_lastUpdatedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.UTCTime)
jobExecution_lastUpdatedAt = Lens.lens (\JobExecution' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecution' {} a -> s {lastUpdatedAt = a} :: JobExecution) Prelude.. Lens.mapping Data._Time

-- | A collection of name\/value pairs that describe the status of the job
-- execution.
jobExecution_statusDetails :: Lens.Lens' JobExecution (Prelude.Maybe JobExecutionStatusDetails)
jobExecution_statusDetails = Lens.lens (\JobExecution' {statusDetails} -> statusDetails) (\s@JobExecution' {} a -> s {statusDetails = a} :: JobExecution)

-- | The unique identifier you assigned to the job when it was created.
jobExecution_jobId :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_jobId = Lens.lens (\JobExecution' {jobId} -> jobId) (\s@JobExecution' {} a -> s {jobId = a} :: JobExecution)

-- | Will be @true@ if the job execution was canceled with the optional
-- @force@ parameter set to @true@.
jobExecution_forceCanceled :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Bool)
jobExecution_forceCanceled = Lens.lens (\JobExecution' {forceCanceled} -> forceCanceled) (\s@JobExecution' {} a -> s {forceCanceled = a} :: JobExecution)

-- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED,
-- TIMED_OUT, CANCELED, or REJECTED).
jobExecution_status :: Lens.Lens' JobExecution (Prelude.Maybe JobExecutionStatus)
jobExecution_status = Lens.lens (\JobExecution' {status} -> status) (\s@JobExecution' {} a -> s {status = a} :: JobExecution)

-- | The time, in seconds since the epoch, when the job execution started.
jobExecution_startedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.UTCTime)
jobExecution_startedAt = Lens.lens (\JobExecution' {startedAt} -> startedAt) (\s@JobExecution' {} a -> s {startedAt = a} :: JobExecution) Prelude.. Lens.mapping Data._Time

-- | The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
jobExecution_versionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_versionNumber = Lens.lens (\JobExecution' {versionNumber} -> versionNumber) (\s@JobExecution' {} a -> s {versionNumber = a} :: JobExecution)

-- | The time, in seconds since the epoch, when the job execution was queued.
jobExecution_queuedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.UTCTime)
jobExecution_queuedAt = Lens.lens (\JobExecution' {queuedAt} -> queuedAt) (\s@JobExecution' {} a -> s {queuedAt = a} :: JobExecution) Prelude.. Lens.mapping Data._Time

-- | The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@. The timeout interval can be
-- anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual
-- job execution timeout can occur up to 60 seconds later than the
-- estimated duration. This value will not be included if the job execution
-- has reached a terminal status.
jobExecution_approximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_approximateSecondsBeforeTimedOut = Lens.lens (\JobExecution' {approximateSecondsBeforeTimedOut} -> approximateSecondsBeforeTimedOut) (\s@JobExecution' {} a -> s {approximateSecondsBeforeTimedOut = a} :: JobExecution)

instance Data.FromJSON JobExecution where
  parseJSON =
    Data.withObject
      "JobExecution"
      ( \x ->
          JobExecution'
            Prelude.<$> (x Data..:? "executionNumber")
            Prelude.<*> (x Data..:? "thingArn")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "statusDetails")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "forceCanceled")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "startedAt")
            Prelude.<*> (x Data..:? "versionNumber")
            Prelude.<*> (x Data..:? "queuedAt")
            Prelude.<*> (x Data..:? "approximateSecondsBeforeTimedOut")
      )

instance Prelude.Hashable JobExecution where
  hashWithSalt _salt JobExecution' {..} =
    _salt `Prelude.hashWithSalt` executionNumber
      `Prelude.hashWithSalt` thingArn
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` statusDetails
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` forceCanceled
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` queuedAt
      `Prelude.hashWithSalt` approximateSecondsBeforeTimedOut

instance Prelude.NFData JobExecution where
  rnf JobExecution' {..} =
    Prelude.rnf executionNumber
      `Prelude.seq` Prelude.rnf thingArn
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf statusDetails
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf forceCanceled
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf queuedAt
      `Prelude.seq` Prelude.rnf approximateSecondsBeforeTimedOut
