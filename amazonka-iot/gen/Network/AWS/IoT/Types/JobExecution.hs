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
-- Module      : Network.AWS.IoT.Types.JobExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecution where

import Network.AWS.IoT.Types.JobExecutionStatus
import Network.AWS.IoT.Types.JobExecutionStatusDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The job execution object represents the execution of a job on a
-- particular device.
--
-- /See:/ 'newJobExecution' smart constructor.
data JobExecution = JobExecution'
  { -- | The time, in seconds since the epoch, when the job execution started.
    startedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED,
    -- TIMED_OUT, CANCELED, or REJECTED).
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The ARN of the thing on which the job execution is running.
    thingArn :: Prelude.Maybe Prelude.Text,
    -- | A collection of name\/value pairs that describe the status of the job
    -- execution.
    statusDetails :: Prelude.Maybe JobExecutionStatusDetails,
    -- | The time, in seconds since the epoch, when the job execution was queued.
    queuedAt :: Prelude.Maybe Prelude.POSIX,
    -- | Will be @true@ if the job execution was canceled with the optional
    -- @force@ parameter set to @true@.
    forceCanceled :: Prelude.Maybe Prelude.Bool,
    -- | The version of the job execution. Job execution versions are incremented
    -- each time they are updated by a device.
    versionNumber :: Prelude.Maybe Prelude.Integer,
    -- | A string (consisting of the digits \"0\" through \"9\") which identifies
    -- this particular job execution on this particular device. It can be used
    -- in commands which return or update job execution information.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The estimated number of seconds that remain before the job execution
    -- status will be changed to @TIMED_OUT@. The timeout interval can be
    -- anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual
    -- job execution timeout can occur up to 60 seconds later than the
    -- estimated duration. This value will not be included if the job execution
    -- has reached a terminal status.
    approximateSecondsBeforeTimedOut :: Prelude.Maybe Prelude.Integer,
    -- | The time, in seconds since the epoch, when the job execution was last
    -- updated.
    lastUpdatedAt :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier you assigned to the job when it was created.
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
-- 'startedAt', 'jobExecution_startedAt' - The time, in seconds since the epoch, when the job execution started.
--
-- 'status', 'jobExecution_status' - The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED,
-- TIMED_OUT, CANCELED, or REJECTED).
--
-- 'thingArn', 'jobExecution_thingArn' - The ARN of the thing on which the job execution is running.
--
-- 'statusDetails', 'jobExecution_statusDetails' - A collection of name\/value pairs that describe the status of the job
-- execution.
--
-- 'queuedAt', 'jobExecution_queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
--
-- 'forceCanceled', 'jobExecution_forceCanceled' - Will be @true@ if the job execution was canceled with the optional
-- @force@ parameter set to @true@.
--
-- 'versionNumber', 'jobExecution_versionNumber' - The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
--
-- 'executionNumber', 'jobExecution_executionNumber' - A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- in commands which return or update job execution information.
--
-- 'approximateSecondsBeforeTimedOut', 'jobExecution_approximateSecondsBeforeTimedOut' - The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@. The timeout interval can be
-- anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual
-- job execution timeout can occur up to 60 seconds later than the
-- estimated duration. This value will not be included if the job execution
-- has reached a terminal status.
--
-- 'lastUpdatedAt', 'jobExecution_lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last
-- updated.
--
-- 'jobId', 'jobExecution_jobId' - The unique identifier you assigned to the job when it was created.
newJobExecution ::
  JobExecution
newJobExecution =
  JobExecution'
    { startedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      thingArn = Prelude.Nothing,
      statusDetails = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      forceCanceled = Prelude.Nothing,
      versionNumber = Prelude.Nothing,
      executionNumber = Prelude.Nothing,
      approximateSecondsBeforeTimedOut = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      jobId = Prelude.Nothing
    }

-- | The time, in seconds since the epoch, when the job execution started.
jobExecution_startedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.UTCTime)
jobExecution_startedAt = Lens.lens (\JobExecution' {startedAt} -> startedAt) (\s@JobExecution' {} a -> s {startedAt = a} :: JobExecution) Prelude.. Lens.mapping Prelude._Time

-- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED,
-- TIMED_OUT, CANCELED, or REJECTED).
jobExecution_status :: Lens.Lens' JobExecution (Prelude.Maybe JobExecutionStatus)
jobExecution_status = Lens.lens (\JobExecution' {status} -> status) (\s@JobExecution' {} a -> s {status = a} :: JobExecution)

-- | The ARN of the thing on which the job execution is running.
jobExecution_thingArn :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Text)
jobExecution_thingArn = Lens.lens (\JobExecution' {thingArn} -> thingArn) (\s@JobExecution' {} a -> s {thingArn = a} :: JobExecution)

-- | A collection of name\/value pairs that describe the status of the job
-- execution.
jobExecution_statusDetails :: Lens.Lens' JobExecution (Prelude.Maybe JobExecutionStatusDetails)
jobExecution_statusDetails = Lens.lens (\JobExecution' {statusDetails} -> statusDetails) (\s@JobExecution' {} a -> s {statusDetails = a} :: JobExecution)

-- | The time, in seconds since the epoch, when the job execution was queued.
jobExecution_queuedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.UTCTime)
jobExecution_queuedAt = Lens.lens (\JobExecution' {queuedAt} -> queuedAt) (\s@JobExecution' {} a -> s {queuedAt = a} :: JobExecution) Prelude.. Lens.mapping Prelude._Time

-- | Will be @true@ if the job execution was canceled with the optional
-- @force@ parameter set to @true@.
jobExecution_forceCanceled :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Bool)
jobExecution_forceCanceled = Lens.lens (\JobExecution' {forceCanceled} -> forceCanceled) (\s@JobExecution' {} a -> s {forceCanceled = a} :: JobExecution)

-- | The version of the job execution. Job execution versions are incremented
-- each time they are updated by a device.
jobExecution_versionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_versionNumber = Lens.lens (\JobExecution' {versionNumber} -> versionNumber) (\s@JobExecution' {} a -> s {versionNumber = a} :: JobExecution)

-- | A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- in commands which return or update job execution information.
jobExecution_executionNumber :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_executionNumber = Lens.lens (\JobExecution' {executionNumber} -> executionNumber) (\s@JobExecution' {} a -> s {executionNumber = a} :: JobExecution)

-- | The estimated number of seconds that remain before the job execution
-- status will be changed to @TIMED_OUT@. The timeout interval can be
-- anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual
-- job execution timeout can occur up to 60 seconds later than the
-- estimated duration. This value will not be included if the job execution
-- has reached a terminal status.
jobExecution_approximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.Integer)
jobExecution_approximateSecondsBeforeTimedOut = Lens.lens (\JobExecution' {approximateSecondsBeforeTimedOut} -> approximateSecondsBeforeTimedOut) (\s@JobExecution' {} a -> s {approximateSecondsBeforeTimedOut = a} :: JobExecution)

-- | The time, in seconds since the epoch, when the job execution was last
-- updated.
jobExecution_lastUpdatedAt :: Lens.Lens' JobExecution (Prelude.Maybe Prelude.UTCTime)
jobExecution_lastUpdatedAt = Lens.lens (\JobExecution' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecution' {} a -> s {lastUpdatedAt = a} :: JobExecution) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier you assigned to the job when it was created.
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
            Prelude.<*> (x Prelude..:? "thingArn")
            Prelude.<*> (x Prelude..:? "statusDetails")
            Prelude.<*> (x Prelude..:? "queuedAt")
            Prelude.<*> (x Prelude..:? "forceCanceled")
            Prelude.<*> (x Prelude..:? "versionNumber")
            Prelude.<*> (x Prelude..:? "executionNumber")
            Prelude.<*> (x Prelude..:? "approximateSecondsBeforeTimedOut")
            Prelude.<*> (x Prelude..:? "lastUpdatedAt")
            Prelude.<*> (x Prelude..:? "jobId")
      )

instance Prelude.Hashable JobExecution

instance Prelude.NFData JobExecution
