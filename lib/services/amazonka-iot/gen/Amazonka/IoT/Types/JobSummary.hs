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
-- Module      : Amazonka.IoT.Types.JobSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.JobStatus
import Amazonka.IoT.Types.TargetSelection
import qualified Amazonka.Prelude as Prelude

-- | The job summary.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The time, in seconds since the epoch, when the job completed.
    completedAt :: Prelude.Maybe Data.POSIX,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Indicates whether a job is concurrent. Will be true when a job is
    -- rolling out new job executions or canceling previously created
    -- executions, otherwise false.
    isConcurrent :: Prelude.Maybe Prelude.Bool,
    -- | The job ARN.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Prelude.Maybe Data.POSIX,
    -- | The job summary status.
    status :: Prelude.Maybe JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    --
    -- We recommend that you use continuous jobs instead of snapshot jobs for
    -- dynamic thing group targets. By using continuous jobs, devices that join
    -- the group receive the job execution even after the job has been created.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | The ID of the thing group.
    thingGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedAt', 'jobSummary_completedAt' - The time, in seconds since the epoch, when the job completed.
--
-- 'createdAt', 'jobSummary_createdAt' - The time, in seconds since the epoch, when the job was created.
--
-- 'isConcurrent', 'jobSummary_isConcurrent' - Indicates whether a job is concurrent. Will be true when a job is
-- rolling out new job executions or canceling previously created
-- executions, otherwise false.
--
-- 'jobArn', 'jobSummary_jobArn' - The job ARN.
--
-- 'jobId', 'jobSummary_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'lastUpdatedAt', 'jobSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- 'status', 'jobSummary_status' - The job summary status.
--
-- 'targetSelection', 'jobSummary_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
--
-- 'thingGroupId', 'jobSummary_thingGroupId' - The ID of the thing group.
newJobSummary ::
  JobSummary
newJobSummary =
  JobSummary'
    { completedAt = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      isConcurrent = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      jobId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      status = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      thingGroupId = Prelude.Nothing
    }

-- | The time, in seconds since the epoch, when the job completed.
jobSummary_completedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_completedAt = Lens.lens (\JobSummary' {completedAt} -> completedAt) (\s@JobSummary' {} a -> s {completedAt = a} :: JobSummary) Prelude.. Lens.mapping Data._Time

-- | The time, in seconds since the epoch, when the job was created.
jobSummary_createdAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary) Prelude.. Lens.mapping Data._Time

-- | Indicates whether a job is concurrent. Will be true when a job is
-- rolling out new job executions or canceling previously created
-- executions, otherwise false.
jobSummary_isConcurrent :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Bool)
jobSummary_isConcurrent = Lens.lens (\JobSummary' {isConcurrent} -> isConcurrent) (\s@JobSummary' {} a -> s {isConcurrent = a} :: JobSummary)

-- | The job ARN.
jobSummary_jobArn :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The unique identifier you assigned to this job when it was created.
jobSummary_jobId :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job was last updated.
jobSummary_lastUpdatedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_lastUpdatedAt = Lens.lens (\JobSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobSummary' {} a -> s {lastUpdatedAt = a} :: JobSummary) Prelude.. Lens.mapping Data._Time

-- | The job summary status.
jobSummary_status :: Lens.Lens' JobSummary (Prelude.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- We recommend that you use continuous jobs instead of snapshot jobs for
-- dynamic thing group targets. By using continuous jobs, devices that join
-- the group receive the job execution even after the job has been created.
jobSummary_targetSelection :: Lens.Lens' JobSummary (Prelude.Maybe TargetSelection)
jobSummary_targetSelection = Lens.lens (\JobSummary' {targetSelection} -> targetSelection) (\s@JobSummary' {} a -> s {targetSelection = a} :: JobSummary)

-- | The ID of the thing group.
jobSummary_thingGroupId :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_thingGroupId = Lens.lens (\JobSummary' {thingGroupId} -> thingGroupId) (\s@JobSummary' {} a -> s {thingGroupId = a} :: JobSummary)

instance Data.FromJSON JobSummary where
  parseJSON =
    Data.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Data..:? "completedAt")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "isConcurrent")
            Prelude.<*> (x Data..:? "jobArn")
            Prelude.<*> (x Data..:? "jobId")
            Prelude.<*> (x Data..:? "lastUpdatedAt")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "targetSelection")
            Prelude.<*> (x Data..:? "thingGroupId")
      )

instance Prelude.Hashable JobSummary where
  hashWithSalt _salt JobSummary' {..} =
    _salt
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` isConcurrent
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` thingGroupId

instance Prelude.NFData JobSummary where
  rnf JobSummary' {..} =
    Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf isConcurrent
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf thingGroupId
