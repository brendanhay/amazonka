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
-- Module      : Network.AWS.IoT.Types.JobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.JobStatus
import Network.AWS.IoT.Types.TargetSelection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The job summary.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The job summary status.
    status :: Prelude.Maybe JobStatus,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The job ARN.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the thing group.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection
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
-- 'status', 'jobSummary_status' - The job summary status.
--
-- 'jobId', 'jobSummary_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'lastUpdatedAt', 'jobSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- 'jobArn', 'jobSummary_jobArn' - The job ARN.
--
-- 'createdAt', 'jobSummary_createdAt' - The time, in seconds since the epoch, when the job was created.
--
-- 'thingGroupId', 'jobSummary_thingGroupId' - The ID of the thing group.
--
-- 'completedAt', 'jobSummary_completedAt' - The time, in seconds since the epoch, when the job completed.
--
-- 'targetSelection', 'jobSummary_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
newJobSummary ::
  JobSummary
newJobSummary =
  JobSummary'
    { status = Prelude.Nothing,
      jobId = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      targetSelection = Prelude.Nothing
    }

-- | The job summary status.
jobSummary_status :: Lens.Lens' JobSummary (Prelude.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | The unique identifier you assigned to this job when it was created.
jobSummary_jobId :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job was last updated.
jobSummary_lastUpdatedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_lastUpdatedAt = Lens.lens (\JobSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobSummary' {} a -> s {lastUpdatedAt = a} :: JobSummary) Prelude.. Lens.mapping Core._Time

-- | The job ARN.
jobSummary_jobArn :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job was created.
jobSummary_createdAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the thing group.
jobSummary_thingGroupId :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_thingGroupId = Lens.lens (\JobSummary' {thingGroupId} -> thingGroupId) (\s@JobSummary' {} a -> s {thingGroupId = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job completed.
jobSummary_completedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_completedAt = Lens.lens (\JobSummary' {completedAt} -> completedAt) (\s@JobSummary' {} a -> s {completedAt = a} :: JobSummary) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
jobSummary_targetSelection :: Lens.Lens' JobSummary (Prelude.Maybe TargetSelection)
jobSummary_targetSelection = Lens.lens (\JobSummary' {targetSelection} -> targetSelection) (\s@JobSummary' {} a -> s {targetSelection = a} :: JobSummary)

instance Core.FromJSON JobSummary where
  parseJSON =
    Core.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "jobArn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "thingGroupId")
            Prelude.<*> (x Core..:? "completedAt")
            Prelude.<*> (x Core..:? "targetSelection")
      )

instance Prelude.Hashable JobSummary

instance Prelude.NFData JobSummary
