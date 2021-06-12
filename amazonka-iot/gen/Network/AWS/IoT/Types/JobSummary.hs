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

-- | The job summary.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The job summary status.
    status :: Core.Maybe JobStatus,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    targetSelection :: Core.Maybe TargetSelection,
    -- | The time, in seconds since the epoch, when the job completed.
    completedAt :: Core.Maybe Core.POSIX,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The job ARN.
    jobArn :: Core.Maybe Core.Text,
    -- | The ID of the thing group.
    thingGroupId :: Core.Maybe Core.Text,
    -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Core.Maybe Core.POSIX,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'targetSelection', 'jobSummary_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- 'completedAt', 'jobSummary_completedAt' - The time, in seconds since the epoch, when the job completed.
--
-- 'createdAt', 'jobSummary_createdAt' - The time, in seconds since the epoch, when the job was created.
--
-- 'jobArn', 'jobSummary_jobArn' - The job ARN.
--
-- 'thingGroupId', 'jobSummary_thingGroupId' - The ID of the thing group.
--
-- 'lastUpdatedAt', 'jobSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- 'jobId', 'jobSummary_jobId' - The unique identifier you assigned to this job when it was created.
newJobSummary ::
  JobSummary
newJobSummary =
  JobSummary'
    { status = Core.Nothing,
      targetSelection = Core.Nothing,
      completedAt = Core.Nothing,
      createdAt = Core.Nothing,
      jobArn = Core.Nothing,
      thingGroupId = Core.Nothing,
      lastUpdatedAt = Core.Nothing,
      jobId = Core.Nothing
    }

-- | The job summary status.
jobSummary_status :: Lens.Lens' JobSummary (Core.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
jobSummary_targetSelection :: Lens.Lens' JobSummary (Core.Maybe TargetSelection)
jobSummary_targetSelection = Lens.lens (\JobSummary' {targetSelection} -> targetSelection) (\s@JobSummary' {} a -> s {targetSelection = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job completed.
jobSummary_completedAt :: Lens.Lens' JobSummary (Core.Maybe Core.UTCTime)
jobSummary_completedAt = Lens.lens (\JobSummary' {completedAt} -> completedAt) (\s@JobSummary' {} a -> s {completedAt = a} :: JobSummary) Core.. Lens.mapping Core._Time

-- | The time, in seconds since the epoch, when the job was created.
jobSummary_createdAt :: Lens.Lens' JobSummary (Core.Maybe Core.UTCTime)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary) Core.. Lens.mapping Core._Time

-- | The job ARN.
jobSummary_jobArn :: Lens.Lens' JobSummary (Core.Maybe Core.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The ID of the thing group.
jobSummary_thingGroupId :: Lens.Lens' JobSummary (Core.Maybe Core.Text)
jobSummary_thingGroupId = Lens.lens (\JobSummary' {thingGroupId} -> thingGroupId) (\s@JobSummary' {} a -> s {thingGroupId = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job was last updated.
jobSummary_lastUpdatedAt :: Lens.Lens' JobSummary (Core.Maybe Core.UTCTime)
jobSummary_lastUpdatedAt = Lens.lens (\JobSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobSummary' {} a -> s {lastUpdatedAt = a} :: JobSummary) Core.. Lens.mapping Core._Time

-- | The unique identifier you assigned to this job when it was created.
jobSummary_jobId :: Lens.Lens' JobSummary (Core.Maybe Core.Text)
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

instance Core.FromJSON JobSummary where
  parseJSON =
    Core.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Core.<$> (x Core..:? "status")
            Core.<*> (x Core..:? "targetSelection")
            Core.<*> (x Core..:? "completedAt")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "jobArn")
            Core.<*> (x Core..:? "thingGroupId")
            Core.<*> (x Core..:? "lastUpdatedAt")
            Core.<*> (x Core..:? "jobId")
      )

instance Core.Hashable JobSummary

instance Core.NFData JobSummary
