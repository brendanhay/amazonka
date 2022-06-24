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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.JobStatus
import Amazonka.IoT.Types.TargetSelection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The job summary.
--
-- /See:/ 'newJobSummary' smart constructor.
data JobSummary = JobSummary'
  { -- | The time, in seconds since the epoch, when the job was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the job will continue to run (CONTINUOUS), or will be
    -- complete after all those things specified as targets have completed the
    -- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
    -- change is detected in a target. For example, a job will run on a thing
    -- when the thing is added to a target group, even after the job was
    -- completed by all things originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The job summary status.
    status :: Prelude.Maybe JobStatus,
    -- | The job ARN.
    jobArn :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the thing group.
    thingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The time, in seconds since the epoch, when the job was created.
    createdAt :: Prelude.Maybe Core.POSIX
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
-- 'lastUpdatedAt', 'jobSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job was last updated.
--
-- 'targetSelection', 'jobSummary_targetSelection' - Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
--
-- 'jobId', 'jobSummary_jobId' - The unique identifier you assigned to this job when it was created.
--
-- 'status', 'jobSummary_status' - The job summary status.
--
-- 'jobArn', 'jobSummary_jobArn' - The job ARN.
--
-- 'completedAt', 'jobSummary_completedAt' - The time, in seconds since the epoch, when the job completed.
--
-- 'thingGroupId', 'jobSummary_thingGroupId' - The ID of the thing group.
--
-- 'createdAt', 'jobSummary_createdAt' - The time, in seconds since the epoch, when the job was created.
newJobSummary ::
  JobSummary
newJobSummary =
  JobSummary'
    { lastUpdatedAt = Prelude.Nothing,
      targetSelection = Prelude.Nothing,
      jobId = Prelude.Nothing,
      status = Prelude.Nothing,
      jobArn = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      thingGroupId = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The time, in seconds since the epoch, when the job was last updated.
jobSummary_lastUpdatedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_lastUpdatedAt = Lens.lens (\JobSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobSummary' {} a -> s {lastUpdatedAt = a} :: JobSummary) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be
-- complete after all those things specified as targets have completed the
-- job (SNAPSHOT). If continuous, the job may also be run on a thing when a
-- change is detected in a target. For example, a job will run on a thing
-- when the thing is added to a target group, even after the job was
-- completed by all things originally in the group.
jobSummary_targetSelection :: Lens.Lens' JobSummary (Prelude.Maybe TargetSelection)
jobSummary_targetSelection = Lens.lens (\JobSummary' {targetSelection} -> targetSelection) (\s@JobSummary' {} a -> s {targetSelection = a} :: JobSummary)

-- | The unique identifier you assigned to this job when it was created.
jobSummary_jobId :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobId = Lens.lens (\JobSummary' {jobId} -> jobId) (\s@JobSummary' {} a -> s {jobId = a} :: JobSummary)

-- | The job summary status.
jobSummary_status :: Lens.Lens' JobSummary (Prelude.Maybe JobStatus)
jobSummary_status = Lens.lens (\JobSummary' {status} -> status) (\s@JobSummary' {} a -> s {status = a} :: JobSummary)

-- | The job ARN.
jobSummary_jobArn :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_jobArn = Lens.lens (\JobSummary' {jobArn} -> jobArn) (\s@JobSummary' {} a -> s {jobArn = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job completed.
jobSummary_completedAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_completedAt = Lens.lens (\JobSummary' {completedAt} -> completedAt) (\s@JobSummary' {} a -> s {completedAt = a} :: JobSummary) Prelude.. Lens.mapping Core._Time

-- | The ID of the thing group.
jobSummary_thingGroupId :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.Text)
jobSummary_thingGroupId = Lens.lens (\JobSummary' {thingGroupId} -> thingGroupId) (\s@JobSummary' {} a -> s {thingGroupId = a} :: JobSummary)

-- | The time, in seconds since the epoch, when the job was created.
jobSummary_createdAt :: Lens.Lens' JobSummary (Prelude.Maybe Prelude.UTCTime)
jobSummary_createdAt = Lens.lens (\JobSummary' {createdAt} -> createdAt) (\s@JobSummary' {} a -> s {createdAt = a} :: JobSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON JobSummary where
  parseJSON =
    Core.withObject
      "JobSummary"
      ( \x ->
          JobSummary'
            Prelude.<$> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "targetSelection")
            Prelude.<*> (x Core..:? "jobId")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "jobArn")
            Prelude.<*> (x Core..:? "completedAt")
            Prelude.<*> (x Core..:? "thingGroupId")
            Prelude.<*> (x Core..:? "createdAt")
      )

instance Prelude.Hashable JobSummary where
  hashWithSalt _salt JobSummary' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` jobArn
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` thingGroupId
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData JobSummary where
  rnf JobSummary' {..} =
    Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf jobArn
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf thingGroupId
      `Prelude.seq` Prelude.rnf createdAt
