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
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information for filtering a list of PII entity detection jobs.
--
-- /See:/ 'newPiiEntitiesDetectionJobFilter' smart constructor.
data PiiEntitiesDetectionJobFilter = PiiEntitiesDetectionJobFilter'
  { -- | Filters the list of jobs based on job status. Returns only jobs with the
    -- specified status.
    jobStatus :: Core.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted before the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Core.Maybe Core.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Returns only jobs submitted after the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Core.Maybe Core.POSIX,
    -- | Filters on the name of the job.
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PiiEntitiesDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'piiEntitiesDetectionJobFilter_jobStatus' - Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
--
-- 'submitTimeBefore', 'piiEntitiesDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'submitTimeAfter', 'piiEntitiesDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'jobName', 'piiEntitiesDetectionJobFilter_jobName' - Filters on the name of the job.
newPiiEntitiesDetectionJobFilter ::
  PiiEntitiesDetectionJobFilter
newPiiEntitiesDetectionJobFilter =
  PiiEntitiesDetectionJobFilter'
    { jobStatus =
        Core.Nothing,
      submitTimeBefore = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      jobName = Core.Nothing
    }

-- | Filters the list of jobs based on job status. Returns only jobs with the
-- specified status.
piiEntitiesDetectionJobFilter_jobStatus :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe JobStatus)
piiEntitiesDetectionJobFilter_jobStatus = Lens.lens (\PiiEntitiesDetectionJobFilter' {jobStatus} -> jobStatus) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {jobStatus = a} :: PiiEntitiesDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted before the specified time.
-- Jobs are returned in ascending order, oldest to newest.
piiEntitiesDetectionJobFilter_submitTimeBefore :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Core.UTCTime)
piiEntitiesDetectionJobFilter_submitTimeBefore = Lens.lens (\PiiEntitiesDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: PiiEntitiesDetectionJobFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Returns only jobs submitted after the specified time.
-- Jobs are returned in descending order, newest to oldest.
piiEntitiesDetectionJobFilter_submitTimeAfter :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Core.UTCTime)
piiEntitiesDetectionJobFilter_submitTimeAfter = Lens.lens (\PiiEntitiesDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: PiiEntitiesDetectionJobFilter) Core.. Lens.mapping Core._Time

-- | Filters on the name of the job.
piiEntitiesDetectionJobFilter_jobName :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Core.Text)
piiEntitiesDetectionJobFilter_jobName = Lens.lens (\PiiEntitiesDetectionJobFilter' {jobName} -> jobName) (\s@PiiEntitiesDetectionJobFilter' {} a -> s {jobName = a} :: PiiEntitiesDetectionJobFilter)

instance Core.Hashable PiiEntitiesDetectionJobFilter

instance Core.NFData PiiEntitiesDetectionJobFilter

instance Core.ToJSON PiiEntitiesDetectionJobFilter where
  toJSON PiiEntitiesDetectionJobFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobStatus" Core..=) Core.<$> jobStatus,
            ("SubmitTimeBefore" Core..=)
              Core.<$> submitTimeBefore,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )
