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
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.TopicsDetectionJobFilter where

import Network.AWS.Comprehend.Types.JobStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information for filtering topic detection jobs. For more
-- information, see .
--
-- /See:/ 'newTopicsDetectionJobFilter' smart constructor.
data TopicsDetectionJobFilter = TopicsDetectionJobFilter'
  { -- | Filters the list of topic detection jobs based on job status. Returns
    -- only jobs with the specified status.
    jobStatus :: Core.Maybe JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Only returns jobs submitted before the specified time.
    -- Jobs are returned in descending order, newest to oldest.
    submitTimeBefore :: Core.Maybe Core.POSIX,
    -- | Filters the list of jobs based on the time that the job was submitted
    -- for processing. Only returns jobs submitted after the specified time.
    -- Jobs are returned in ascending order, oldest to newest.
    submitTimeAfter :: Core.Maybe Core.POSIX,
    jobName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TopicsDetectionJobFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobStatus', 'topicsDetectionJobFilter_jobStatus' - Filters the list of topic detection jobs based on job status. Returns
-- only jobs with the specified status.
--
-- 'submitTimeBefore', 'topicsDetectionJobFilter_submitTimeBefore' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted before the specified time.
-- Jobs are returned in descending order, newest to oldest.
--
-- 'submitTimeAfter', 'topicsDetectionJobFilter_submitTimeAfter' - Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted after the specified time.
-- Jobs are returned in ascending order, oldest to newest.
--
-- 'jobName', 'topicsDetectionJobFilter_jobName' -
newTopicsDetectionJobFilter ::
  TopicsDetectionJobFilter
newTopicsDetectionJobFilter =
  TopicsDetectionJobFilter'
    { jobStatus = Core.Nothing,
      submitTimeBefore = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      jobName = Core.Nothing
    }

-- | Filters the list of topic detection jobs based on job status. Returns
-- only jobs with the specified status.
topicsDetectionJobFilter_jobStatus :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe JobStatus)
topicsDetectionJobFilter_jobStatus = Lens.lens (\TopicsDetectionJobFilter' {jobStatus} -> jobStatus) (\s@TopicsDetectionJobFilter' {} a -> s {jobStatus = a} :: TopicsDetectionJobFilter)

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted before the specified time.
-- Jobs are returned in descending order, newest to oldest.
topicsDetectionJobFilter_submitTimeBefore :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Core.UTCTime)
topicsDetectionJobFilter_submitTimeBefore = Lens.lens (\TopicsDetectionJobFilter' {submitTimeBefore} -> submitTimeBefore) (\s@TopicsDetectionJobFilter' {} a -> s {submitTimeBefore = a} :: TopicsDetectionJobFilter) Core.. Lens.mapping Core._Time

-- | Filters the list of jobs based on the time that the job was submitted
-- for processing. Only returns jobs submitted after the specified time.
-- Jobs are returned in ascending order, oldest to newest.
topicsDetectionJobFilter_submitTimeAfter :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Core.UTCTime)
topicsDetectionJobFilter_submitTimeAfter = Lens.lens (\TopicsDetectionJobFilter' {submitTimeAfter} -> submitTimeAfter) (\s@TopicsDetectionJobFilter' {} a -> s {submitTimeAfter = a} :: TopicsDetectionJobFilter) Core.. Lens.mapping Core._Time

-- |
topicsDetectionJobFilter_jobName :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Core.Text)
topicsDetectionJobFilter_jobName = Lens.lens (\TopicsDetectionJobFilter' {jobName} -> jobName) (\s@TopicsDetectionJobFilter' {} a -> s {jobName = a} :: TopicsDetectionJobFilter)

instance Core.Hashable TopicsDetectionJobFilter

instance Core.NFData TopicsDetectionJobFilter

instance Core.ToJSON TopicsDetectionJobFilter where
  toJSON TopicsDetectionJobFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobStatus" Core..=) Core.<$> jobStatus,
            ("SubmitTimeBefore" Core..=)
              Core.<$> submitTimeBefore,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("JobName" Core..=) Core.<$> jobName
          ]
      )
