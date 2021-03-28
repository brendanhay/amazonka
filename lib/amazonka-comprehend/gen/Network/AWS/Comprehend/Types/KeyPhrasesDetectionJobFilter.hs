{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.KeyPhrasesDetectionJobFilter
  ( KeyPhrasesDetectionJobFilter (..)
  -- * Smart constructor
  , mkKeyPhrasesDetectionJobFilter
  -- * Lenses
  , kpdjfJobName
  , kpdjfJobStatus
  , kpdjfSubmitTimeAfter
  , kpdjfSubmitTimeBefore
  ) where

import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
-- /See:/ 'mkKeyPhrasesDetectionJobFilter' smart constructor.
data KeyPhrasesDetectionJobFilter = KeyPhrasesDetectionJobFilter'
  { jobName :: Core.Maybe Types.JobName
    -- ^ Filters on the name of the job.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Filters the list of jobs based on job status. Returns only jobs with the specified status.
  , submitTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
  , submitTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'KeyPhrasesDetectionJobFilter' value with any optional fields omitted.
mkKeyPhrasesDetectionJobFilter
    :: KeyPhrasesDetectionJobFilter
mkKeyPhrasesDetectionJobFilter
  = KeyPhrasesDetectionJobFilter'{jobName = Core.Nothing,
                                  jobStatus = Core.Nothing, submitTimeAfter = Core.Nothing,
                                  submitTimeBefore = Core.Nothing}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjfJobName :: Lens.Lens' KeyPhrasesDetectionJobFilter (Core.Maybe Types.JobName)
kpdjfJobName = Lens.field @"jobName"
{-# INLINEABLE kpdjfJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjfJobStatus :: Lens.Lens' KeyPhrasesDetectionJobFilter (Core.Maybe Types.JobStatus)
kpdjfJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE kpdjfJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjfSubmitTimeAfter :: Lens.Lens' KeyPhrasesDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
kpdjfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# INLINEABLE kpdjfSubmitTimeAfter #-}
{-# DEPRECATED submitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpdjfSubmitTimeBefore :: Lens.Lens' KeyPhrasesDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
kpdjfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# INLINEABLE kpdjfSubmitTimeBefore #-}
{-# DEPRECATED submitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead"  #-}

instance Core.FromJSON KeyPhrasesDetectionJobFilter where
        toJSON KeyPhrasesDetectionJobFilter{..}
          = Core.object
              (Core.catMaybes
                 [("JobName" Core..=) Core.<$> jobName,
                  ("JobStatus" Core..=) Core.<$> jobStatus,
                  ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
                  ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore])
