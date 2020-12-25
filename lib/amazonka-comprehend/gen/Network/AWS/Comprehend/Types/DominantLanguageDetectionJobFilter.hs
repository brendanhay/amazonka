{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DominantLanguageDetectionJobFilter
  ( DominantLanguageDetectionJobFilter (..),

    -- * Smart constructor
    mkDominantLanguageDetectionJobFilter,

    -- * Lenses
    dldjfJobName,
    dldjfJobStatus,
    dldjfSubmitTimeAfter,
    dldjfSubmitTimeBefore,
  )
where

import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering a list of dominant language detection jobs. For more information, see the operation.
--
-- /See:/ 'mkDominantLanguageDetectionJobFilter' smart constructor.
data DominantLanguageDetectionJobFilter = DominantLanguageDetectionJobFilter'
  { -- | Filters on the name of the job.
    jobName :: Core.Maybe Types.JobName,
    -- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
    jobStatus :: Core.Maybe Types.JobStatus,
    -- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
    submitTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
    submitTimeBefore :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DominantLanguageDetectionJobFilter' value with any optional fields omitted.
mkDominantLanguageDetectionJobFilter ::
  DominantLanguageDetectionJobFilter
mkDominantLanguageDetectionJobFilter =
  DominantLanguageDetectionJobFilter'
    { jobName = Core.Nothing,
      jobStatus = Core.Nothing,
      submitTimeAfter = Core.Nothing,
      submitTimeBefore = Core.Nothing
    }

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfJobName :: Lens.Lens' DominantLanguageDetectionJobFilter (Core.Maybe Types.JobName)
dldjfJobName = Lens.field @"jobName"
{-# DEPRECATED dldjfJobName "Use generic-lens or generic-optics with 'jobName' instead." #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfJobStatus :: Lens.Lens' DominantLanguageDetectionJobFilter (Core.Maybe Types.JobStatus)
dldjfJobStatus = Lens.field @"jobStatus"
{-# DEPRECATED dldjfJobStatus "Use generic-lens or generic-optics with 'jobStatus' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfSubmitTimeAfter :: Lens.Lens' DominantLanguageDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
dldjfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# DEPRECATED dldjfSubmitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead." #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dldjfSubmitTimeBefore :: Lens.Lens' DominantLanguageDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
dldjfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# DEPRECATED dldjfSubmitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead." #-}

instance Core.FromJSON DominantLanguageDetectionJobFilter where
  toJSON DominantLanguageDetectionJobFilter {..} =
    Core.object
      ( Core.catMaybes
          [ ("JobName" Core..=) Core.<$> jobName,
            ("JobStatus" Core..=) Core.<$> jobStatus,
            ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
            ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore
          ]
      )
