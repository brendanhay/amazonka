{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EventsDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EventsDetectionJobFilter
  ( EventsDetectionJobFilter (..)
  -- * Smart constructor
  , mkEventsDetectionJobFilter
  -- * Lenses
  , edjfJobName
  , edjfJobStatus
  , edjfSubmitTimeAfter
  , edjfSubmitTimeBefore
  ) where

import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering a list of event detection jobs.
--
-- /See:/ 'mkEventsDetectionJobFilter' smart constructor.
data EventsDetectionJobFilter = EventsDetectionJobFilter'
  { jobName :: Core.Maybe Types.JobName
    -- ^ Filters on the name of the events detection job.
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Filters the list of jobs based on job status. Returns only jobs with the specified status.
  , submitTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
  , submitTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'EventsDetectionJobFilter' value with any optional fields omitted.
mkEventsDetectionJobFilter
    :: EventsDetectionJobFilter
mkEventsDetectionJobFilter
  = EventsDetectionJobFilter'{jobName = Core.Nothing,
                              jobStatus = Core.Nothing, submitTimeAfter = Core.Nothing,
                              submitTimeBefore = Core.Nothing}

-- | Filters on the name of the events detection job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfJobName :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Types.JobName)
edjfJobName = Lens.field @"jobName"
{-# INLINEABLE edjfJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfJobStatus :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Types.JobStatus)
edjfJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE edjfJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfSubmitTimeAfter :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
edjfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# INLINEABLE edjfSubmitTimeAfter #-}
{-# DEPRECATED submitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
edjfSubmitTimeBefore :: Lens.Lens' EventsDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
edjfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# INLINEABLE edjfSubmitTimeBefore #-}
{-# DEPRECATED submitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead"  #-}

instance Core.FromJSON EventsDetectionJobFilter where
        toJSON EventsDetectionJobFilter{..}
          = Core.object
              (Core.catMaybes
                 [("JobName" Core..=) Core.<$> jobName,
                  ("JobStatus" Core..=) Core.<$> jobStatus,
                  ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
                  ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore])
