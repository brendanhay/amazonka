{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.TopicsDetectionJobFilter
  ( TopicsDetectionJobFilter (..)
  -- * Smart constructor
  , mkTopicsDetectionJobFilter
  -- * Lenses
  , tdjfJobName
  , tdjfJobStatus
  , tdjfSubmitTimeAfter
  , tdjfSubmitTimeBefore
  ) where

import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering topic detection jobs. For more information, see .
--
-- /See:/ 'mkTopicsDetectionJobFilter' smart constructor.
data TopicsDetectionJobFilter = TopicsDetectionJobFilter'
  { jobName :: Core.Maybe Types.JobName
    -- ^ 
  , jobStatus :: Core.Maybe Types.JobStatus
    -- ^ Filters the list of topic detection jobs based on job status. Returns only jobs with the specified status.
  , submitTimeAfter :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
  , submitTimeBefore :: Core.Maybe Core.NominalDiffTime
    -- ^ Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TopicsDetectionJobFilter' value with any optional fields omitted.
mkTopicsDetectionJobFilter
    :: TopicsDetectionJobFilter
mkTopicsDetectionJobFilter
  = TopicsDetectionJobFilter'{jobName = Core.Nothing,
                              jobStatus = Core.Nothing, submitTimeAfter = Core.Nothing,
                              submitTimeBefore = Core.Nothing}

-- | 
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfJobName :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Types.JobName)
tdjfJobName = Lens.field @"jobName"
{-# INLINEABLE tdjfJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Filters the list of topic detection jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfJobStatus :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Types.JobStatus)
tdjfJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE tdjfJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted after the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfSubmitTimeAfter :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
tdjfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# INLINEABLE tdjfSubmitTimeAfter #-}
{-# DEPRECATED submitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Only returns jobs submitted before the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdjfSubmitTimeBefore :: Lens.Lens' TopicsDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
tdjfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# INLINEABLE tdjfSubmitTimeBefore #-}
{-# DEPRECATED submitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead"  #-}

instance Core.FromJSON TopicsDetectionJobFilter where
        toJSON TopicsDetectionJobFilter{..}
          = Core.object
              (Core.catMaybes
                 [("JobName" Core..=) Core.<$> jobName,
                  ("JobStatus" Core..=) Core.<$> jobStatus,
                  ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
                  ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore])
