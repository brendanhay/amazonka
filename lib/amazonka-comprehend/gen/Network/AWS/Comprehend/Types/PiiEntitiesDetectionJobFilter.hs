{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.PiiEntitiesDetectionJobFilter
  ( PiiEntitiesDetectionJobFilter (..)
  -- * Smart constructor
  , mkPiiEntitiesDetectionJobFilter
  -- * Lenses
  , pedjfJobName
  , pedjfJobStatus
  , pedjfSubmitTimeAfter
  , pedjfSubmitTimeBefore
  ) where

import qualified Network.AWS.Comprehend.Types.JobName as Types
import qualified Network.AWS.Comprehend.Types.JobStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information for filtering a list of PII entity detection jobs.
--
-- /See:/ 'mkPiiEntitiesDetectionJobFilter' smart constructor.
data PiiEntitiesDetectionJobFilter = PiiEntitiesDetectionJobFilter'
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

-- | Creates a 'PiiEntitiesDetectionJobFilter' value with any optional fields omitted.
mkPiiEntitiesDetectionJobFilter
    :: PiiEntitiesDetectionJobFilter
mkPiiEntitiesDetectionJobFilter
  = PiiEntitiesDetectionJobFilter'{jobName = Core.Nothing,
                                   jobStatus = Core.Nothing, submitTimeAfter = Core.Nothing,
                                   submitTimeBefore = Core.Nothing}

-- | Filters on the name of the job.
--
-- /Note:/ Consider using 'jobName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfJobName :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Types.JobName)
pedjfJobName = Lens.field @"jobName"
{-# INLINEABLE pedjfJobName #-}
{-# DEPRECATED jobName "Use generic-lens or generic-optics with 'jobName' instead"  #-}

-- | Filters the list of jobs based on job status. Returns only jobs with the specified status.
--
-- /Note:/ Consider using 'jobStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfJobStatus :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Types.JobStatus)
pedjfJobStatus = Lens.field @"jobStatus"
{-# INLINEABLE pedjfJobStatus #-}
{-# DEPRECATED jobStatus "Use generic-lens or generic-optics with 'jobStatus' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted after the specified time. Jobs are returned in descending order, newest to oldest.
--
-- /Note:/ Consider using 'submitTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfSubmitTimeAfter :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
pedjfSubmitTimeAfter = Lens.field @"submitTimeAfter"
{-# INLINEABLE pedjfSubmitTimeAfter #-}
{-# DEPRECATED submitTimeAfter "Use generic-lens or generic-optics with 'submitTimeAfter' instead"  #-}

-- | Filters the list of jobs based on the time that the job was submitted for processing. Returns only jobs submitted before the specified time. Jobs are returned in ascending order, oldest to newest.
--
-- /Note:/ Consider using 'submitTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pedjfSubmitTimeBefore :: Lens.Lens' PiiEntitiesDetectionJobFilter (Core.Maybe Core.NominalDiffTime)
pedjfSubmitTimeBefore = Lens.field @"submitTimeBefore"
{-# INLINEABLE pedjfSubmitTimeBefore #-}
{-# DEPRECATED submitTimeBefore "Use generic-lens or generic-optics with 'submitTimeBefore' instead"  #-}

instance Core.FromJSON PiiEntitiesDetectionJobFilter where
        toJSON PiiEntitiesDetectionJobFilter{..}
          = Core.object
              (Core.catMaybes
                 [("JobName" Core..=) Core.<$> jobName,
                  ("JobStatus" Core..=) Core.<$> jobStatus,
                  ("SubmitTimeAfter" Core..=) Core.<$> submitTimeAfter,
                  ("SubmitTimeBefore" Core..=) Core.<$> submitTimeBefore])
