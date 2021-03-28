{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobSummary
  ( JobSummary (..)
  -- * Smart constructor
  , mkJobSummary
  -- * Lenses
  , jsCompletedAt
  , jsCreatedAt
  , jsJobArn
  , jsJobId
  , jsLastUpdatedAt
  , jsStatus
  , jsTargetSelection
  , jsThingGroupId
  ) where

import qualified Network.AWS.IoT.Types.JobArn as Types
import qualified Network.AWS.IoT.Types.JobId as Types
import qualified Network.AWS.IoT.Types.JobStatus as Types
import qualified Network.AWS.IoT.Types.TargetSelection as Types
import qualified Network.AWS.IoT.Types.ThingGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The job summary.
--
-- /See:/ 'mkJobSummary' smart constructor.
data JobSummary = JobSummary'
  { completedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job completed.
  , createdAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job was created.
  , jobArn :: Core.Maybe Types.JobArn
    -- ^ The job ARN.
  , jobId :: Core.Maybe Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job was last updated.
  , status :: Core.Maybe Types.JobStatus
    -- ^ The job summary status.
  , targetSelection :: Core.Maybe Types.TargetSelection
    -- ^ Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
  , thingGroupId :: Core.Maybe Types.ThingGroupId
    -- ^ The ID of the thing group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JobSummary' value with any optional fields omitted.
mkJobSummary
    :: JobSummary
mkJobSummary
  = JobSummary'{completedAt = Core.Nothing, createdAt = Core.Nothing,
                jobArn = Core.Nothing, jobId = Core.Nothing,
                lastUpdatedAt = Core.Nothing, status = Core.Nothing,
                targetSelection = Core.Nothing, thingGroupId = Core.Nothing}

-- | The time, in seconds since the epoch, when the job completed.
--
-- /Note:/ Consider using 'completedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsCompletedAt :: Lens.Lens' JobSummary (Core.Maybe Core.NominalDiffTime)
jsCompletedAt = Lens.field @"completedAt"
{-# INLINEABLE jsCompletedAt #-}
{-# DEPRECATED completedAt "Use generic-lens or generic-optics with 'completedAt' instead"  #-}

-- | The time, in seconds since the epoch, when the job was created.
--
-- /Note:/ Consider using 'createdAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsCreatedAt :: Lens.Lens' JobSummary (Core.Maybe Core.NominalDiffTime)
jsCreatedAt = Lens.field @"createdAt"
{-# INLINEABLE jsCreatedAt #-}
{-# DEPRECATED createdAt "Use generic-lens or generic-optics with 'createdAt' instead"  #-}

-- | The job ARN.
--
-- /Note:/ Consider using 'jobArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobArn :: Lens.Lens' JobSummary (Core.Maybe Types.JobArn)
jsJobArn = Lens.field @"jobArn"
{-# INLINEABLE jsJobArn #-}
{-# DEPRECATED jobArn "Use generic-lens or generic-optics with 'jobArn' instead"  #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsJobId :: Lens.Lens' JobSummary (Core.Maybe Types.JobId)
jsJobId = Lens.field @"jobId"
{-# INLINEABLE jsJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The time, in seconds since the epoch, when the job was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsLastUpdatedAt :: Lens.Lens' JobSummary (Core.Maybe Core.NominalDiffTime)
jsLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE jsLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The job summary status.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsStatus :: Lens.Lens' JobSummary (Core.Maybe Types.JobStatus)
jsStatus = Lens.field @"status"
{-# INLINEABLE jsStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | Specifies whether the job will continue to run (CONTINUOUS), or will be complete after all those things specified as targets have completed the job (SNAPSHOT). If continuous, the job may also be run on a thing when a change is detected in a target. For example, a job will run on a thing when the thing is added to a target group, even after the job was completed by all things originally in the group.
--
-- /Note:/ Consider using 'targetSelection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsTargetSelection :: Lens.Lens' JobSummary (Core.Maybe Types.TargetSelection)
jsTargetSelection = Lens.field @"targetSelection"
{-# INLINEABLE jsTargetSelection #-}
{-# DEPRECATED targetSelection "Use generic-lens or generic-optics with 'targetSelection' instead"  #-}

-- | The ID of the thing group.
--
-- /Note:/ Consider using 'thingGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jsThingGroupId :: Lens.Lens' JobSummary (Core.Maybe Types.ThingGroupId)
jsThingGroupId = Lens.field @"thingGroupId"
{-# INLINEABLE jsThingGroupId #-}
{-# DEPRECATED thingGroupId "Use generic-lens or generic-optics with 'thingGroupId' instead"  #-}

instance Core.FromJSON JobSummary where
        parseJSON
          = Core.withObject "JobSummary" Core.$
              \ x ->
                JobSummary' Core.<$>
                  (x Core..:? "completedAt") Core.<*> x Core..:? "createdAt" Core.<*>
                    x Core..:? "jobArn"
                    Core.<*> x Core..:? "jobId"
                    Core.<*> x Core..:? "lastUpdatedAt"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "targetSelection"
                    Core.<*> x Core..:? "thingGroupId"
