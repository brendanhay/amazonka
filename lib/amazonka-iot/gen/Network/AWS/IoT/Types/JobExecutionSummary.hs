{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobExecutionSummary
  ( JobExecutionSummary (..)
  -- * Smart constructor
  , mkJobExecutionSummary
  -- * Lenses
  , jesExecutionNumber
  , jesLastUpdatedAt
  , jesQueuedAt
  , jesStartedAt
  , jesStatus
  ) where

import qualified Network.AWS.IoT.Types.JobExecutionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The job execution summary.
--
-- /See:/ 'mkJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { executionNumber :: Core.Maybe Core.Integer
    -- ^ A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used later in commands which return or update job execution information.
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job execution was last updated.
  , queuedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job execution was queued.
  , startedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job execution started.
  , status :: Core.Maybe Types.JobExecutionStatus
    -- ^ The status of the job execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JobExecutionSummary' value with any optional fields omitted.
mkJobExecutionSummary
    :: JobExecutionSummary
mkJobExecutionSummary
  = JobExecutionSummary'{executionNumber = Core.Nothing,
                         lastUpdatedAt = Core.Nothing, queuedAt = Core.Nothing,
                         startedAt = Core.Nothing, status = Core.Nothing}

-- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used later in commands which return or update job execution information.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesExecutionNumber :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jesExecutionNumber = Lens.field @"executionNumber"
{-# INLINEABLE jesExecutionNumber #-}
{-# DEPRECATED executionNumber "Use generic-lens or generic-optics with 'executionNumber' instead"  #-}

-- | The time, in seconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesLastUpdatedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.NominalDiffTime)
jesLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE jesLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The time, in seconds since the epoch, when the job execution was queued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesQueuedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.NominalDiffTime)
jesQueuedAt = Lens.field @"queuedAt"
{-# INLINEABLE jesQueuedAt #-}
{-# DEPRECATED queuedAt "Use generic-lens or generic-optics with 'queuedAt' instead"  #-}

-- | The time, in seconds since the epoch, when the job execution started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStartedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.NominalDiffTime)
jesStartedAt = Lens.field @"startedAt"
{-# INLINEABLE jesStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

-- | The status of the job execution.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jesStatus :: Lens.Lens' JobExecutionSummary (Core.Maybe Types.JobExecutionStatus)
jesStatus = Lens.field @"status"
{-# INLINEABLE jesStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromJSON JobExecutionSummary where
        parseJSON
          = Core.withObject "JobExecutionSummary" Core.$
              \ x ->
                JobExecutionSummary' Core.<$>
                  (x Core..:? "executionNumber") Core.<*> x Core..:? "lastUpdatedAt"
                    Core.<*> x Core..:? "queuedAt"
                    Core.<*> x Core..:? "startedAt"
                    Core.<*> x Core..:? "status"
