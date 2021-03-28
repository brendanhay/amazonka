{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTJobsData.Types.JobExecutionSummary
  ( JobExecutionSummary (..)
  -- * Smart constructor
  , mkJobExecutionSummary
  -- * Lenses
  , jExecutionNumber
  , jJobId
  , jLastUpdatedAt
  , jQueuedAt
  , jStartedAt
  , jVersionNumber
  ) where

import qualified Network.AWS.IoTJobsData.Types.JobId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains a subset of information about a job execution.
--
-- /See:/ 'mkJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { executionNumber :: Core.Maybe Core.Integer
    -- ^ A number that identifies a particular job execution on a particular device.
  , jobId :: Core.Maybe Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  , lastUpdatedAt :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds since the epoch, when the job execution was last updated.
  , queuedAt :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds since the epoch, when the job execution was enqueued.
  , startedAt :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds since the epoch, when the job execution started.
  , versionNumber :: Core.Maybe Core.Integer
    -- ^ The version of the job execution. Job execution versions are incremented each time AWS IoT Jobs receives an update from a device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobExecutionSummary' value with any optional fields omitted.
mkJobExecutionSummary
    :: JobExecutionSummary
mkJobExecutionSummary
  = JobExecutionSummary'{executionNumber = Core.Nothing,
                         jobId = Core.Nothing, lastUpdatedAt = Core.Nothing,
                         queuedAt = Core.Nothing, startedAt = Core.Nothing,
                         versionNumber = Core.Nothing}

-- | A number that identifies a particular job execution on a particular device.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jExecutionNumber :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jExecutionNumber = Lens.field @"executionNumber"
{-# INLINEABLE jExecutionNumber #-}
{-# DEPRECATED executionNumber "Use generic-lens or generic-optics with 'executionNumber' instead"  #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jJobId :: Lens.Lens' JobExecutionSummary (Core.Maybe Types.JobId)
jJobId = Lens.field @"jobId"
{-# INLINEABLE jJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The time, in milliseconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jLastUpdatedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE jLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jQueuedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jQueuedAt = Lens.field @"queuedAt"
{-# INLINEABLE jQueuedAt #-}
{-# DEPRECATED queuedAt "Use generic-lens or generic-optics with 'queuedAt' instead"  #-}

-- | The time, in milliseconds since the epoch, when the job execution started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jStartedAt :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jStartedAt = Lens.field @"startedAt"
{-# INLINEABLE jStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

-- | The version of the job execution. Job execution versions are incremented each time AWS IoT Jobs receives an update from a device.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jVersionNumber :: Lens.Lens' JobExecutionSummary (Core.Maybe Core.Integer)
jVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE jVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

instance Core.FromJSON JobExecutionSummary where
        parseJSON
          = Core.withObject "JobExecutionSummary" Core.$
              \ x ->
                JobExecutionSummary' Core.<$>
                  (x Core..:? "executionNumber") Core.<*> x Core..:? "jobId" Core.<*>
                    x Core..:? "lastUpdatedAt"
                    Core.<*> x Core..:? "queuedAt"
                    Core.<*> x Core..:? "startedAt"
                    Core.<*> x Core..:? "versionNumber"
