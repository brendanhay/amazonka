{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoTJobsData.Types.JobExecution
  ( JobExecution (..)
  -- * Smart constructor
  , mkJobExecution
  -- * Lenses
  , jeApproximateSecondsBeforeTimedOut
  , jeExecutionNumber
  , jeJobDocument
  , jeJobId
  , jeLastUpdatedAt
  , jeQueuedAt
  , jeStartedAt
  , jeStatus
  , jeStatusDetails
  , jeThingName
  , jeVersionNumber
  ) where

import qualified Network.AWS.IoTJobsData.Types.DetailsKey as Types
import qualified Network.AWS.IoTJobsData.Types.DetailsValue as Types
import qualified Network.AWS.IoTJobsData.Types.JobDocument as Types
import qualified Network.AWS.IoTJobsData.Types.JobExecutionStatus as Types
import qualified Network.AWS.IoTJobsData.Types.JobId as Types
import qualified Network.AWS.IoTJobsData.Types.ThingName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains data about a job execution.
--
-- /See:/ 'mkJobExecution' smart constructor.
data JobExecution = JobExecution'
  { approximateSecondsBeforeTimedOut :: Core.Maybe Core.Integer
    -- ^ The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ .
  , executionNumber :: Core.Maybe Core.Integer
    -- ^ A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
  , jobDocument :: Core.Maybe Types.JobDocument
    -- ^ The content of the job document.
  , jobId :: Core.Maybe Types.JobId
    -- ^ The unique identifier you assigned to this job when it was created.
  , lastUpdatedAt :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds since the epoch, when the job execution was last updated. 
  , queuedAt :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds since the epoch, when the job execution was enqueued.
  , startedAt :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds since the epoch, when the job execution was started.
  , status :: Core.Maybe Types.JobExecutionStatus
    -- ^ The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
  , statusDetails :: Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue)
    -- ^ A collection of name/value pairs that describe the status of the job execution.
  , thingName :: Core.Maybe Types.ThingName
    -- ^ The name of the thing that is executing the job.
  , versionNumber :: Core.Maybe Core.Integer
    -- ^ The version of the job execution. Job execution versions are incremented each time they are updated by a device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobExecution' value with any optional fields omitted.
mkJobExecution
    :: JobExecution
mkJobExecution
  = JobExecution'{approximateSecondsBeforeTimedOut = Core.Nothing,
                  executionNumber = Core.Nothing, jobDocument = Core.Nothing,
                  jobId = Core.Nothing, lastUpdatedAt = Core.Nothing,
                  queuedAt = Core.Nothing, startedAt = Core.Nothing,
                  status = Core.Nothing, statusDetails = Core.Nothing,
                  thingName = Core.Nothing, versionNumber = Core.Nothing}

-- | The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ .
--
-- /Note:/ Consider using 'approximateSecondsBeforeTimedOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeApproximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeApproximateSecondsBeforeTimedOut = Lens.field @"approximateSecondsBeforeTimedOut"
{-# INLINEABLE jeApproximateSecondsBeforeTimedOut #-}
{-# DEPRECATED approximateSecondsBeforeTimedOut "Use generic-lens or generic-optics with 'approximateSecondsBeforeTimedOut' instead"  #-}

-- | A number that identifies a particular job execution on a particular device. It can be used later in commands that return or update job execution information.
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeExecutionNumber :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeExecutionNumber = Lens.field @"executionNumber"
{-# INLINEABLE jeExecutionNumber #-}
{-# DEPRECATED executionNumber "Use generic-lens or generic-optics with 'executionNumber' instead"  #-}

-- | The content of the job document.
--
-- /Note:/ Consider using 'jobDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeJobDocument :: Lens.Lens' JobExecution (Core.Maybe Types.JobDocument)
jeJobDocument = Lens.field @"jobDocument"
{-# INLINEABLE jeJobDocument #-}
{-# DEPRECATED jobDocument "Use generic-lens or generic-optics with 'jobDocument' instead"  #-}

-- | The unique identifier you assigned to this job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeJobId :: Lens.Lens' JobExecution (Core.Maybe Types.JobId)
jeJobId = Lens.field @"jobId"
{-# INLINEABLE jeJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The time, in milliseconds since the epoch, when the job execution was last updated. 
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeLastUpdatedAt :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE jeLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The time, in milliseconds since the epoch, when the job execution was enqueued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeQueuedAt :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeQueuedAt = Lens.field @"queuedAt"
{-# INLINEABLE jeQueuedAt #-}
{-# DEPRECATED queuedAt "Use generic-lens or generic-optics with 'queuedAt' instead"  #-}

-- | The time, in milliseconds since the epoch, when the job execution was started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStartedAt :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeStartedAt = Lens.field @"startedAt"
{-# INLINEABLE jeStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

-- | The status of the job execution. Can be one of: "QUEUED", "IN_PROGRESS", "FAILED", "SUCCESS", "CANCELED", "REJECTED", or "REMOVED".
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatus :: Lens.Lens' JobExecution (Core.Maybe Types.JobExecutionStatus)
jeStatus = Lens.field @"status"
{-# INLINEABLE jeStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A collection of name/value pairs that describe the status of the job execution.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatusDetails :: Lens.Lens' JobExecution (Core.Maybe (Core.HashMap Types.DetailsKey Types.DetailsValue))
jeStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE jeStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The name of the thing that is executing the job.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeThingName :: Lens.Lens' JobExecution (Core.Maybe Types.ThingName)
jeThingName = Lens.field @"thingName"
{-# INLINEABLE jeThingName #-}
{-# DEPRECATED thingName "Use generic-lens or generic-optics with 'thingName' instead"  #-}

-- | The version of the job execution. Job execution versions are incremented each time they are updated by a device.
--
-- /Note:/ Consider using 'versionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeVersionNumber :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeVersionNumber = Lens.field @"versionNumber"
{-# INLINEABLE jeVersionNumber #-}
{-# DEPRECATED versionNumber "Use generic-lens or generic-optics with 'versionNumber' instead"  #-}

instance Core.FromJSON JobExecution where
        parseJSON
          = Core.withObject "JobExecution" Core.$
              \ x ->
                JobExecution' Core.<$>
                  (x Core..:? "approximateSecondsBeforeTimedOut") Core.<*>
                    x Core..:? "executionNumber"
                    Core.<*> x Core..:? "jobDocument"
                    Core.<*> x Core..:? "jobId"
                    Core.<*> x Core..:? "lastUpdatedAt"
                    Core.<*> x Core..:? "queuedAt"
                    Core.<*> x Core..:? "startedAt"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusDetails"
                    Core.<*> x Core..:? "thingName"
                    Core.<*> x Core..:? "versionNumber"
