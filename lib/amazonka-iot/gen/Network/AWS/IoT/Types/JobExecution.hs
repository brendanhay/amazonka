{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobExecution
  ( JobExecution (..)
  -- * Smart constructor
  , mkJobExecution
  -- * Lenses
  , jeApproximateSecondsBeforeTimedOut
  , jeExecutionNumber
  , jeForceCanceled
  , jeJobId
  , jeLastUpdatedAt
  , jeQueuedAt
  , jeStartedAt
  , jeStatus
  , jeStatusDetails
  , jeThingArn
  , jeVersionNumber
  ) where

import qualified Network.AWS.IoT.Types.JobExecutionStatus as Types
import qualified Network.AWS.IoT.Types.JobExecutionStatusDetails as Types
import qualified Network.AWS.IoT.Types.JobId as Types
import qualified Network.AWS.IoT.Types.ThingArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The job execution object represents the execution of a job on a particular device.
--
-- /See:/ 'mkJobExecution' smart constructor.
data JobExecution = JobExecution'
  { approximateSecondsBeforeTimedOut :: Core.Maybe Core.Integer
    -- ^ The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ . The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual job execution timeout can occur up to 60 seconds later than the estimated duration. This value will not be included if the job execution has reached a terminal status.
  , executionNumber :: Core.Maybe Core.Integer
    -- ^ A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information. 
  , forceCanceled :: Core.Maybe Core.Bool
    -- ^ Will be @true@ if the job execution was canceled with the optional @force@ parameter set to @true@ .
  , jobId :: Core.Maybe Types.JobId
    -- ^ The unique identifier you assigned to the job when it was created.
  , lastUpdatedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job execution was last updated.
  , queuedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job execution was queued.
  , startedAt :: Core.Maybe Core.NominalDiffTime
    -- ^ The time, in seconds since the epoch, when the job execution started.
  , status :: Core.Maybe Types.JobExecutionStatus
    -- ^ The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED, TIMED_OUT, CANCELED, or REJECTED).
  , statusDetails :: Core.Maybe Types.JobExecutionStatusDetails
    -- ^ A collection of name/value pairs that describe the status of the job execution.
  , thingArn :: Core.Maybe Types.ThingArn
    -- ^ The ARN of the thing on which the job execution is running.
  , versionNumber :: Core.Maybe Core.Integer
    -- ^ The version of the job execution. Job execution versions are incremented each time they are updated by a device.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'JobExecution' value with any optional fields omitted.
mkJobExecution
    :: JobExecution
mkJobExecution
  = JobExecution'{approximateSecondsBeforeTimedOut = Core.Nothing,
                  executionNumber = Core.Nothing, forceCanceled = Core.Nothing,
                  jobId = Core.Nothing, lastUpdatedAt = Core.Nothing,
                  queuedAt = Core.Nothing, startedAt = Core.Nothing,
                  status = Core.Nothing, statusDetails = Core.Nothing,
                  thingArn = Core.Nothing, versionNumber = Core.Nothing}

-- | The estimated number of seconds that remain before the job execution status will be changed to @TIMED_OUT@ . The timeout interval can be anywhere between 1 minute and 7 days (1 to 10080 minutes). The actual job execution timeout can occur up to 60 seconds later than the estimated duration. This value will not be included if the job execution has reached a terminal status.
--
-- /Note:/ Consider using 'approximateSecondsBeforeTimedOut' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeApproximateSecondsBeforeTimedOut :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeApproximateSecondsBeforeTimedOut = Lens.field @"approximateSecondsBeforeTimedOut"
{-# INLINEABLE jeApproximateSecondsBeforeTimedOut #-}
{-# DEPRECATED approximateSecondsBeforeTimedOut "Use generic-lens or generic-optics with 'approximateSecondsBeforeTimedOut' instead"  #-}

-- | A string (consisting of the digits "0" through "9") which identifies this particular job execution on this particular device. It can be used in commands which return or update job execution information. 
--
-- /Note:/ Consider using 'executionNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeExecutionNumber :: Lens.Lens' JobExecution (Core.Maybe Core.Integer)
jeExecutionNumber = Lens.field @"executionNumber"
{-# INLINEABLE jeExecutionNumber #-}
{-# DEPRECATED executionNumber "Use generic-lens or generic-optics with 'executionNumber' instead"  #-}

-- | Will be @true@ if the job execution was canceled with the optional @force@ parameter set to @true@ .
--
-- /Note:/ Consider using 'forceCanceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeForceCanceled :: Lens.Lens' JobExecution (Core.Maybe Core.Bool)
jeForceCanceled = Lens.field @"forceCanceled"
{-# INLINEABLE jeForceCanceled #-}
{-# DEPRECATED forceCanceled "Use generic-lens or generic-optics with 'forceCanceled' instead"  #-}

-- | The unique identifier you assigned to the job when it was created.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeJobId :: Lens.Lens' JobExecution (Core.Maybe Types.JobId)
jeJobId = Lens.field @"jobId"
{-# INLINEABLE jeJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The time, in seconds since the epoch, when the job execution was last updated.
--
-- /Note:/ Consider using 'lastUpdatedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeLastUpdatedAt :: Lens.Lens' JobExecution (Core.Maybe Core.NominalDiffTime)
jeLastUpdatedAt = Lens.field @"lastUpdatedAt"
{-# INLINEABLE jeLastUpdatedAt #-}
{-# DEPRECATED lastUpdatedAt "Use generic-lens or generic-optics with 'lastUpdatedAt' instead"  #-}

-- | The time, in seconds since the epoch, when the job execution was queued.
--
-- /Note:/ Consider using 'queuedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeQueuedAt :: Lens.Lens' JobExecution (Core.Maybe Core.NominalDiffTime)
jeQueuedAt = Lens.field @"queuedAt"
{-# INLINEABLE jeQueuedAt #-}
{-# DEPRECATED queuedAt "Use generic-lens or generic-optics with 'queuedAt' instead"  #-}

-- | The time, in seconds since the epoch, when the job execution started.
--
-- /Note:/ Consider using 'startedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStartedAt :: Lens.Lens' JobExecution (Core.Maybe Core.NominalDiffTime)
jeStartedAt = Lens.field @"startedAt"
{-# INLINEABLE jeStartedAt #-}
{-# DEPRECATED startedAt "Use generic-lens or generic-optics with 'startedAt' instead"  #-}

-- | The status of the job execution (IN_PROGRESS, QUEUED, FAILED, SUCCEEDED, TIMED_OUT, CANCELED, or REJECTED).
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatus :: Lens.Lens' JobExecution (Core.Maybe Types.JobExecutionStatus)
jeStatus = Lens.field @"status"
{-# INLINEABLE jeStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A collection of name/value pairs that describe the status of the job execution.
--
-- /Note:/ Consider using 'statusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeStatusDetails :: Lens.Lens' JobExecution (Core.Maybe Types.JobExecutionStatusDetails)
jeStatusDetails = Lens.field @"statusDetails"
{-# INLINEABLE jeStatusDetails #-}
{-# DEPRECATED statusDetails "Use generic-lens or generic-optics with 'statusDetails' instead"  #-}

-- | The ARN of the thing on which the job execution is running.
--
-- /Note:/ Consider using 'thingArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jeThingArn :: Lens.Lens' JobExecution (Core.Maybe Types.ThingArn)
jeThingArn = Lens.field @"thingArn"
{-# INLINEABLE jeThingArn #-}
{-# DEPRECATED thingArn "Use generic-lens or generic-optics with 'thingArn' instead"  #-}

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
                    Core.<*> x Core..:? "forceCanceled"
                    Core.<*> x Core..:? "jobId"
                    Core.<*> x Core..:? "lastUpdatedAt"
                    Core.<*> x Core..:? "queuedAt"
                    Core.<*> x Core..:? "startedAt"
                    Core.<*> x Core..:? "status"
                    Core.<*> x Core..:? "statusDetails"
                    Core.<*> x Core..:? "thingArn"
                    Core.<*> x Core..:? "versionNumber"
