{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobProcessDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobProcessDetails
  ( JobProcessDetails (..),

    -- * Smart constructor
    mkJobProcessDetails,

    -- * Lenses
    jpdNumberOfCanceledThings,
    jpdNumberOfFailedThings,
    jpdNumberOfInProgressThings,
    jpdNumberOfQueuedThings,
    jpdNumberOfRejectedThings,
    jpdNumberOfRemovedThings,
    jpdNumberOfSucceededThings,
    jpdNumberOfTimedOutThings,
    jpdProcessingTargets,
  )
where

import qualified Network.AWS.IoT.Types.ProcessingTargetName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The job process details.
--
-- /See:/ 'mkJobProcessDetails' smart constructor.
data JobProcessDetails = JobProcessDetails'
  { -- | The number of things that cancelled the job.
    numberOfCanceledThings :: Core.Maybe Core.Int,
    -- | The number of things that failed executing the job.
    numberOfFailedThings :: Core.Maybe Core.Int,
    -- | The number of things currently executing the job.
    numberOfInProgressThings :: Core.Maybe Core.Int,
    -- | The number of things that are awaiting execution of the job.
    numberOfQueuedThings :: Core.Maybe Core.Int,
    -- | The number of things that rejected the job.
    numberOfRejectedThings :: Core.Maybe Core.Int,
    -- | The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
    numberOfRemovedThings :: Core.Maybe Core.Int,
    -- | The number of things which successfully completed the job.
    numberOfSucceededThings :: Core.Maybe Core.Int,
    -- | The number of things whose job execution status is @TIMED_OUT@ .
    numberOfTimedOutThings :: Core.Maybe Core.Int,
    -- | The target devices to which the job execution is being rolled out. This value will be null after the job execution has finished rolling out to all the target devices.
    processingTargets :: Core.Maybe [Types.ProcessingTargetName]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'JobProcessDetails' value with any optional fields omitted.
mkJobProcessDetails ::
  JobProcessDetails
mkJobProcessDetails =
  JobProcessDetails'
    { numberOfCanceledThings = Core.Nothing,
      numberOfFailedThings = Core.Nothing,
      numberOfInProgressThings = Core.Nothing,
      numberOfQueuedThings = Core.Nothing,
      numberOfRejectedThings = Core.Nothing,
      numberOfRemovedThings = Core.Nothing,
      numberOfSucceededThings = Core.Nothing,
      numberOfTimedOutThings = Core.Nothing,
      processingTargets = Core.Nothing
    }

-- | The number of things that cancelled the job.
--
-- /Note:/ Consider using 'numberOfCanceledThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfCanceledThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfCanceledThings = Lens.field @"numberOfCanceledThings"
{-# DEPRECATED jpdNumberOfCanceledThings "Use generic-lens or generic-optics with 'numberOfCanceledThings' instead." #-}

-- | The number of things that failed executing the job.
--
-- /Note:/ Consider using 'numberOfFailedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfFailedThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfFailedThings = Lens.field @"numberOfFailedThings"
{-# DEPRECATED jpdNumberOfFailedThings "Use generic-lens or generic-optics with 'numberOfFailedThings' instead." #-}

-- | The number of things currently executing the job.
--
-- /Note:/ Consider using 'numberOfInProgressThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfInProgressThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfInProgressThings = Lens.field @"numberOfInProgressThings"
{-# DEPRECATED jpdNumberOfInProgressThings "Use generic-lens or generic-optics with 'numberOfInProgressThings' instead." #-}

-- | The number of things that are awaiting execution of the job.
--
-- /Note:/ Consider using 'numberOfQueuedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfQueuedThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfQueuedThings = Lens.field @"numberOfQueuedThings"
{-# DEPRECATED jpdNumberOfQueuedThings "Use generic-lens or generic-optics with 'numberOfQueuedThings' instead." #-}

-- | The number of things that rejected the job.
--
-- /Note:/ Consider using 'numberOfRejectedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfRejectedThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfRejectedThings = Lens.field @"numberOfRejectedThings"
{-# DEPRECATED jpdNumberOfRejectedThings "Use generic-lens or generic-optics with 'numberOfRejectedThings' instead." #-}

-- | The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
--
-- /Note:/ Consider using 'numberOfRemovedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfRemovedThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfRemovedThings = Lens.field @"numberOfRemovedThings"
{-# DEPRECATED jpdNumberOfRemovedThings "Use generic-lens or generic-optics with 'numberOfRemovedThings' instead." #-}

-- | The number of things which successfully completed the job.
--
-- /Note:/ Consider using 'numberOfSucceededThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfSucceededThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfSucceededThings = Lens.field @"numberOfSucceededThings"
{-# DEPRECATED jpdNumberOfSucceededThings "Use generic-lens or generic-optics with 'numberOfSucceededThings' instead." #-}

-- | The number of things whose job execution status is @TIMED_OUT@ .
--
-- /Note:/ Consider using 'numberOfTimedOutThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfTimedOutThings :: Lens.Lens' JobProcessDetails (Core.Maybe Core.Int)
jpdNumberOfTimedOutThings = Lens.field @"numberOfTimedOutThings"
{-# DEPRECATED jpdNumberOfTimedOutThings "Use generic-lens or generic-optics with 'numberOfTimedOutThings' instead." #-}

-- | The target devices to which the job execution is being rolled out. This value will be null after the job execution has finished rolling out to all the target devices.
--
-- /Note:/ Consider using 'processingTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdProcessingTargets :: Lens.Lens' JobProcessDetails (Core.Maybe [Types.ProcessingTargetName])
jpdProcessingTargets = Lens.field @"processingTargets"
{-# DEPRECATED jpdProcessingTargets "Use generic-lens or generic-optics with 'processingTargets' instead." #-}

instance Core.FromJSON JobProcessDetails where
  parseJSON =
    Core.withObject "JobProcessDetails" Core.$
      \x ->
        JobProcessDetails'
          Core.<$> (x Core..:? "numberOfCanceledThings")
          Core.<*> (x Core..:? "numberOfFailedThings")
          Core.<*> (x Core..:? "numberOfInProgressThings")
          Core.<*> (x Core..:? "numberOfQueuedThings")
          Core.<*> (x Core..:? "numberOfRejectedThings")
          Core.<*> (x Core..:? "numberOfRemovedThings")
          Core.<*> (x Core..:? "numberOfSucceededThings")
          Core.<*> (x Core..:? "numberOfTimedOutThings")
          Core.<*> (x Core..:? "processingTargets")
