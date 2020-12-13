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
    jpdNumberOfRemovedThings,
    jpdNumberOfQueuedThings,
    jpdNumberOfFailedThings,
    jpdNumberOfSucceededThings,
    jpdNumberOfInProgressThings,
    jpdNumberOfCanceledThings,
    jpdNumberOfTimedOutThings,
    jpdNumberOfRejectedThings,
    jpdProcessingTargets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The job process details.
--
-- /See:/ 'mkJobProcessDetails' smart constructor.
data JobProcessDetails = JobProcessDetails'
  { -- | The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
    numberOfRemovedThings :: Lude.Maybe Lude.Int,
    -- | The number of things that are awaiting execution of the job.
    numberOfQueuedThings :: Lude.Maybe Lude.Int,
    -- | The number of things that failed executing the job.
    numberOfFailedThings :: Lude.Maybe Lude.Int,
    -- | The number of things which successfully completed the job.
    numberOfSucceededThings :: Lude.Maybe Lude.Int,
    -- | The number of things currently executing the job.
    numberOfInProgressThings :: Lude.Maybe Lude.Int,
    -- | The number of things that cancelled the job.
    numberOfCanceledThings :: Lude.Maybe Lude.Int,
    -- | The number of things whose job execution status is @TIMED_OUT@ .
    numberOfTimedOutThings :: Lude.Maybe Lude.Int,
    -- | The number of things that rejected the job.
    numberOfRejectedThings :: Lude.Maybe Lude.Int,
    -- | The target devices to which the job execution is being rolled out. This value will be null after the job execution has finished rolling out to all the target devices.
    processingTargets :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'JobProcessDetails' with the minimum fields required to make a request.
--
-- * 'numberOfRemovedThings' - The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
-- * 'numberOfQueuedThings' - The number of things that are awaiting execution of the job.
-- * 'numberOfFailedThings' - The number of things that failed executing the job.
-- * 'numberOfSucceededThings' - The number of things which successfully completed the job.
-- * 'numberOfInProgressThings' - The number of things currently executing the job.
-- * 'numberOfCanceledThings' - The number of things that cancelled the job.
-- * 'numberOfTimedOutThings' - The number of things whose job execution status is @TIMED_OUT@ .
-- * 'numberOfRejectedThings' - The number of things that rejected the job.
-- * 'processingTargets' - The target devices to which the job execution is being rolled out. This value will be null after the job execution has finished rolling out to all the target devices.
mkJobProcessDetails ::
  JobProcessDetails
mkJobProcessDetails =
  JobProcessDetails'
    { numberOfRemovedThings = Lude.Nothing,
      numberOfQueuedThings = Lude.Nothing,
      numberOfFailedThings = Lude.Nothing,
      numberOfSucceededThings = Lude.Nothing,
      numberOfInProgressThings = Lude.Nothing,
      numberOfCanceledThings = Lude.Nothing,
      numberOfTimedOutThings = Lude.Nothing,
      numberOfRejectedThings = Lude.Nothing,
      processingTargets = Lude.Nothing
    }

-- | The number of things that are no longer scheduled to execute the job because they have been deleted or have been removed from the group that was a target of the job.
--
-- /Note:/ Consider using 'numberOfRemovedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfRemovedThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfRemovedThings = Lens.lens (numberOfRemovedThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfRemovedThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfRemovedThings "Use generic-lens or generic-optics with 'numberOfRemovedThings' instead." #-}

-- | The number of things that are awaiting execution of the job.
--
-- /Note:/ Consider using 'numberOfQueuedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfQueuedThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfQueuedThings = Lens.lens (numberOfQueuedThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfQueuedThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfQueuedThings "Use generic-lens or generic-optics with 'numberOfQueuedThings' instead." #-}

-- | The number of things that failed executing the job.
--
-- /Note:/ Consider using 'numberOfFailedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfFailedThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfFailedThings = Lens.lens (numberOfFailedThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfFailedThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfFailedThings "Use generic-lens or generic-optics with 'numberOfFailedThings' instead." #-}

-- | The number of things which successfully completed the job.
--
-- /Note:/ Consider using 'numberOfSucceededThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfSucceededThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfSucceededThings = Lens.lens (numberOfSucceededThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfSucceededThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfSucceededThings "Use generic-lens or generic-optics with 'numberOfSucceededThings' instead." #-}

-- | The number of things currently executing the job.
--
-- /Note:/ Consider using 'numberOfInProgressThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfInProgressThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfInProgressThings = Lens.lens (numberOfInProgressThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfInProgressThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfInProgressThings "Use generic-lens or generic-optics with 'numberOfInProgressThings' instead." #-}

-- | The number of things that cancelled the job.
--
-- /Note:/ Consider using 'numberOfCanceledThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfCanceledThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfCanceledThings = Lens.lens (numberOfCanceledThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfCanceledThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfCanceledThings "Use generic-lens or generic-optics with 'numberOfCanceledThings' instead." #-}

-- | The number of things whose job execution status is @TIMED_OUT@ .
--
-- /Note:/ Consider using 'numberOfTimedOutThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfTimedOutThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfTimedOutThings = Lens.lens (numberOfTimedOutThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTimedOutThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfTimedOutThings "Use generic-lens or generic-optics with 'numberOfTimedOutThings' instead." #-}

-- | The number of things that rejected the job.
--
-- /Note:/ Consider using 'numberOfRejectedThings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdNumberOfRejectedThings :: Lens.Lens' JobProcessDetails (Lude.Maybe Lude.Int)
jpdNumberOfRejectedThings = Lens.lens (numberOfRejectedThings :: JobProcessDetails -> Lude.Maybe Lude.Int) (\s a -> s {numberOfRejectedThings = a} :: JobProcessDetails)
{-# DEPRECATED jpdNumberOfRejectedThings "Use generic-lens or generic-optics with 'numberOfRejectedThings' instead." #-}

-- | The target devices to which the job execution is being rolled out. This value will be null after the job execution has finished rolling out to all the target devices.
--
-- /Note:/ Consider using 'processingTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
jpdProcessingTargets :: Lens.Lens' JobProcessDetails (Lude.Maybe [Lude.Text])
jpdProcessingTargets = Lens.lens (processingTargets :: JobProcessDetails -> Lude.Maybe [Lude.Text]) (\s a -> s {processingTargets = a} :: JobProcessDetails)
{-# DEPRECATED jpdProcessingTargets "Use generic-lens or generic-optics with 'processingTargets' instead." #-}

instance Lude.FromJSON JobProcessDetails where
  parseJSON =
    Lude.withObject
      "JobProcessDetails"
      ( \x ->
          JobProcessDetails'
            Lude.<$> (x Lude..:? "numberOfRemovedThings")
            Lude.<*> (x Lude..:? "numberOfQueuedThings")
            Lude.<*> (x Lude..:? "numberOfFailedThings")
            Lude.<*> (x Lude..:? "numberOfSucceededThings")
            Lude.<*> (x Lude..:? "numberOfInProgressThings")
            Lude.<*> (x Lude..:? "numberOfCanceledThings")
            Lude.<*> (x Lude..:? "numberOfTimedOutThings")
            Lude.<*> (x Lude..:? "numberOfRejectedThings")
            Lude.<*> (x Lude..:? "processingTargets" Lude..!= Lude.mempty)
      )
