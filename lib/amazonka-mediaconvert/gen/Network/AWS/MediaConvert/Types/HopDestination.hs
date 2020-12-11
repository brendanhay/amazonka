-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.HopDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.HopDestination
  ( HopDestination (..),

    -- * Smart constructor
    mkHopDestination,

    -- * Lenses
    hdPriority,
    hdQueue,
    hdWaitMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Optional. Configuration for a destination queue to which the job can hop once a customer-defined minimum wait time has passed.
--
-- /See:/ 'mkHopDestination' smart constructor.
data HopDestination = HopDestination'
  { priority ::
      Lude.Maybe Lude.Int,
    queue :: Lude.Maybe Lude.Text,
    waitMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HopDestination' with the minimum fields required to make a request.
--
-- * 'priority' - Optional. When you set up a job to use queue hopping, you can specify a different relative priority for the job in the destination queue. If you don't specify, the relative priority will remain the same as in the previous queue.
-- * 'queue' - Optional unless the job is submitted on the default queue. When you set up a job to use queue hopping, you can specify a destination queue. This queue cannot be the original queue to which the job is submitted. If the original queue isn't the default queue and you don't specify the destination queue, the job will move to the default queue.
-- * 'waitMinutes' - Required for setting up a job to use queue hopping. Minimum wait time in minutes until the job can hop to the destination queue. Valid range is 1 to 1440 minutes, inclusive.
mkHopDestination ::
  HopDestination
mkHopDestination =
  HopDestination'
    { priority = Lude.Nothing,
      queue = Lude.Nothing,
      waitMinutes = Lude.Nothing
    }

-- | Optional. When you set up a job to use queue hopping, you can specify a different relative priority for the job in the destination queue. If you don't specify, the relative priority will remain the same as in the previous queue.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdPriority :: Lens.Lens' HopDestination (Lude.Maybe Lude.Int)
hdPriority = Lens.lens (priority :: HopDestination -> Lude.Maybe Lude.Int) (\s a -> s {priority = a} :: HopDestination)
{-# DEPRECATED hdPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Optional unless the job is submitted on the default queue. When you set up a job to use queue hopping, you can specify a destination queue. This queue cannot be the original queue to which the job is submitted. If the original queue isn't the default queue and you don't specify the destination queue, the job will move to the default queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdQueue :: Lens.Lens' HopDestination (Lude.Maybe Lude.Text)
hdQueue = Lens.lens (queue :: HopDestination -> Lude.Maybe Lude.Text) (\s a -> s {queue = a} :: HopDestination)
{-# DEPRECATED hdQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Required for setting up a job to use queue hopping. Minimum wait time in minutes until the job can hop to the destination queue. Valid range is 1 to 1440 minutes, inclusive.
--
-- /Note:/ Consider using 'waitMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdWaitMinutes :: Lens.Lens' HopDestination (Lude.Maybe Lude.Int)
hdWaitMinutes = Lens.lens (waitMinutes :: HopDestination -> Lude.Maybe Lude.Int) (\s a -> s {waitMinutes = a} :: HopDestination)
{-# DEPRECATED hdWaitMinutes "Use generic-lens or generic-optics with 'waitMinutes' instead." #-}

instance Lude.FromJSON HopDestination where
  parseJSON =
    Lude.withObject
      "HopDestination"
      ( \x ->
          HopDestination'
            Lude.<$> (x Lude..:? "priority")
            Lude.<*> (x Lude..:? "queue")
            Lude.<*> (x Lude..:? "waitMinutes")
      )

instance Lude.ToJSON HopDestination where
  toJSON HopDestination' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("priority" Lude..=) Lude.<$> priority,
            ("queue" Lude..=) Lude.<$> queue,
            ("waitMinutes" Lude..=) Lude.<$> waitMinutes
          ]
      )
