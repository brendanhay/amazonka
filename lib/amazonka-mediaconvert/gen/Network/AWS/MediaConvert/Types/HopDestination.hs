{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
import qualified Network.AWS.Prelude as Core

-- | Optional. Configuration for a destination queue to which the job can hop once a customer-defined minimum wait time has passed.
--
-- /See:/ 'mkHopDestination' smart constructor.
data HopDestination = HopDestination'
  { -- | Optional. When you set up a job to use queue hopping, you can specify a different relative priority for the job in the destination queue. If you don't specify, the relative priority will remain the same as in the previous queue.
    priority :: Core.Maybe Core.Int,
    -- | Optional unless the job is submitted on the default queue. When you set up a job to use queue hopping, you can specify a destination queue. This queue cannot be the original queue to which the job is submitted. If the original queue isn't the default queue and you don't specify the destination queue, the job will move to the default queue.
    queue :: Core.Maybe Core.Text,
    -- | Required for setting up a job to use queue hopping. Minimum wait time in minutes until the job can hop to the destination queue. Valid range is 1 to 1440 minutes, inclusive.
    waitMinutes :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HopDestination' value with any optional fields omitted.
mkHopDestination ::
  HopDestination
mkHopDestination =
  HopDestination'
    { priority = Core.Nothing,
      queue = Core.Nothing,
      waitMinutes = Core.Nothing
    }

-- | Optional. When you set up a job to use queue hopping, you can specify a different relative priority for the job in the destination queue. If you don't specify, the relative priority will remain the same as in the previous queue.
--
-- /Note:/ Consider using 'priority' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdPriority :: Lens.Lens' HopDestination (Core.Maybe Core.Int)
hdPriority = Lens.field @"priority"
{-# DEPRECATED hdPriority "Use generic-lens or generic-optics with 'priority' instead." #-}

-- | Optional unless the job is submitted on the default queue. When you set up a job to use queue hopping, you can specify a destination queue. This queue cannot be the original queue to which the job is submitted. If the original queue isn't the default queue and you don't specify the destination queue, the job will move to the default queue.
--
-- /Note:/ Consider using 'queue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdQueue :: Lens.Lens' HopDestination (Core.Maybe Core.Text)
hdQueue = Lens.field @"queue"
{-# DEPRECATED hdQueue "Use generic-lens or generic-optics with 'queue' instead." #-}

-- | Required for setting up a job to use queue hopping. Minimum wait time in minutes until the job can hop to the destination queue. Valid range is 1 to 1440 minutes, inclusive.
--
-- /Note:/ Consider using 'waitMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hdWaitMinutes :: Lens.Lens' HopDestination (Core.Maybe Core.Int)
hdWaitMinutes = Lens.field @"waitMinutes"
{-# DEPRECATED hdWaitMinutes "Use generic-lens or generic-optics with 'waitMinutes' instead." #-}

instance Core.FromJSON HopDestination where
  toJSON HopDestination {..} =
    Core.object
      ( Core.catMaybes
          [ ("priority" Core..=) Core.<$> priority,
            ("queue" Core..=) Core.<$> queue,
            ("waitMinutes" Core..=) Core.<$> waitMinutes
          ]
      )

instance Core.FromJSON HopDestination where
  parseJSON =
    Core.withObject "HopDestination" Core.$
      \x ->
        HopDestination'
          Core.<$> (x Core..:? "priority")
          Core.<*> (x Core..:? "queue")
          Core.<*> (x Core..:? "waitMinutes")
