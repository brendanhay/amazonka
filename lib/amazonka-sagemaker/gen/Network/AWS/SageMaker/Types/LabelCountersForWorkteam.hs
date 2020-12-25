{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelCountersForWorkteam
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelCountersForWorkteam
  ( LabelCountersForWorkteam (..),

    -- * Smart constructor
    mkLabelCountersForWorkteam,

    -- * Lenses
    lcfwHumanLabeled,
    lcfwPendingHuman,
    lcfwTotal,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides counts for human-labeled tasks in the labeling job.
--
-- /See:/ 'mkLabelCountersForWorkteam' smart constructor.
data LabelCountersForWorkteam = LabelCountersForWorkteam'
  { -- | The total number of data objects labeled by a human worker.
    humanLabeled :: Core.Maybe Core.Natural,
    -- | The total number of data objects that need to be labeled by a human worker.
    pendingHuman :: Core.Maybe Core.Natural,
    -- | The total number of tasks in the labeling job.
    total :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelCountersForWorkteam' value with any optional fields omitted.
mkLabelCountersForWorkteam ::
  LabelCountersForWorkteam
mkLabelCountersForWorkteam =
  LabelCountersForWorkteam'
    { humanLabeled = Core.Nothing,
      pendingHuman = Core.Nothing,
      total = Core.Nothing
    }

-- | The total number of data objects labeled by a human worker.
--
-- /Note:/ Consider using 'humanLabeled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfwHumanLabeled :: Lens.Lens' LabelCountersForWorkteam (Core.Maybe Core.Natural)
lcfwHumanLabeled = Lens.field @"humanLabeled"
{-# DEPRECATED lcfwHumanLabeled "Use generic-lens or generic-optics with 'humanLabeled' instead." #-}

-- | The total number of data objects that need to be labeled by a human worker.
--
-- /Note:/ Consider using 'pendingHuman' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfwPendingHuman :: Lens.Lens' LabelCountersForWorkteam (Core.Maybe Core.Natural)
lcfwPendingHuman = Lens.field @"pendingHuman"
{-# DEPRECATED lcfwPendingHuman "Use generic-lens or generic-optics with 'pendingHuman' instead." #-}

-- | The total number of tasks in the labeling job.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcfwTotal :: Lens.Lens' LabelCountersForWorkteam (Core.Maybe Core.Natural)
lcfwTotal = Lens.field @"total"
{-# DEPRECATED lcfwTotal "Use generic-lens or generic-optics with 'total' instead." #-}

instance Core.FromJSON LabelCountersForWorkteam where
  parseJSON =
    Core.withObject "LabelCountersForWorkteam" Core.$
      \x ->
        LabelCountersForWorkteam'
          Core.<$> (x Core..:? "HumanLabeled")
          Core.<*> (x Core..:? "PendingHuman")
          Core.<*> (x Core..:? "Total")
