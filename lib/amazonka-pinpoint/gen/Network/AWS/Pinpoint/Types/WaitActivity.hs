{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WaitActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WaitActivity
  ( WaitActivity (..),

    -- * Smart constructor
    mkWaitActivity,

    -- * Lenses
    waNextActivity,
    waWaitTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.WaitTime as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a wait activity in a journey. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
-- /See:/ 'mkWaitActivity' smart constructor.
data WaitActivity = WaitActivity'
  { -- | The unique identifier for the next activity to perform, after performing the wait activity.
    nextActivity :: Core.Maybe Core.Text,
    -- | The amount of time to wait or the date and time when the activity moves participants to the next activity in the journey.
    waitTime :: Core.Maybe Types.WaitTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WaitActivity' value with any optional fields omitted.
mkWaitActivity ::
  WaitActivity
mkWaitActivity =
  WaitActivity'
    { nextActivity = Core.Nothing,
      waitTime = Core.Nothing
    }

-- | The unique identifier for the next activity to perform, after performing the wait activity.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waNextActivity :: Lens.Lens' WaitActivity (Core.Maybe Core.Text)
waNextActivity = Lens.field @"nextActivity"
{-# DEPRECATED waNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The amount of time to wait or the date and time when the activity moves participants to the next activity in the journey.
--
-- /Note:/ Consider using 'waitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waWaitTime :: Lens.Lens' WaitActivity (Core.Maybe Types.WaitTime)
waWaitTime = Lens.field @"waitTime"
{-# DEPRECATED waWaitTime "Use generic-lens or generic-optics with 'waitTime' instead." #-}

instance Core.FromJSON WaitActivity where
  toJSON WaitActivity {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextActivity" Core..=) Core.<$> nextActivity,
            ("WaitTime" Core..=) Core.<$> waitTime
          ]
      )

instance Core.FromJSON WaitActivity where
  parseJSON =
    Core.withObject "WaitActivity" Core.$
      \x ->
        WaitActivity'
          Core.<$> (x Core..:? "NextActivity") Core.<*> (x Core..:? "WaitTime")
