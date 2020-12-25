{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.HoldoutActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.HoldoutActivity
  ( HoldoutActivity (..),

    -- * Smart constructor
    mkHoldoutActivity,

    -- * Lenses
    haPercentage,
    haNextActivity,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a holdout activity in a journey. This type of activity stops a journey for a specified percentage of participants.
--
-- /See:/ 'mkHoldoutActivity' smart constructor.
data HoldoutActivity = HoldoutActivity'
  { -- | The percentage of participants who shouldn't continue the journey.
    --
    -- To determine which participants are held out, Amazon Pinpoint applies a probability-based algorithm to the percentage that you specify. Therefore, the actual percentage of participants who are held out may not be equal to the percentage that you specify.
    percentage :: Core.Int,
    -- | The unique identifier for the next activity to perform, after performing the holdout activity.
    nextActivity :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HoldoutActivity' value with any optional fields omitted.
mkHoldoutActivity ::
  -- | 'percentage'
  Core.Int ->
  HoldoutActivity
mkHoldoutActivity percentage =
  HoldoutActivity' {percentage, nextActivity = Core.Nothing}

-- | The percentage of participants who shouldn't continue the journey.
--
-- To determine which participants are held out, Amazon Pinpoint applies a probability-based algorithm to the percentage that you specify. Therefore, the actual percentage of participants who are held out may not be equal to the percentage that you specify.
--
-- /Note:/ Consider using 'percentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haPercentage :: Lens.Lens' HoldoutActivity Core.Int
haPercentage = Lens.field @"percentage"
{-# DEPRECATED haPercentage "Use generic-lens or generic-optics with 'percentage' instead." #-}

-- | The unique identifier for the next activity to perform, after performing the holdout activity.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haNextActivity :: Lens.Lens' HoldoutActivity (Core.Maybe Core.Text)
haNextActivity = Lens.field @"nextActivity"
{-# DEPRECATED haNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

instance Core.FromJSON HoldoutActivity where
  toJSON HoldoutActivity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Percentage" Core..= percentage),
            ("NextActivity" Core..=) Core.<$> nextActivity
          ]
      )

instance Core.FromJSON HoldoutActivity where
  parseJSON =
    Core.withObject "HoldoutActivity" Core.$
      \x ->
        HoldoutActivity'
          Core.<$> (x Core..: "Percentage") Core.<*> (x Core..:? "NextActivity")
