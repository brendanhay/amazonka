{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.RandomSplitEntry
  ( RandomSplitEntry (..)
  -- * Smart constructor
  , mkRandomSplitEntry
  -- * Lenses
  , rseNextActivity
  , rsePercentage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the settings for a path in a random split activity in a journey.
--
-- /See:/ 'mkRandomSplitEntry' smart constructor.
data RandomSplitEntry = RandomSplitEntry'
  { nextActivity :: Core.Maybe Core.Text
    -- ^ The unique identifier for the next activity to perform, after completing the activity for the path.
  , percentage :: Core.Maybe Core.Int
    -- ^ The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint applies a probability-based algorithm to the percentages that you specify for the paths. Therefore, the actual percentage of participants who are sent down a path may not be equal to the percentage that you specify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RandomSplitEntry' value with any optional fields omitted.
mkRandomSplitEntry
    :: RandomSplitEntry
mkRandomSplitEntry
  = RandomSplitEntry'{nextActivity = Core.Nothing,
                      percentage = Core.Nothing}

-- | The unique identifier for the next activity to perform, after completing the activity for the path.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseNextActivity :: Lens.Lens' RandomSplitEntry (Core.Maybe Core.Text)
rseNextActivity = Lens.field @"nextActivity"
{-# INLINEABLE rseNextActivity #-}
{-# DEPRECATED nextActivity "Use generic-lens or generic-optics with 'nextActivity' instead"  #-}

-- | The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint applies a probability-based algorithm to the percentages that you specify for the paths. Therefore, the actual percentage of participants who are sent down a path may not be equal to the percentage that you specify.
--
-- /Note:/ Consider using 'percentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePercentage :: Lens.Lens' RandomSplitEntry (Core.Maybe Core.Int)
rsePercentage = Lens.field @"percentage"
{-# INLINEABLE rsePercentage #-}
{-# DEPRECATED percentage "Use generic-lens or generic-optics with 'percentage' instead"  #-}

instance Core.FromJSON RandomSplitEntry where
        toJSON RandomSplitEntry{..}
          = Core.object
              (Core.catMaybes
                 [("NextActivity" Core..=) Core.<$> nextActivity,
                  ("Percentage" Core..=) Core.<$> percentage])

instance Core.FromJSON RandomSplitEntry where
        parseJSON
          = Core.withObject "RandomSplitEntry" Core.$
              \ x ->
                RandomSplitEntry' Core.<$>
                  (x Core..:? "NextActivity") Core.<*> x Core..:? "Percentage"
