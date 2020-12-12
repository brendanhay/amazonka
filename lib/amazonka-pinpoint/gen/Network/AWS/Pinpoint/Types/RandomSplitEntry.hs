{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.RandomSplitEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.RandomSplitEntry
  ( RandomSplitEntry (..),

    -- * Smart constructor
    mkRandomSplitEntry,

    -- * Lenses
    rseNextActivity,
    rsePercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a path in a random split activity in a journey.
--
-- /See:/ 'mkRandomSplitEntry' smart constructor.
data RandomSplitEntry = RandomSplitEntry'
  { nextActivity ::
      Lude.Maybe Lude.Text,
    percentage :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RandomSplitEntry' with the minimum fields required to make a request.
--
-- * 'nextActivity' - The unique identifier for the next activity to perform, after completing the activity for the path.
-- * 'percentage' - The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint applies a probability-based algorithm to the percentages that you specify for the paths. Therefore, the actual percentage of participants who are sent down a path may not be equal to the percentage that you specify.
mkRandomSplitEntry ::
  RandomSplitEntry
mkRandomSplitEntry =
  RandomSplitEntry'
    { nextActivity = Lude.Nothing,
      percentage = Lude.Nothing
    }

-- | The unique identifier for the next activity to perform, after completing the activity for the path.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rseNextActivity :: Lens.Lens' RandomSplitEntry (Lude.Maybe Lude.Text)
rseNextActivity = Lens.lens (nextActivity :: RandomSplitEntry -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: RandomSplitEntry)
{-# DEPRECATED rseNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The percentage of participants to send down the activity path.
--
-- To determine which participants are sent down each path, Amazon Pinpoint applies a probability-based algorithm to the percentages that you specify for the paths. Therefore, the actual percentage of participants who are sent down a path may not be equal to the percentage that you specify.
--
-- /Note:/ Consider using 'percentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsePercentage :: Lens.Lens' RandomSplitEntry (Lude.Maybe Lude.Int)
rsePercentage = Lens.lens (percentage :: RandomSplitEntry -> Lude.Maybe Lude.Int) (\s a -> s {percentage = a} :: RandomSplitEntry)
{-# DEPRECATED rsePercentage "Use generic-lens or generic-optics with 'percentage' instead." #-}

instance Lude.FromJSON RandomSplitEntry where
  parseJSON =
    Lude.withObject
      "RandomSplitEntry"
      ( \x ->
          RandomSplitEntry'
            Lude.<$> (x Lude..:? "NextActivity") Lude.<*> (x Lude..:? "Percentage")
      )

instance Lude.ToJSON RandomSplitEntry where
  toJSON RandomSplitEntry' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("Percentage" Lude..=) Lude.<$> percentage
          ]
      )
