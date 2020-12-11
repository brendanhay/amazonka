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
    haNextActivity,
    haPercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a holdout activity in a journey. This type of activity stops a journey for a specified percentage of participants.
--
-- /See:/ 'mkHoldoutActivity' smart constructor.
data HoldoutActivity = HoldoutActivity'
  { nextActivity ::
      Lude.Maybe Lude.Text,
    percentage :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HoldoutActivity' with the minimum fields required to make a request.
--
-- * 'nextActivity' - The unique identifier for the next activity to perform, after performing the holdout activity.
-- * 'percentage' - The percentage of participants who shouldn't continue the journey.
--
-- To determine which participants are held out, Amazon Pinpoint applies a probability-based algorithm to the percentage that you specify. Therefore, the actual percentage of participants who are held out may not be equal to the percentage that you specify.
mkHoldoutActivity ::
  -- | 'percentage'
  Lude.Int ->
  HoldoutActivity
mkHoldoutActivity pPercentage_ =
  HoldoutActivity'
    { nextActivity = Lude.Nothing,
      percentage = pPercentage_
    }

-- | The unique identifier for the next activity to perform, after performing the holdout activity.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haNextActivity :: Lens.Lens' HoldoutActivity (Lude.Maybe Lude.Text)
haNextActivity = Lens.lens (nextActivity :: HoldoutActivity -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: HoldoutActivity)
{-# DEPRECATED haNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The percentage of participants who shouldn't continue the journey.
--
-- To determine which participants are held out, Amazon Pinpoint applies a probability-based algorithm to the percentage that you specify. Therefore, the actual percentage of participants who are held out may not be equal to the percentage that you specify.
--
-- /Note:/ Consider using 'percentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
haPercentage :: Lens.Lens' HoldoutActivity Lude.Int
haPercentage = Lens.lens (percentage :: HoldoutActivity -> Lude.Int) (\s a -> s {percentage = a} :: HoldoutActivity)
{-# DEPRECATED haPercentage "Use generic-lens or generic-optics with 'percentage' instead." #-}

instance Lude.FromJSON HoldoutActivity where
  parseJSON =
    Lude.withObject
      "HoldoutActivity"
      ( \x ->
          HoldoutActivity'
            Lude.<$> (x Lude..:? "NextActivity") Lude.<*> (x Lude..: "Percentage")
      )

instance Lude.ToJSON HoldoutActivity where
  toJSON HoldoutActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextActivity" Lude..=) Lude.<$> nextActivity,
            Lude.Just ("Percentage" Lude..= percentage)
          ]
      )
