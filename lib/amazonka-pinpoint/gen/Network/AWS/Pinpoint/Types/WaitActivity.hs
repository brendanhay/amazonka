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
import Network.AWS.Pinpoint.Types.WaitTime
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a wait activity in a journey. This type of activity waits for a certain amount of time or until a specific date and time before moving participants to the next activity in a journey.
--
-- /See:/ 'mkWaitActivity' smart constructor.
data WaitActivity = WaitActivity'
  { nextActivity ::
      Lude.Maybe Lude.Text,
    waitTime :: Lude.Maybe WaitTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WaitActivity' with the minimum fields required to make a request.
--
-- * 'nextActivity' - The unique identifier for the next activity to perform, after performing the wait activity.
-- * 'waitTime' - The amount of time to wait or the date and time when the activity moves participants to the next activity in the journey.
mkWaitActivity ::
  WaitActivity
mkWaitActivity =
  WaitActivity'
    { nextActivity = Lude.Nothing,
      waitTime = Lude.Nothing
    }

-- | The unique identifier for the next activity to perform, after performing the wait activity.
--
-- /Note:/ Consider using 'nextActivity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waNextActivity :: Lens.Lens' WaitActivity (Lude.Maybe Lude.Text)
waNextActivity = Lens.lens (nextActivity :: WaitActivity -> Lude.Maybe Lude.Text) (\s a -> s {nextActivity = a} :: WaitActivity)
{-# DEPRECATED waNextActivity "Use generic-lens or generic-optics with 'nextActivity' instead." #-}

-- | The amount of time to wait or the date and time when the activity moves participants to the next activity in the journey.
--
-- /Note:/ Consider using 'waitTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
waWaitTime :: Lens.Lens' WaitActivity (Lude.Maybe WaitTime)
waWaitTime = Lens.lens (waitTime :: WaitActivity -> Lude.Maybe WaitTime) (\s a -> s {waitTime = a} :: WaitActivity)
{-# DEPRECATED waWaitTime "Use generic-lens or generic-optics with 'waitTime' instead." #-}

instance Lude.FromJSON WaitActivity where
  parseJSON =
    Lude.withObject
      "WaitActivity"
      ( \x ->
          WaitActivity'
            Lude.<$> (x Lude..:? "NextActivity") Lude.<*> (x Lude..:? "WaitTime")
      )

instance Lude.ToJSON WaitActivity where
  toJSON WaitActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextActivity" Lude..=) Lude.<$> nextActivity,
            ("WaitTime" Lude..=) Lude.<$> waitTime
          ]
      )
