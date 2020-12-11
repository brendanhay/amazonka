-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.WaitTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.WaitTime
  ( WaitTime (..),

    -- * Smart constructor
    mkWaitTime,

    -- * Lenses
    wtWaitFor,
    wtWaitUntil,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies a duration or a date and time that indicates when Amazon Pinpoint determines whether an activity's conditions have been met or an activity moves participants to the next activity in a journey.
--
-- /See:/ 'mkWaitTime' smart constructor.
data WaitTime = WaitTime'
  { waitFor :: Lude.Maybe Lude.Text,
    waitUntil :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WaitTime' with the minimum fields required to make a request.
--
-- * 'waitFor' - The amount of time to wait, as a duration in ISO 8601 format, before determining whether the activity's conditions have been met or moving participants to the next activity in the journey.
-- * 'waitUntil' - The date and time, in ISO 8601 format, when Amazon Pinpoint determines whether the activity's conditions have been met or the activity moves participants to the next activity in the journey.
mkWaitTime ::
  WaitTime
mkWaitTime =
  WaitTime' {waitFor = Lude.Nothing, waitUntil = Lude.Nothing}

-- | The amount of time to wait, as a duration in ISO 8601 format, before determining whether the activity's conditions have been met or moving participants to the next activity in the journey.
--
-- /Note:/ Consider using 'waitFor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtWaitFor :: Lens.Lens' WaitTime (Lude.Maybe Lude.Text)
wtWaitFor = Lens.lens (waitFor :: WaitTime -> Lude.Maybe Lude.Text) (\s a -> s {waitFor = a} :: WaitTime)
{-# DEPRECATED wtWaitFor "Use generic-lens or generic-optics with 'waitFor' instead." #-}

-- | The date and time, in ISO 8601 format, when Amazon Pinpoint determines whether the activity's conditions have been met or the activity moves participants to the next activity in the journey.
--
-- /Note:/ Consider using 'waitUntil' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wtWaitUntil :: Lens.Lens' WaitTime (Lude.Maybe Lude.Text)
wtWaitUntil = Lens.lens (waitUntil :: WaitTime -> Lude.Maybe Lude.Text) (\s a -> s {waitUntil = a} :: WaitTime)
{-# DEPRECATED wtWaitUntil "Use generic-lens or generic-optics with 'waitUntil' instead." #-}

instance Lude.FromJSON WaitTime where
  parseJSON =
    Lude.withObject
      "WaitTime"
      ( \x ->
          WaitTime'
            Lude.<$> (x Lude..:? "WaitFor") Lude.<*> (x Lude..:? "WaitUntil")
      )

instance Lude.ToJSON WaitTime where
  toJSON WaitTime' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("WaitFor" Lude..=) Lude.<$> waitFor,
            ("WaitUntil" Lude..=) Lude.<$> waitUntil
          ]
      )
