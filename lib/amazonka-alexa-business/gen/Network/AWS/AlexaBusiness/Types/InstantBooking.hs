{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.InstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.InstantBooking
  ( InstantBooking (..),

    -- * Smart constructor
    mkInstantBooking,

    -- * Lenses
    ibEnabled,
    ibDurationInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Settings for the instant booking feature that are applied to a room profile. When users start their meeting with Alexa, Alexa automatically books the room for the configured duration if the room is available.
--
-- /See:/ 'mkInstantBooking' smart constructor.
data InstantBooking = InstantBooking'
  { enabled ::
      Lude.Maybe Lude.Bool,
    durationInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstantBooking' with the minimum fields required to make a request.
--
-- * 'durationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
-- * 'enabled' - Whether instant booking is enabled or not.
mkInstantBooking ::
  InstantBooking
mkInstantBooking =
  InstantBooking'
    { enabled = Lude.Nothing,
      durationInMinutes = Lude.Nothing
    }

-- | Whether instant booking is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibEnabled :: Lens.Lens' InstantBooking (Lude.Maybe Lude.Bool)
ibEnabled = Lens.lens (enabled :: InstantBooking -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: InstantBooking)
{-# DEPRECATED ibEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDurationInMinutes :: Lens.Lens' InstantBooking (Lude.Maybe Lude.Int)
ibDurationInMinutes = Lens.lens (durationInMinutes :: InstantBooking -> Lude.Maybe Lude.Int) (\s a -> s {durationInMinutes = a} :: InstantBooking)
{-# DEPRECATED ibDurationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead." #-}

instance Lude.FromJSON InstantBooking where
  parseJSON =
    Lude.withObject
      "InstantBooking"
      ( \x ->
          InstantBooking'
            Lude.<$> (x Lude..:? "Enabled") Lude.<*> (x Lude..:? "DurationInMinutes")
      )
