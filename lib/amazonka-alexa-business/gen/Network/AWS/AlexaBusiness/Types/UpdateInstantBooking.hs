{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
  ( UpdateInstantBooking (..),

    -- * Smart constructor
    mkUpdateInstantBooking,

    -- * Lenses
    uibEnabled,
    uibDurationInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Updates settings for the instant booking feature that are applied to a room profile. If instant booking is enabled, Alexa automatically reserves a room if it is free when a user joins a meeting with Alexa.
--
-- /See:/ 'mkUpdateInstantBooking' smart constructor.
data UpdateInstantBooking = UpdateInstantBooking'
  { -- | Whether instant booking is enabled or not.
    enabled :: Lude.Maybe Lude.Bool,
    -- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateInstantBooking' with the minimum fields required to make a request.
--
-- * 'enabled' - Whether instant booking is enabled or not.
-- * 'durationInMinutes' - Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
mkUpdateInstantBooking ::
  UpdateInstantBooking
mkUpdateInstantBooking =
  UpdateInstantBooking'
    { enabled = Lude.Nothing,
      durationInMinutes = Lude.Nothing
    }

-- | Whether instant booking is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uibEnabled :: Lens.Lens' UpdateInstantBooking (Lude.Maybe Lude.Bool)
uibEnabled = Lens.lens (enabled :: UpdateInstantBooking -> Lude.Maybe Lude.Bool) (\s a -> s {enabled = a} :: UpdateInstantBooking)
{-# DEPRECATED uibEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uibDurationInMinutes :: Lens.Lens' UpdateInstantBooking (Lude.Maybe Lude.Int)
uibDurationInMinutes = Lens.lens (durationInMinutes :: UpdateInstantBooking -> Lude.Maybe Lude.Int) (\s a -> s {durationInMinutes = a} :: UpdateInstantBooking)
{-# DEPRECATED uibDurationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead." #-}

instance Lude.ToJSON UpdateInstantBooking where
  toJSON UpdateInstantBooking' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Enabled" Lude..=) Lude.<$> enabled,
            ("DurationInMinutes" Lude..=) Lude.<$> durationInMinutes
          ]
      )
