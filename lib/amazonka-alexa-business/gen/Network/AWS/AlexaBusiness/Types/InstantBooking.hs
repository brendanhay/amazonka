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
    ibDurationInMinutes,
    ibEnabled,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Settings for the instant booking feature that are applied to a room profile. When users start their meeting with Alexa, Alexa automatically books the room for the configured duration if the room is available.
--
-- /See:/ 'mkInstantBooking' smart constructor.
data InstantBooking = InstantBooking'
  { -- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
    durationInMinutes :: Core.Maybe Core.Int,
    -- | Whether instant booking is enabled or not.
    enabled :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstantBooking' value with any optional fields omitted.
mkInstantBooking ::
  InstantBooking
mkInstantBooking =
  InstantBooking'
    { durationInMinutes = Core.Nothing,
      enabled = Core.Nothing
    }

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibDurationInMinutes :: Lens.Lens' InstantBooking (Core.Maybe Core.Int)
ibDurationInMinutes = Lens.field @"durationInMinutes"
{-# DEPRECATED ibDurationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead." #-}

-- | Whether instant booking is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ibEnabled :: Lens.Lens' InstantBooking (Core.Maybe Core.Bool)
ibEnabled = Lens.field @"enabled"
{-# DEPRECATED ibEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

instance Core.FromJSON InstantBooking where
  parseJSON =
    Core.withObject "InstantBooking" Core.$
      \x ->
        InstantBooking'
          Core.<$> (x Core..:? "DurationInMinutes") Core.<*> (x Core..:? "Enabled")
