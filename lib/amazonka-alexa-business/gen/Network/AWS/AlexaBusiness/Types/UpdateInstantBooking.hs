{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
  ( UpdateInstantBooking (..)
  -- * Smart constructor
  , mkUpdateInstantBooking
  -- * Lenses
  , uibDurationInMinutes
  , uibEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Updates settings for the instant booking feature that are applied to a room profile. If instant booking is enabled, Alexa automatically reserves a room if it is free when a user joins a meeting with Alexa.
--
-- /See:/ 'mkUpdateInstantBooking' smart constructor.
data UpdateInstantBooking = UpdateInstantBooking'
  { durationInMinutes :: Core.Maybe Core.Int
    -- ^ Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
  , enabled :: Core.Maybe Core.Bool
    -- ^ Whether instant booking is enabled or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateInstantBooking' value with any optional fields omitted.
mkUpdateInstantBooking
    :: UpdateInstantBooking
mkUpdateInstantBooking
  = UpdateInstantBooking'{durationInMinutes = Core.Nothing,
                          enabled = Core.Nothing}

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uibDurationInMinutes :: Lens.Lens' UpdateInstantBooking (Core.Maybe Core.Int)
uibDurationInMinutes = Lens.field @"durationInMinutes"
{-# INLINEABLE uibDurationInMinutes #-}
{-# DEPRECATED durationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead"  #-}

-- | Whether instant booking is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uibEnabled :: Lens.Lens' UpdateInstantBooking (Core.Maybe Core.Bool)
uibEnabled = Lens.field @"enabled"
{-# INLINEABLE uibEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON UpdateInstantBooking where
        toJSON UpdateInstantBooking{..}
          = Core.object
              (Core.catMaybes
                 [("DurationInMinutes" Core..=) Core.<$> durationInMinutes,
                  ("Enabled" Core..=) Core.<$> enabled])
