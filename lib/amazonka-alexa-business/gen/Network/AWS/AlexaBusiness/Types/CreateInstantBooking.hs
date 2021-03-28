{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateInstantBooking
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.CreateInstantBooking
  ( CreateInstantBooking (..)
  -- * Smart constructor
  , mkCreateInstantBooking
  -- * Lenses
  , cibDurationInMinutes
  , cibEnabled
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Creates settings for the instant booking feature that are applied to a room profile. When users start their meeting with Alexa, Alexa automatically books the room for the configured duration if the room is available.
--
-- /See:/ 'mkCreateInstantBooking' smart constructor.
data CreateInstantBooking = CreateInstantBooking'
  { durationInMinutes :: Core.Int
    -- ^ Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
  , enabled :: Core.Bool
    -- ^ Whether instant booking is enabled or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateInstantBooking' value with any optional fields omitted.
mkCreateInstantBooking
    :: Core.Int -- ^ 'durationInMinutes'
    -> Core.Bool -- ^ 'enabled'
    -> CreateInstantBooking
mkCreateInstantBooking durationInMinutes enabled
  = CreateInstantBooking'{durationInMinutes, enabled}

-- | Duration between 15 and 240 minutes at increments of 15 that determines how long to book an available room when a meeting is started with Alexa.
--
-- /Note:/ Consider using 'durationInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibDurationInMinutes :: Lens.Lens' CreateInstantBooking Core.Int
cibDurationInMinutes = Lens.field @"durationInMinutes"
{-# INLINEABLE cibDurationInMinutes #-}
{-# DEPRECATED durationInMinutes "Use generic-lens or generic-optics with 'durationInMinutes' instead"  #-}

-- | Whether instant booking is enabled or not.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cibEnabled :: Lens.Lens' CreateInstantBooking Core.Bool
cibEnabled = Lens.field @"enabled"
{-# INLINEABLE cibEnabled #-}
{-# DEPRECATED enabled "Use generic-lens or generic-optics with 'enabled' instead"  #-}

instance Core.FromJSON CreateInstantBooking where
        toJSON CreateInstantBooking{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("DurationInMinutes" Core..= durationInMinutes),
                  Core.Just ("Enabled" Core..= enabled)])
