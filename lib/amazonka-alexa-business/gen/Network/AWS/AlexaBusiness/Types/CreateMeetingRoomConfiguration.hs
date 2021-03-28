{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
  ( CreateMeetingRoomConfiguration (..)
  -- * Smart constructor
  , mkCreateMeetingRoomConfiguration
  -- * Lenses
  , cmrcEndOfMeetingReminder
  , cmrcInstantBooking
  , cmrcRequireCheckIn
  , cmrcRoomUtilizationMetricsEnabled
  ) where

import qualified Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder as Types
import qualified Network.AWS.AlexaBusiness.Types.CreateInstantBooking as Types
import qualified Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Creates meeting room settings of a room profile.
--
-- /See:/ 'mkCreateMeetingRoomConfiguration' smart constructor.
data CreateMeetingRoomConfiguration = CreateMeetingRoomConfiguration'
  { endOfMeetingReminder :: Core.Maybe Types.CreateEndOfMeetingReminder
  , instantBooking :: Core.Maybe Types.CreateInstantBooking
    -- ^ Settings to automatically book a room for a configured duration if it's free when joining a meeting with Alexa.
  , requireCheckIn :: Core.Maybe Types.CreateRequireCheckIn
    -- ^ Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
  , roomUtilizationMetricsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether room utilization metrics are enabled or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateMeetingRoomConfiguration' value with any optional fields omitted.
mkCreateMeetingRoomConfiguration
    :: CreateMeetingRoomConfiguration
mkCreateMeetingRoomConfiguration
  = CreateMeetingRoomConfiguration'{endOfMeetingReminder =
                                      Core.Nothing,
                                    instantBooking = Core.Nothing, requireCheckIn = Core.Nothing,
                                    roomUtilizationMetricsEnabled = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'endOfMeetingReminder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcEndOfMeetingReminder :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe Types.CreateEndOfMeetingReminder)
cmrcEndOfMeetingReminder = Lens.field @"endOfMeetingReminder"
{-# INLINEABLE cmrcEndOfMeetingReminder #-}
{-# DEPRECATED endOfMeetingReminder "Use generic-lens or generic-optics with 'endOfMeetingReminder' instead"  #-}

-- | Settings to automatically book a room for a configured duration if it's free when joining a meeting with Alexa.
--
-- /Note:/ Consider using 'instantBooking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcInstantBooking :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe Types.CreateInstantBooking)
cmrcInstantBooking = Lens.field @"instantBooking"
{-# INLINEABLE cmrcInstantBooking #-}
{-# DEPRECATED instantBooking "Use generic-lens or generic-optics with 'instantBooking' instead"  #-}

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- /Note:/ Consider using 'requireCheckIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcRequireCheckIn :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe Types.CreateRequireCheckIn)
cmrcRequireCheckIn = Lens.field @"requireCheckIn"
{-# INLINEABLE cmrcRequireCheckIn #-}
{-# DEPRECATED requireCheckIn "Use generic-lens or generic-optics with 'requireCheckIn' instead"  #-}

-- | Whether room utilization metrics are enabled or not.
--
-- /Note:/ Consider using 'roomUtilizationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmrcRoomUtilizationMetricsEnabled :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe Core.Bool)
cmrcRoomUtilizationMetricsEnabled = Lens.field @"roomUtilizationMetricsEnabled"
{-# INLINEABLE cmrcRoomUtilizationMetricsEnabled #-}
{-# DEPRECATED roomUtilizationMetricsEnabled "Use generic-lens or generic-optics with 'roomUtilizationMetricsEnabled' instead"  #-}

instance Core.FromJSON CreateMeetingRoomConfiguration where
        toJSON CreateMeetingRoomConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("EndOfMeetingReminder" Core..=) Core.<$> endOfMeetingReminder,
                  ("InstantBooking" Core..=) Core.<$> instantBooking,
                  ("RequireCheckIn" Core..=) Core.<$> requireCheckIn,
                  ("RoomUtilizationMetricsEnabled" Core..=) Core.<$>
                    roomUtilizationMetricsEnabled])
