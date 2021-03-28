{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
  ( UpdateMeetingRoomConfiguration (..)
  -- * Smart constructor
  , mkUpdateMeetingRoomConfiguration
  -- * Lenses
  , umrcEndOfMeetingReminder
  , umrcInstantBooking
  , umrcRequireCheckIn
  , umrcRoomUtilizationMetricsEnabled
  ) where

import qualified Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder as Types
import qualified Network.AWS.AlexaBusiness.Types.UpdateInstantBooking as Types
import qualified Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Updates meeting room settings of a room profile.
--
-- /See:/ 'mkUpdateMeetingRoomConfiguration' smart constructor.
data UpdateMeetingRoomConfiguration = UpdateMeetingRoomConfiguration'
  { endOfMeetingReminder :: Core.Maybe Types.UpdateEndOfMeetingReminder
    -- ^ Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending. 
  , instantBooking :: Core.Maybe Types.UpdateInstantBooking
    -- ^ Settings to automatically book an available room available for a configured duration when joining a meeting with Alexa.
  , requireCheckIn :: Core.Maybe Types.UpdateRequireCheckIn
    -- ^ Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.” 
  , roomUtilizationMetricsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether room utilization metrics are enabled or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateMeetingRoomConfiguration' value with any optional fields omitted.
mkUpdateMeetingRoomConfiguration
    :: UpdateMeetingRoomConfiguration
mkUpdateMeetingRoomConfiguration
  = UpdateMeetingRoomConfiguration'{endOfMeetingReminder =
                                      Core.Nothing,
                                    instantBooking = Core.Nothing, requireCheckIn = Core.Nothing,
                                    roomUtilizationMetricsEnabled = Core.Nothing}

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending. 
--
-- /Note:/ Consider using 'endOfMeetingReminder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcEndOfMeetingReminder :: Lens.Lens' UpdateMeetingRoomConfiguration (Core.Maybe Types.UpdateEndOfMeetingReminder)
umrcEndOfMeetingReminder = Lens.field @"endOfMeetingReminder"
{-# INLINEABLE umrcEndOfMeetingReminder #-}
{-# DEPRECATED endOfMeetingReminder "Use generic-lens or generic-optics with 'endOfMeetingReminder' instead"  #-}

-- | Settings to automatically book an available room available for a configured duration when joining a meeting with Alexa.
--
-- /Note:/ Consider using 'instantBooking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcInstantBooking :: Lens.Lens' UpdateMeetingRoomConfiguration (Core.Maybe Types.UpdateInstantBooking)
umrcInstantBooking = Lens.field @"instantBooking"
{-# INLINEABLE umrcInstantBooking #-}
{-# DEPRECATED instantBooking "Use generic-lens or generic-optics with 'instantBooking' instead"  #-}

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.” 
--
-- /Note:/ Consider using 'requireCheckIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcRequireCheckIn :: Lens.Lens' UpdateMeetingRoomConfiguration (Core.Maybe Types.UpdateRequireCheckIn)
umrcRequireCheckIn = Lens.field @"requireCheckIn"
{-# INLINEABLE umrcRequireCheckIn #-}
{-# DEPRECATED requireCheckIn "Use generic-lens or generic-optics with 'requireCheckIn' instead"  #-}

-- | Whether room utilization metrics are enabled or not.
--
-- /Note:/ Consider using 'roomUtilizationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umrcRoomUtilizationMetricsEnabled :: Lens.Lens' UpdateMeetingRoomConfiguration (Core.Maybe Core.Bool)
umrcRoomUtilizationMetricsEnabled = Lens.field @"roomUtilizationMetricsEnabled"
{-# INLINEABLE umrcRoomUtilizationMetricsEnabled #-}
{-# DEPRECATED roomUtilizationMetricsEnabled "Use generic-lens or generic-optics with 'roomUtilizationMetricsEnabled' instead"  #-}

instance Core.FromJSON UpdateMeetingRoomConfiguration where
        toJSON UpdateMeetingRoomConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("EndOfMeetingReminder" Core..=) Core.<$> endOfMeetingReminder,
                  ("InstantBooking" Core..=) Core.<$> instantBooking,
                  ("RequireCheckIn" Core..=) Core.<$> requireCheckIn,
                  ("RoomUtilizationMetricsEnabled" Core..=) Core.<$>
                    roomUtilizationMetricsEnabled])
