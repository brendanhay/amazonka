{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
  ( MeetingRoomConfiguration (..)
  -- * Smart constructor
  , mkMeetingRoomConfiguration
  -- * Lenses
  , mrcEndOfMeetingReminder
  , mrcInstantBooking
  , mrcRequireCheckIn
  , mrcRoomUtilizationMetricsEnabled
  ) where

import qualified Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder as Types
import qualified Network.AWS.AlexaBusiness.Types.InstantBooking as Types
import qualified Network.AWS.AlexaBusiness.Types.RequireCheckIn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Meeting room settings of a room profile.
--
-- /See:/ 'mkMeetingRoomConfiguration' smart constructor.
data MeetingRoomConfiguration = MeetingRoomConfiguration'
  { endOfMeetingReminder :: Core.Maybe Types.EndOfMeetingReminder
    -- ^ Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending. 
  , instantBooking :: Core.Maybe Types.InstantBooking
    -- ^ Settings to automatically book the room if available for a configured duration when joining a meeting with Alexa. 
  , requireCheckIn :: Core.Maybe Types.RequireCheckIn
    -- ^ Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into. This makes the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.” 
  , roomUtilizationMetricsEnabled :: Core.Maybe Core.Bool
    -- ^ Whether room utilization metrics are enabled or not.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MeetingRoomConfiguration' value with any optional fields omitted.
mkMeetingRoomConfiguration
    :: MeetingRoomConfiguration
mkMeetingRoomConfiguration
  = MeetingRoomConfiguration'{endOfMeetingReminder = Core.Nothing,
                              instantBooking = Core.Nothing, requireCheckIn = Core.Nothing,
                              roomUtilizationMetricsEnabled = Core.Nothing}

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending. 
--
-- /Note:/ Consider using 'endOfMeetingReminder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcEndOfMeetingReminder :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe Types.EndOfMeetingReminder)
mrcEndOfMeetingReminder = Lens.field @"endOfMeetingReminder"
{-# INLINEABLE mrcEndOfMeetingReminder #-}
{-# DEPRECATED endOfMeetingReminder "Use generic-lens or generic-optics with 'endOfMeetingReminder' instead"  #-}

-- | Settings to automatically book the room if available for a configured duration when joining a meeting with Alexa. 
--
-- /Note:/ Consider using 'instantBooking' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcInstantBooking :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe Types.InstantBooking)
mrcInstantBooking = Lens.field @"instantBooking"
{-# INLINEABLE mrcInstantBooking #-}
{-# DEPRECATED instantBooking "Use generic-lens or generic-optics with 'instantBooking' instead"  #-}

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into. This makes the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.” 
--
-- /Note:/ Consider using 'requireCheckIn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcRequireCheckIn :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe Types.RequireCheckIn)
mrcRequireCheckIn = Lens.field @"requireCheckIn"
{-# INLINEABLE mrcRequireCheckIn #-}
{-# DEPRECATED requireCheckIn "Use generic-lens or generic-optics with 'requireCheckIn' instead"  #-}

-- | Whether room utilization metrics are enabled or not.
--
-- /Note:/ Consider using 'roomUtilizationMetricsEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrcRoomUtilizationMetricsEnabled :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe Core.Bool)
mrcRoomUtilizationMetricsEnabled = Lens.field @"roomUtilizationMetricsEnabled"
{-# INLINEABLE mrcRoomUtilizationMetricsEnabled #-}
{-# DEPRECATED roomUtilizationMetricsEnabled "Use generic-lens or generic-optics with 'roomUtilizationMetricsEnabled' instead"  #-}

instance Core.FromJSON MeetingRoomConfiguration where
        parseJSON
          = Core.withObject "MeetingRoomConfiguration" Core.$
              \ x ->
                MeetingRoomConfiguration' Core.<$>
                  (x Core..:? "EndOfMeetingReminder") Core.<*>
                    x Core..:? "InstantBooking"
                    Core.<*> x Core..:? "RequireCheckIn"
                    Core.<*> x Core..:? "RoomUtilizationMetricsEnabled"
