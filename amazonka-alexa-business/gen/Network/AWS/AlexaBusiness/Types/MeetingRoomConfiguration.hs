{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Meeting room settings of a room profile.
--
-- /See:/ 'newMeetingRoomConfiguration' smart constructor.
data MeetingRoomConfiguration = MeetingRoomConfiguration'
  { -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Core.Maybe Core.Bool,
    -- | Settings for the end of meeting reminder feature that are applied to a
    -- room profile. The end of meeting reminder enables Alexa to remind users
    -- when a meeting is ending.
    endOfMeetingReminder :: Core.Maybe EndOfMeetingReminder,
    -- | Settings to automatically book the room if available for a configured
    -- duration when joining a meeting with Alexa.
    instantBooking :: Core.Maybe InstantBooking,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into. This makes the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Core.Maybe RequireCheckIn
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'MeetingRoomConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomUtilizationMetricsEnabled', 'meetingRoomConfiguration_roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
--
-- 'endOfMeetingReminder', 'meetingRoomConfiguration_endOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- 'instantBooking', 'meetingRoomConfiguration_instantBooking' - Settings to automatically book the room if available for a configured
-- duration when joining a meeting with Alexa.
--
-- 'requireCheckIn', 'meetingRoomConfiguration_requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into. This makes the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
newMeetingRoomConfiguration ::
  MeetingRoomConfiguration
newMeetingRoomConfiguration =
  MeetingRoomConfiguration'
    { roomUtilizationMetricsEnabled =
        Core.Nothing,
      endOfMeetingReminder = Core.Nothing,
      instantBooking = Core.Nothing,
      requireCheckIn = Core.Nothing
    }

-- | Whether room utilization metrics are enabled or not.
meetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe Core.Bool)
meetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\MeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@MeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: MeetingRoomConfiguration)

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
meetingRoomConfiguration_endOfMeetingReminder :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe EndOfMeetingReminder)
meetingRoomConfiguration_endOfMeetingReminder = Lens.lens (\MeetingRoomConfiguration' {endOfMeetingReminder} -> endOfMeetingReminder) (\s@MeetingRoomConfiguration' {} a -> s {endOfMeetingReminder = a} :: MeetingRoomConfiguration)

-- | Settings to automatically book the room if available for a configured
-- duration when joining a meeting with Alexa.
meetingRoomConfiguration_instantBooking :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe InstantBooking)
meetingRoomConfiguration_instantBooking = Lens.lens (\MeetingRoomConfiguration' {instantBooking} -> instantBooking) (\s@MeetingRoomConfiguration' {} a -> s {instantBooking = a} :: MeetingRoomConfiguration)

-- | Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into. This makes the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
meetingRoomConfiguration_requireCheckIn :: Lens.Lens' MeetingRoomConfiguration (Core.Maybe RequireCheckIn)
meetingRoomConfiguration_requireCheckIn = Lens.lens (\MeetingRoomConfiguration' {requireCheckIn} -> requireCheckIn) (\s@MeetingRoomConfiguration' {} a -> s {requireCheckIn = a} :: MeetingRoomConfiguration)

instance Core.FromJSON MeetingRoomConfiguration where
  parseJSON =
    Core.withObject
      "MeetingRoomConfiguration"
      ( \x ->
          MeetingRoomConfiguration'
            Core.<$> (x Core..:? "RoomUtilizationMetricsEnabled")
            Core.<*> (x Core..:? "EndOfMeetingReminder")
            Core.<*> (x Core..:? "InstantBooking")
            Core.<*> (x Core..:? "RequireCheckIn")
      )

instance Core.Hashable MeetingRoomConfiguration

instance Core.NFData MeetingRoomConfiguration
