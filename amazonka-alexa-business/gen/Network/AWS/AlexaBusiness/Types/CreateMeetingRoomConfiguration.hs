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
-- Module      : Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration where

import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Creates meeting room settings of a room profile.
--
-- /See:/ 'newCreateMeetingRoomConfiguration' smart constructor.
data CreateMeetingRoomConfiguration = CreateMeetingRoomConfiguration'
  { -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Core.Maybe Core.Bool,
    endOfMeetingReminder :: Core.Maybe CreateEndOfMeetingReminder,
    -- | Settings to automatically book a room for a configured duration if it\'s
    -- free when joining a meeting with Alexa.
    instantBooking :: Core.Maybe CreateInstantBooking,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into to make the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Core.Maybe CreateRequireCheckIn
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateMeetingRoomConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomUtilizationMetricsEnabled', 'createMeetingRoomConfiguration_roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
--
-- 'endOfMeetingReminder', 'createMeetingRoomConfiguration_endOfMeetingReminder' - Undocumented member.
--
-- 'instantBooking', 'createMeetingRoomConfiguration_instantBooking' - Settings to automatically book a room for a configured duration if it\'s
-- free when joining a meeting with Alexa.
--
-- 'requireCheckIn', 'createMeetingRoomConfiguration_requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into to make the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
newCreateMeetingRoomConfiguration ::
  CreateMeetingRoomConfiguration
newCreateMeetingRoomConfiguration =
  CreateMeetingRoomConfiguration'
    { roomUtilizationMetricsEnabled =
        Core.Nothing,
      endOfMeetingReminder = Core.Nothing,
      instantBooking = Core.Nothing,
      requireCheckIn = Core.Nothing
    }

-- | Whether room utilization metrics are enabled or not.
createMeetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe Core.Bool)
createMeetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\CreateMeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@CreateMeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: CreateMeetingRoomConfiguration)

-- | Undocumented member.
createMeetingRoomConfiguration_endOfMeetingReminder :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe CreateEndOfMeetingReminder)
createMeetingRoomConfiguration_endOfMeetingReminder = Lens.lens (\CreateMeetingRoomConfiguration' {endOfMeetingReminder} -> endOfMeetingReminder) (\s@CreateMeetingRoomConfiguration' {} a -> s {endOfMeetingReminder = a} :: CreateMeetingRoomConfiguration)

-- | Settings to automatically book a room for a configured duration if it\'s
-- free when joining a meeting with Alexa.
createMeetingRoomConfiguration_instantBooking :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe CreateInstantBooking)
createMeetingRoomConfiguration_instantBooking = Lens.lens (\CreateMeetingRoomConfiguration' {instantBooking} -> instantBooking) (\s@CreateMeetingRoomConfiguration' {} a -> s {instantBooking = a} :: CreateMeetingRoomConfiguration)

-- | Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into to make the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
createMeetingRoomConfiguration_requireCheckIn :: Lens.Lens' CreateMeetingRoomConfiguration (Core.Maybe CreateRequireCheckIn)
createMeetingRoomConfiguration_requireCheckIn = Lens.lens (\CreateMeetingRoomConfiguration' {requireCheckIn} -> requireCheckIn) (\s@CreateMeetingRoomConfiguration' {} a -> s {requireCheckIn = a} :: CreateMeetingRoomConfiguration)

instance Core.Hashable CreateMeetingRoomConfiguration

instance Core.NFData CreateMeetingRoomConfiguration

instance Core.ToJSON CreateMeetingRoomConfiguration where
  toJSON CreateMeetingRoomConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RoomUtilizationMetricsEnabled" Core..=)
              Core.<$> roomUtilizationMetricsEnabled,
            ("EndOfMeetingReminder" Core..=)
              Core.<$> endOfMeetingReminder,
            ("InstantBooking" Core..=) Core.<$> instantBooking,
            ("RequireCheckIn" Core..=) Core.<$> requireCheckIn
          ]
      )
