{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Creates meeting room settings of a room profile.
--
-- /See:/ 'newCreateMeetingRoomConfiguration' smart constructor.
data CreateMeetingRoomConfiguration = CreateMeetingRoomConfiguration'
  { -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    endOfMeetingReminder :: Prelude.Maybe CreateEndOfMeetingReminder,
    -- | Settings to automatically book a room for a configured duration if it\'s
    -- free when joining a meeting with Alexa.
    instantBooking :: Prelude.Maybe CreateInstantBooking,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into to make the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Prelude.Maybe CreateRequireCheckIn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      endOfMeetingReminder = Prelude.Nothing,
      instantBooking = Prelude.Nothing,
      requireCheckIn = Prelude.Nothing
    }

-- | Whether room utilization metrics are enabled or not.
createMeetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' CreateMeetingRoomConfiguration (Prelude.Maybe Prelude.Bool)
createMeetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\CreateMeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@CreateMeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: CreateMeetingRoomConfiguration)

-- | Undocumented member.
createMeetingRoomConfiguration_endOfMeetingReminder :: Lens.Lens' CreateMeetingRoomConfiguration (Prelude.Maybe CreateEndOfMeetingReminder)
createMeetingRoomConfiguration_endOfMeetingReminder = Lens.lens (\CreateMeetingRoomConfiguration' {endOfMeetingReminder} -> endOfMeetingReminder) (\s@CreateMeetingRoomConfiguration' {} a -> s {endOfMeetingReminder = a} :: CreateMeetingRoomConfiguration)

-- | Settings to automatically book a room for a configured duration if it\'s
-- free when joining a meeting with Alexa.
createMeetingRoomConfiguration_instantBooking :: Lens.Lens' CreateMeetingRoomConfiguration (Prelude.Maybe CreateInstantBooking)
createMeetingRoomConfiguration_instantBooking = Lens.lens (\CreateMeetingRoomConfiguration' {instantBooking} -> instantBooking) (\s@CreateMeetingRoomConfiguration' {} a -> s {instantBooking = a} :: CreateMeetingRoomConfiguration)

-- | Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into to make the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
createMeetingRoomConfiguration_requireCheckIn :: Lens.Lens' CreateMeetingRoomConfiguration (Prelude.Maybe CreateRequireCheckIn)
createMeetingRoomConfiguration_requireCheckIn = Lens.lens (\CreateMeetingRoomConfiguration' {requireCheckIn} -> requireCheckIn) (\s@CreateMeetingRoomConfiguration' {} a -> s {requireCheckIn = a} :: CreateMeetingRoomConfiguration)

instance
  Prelude.Hashable
    CreateMeetingRoomConfiguration

instance
  Prelude.NFData
    CreateMeetingRoomConfiguration

instance
  Prelude.ToJSON
    CreateMeetingRoomConfiguration
  where
  toJSON CreateMeetingRoomConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("RoomUtilizationMetricsEnabled" Prelude..=)
              Prelude.<$> roomUtilizationMetricsEnabled,
            ("EndOfMeetingReminder" Prelude..=)
              Prelude.<$> endOfMeetingReminder,
            ("InstantBooking" Prelude..=)
              Prelude.<$> instantBooking,
            ("RequireCheckIn" Prelude..=)
              Prelude.<$> requireCheckIn
          ]
      )
