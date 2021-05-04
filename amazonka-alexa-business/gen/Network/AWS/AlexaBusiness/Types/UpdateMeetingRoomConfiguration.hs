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
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration where

import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Updates meeting room settings of a room profile.
--
-- /See:/ 'newUpdateMeetingRoomConfiguration' smart constructor.
data UpdateMeetingRoomConfiguration = UpdateMeetingRoomConfiguration'
  { -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Settings for the end of meeting reminder feature that are applied to a
    -- room profile. The end of meeting reminder enables Alexa to remind users
    -- when a meeting is ending.
    endOfMeetingReminder :: Prelude.Maybe UpdateEndOfMeetingReminder,
    -- | Settings to automatically book an available room available for a
    -- configured duration when joining a meeting with Alexa.
    instantBooking :: Prelude.Maybe UpdateInstantBooking,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into to make the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Prelude.Maybe UpdateRequireCheckIn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateMeetingRoomConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomUtilizationMetricsEnabled', 'updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
--
-- 'endOfMeetingReminder', 'updateMeetingRoomConfiguration_endOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- 'instantBooking', 'updateMeetingRoomConfiguration_instantBooking' - Settings to automatically book an available room available for a
-- configured duration when joining a meeting with Alexa.
--
-- 'requireCheckIn', 'updateMeetingRoomConfiguration_requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into to make the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
newUpdateMeetingRoomConfiguration ::
  UpdateMeetingRoomConfiguration
newUpdateMeetingRoomConfiguration =
  UpdateMeetingRoomConfiguration'
    { roomUtilizationMetricsEnabled =
        Prelude.Nothing,
      endOfMeetingReminder = Prelude.Nothing,
      instantBooking = Prelude.Nothing,
      requireCheckIn = Prelude.Nothing
    }

-- | Whether room utilization metrics are enabled or not.
updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' UpdateMeetingRoomConfiguration (Prelude.Maybe Prelude.Bool)
updateMeetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\UpdateMeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@UpdateMeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: UpdateMeetingRoomConfiguration)

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
updateMeetingRoomConfiguration_endOfMeetingReminder :: Lens.Lens' UpdateMeetingRoomConfiguration (Prelude.Maybe UpdateEndOfMeetingReminder)
updateMeetingRoomConfiguration_endOfMeetingReminder = Lens.lens (\UpdateMeetingRoomConfiguration' {endOfMeetingReminder} -> endOfMeetingReminder) (\s@UpdateMeetingRoomConfiguration' {} a -> s {endOfMeetingReminder = a} :: UpdateMeetingRoomConfiguration)

-- | Settings to automatically book an available room available for a
-- configured duration when joining a meeting with Alexa.
updateMeetingRoomConfiguration_instantBooking :: Lens.Lens' UpdateMeetingRoomConfiguration (Prelude.Maybe UpdateInstantBooking)
updateMeetingRoomConfiguration_instantBooking = Lens.lens (\UpdateMeetingRoomConfiguration' {instantBooking} -> instantBooking) (\s@UpdateMeetingRoomConfiguration' {} a -> s {instantBooking = a} :: UpdateMeetingRoomConfiguration)

-- | Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into to make the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
updateMeetingRoomConfiguration_requireCheckIn :: Lens.Lens' UpdateMeetingRoomConfiguration (Prelude.Maybe UpdateRequireCheckIn)
updateMeetingRoomConfiguration_requireCheckIn = Lens.lens (\UpdateMeetingRoomConfiguration' {requireCheckIn} -> requireCheckIn) (\s@UpdateMeetingRoomConfiguration' {} a -> s {requireCheckIn = a} :: UpdateMeetingRoomConfiguration)

instance
  Prelude.Hashable
    UpdateMeetingRoomConfiguration

instance
  Prelude.NFData
    UpdateMeetingRoomConfiguration

instance
  Prelude.ToJSON
    UpdateMeetingRoomConfiguration
  where
  toJSON UpdateMeetingRoomConfiguration' {..} =
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
