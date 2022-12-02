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
-- Module      : Amazonka.AlexaBusiness.Types.MeetingRoomConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.MeetingRoomConfiguration where

import Amazonka.AlexaBusiness.Types.EndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.InstantBooking
import Amazonka.AlexaBusiness.Types.RequireCheckIn
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Meeting room settings of a room profile.
--
-- /See:/ 'newMeetingRoomConfiguration' smart constructor.
data MeetingRoomConfiguration = MeetingRoomConfiguration'
  { -- | Settings to automatically book the room if available for a configured
    -- duration when joining a meeting with Alexa.
    instantBooking :: Prelude.Maybe InstantBooking,
    -- | Settings for the end of meeting reminder feature that are applied to a
    -- room profile. The end of meeting reminder enables Alexa to remind users
    -- when a meeting is ending.
    endOfMeetingReminder :: Prelude.Maybe EndOfMeetingReminder,
    -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into. This makes the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Prelude.Maybe RequireCheckIn
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MeetingRoomConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instantBooking', 'meetingRoomConfiguration_instantBooking' - Settings to automatically book the room if available for a configured
-- duration when joining a meeting with Alexa.
--
-- 'endOfMeetingReminder', 'meetingRoomConfiguration_endOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
--
-- 'roomUtilizationMetricsEnabled', 'meetingRoomConfiguration_roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
--
-- 'requireCheckIn', 'meetingRoomConfiguration_requireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into. This makes the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
newMeetingRoomConfiguration ::
  MeetingRoomConfiguration
newMeetingRoomConfiguration =
  MeetingRoomConfiguration'
    { instantBooking =
        Prelude.Nothing,
      endOfMeetingReminder = Prelude.Nothing,
      roomUtilizationMetricsEnabled = Prelude.Nothing,
      requireCheckIn = Prelude.Nothing
    }

-- | Settings to automatically book the room if available for a configured
-- duration when joining a meeting with Alexa.
meetingRoomConfiguration_instantBooking :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe InstantBooking)
meetingRoomConfiguration_instantBooking = Lens.lens (\MeetingRoomConfiguration' {instantBooking} -> instantBooking) (\s@MeetingRoomConfiguration' {} a -> s {instantBooking = a} :: MeetingRoomConfiguration)

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
meetingRoomConfiguration_endOfMeetingReminder :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe EndOfMeetingReminder)
meetingRoomConfiguration_endOfMeetingReminder = Lens.lens (\MeetingRoomConfiguration' {endOfMeetingReminder} -> endOfMeetingReminder) (\s@MeetingRoomConfiguration' {} a -> s {endOfMeetingReminder = a} :: MeetingRoomConfiguration)

-- | Whether room utilization metrics are enabled or not.
meetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe Prelude.Bool)
meetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\MeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@MeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: MeetingRoomConfiguration)

-- | Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into. This makes the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
meetingRoomConfiguration_requireCheckIn :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe RequireCheckIn)
meetingRoomConfiguration_requireCheckIn = Lens.lens (\MeetingRoomConfiguration' {requireCheckIn} -> requireCheckIn) (\s@MeetingRoomConfiguration' {} a -> s {requireCheckIn = a} :: MeetingRoomConfiguration)

instance Data.FromJSON MeetingRoomConfiguration where
  parseJSON =
    Data.withObject
      "MeetingRoomConfiguration"
      ( \x ->
          MeetingRoomConfiguration'
            Prelude.<$> (x Data..:? "InstantBooking")
            Prelude.<*> (x Data..:? "EndOfMeetingReminder")
            Prelude.<*> (x Data..:? "RoomUtilizationMetricsEnabled")
            Prelude.<*> (x Data..:? "RequireCheckIn")
      )

instance Prelude.Hashable MeetingRoomConfiguration where
  hashWithSalt _salt MeetingRoomConfiguration' {..} =
    _salt `Prelude.hashWithSalt` instantBooking
      `Prelude.hashWithSalt` endOfMeetingReminder
      `Prelude.hashWithSalt` roomUtilizationMetricsEnabled
      `Prelude.hashWithSalt` requireCheckIn

instance Prelude.NFData MeetingRoomConfiguration where
  rnf MeetingRoomConfiguration' {..} =
    Prelude.rnf instantBooking
      `Prelude.seq` Prelude.rnf endOfMeetingReminder
      `Prelude.seq` Prelude.rnf roomUtilizationMetricsEnabled
      `Prelude.seq` Prelude.rnf requireCheckIn
