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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | Settings for the end of meeting reminder feature that are applied to a
    -- room profile. The end of meeting reminder enables Alexa to remind users
    -- when a meeting is ending.
    endOfMeetingReminder :: Prelude.Maybe EndOfMeetingReminder,
    -- | Settings to automatically book the room if available for a configured
    -- duration when joining a meeting with Alexa.
    instantBooking :: Prelude.Maybe InstantBooking,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into. This makes the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Prelude.Maybe RequireCheckIn,
    -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Prelude.Maybe Prelude.Bool
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
--
-- 'roomUtilizationMetricsEnabled', 'meetingRoomConfiguration_roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
newMeetingRoomConfiguration ::
  MeetingRoomConfiguration
newMeetingRoomConfiguration =
  MeetingRoomConfiguration'
    { endOfMeetingReminder =
        Prelude.Nothing,
      instantBooking = Prelude.Nothing,
      requireCheckIn = Prelude.Nothing,
      roomUtilizationMetricsEnabled = Prelude.Nothing
    }

-- | Settings for the end of meeting reminder feature that are applied to a
-- room profile. The end of meeting reminder enables Alexa to remind users
-- when a meeting is ending.
meetingRoomConfiguration_endOfMeetingReminder :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe EndOfMeetingReminder)
meetingRoomConfiguration_endOfMeetingReminder = Lens.lens (\MeetingRoomConfiguration' {endOfMeetingReminder} -> endOfMeetingReminder) (\s@MeetingRoomConfiguration' {} a -> s {endOfMeetingReminder = a} :: MeetingRoomConfiguration)

-- | Settings to automatically book the room if available for a configured
-- duration when joining a meeting with Alexa.
meetingRoomConfiguration_instantBooking :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe InstantBooking)
meetingRoomConfiguration_instantBooking = Lens.lens (\MeetingRoomConfiguration' {instantBooking} -> instantBooking) (\s@MeetingRoomConfiguration' {} a -> s {instantBooking = a} :: MeetingRoomConfiguration)

-- | Settings for requiring a check in when a room is reserved. Alexa can
-- cancel a room reservation if it\'s not checked into. This makes the room
-- available for others. Users can check in by joining the meeting with
-- Alexa or an AVS device, or by saying “Alexa, check in.”
meetingRoomConfiguration_requireCheckIn :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe RequireCheckIn)
meetingRoomConfiguration_requireCheckIn = Lens.lens (\MeetingRoomConfiguration' {requireCheckIn} -> requireCheckIn) (\s@MeetingRoomConfiguration' {} a -> s {requireCheckIn = a} :: MeetingRoomConfiguration)

-- | Whether room utilization metrics are enabled or not.
meetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' MeetingRoomConfiguration (Prelude.Maybe Prelude.Bool)
meetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\MeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@MeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: MeetingRoomConfiguration)

instance Data.FromJSON MeetingRoomConfiguration where
  parseJSON =
    Data.withObject
      "MeetingRoomConfiguration"
      ( \x ->
          MeetingRoomConfiguration'
            Prelude.<$> (x Data..:? "EndOfMeetingReminder")
            Prelude.<*> (x Data..:? "InstantBooking")
            Prelude.<*> (x Data..:? "RequireCheckIn")
            Prelude.<*> (x Data..:? "RoomUtilizationMetricsEnabled")
      )

instance Prelude.Hashable MeetingRoomConfiguration where
  hashWithSalt _salt MeetingRoomConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` endOfMeetingReminder
      `Prelude.hashWithSalt` instantBooking
      `Prelude.hashWithSalt` requireCheckIn
      `Prelude.hashWithSalt` roomUtilizationMetricsEnabled

instance Prelude.NFData MeetingRoomConfiguration where
  rnf MeetingRoomConfiguration' {..} =
    Prelude.rnf endOfMeetingReminder
      `Prelude.seq` Prelude.rnf instantBooking
      `Prelude.seq` Prelude.rnf requireCheckIn
      `Prelude.seq` Prelude.rnf roomUtilizationMetricsEnabled
