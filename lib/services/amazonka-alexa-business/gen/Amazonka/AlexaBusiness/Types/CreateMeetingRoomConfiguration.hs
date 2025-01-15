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
-- Module      : Amazonka.AlexaBusiness.Types.CreateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.CreateMeetingRoomConfiguration where

import Amazonka.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Amazonka.AlexaBusiness.Types.CreateInstantBooking
import Amazonka.AlexaBusiness.Types.CreateRequireCheckIn
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Creates meeting room settings of a room profile.
--
-- /See:/ 'newCreateMeetingRoomConfiguration' smart constructor.
data CreateMeetingRoomConfiguration = CreateMeetingRoomConfiguration'
  { endOfMeetingReminder :: Prelude.Maybe CreateEndOfMeetingReminder,
    -- | Settings to automatically book a room for a configured duration if it\'s
    -- free when joining a meeting with Alexa.
    instantBooking :: Prelude.Maybe CreateInstantBooking,
    -- | Settings for requiring a check in when a room is reserved. Alexa can
    -- cancel a room reservation if it\'s not checked into to make the room
    -- available for others. Users can check in by joining the meeting with
    -- Alexa or an AVS device, or by saying “Alexa, check in.”
    requireCheckIn :: Prelude.Maybe CreateRequireCheckIn,
    -- | Whether room utilization metrics are enabled or not.
    roomUtilizationMetricsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeetingRoomConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'roomUtilizationMetricsEnabled', 'createMeetingRoomConfiguration_roomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
newCreateMeetingRoomConfiguration ::
  CreateMeetingRoomConfiguration
newCreateMeetingRoomConfiguration =
  CreateMeetingRoomConfiguration'
    { endOfMeetingReminder =
        Prelude.Nothing,
      instantBooking = Prelude.Nothing,
      requireCheckIn = Prelude.Nothing,
      roomUtilizationMetricsEnabled =
        Prelude.Nothing
    }

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

-- | Whether room utilization metrics are enabled or not.
createMeetingRoomConfiguration_roomUtilizationMetricsEnabled :: Lens.Lens' CreateMeetingRoomConfiguration (Prelude.Maybe Prelude.Bool)
createMeetingRoomConfiguration_roomUtilizationMetricsEnabled = Lens.lens (\CreateMeetingRoomConfiguration' {roomUtilizationMetricsEnabled} -> roomUtilizationMetricsEnabled) (\s@CreateMeetingRoomConfiguration' {} a -> s {roomUtilizationMetricsEnabled = a} :: CreateMeetingRoomConfiguration)

instance
  Prelude.Hashable
    CreateMeetingRoomConfiguration
  where
  hashWithSalt
    _salt
    CreateMeetingRoomConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` endOfMeetingReminder
        `Prelude.hashWithSalt` instantBooking
        `Prelude.hashWithSalt` requireCheckIn
        `Prelude.hashWithSalt` roomUtilizationMetricsEnabled

instance
  Prelude.NFData
    CreateMeetingRoomConfiguration
  where
  rnf CreateMeetingRoomConfiguration' {..} =
    Prelude.rnf endOfMeetingReminder `Prelude.seq`
      Prelude.rnf instantBooking `Prelude.seq`
        Prelude.rnf requireCheckIn `Prelude.seq`
          Prelude.rnf roomUtilizationMetricsEnabled

instance Data.ToJSON CreateMeetingRoomConfiguration where
  toJSON CreateMeetingRoomConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndOfMeetingReminder" Data..=)
              Prelude.<$> endOfMeetingReminder,
            ("InstantBooking" Data..=)
              Prelude.<$> instantBooking,
            ("RequireCheckIn" Data..=)
              Prelude.<$> requireCheckIn,
            ("RoomUtilizationMetricsEnabled" Data..=)
              Prelude.<$> roomUtilizationMetricsEnabled
          ]
      )
