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
-- Module      : Amazonka.AlexaBusiness.Types.Room
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Room where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A room with attributes.
--
-- /See:/ 'newRoom' smart constructor.
data Room = Room'
  { -- | The description of a room.
    description :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN of a room.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The provider calendar ARN of a room.
    providerCalendarId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a room.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a room.
    roomName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Room' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'room_description' - The description of a room.
--
-- 'profileArn', 'room_profileArn' - The profile ARN of a room.
--
-- 'providerCalendarId', 'room_providerCalendarId' - The provider calendar ARN of a room.
--
-- 'roomArn', 'room_roomArn' - The ARN of a room.
--
-- 'roomName', 'room_roomName' - The name of a room.
newRoom ::
  Room
newRoom =
  Room'
    { description = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      providerCalendarId = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      roomName = Prelude.Nothing
    }

-- | The description of a room.
room_description :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_description = Lens.lens (\Room' {description} -> description) (\s@Room' {} a -> s {description = a} :: Room)

-- | The profile ARN of a room.
room_profileArn :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_profileArn = Lens.lens (\Room' {profileArn} -> profileArn) (\s@Room' {} a -> s {profileArn = a} :: Room)

-- | The provider calendar ARN of a room.
room_providerCalendarId :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_providerCalendarId = Lens.lens (\Room' {providerCalendarId} -> providerCalendarId) (\s@Room' {} a -> s {providerCalendarId = a} :: Room)

-- | The ARN of a room.
room_roomArn :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_roomArn = Lens.lens (\Room' {roomArn} -> roomArn) (\s@Room' {} a -> s {roomArn = a} :: Room)

-- | The name of a room.
room_roomName :: Lens.Lens' Room (Prelude.Maybe Prelude.Text)
room_roomName = Lens.lens (\Room' {roomName} -> roomName) (\s@Room' {} a -> s {roomName = a} :: Room)

instance Data.FromJSON Room where
  parseJSON =
    Data.withObject
      "Room"
      ( \x ->
          Room'
            Prelude.<$> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "ProfileArn")
            Prelude.<*> (x Data..:? "ProviderCalendarId")
            Prelude.<*> (x Data..:? "RoomArn")
            Prelude.<*> (x Data..:? "RoomName")
      )

instance Prelude.Hashable Room where
  hashWithSalt _salt Room' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` profileArn
      `Prelude.hashWithSalt` providerCalendarId
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` roomName

instance Prelude.NFData Room where
  rnf Room' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf profileArn
      `Prelude.seq` Prelude.rnf providerCalendarId
      `Prelude.seq` Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf roomName
