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
-- Module      : Network.AWS.AlexaBusiness.Types.Room
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Room where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A room with attributes.
--
-- /See:/ 'newRoom' smart constructor.
data Room = Room'
  { -- | The ARN of a room.
    roomArn :: Core.Maybe Core.Text,
    -- | The provider calendar ARN of a room.
    providerCalendarId :: Core.Maybe Core.Text,
    -- | The profile ARN of a room.
    profileArn :: Core.Maybe Core.Text,
    -- | The description of a room.
    description :: Core.Maybe Core.Text,
    -- | The name of a room.
    roomName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Room' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'room_roomArn' - The ARN of a room.
--
-- 'providerCalendarId', 'room_providerCalendarId' - The provider calendar ARN of a room.
--
-- 'profileArn', 'room_profileArn' - The profile ARN of a room.
--
-- 'description', 'room_description' - The description of a room.
--
-- 'roomName', 'room_roomName' - The name of a room.
newRoom ::
  Room
newRoom =
  Room'
    { roomArn = Core.Nothing,
      providerCalendarId = Core.Nothing,
      profileArn = Core.Nothing,
      description = Core.Nothing,
      roomName = Core.Nothing
    }

-- | The ARN of a room.
room_roomArn :: Lens.Lens' Room (Core.Maybe Core.Text)
room_roomArn = Lens.lens (\Room' {roomArn} -> roomArn) (\s@Room' {} a -> s {roomArn = a} :: Room)

-- | The provider calendar ARN of a room.
room_providerCalendarId :: Lens.Lens' Room (Core.Maybe Core.Text)
room_providerCalendarId = Lens.lens (\Room' {providerCalendarId} -> providerCalendarId) (\s@Room' {} a -> s {providerCalendarId = a} :: Room)

-- | The profile ARN of a room.
room_profileArn :: Lens.Lens' Room (Core.Maybe Core.Text)
room_profileArn = Lens.lens (\Room' {profileArn} -> profileArn) (\s@Room' {} a -> s {profileArn = a} :: Room)

-- | The description of a room.
room_description :: Lens.Lens' Room (Core.Maybe Core.Text)
room_description = Lens.lens (\Room' {description} -> description) (\s@Room' {} a -> s {description = a} :: Room)

-- | The name of a room.
room_roomName :: Lens.Lens' Room (Core.Maybe Core.Text)
room_roomName = Lens.lens (\Room' {roomName} -> roomName) (\s@Room' {} a -> s {roomName = a} :: Room)

instance Core.FromJSON Room where
  parseJSON =
    Core.withObject
      "Room"
      ( \x ->
          Room'
            Core.<$> (x Core..:? "RoomArn")
            Core.<*> (x Core..:? "ProviderCalendarId")
            Core.<*> (x Core..:? "ProfileArn")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "RoomName")
      )

instance Core.Hashable Room

instance Core.NFData Room
