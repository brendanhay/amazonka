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
-- Module      : Network.AWS.AlexaBusiness.Types.RoomData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RoomData where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The data of a room.
--
-- /See:/ 'newRoomData' smart constructor.
data RoomData = RoomData'
  { -- | The profile name of a room.
    profileName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a room.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The provider calendar ARN of a room.
    providerCalendarId :: Prelude.Maybe Prelude.Text,
    -- | The profile ARN of a room.
    profileArn :: Prelude.Maybe Prelude.Text,
    -- | The description of a room.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of a room.
    roomName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RoomData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileName', 'roomData_profileName' - The profile name of a room.
--
-- 'roomArn', 'roomData_roomArn' - The ARN of a room.
--
-- 'providerCalendarId', 'roomData_providerCalendarId' - The provider calendar ARN of a room.
--
-- 'profileArn', 'roomData_profileArn' - The profile ARN of a room.
--
-- 'description', 'roomData_description' - The description of a room.
--
-- 'roomName', 'roomData_roomName' - The name of a room.
newRoomData ::
  RoomData
newRoomData =
  RoomData'
    { profileName = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      providerCalendarId = Prelude.Nothing,
      profileArn = Prelude.Nothing,
      description = Prelude.Nothing,
      roomName = Prelude.Nothing
    }

-- | The profile name of a room.
roomData_profileName :: Lens.Lens' RoomData (Prelude.Maybe Prelude.Text)
roomData_profileName = Lens.lens (\RoomData' {profileName} -> profileName) (\s@RoomData' {} a -> s {profileName = a} :: RoomData)

-- | The ARN of a room.
roomData_roomArn :: Lens.Lens' RoomData (Prelude.Maybe Prelude.Text)
roomData_roomArn = Lens.lens (\RoomData' {roomArn} -> roomArn) (\s@RoomData' {} a -> s {roomArn = a} :: RoomData)

-- | The provider calendar ARN of a room.
roomData_providerCalendarId :: Lens.Lens' RoomData (Prelude.Maybe Prelude.Text)
roomData_providerCalendarId = Lens.lens (\RoomData' {providerCalendarId} -> providerCalendarId) (\s@RoomData' {} a -> s {providerCalendarId = a} :: RoomData)

-- | The profile ARN of a room.
roomData_profileArn :: Lens.Lens' RoomData (Prelude.Maybe Prelude.Text)
roomData_profileArn = Lens.lens (\RoomData' {profileArn} -> profileArn) (\s@RoomData' {} a -> s {profileArn = a} :: RoomData)

-- | The description of a room.
roomData_description :: Lens.Lens' RoomData (Prelude.Maybe Prelude.Text)
roomData_description = Lens.lens (\RoomData' {description} -> description) (\s@RoomData' {} a -> s {description = a} :: RoomData)

-- | The name of a room.
roomData_roomName :: Lens.Lens' RoomData (Prelude.Maybe Prelude.Text)
roomData_roomName = Lens.lens (\RoomData' {roomName} -> roomName) (\s@RoomData' {} a -> s {roomName = a} :: RoomData)

instance Prelude.FromJSON RoomData where
  parseJSON =
    Prelude.withObject
      "RoomData"
      ( \x ->
          RoomData'
            Prelude.<$> (x Prelude..:? "ProfileName")
            Prelude.<*> (x Prelude..:? "RoomArn")
            Prelude.<*> (x Prelude..:? "ProviderCalendarId")
            Prelude.<*> (x Prelude..:? "ProfileArn")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "RoomName")
      )

instance Prelude.Hashable RoomData

instance Prelude.NFData RoomData
