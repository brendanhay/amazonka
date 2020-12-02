{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.Room
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.Room where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A room with attributes.
--
--
--
-- /See:/ 'room' smart constructor.
data Room = Room'
  { _rProfileARN :: !(Maybe Text),
    _rProviderCalendarId :: !(Maybe Text),
    _rRoomARN :: !(Maybe Text),
    _rRoomName :: !(Maybe Text),
    _rDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Room' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rProfileARN' - The profile ARN of a room.
--
-- * 'rProviderCalendarId' - The provider calendar ARN of a room.
--
-- * 'rRoomARN' - The ARN of a room.
--
-- * 'rRoomName' - The name of a room.
--
-- * 'rDescription' - The description of a room.
room ::
  Room
room =
  Room'
    { _rProfileARN = Nothing,
      _rProviderCalendarId = Nothing,
      _rRoomARN = Nothing,
      _rRoomName = Nothing,
      _rDescription = Nothing
    }

-- | The profile ARN of a room.
rProfileARN :: Lens' Room (Maybe Text)
rProfileARN = lens _rProfileARN (\s a -> s {_rProfileARN = a})

-- | The provider calendar ARN of a room.
rProviderCalendarId :: Lens' Room (Maybe Text)
rProviderCalendarId = lens _rProviderCalendarId (\s a -> s {_rProviderCalendarId = a})

-- | The ARN of a room.
rRoomARN :: Lens' Room (Maybe Text)
rRoomARN = lens _rRoomARN (\s a -> s {_rRoomARN = a})

-- | The name of a room.
rRoomName :: Lens' Room (Maybe Text)
rRoomName = lens _rRoomName (\s a -> s {_rRoomName = a})

-- | The description of a room.
rDescription :: Lens' Room (Maybe Text)
rDescription = lens _rDescription (\s a -> s {_rDescription = a})

instance FromJSON Room where
  parseJSON =
    withObject
      "Room"
      ( \x ->
          Room'
            <$> (x .:? "ProfileArn")
            <*> (x .:? "ProviderCalendarId")
            <*> (x .:? "RoomArn")
            <*> (x .:? "RoomName")
            <*> (x .:? "Description")
      )

instance Hashable Room

instance NFData Room
