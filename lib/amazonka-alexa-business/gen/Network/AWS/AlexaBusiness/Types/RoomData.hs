{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.RoomData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.RoomData where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The data of a room.
--
--
--
-- /See:/ 'roomData' smart constructor.
data RoomData = RoomData'
  { _rdProfileARN :: !(Maybe Text),
    _rdProviderCalendarId :: !(Maybe Text),
    _rdProfileName :: !(Maybe Text),
    _rdRoomARN :: !(Maybe Text),
    _rdRoomName :: !(Maybe Text),
    _rdDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RoomData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdProfileARN' - The profile ARN of a room.
--
-- * 'rdProviderCalendarId' - The provider calendar ARN of a room.
--
-- * 'rdProfileName' - The profile name of a room.
--
-- * 'rdRoomARN' - The ARN of a room.
--
-- * 'rdRoomName' - The name of a room.
--
-- * 'rdDescription' - The description of a room.
roomData ::
  RoomData
roomData =
  RoomData'
    { _rdProfileARN = Nothing,
      _rdProviderCalendarId = Nothing,
      _rdProfileName = Nothing,
      _rdRoomARN = Nothing,
      _rdRoomName = Nothing,
      _rdDescription = Nothing
    }

-- | The profile ARN of a room.
rdProfileARN :: Lens' RoomData (Maybe Text)
rdProfileARN = lens _rdProfileARN (\s a -> s {_rdProfileARN = a})

-- | The provider calendar ARN of a room.
rdProviderCalendarId :: Lens' RoomData (Maybe Text)
rdProviderCalendarId = lens _rdProviderCalendarId (\s a -> s {_rdProviderCalendarId = a})

-- | The profile name of a room.
rdProfileName :: Lens' RoomData (Maybe Text)
rdProfileName = lens _rdProfileName (\s a -> s {_rdProfileName = a})

-- | The ARN of a room.
rdRoomARN :: Lens' RoomData (Maybe Text)
rdRoomARN = lens _rdRoomARN (\s a -> s {_rdRoomARN = a})

-- | The name of a room.
rdRoomName :: Lens' RoomData (Maybe Text)
rdRoomName = lens _rdRoomName (\s a -> s {_rdRoomName = a})

-- | The description of a room.
rdDescription :: Lens' RoomData (Maybe Text)
rdDescription = lens _rdDescription (\s a -> s {_rdDescription = a})

instance FromJSON RoomData where
  parseJSON =
    withObject
      "RoomData"
      ( \x ->
          RoomData'
            <$> (x .:? "ProfileArn")
            <*> (x .:? "ProviderCalendarId")
            <*> (x .:? "ProfileName")
            <*> (x .:? "RoomArn")
            <*> (x .:? "RoomName")
            <*> (x .:? "Description")
      )

instance Hashable RoomData

instance NFData RoomData
