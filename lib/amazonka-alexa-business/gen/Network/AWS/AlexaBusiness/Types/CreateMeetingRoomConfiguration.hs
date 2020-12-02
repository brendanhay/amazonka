{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.CreateMeetingRoomConfiguration where

import Network.AWS.AlexaBusiness.Types.CreateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.CreateInstantBooking
import Network.AWS.AlexaBusiness.Types.CreateRequireCheckIn
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Creates meeting room settings of a room profile.
--
--
--
-- /See:/ 'createMeetingRoomConfiguration' smart constructor.
data CreateMeetingRoomConfiguration = CreateMeetingRoomConfiguration'
  { _cmrcInstantBooking ::
      !(Maybe CreateInstantBooking),
    _cmrcEndOfMeetingReminder ::
      !( Maybe
           CreateEndOfMeetingReminder
       ),
    _cmrcRequireCheckIn ::
      !(Maybe CreateRequireCheckIn),
    _cmrcRoomUtilizationMetricsEnabled ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateMeetingRoomConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmrcInstantBooking' - Settings to automatically book a room for a configured duration if it's free when joining a meeting with Alexa.
--
-- * 'cmrcEndOfMeetingReminder' - Undocumented member.
--
-- * 'cmrcRequireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- * 'cmrcRoomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
createMeetingRoomConfiguration ::
  CreateMeetingRoomConfiguration
createMeetingRoomConfiguration =
  CreateMeetingRoomConfiguration'
    { _cmrcInstantBooking = Nothing,
      _cmrcEndOfMeetingReminder = Nothing,
      _cmrcRequireCheckIn = Nothing,
      _cmrcRoomUtilizationMetricsEnabled = Nothing
    }

-- | Settings to automatically book a room for a configured duration if it's free when joining a meeting with Alexa.
cmrcInstantBooking :: Lens' CreateMeetingRoomConfiguration (Maybe CreateInstantBooking)
cmrcInstantBooking = lens _cmrcInstantBooking (\s a -> s {_cmrcInstantBooking = a})

-- | Undocumented member.
cmrcEndOfMeetingReminder :: Lens' CreateMeetingRoomConfiguration (Maybe CreateEndOfMeetingReminder)
cmrcEndOfMeetingReminder = lens _cmrcEndOfMeetingReminder (\s a -> s {_cmrcEndOfMeetingReminder = a})

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
cmrcRequireCheckIn :: Lens' CreateMeetingRoomConfiguration (Maybe CreateRequireCheckIn)
cmrcRequireCheckIn = lens _cmrcRequireCheckIn (\s a -> s {_cmrcRequireCheckIn = a})

-- | Whether room utilization metrics are enabled or not.
cmrcRoomUtilizationMetricsEnabled :: Lens' CreateMeetingRoomConfiguration (Maybe Bool)
cmrcRoomUtilizationMetricsEnabled = lens _cmrcRoomUtilizationMetricsEnabled (\s a -> s {_cmrcRoomUtilizationMetricsEnabled = a})

instance Hashable CreateMeetingRoomConfiguration

instance NFData CreateMeetingRoomConfiguration

instance ToJSON CreateMeetingRoomConfiguration where
  toJSON CreateMeetingRoomConfiguration' {..} =
    object
      ( catMaybes
          [ ("InstantBooking" .=) <$> _cmrcInstantBooking,
            ("EndOfMeetingReminder" .=) <$> _cmrcEndOfMeetingReminder,
            ("RequireCheckIn" .=) <$> _cmrcRequireCheckIn,
            ("RoomUtilizationMetricsEnabled" .=)
              <$> _cmrcRoomUtilizationMetricsEnabled
          ]
      )
