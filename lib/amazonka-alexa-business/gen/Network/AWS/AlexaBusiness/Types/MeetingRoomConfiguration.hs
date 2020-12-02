{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.MeetingRoomConfiguration where

import Network.AWS.AlexaBusiness.Types.EndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.InstantBooking
import Network.AWS.AlexaBusiness.Types.RequireCheckIn
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Meeting room settings of a room profile.
--
--
--
-- /See:/ 'meetingRoomConfiguration' smart constructor.
data MeetingRoomConfiguration = MeetingRoomConfiguration'
  { _mrcInstantBooking ::
      !(Maybe InstantBooking),
    _mrcEndOfMeetingReminder ::
      !(Maybe EndOfMeetingReminder),
    _mrcRequireCheckIn ::
      !(Maybe RequireCheckIn),
    _mrcRoomUtilizationMetricsEnabled ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MeetingRoomConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrcInstantBooking' - Settings to automatically book the room if available for a configured duration when joining a meeting with Alexa.
--
-- * 'mrcEndOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- * 'mrcRequireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into. This makes the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- * 'mrcRoomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
meetingRoomConfiguration ::
  MeetingRoomConfiguration
meetingRoomConfiguration =
  MeetingRoomConfiguration'
    { _mrcInstantBooking = Nothing,
      _mrcEndOfMeetingReminder = Nothing,
      _mrcRequireCheckIn = Nothing,
      _mrcRoomUtilizationMetricsEnabled = Nothing
    }

-- | Settings to automatically book the room if available for a configured duration when joining a meeting with Alexa.
mrcInstantBooking :: Lens' MeetingRoomConfiguration (Maybe InstantBooking)
mrcInstantBooking = lens _mrcInstantBooking (\s a -> s {_mrcInstantBooking = a})

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
mrcEndOfMeetingReminder :: Lens' MeetingRoomConfiguration (Maybe EndOfMeetingReminder)
mrcEndOfMeetingReminder = lens _mrcEndOfMeetingReminder (\s a -> s {_mrcEndOfMeetingReminder = a})

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into. This makes the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
mrcRequireCheckIn :: Lens' MeetingRoomConfiguration (Maybe RequireCheckIn)
mrcRequireCheckIn = lens _mrcRequireCheckIn (\s a -> s {_mrcRequireCheckIn = a})

-- | Whether room utilization metrics are enabled or not.
mrcRoomUtilizationMetricsEnabled :: Lens' MeetingRoomConfiguration (Maybe Bool)
mrcRoomUtilizationMetricsEnabled = lens _mrcRoomUtilizationMetricsEnabled (\s a -> s {_mrcRoomUtilizationMetricsEnabled = a})

instance FromJSON MeetingRoomConfiguration where
  parseJSON =
    withObject
      "MeetingRoomConfiguration"
      ( \x ->
          MeetingRoomConfiguration'
            <$> (x .:? "InstantBooking")
            <*> (x .:? "EndOfMeetingReminder")
            <*> (x .:? "RequireCheckIn")
            <*> (x .:? "RoomUtilizationMetricsEnabled")
      )

instance Hashable MeetingRoomConfiguration

instance NFData MeetingRoomConfiguration
