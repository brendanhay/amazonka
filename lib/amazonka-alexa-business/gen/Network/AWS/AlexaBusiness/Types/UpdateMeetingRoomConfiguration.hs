{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.UpdateMeetingRoomConfiguration where

import Network.AWS.AlexaBusiness.Types.UpdateEndOfMeetingReminder
import Network.AWS.AlexaBusiness.Types.UpdateInstantBooking
import Network.AWS.AlexaBusiness.Types.UpdateRequireCheckIn
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Updates meeting room settings of a room profile.
--
--
--
-- /See:/ 'updateMeetingRoomConfiguration' smart constructor.
data UpdateMeetingRoomConfiguration = UpdateMeetingRoomConfiguration'
  { _umrcInstantBooking ::
      !(Maybe UpdateInstantBooking),
    _umrcEndOfMeetingReminder ::
      !( Maybe
           UpdateEndOfMeetingReminder
       ),
    _umrcRequireCheckIn ::
      !(Maybe UpdateRequireCheckIn),
    _umrcRoomUtilizationMetricsEnabled ::
      !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateMeetingRoomConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umrcInstantBooking' - Settings to automatically book an available room available for a configured duration when joining a meeting with Alexa.
--
-- * 'umrcEndOfMeetingReminder' - Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
--
-- * 'umrcRequireCheckIn' - Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
--
-- * 'umrcRoomUtilizationMetricsEnabled' - Whether room utilization metrics are enabled or not.
updateMeetingRoomConfiguration ::
  UpdateMeetingRoomConfiguration
updateMeetingRoomConfiguration =
  UpdateMeetingRoomConfiguration'
    { _umrcInstantBooking = Nothing,
      _umrcEndOfMeetingReminder = Nothing,
      _umrcRequireCheckIn = Nothing,
      _umrcRoomUtilizationMetricsEnabled = Nothing
    }

-- | Settings to automatically book an available room available for a configured duration when joining a meeting with Alexa.
umrcInstantBooking :: Lens' UpdateMeetingRoomConfiguration (Maybe UpdateInstantBooking)
umrcInstantBooking = lens _umrcInstantBooking (\s a -> s {_umrcInstantBooking = a})

-- | Settings for the end of meeting reminder feature that are applied to a room profile. The end of meeting reminder enables Alexa to remind users when a meeting is ending.
umrcEndOfMeetingReminder :: Lens' UpdateMeetingRoomConfiguration (Maybe UpdateEndOfMeetingReminder)
umrcEndOfMeetingReminder = lens _umrcEndOfMeetingReminder (\s a -> s {_umrcEndOfMeetingReminder = a})

-- | Settings for requiring a check in when a room is reserved. Alexa can cancel a room reservation if it's not checked into to make the room available for others. Users can check in by joining the meeting with Alexa or an AVS device, or by saying “Alexa, check in.”
umrcRequireCheckIn :: Lens' UpdateMeetingRoomConfiguration (Maybe UpdateRequireCheckIn)
umrcRequireCheckIn = lens _umrcRequireCheckIn (\s a -> s {_umrcRequireCheckIn = a})

-- | Whether room utilization metrics are enabled or not.
umrcRoomUtilizationMetricsEnabled :: Lens' UpdateMeetingRoomConfiguration (Maybe Bool)
umrcRoomUtilizationMetricsEnabled = lens _umrcRoomUtilizationMetricsEnabled (\s a -> s {_umrcRoomUtilizationMetricsEnabled = a})

instance Hashable UpdateMeetingRoomConfiguration

instance NFData UpdateMeetingRoomConfiguration

instance ToJSON UpdateMeetingRoomConfiguration where
  toJSON UpdateMeetingRoomConfiguration' {..} =
    object
      ( catMaybes
          [ ("InstantBooking" .=) <$> _umrcInstantBooking,
            ("EndOfMeetingReminder" .=) <$> _umrcEndOfMeetingReminder,
            ("RequireCheckIn" .=) <$> _umrcRequireCheckIn,
            ("RoomUtilizationMetricsEnabled" .=)
              <$> _umrcRoomUtilizationMetricsEnabled
          ]
      )
