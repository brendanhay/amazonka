{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.Schedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.Schedule where

import Network.AWS.Lens
import Network.AWS.Pinpoint.Types.CampaignEventFilter
import Network.AWS.Pinpoint.Types.Frequency
import Network.AWS.Pinpoint.Types.QuietTime
import Network.AWS.Prelude

-- | Specifies the schedule settings for a campaign.
--
--
--
-- /See:/ 'schedule' smart constructor.
data Schedule = Schedule'
  { _sFrequency :: !(Maybe Frequency),
    _sQuietTime :: !(Maybe QuietTime),
    _sEventFilter :: !(Maybe CampaignEventFilter),
    _sIsLocalTime :: !(Maybe Bool),
    _sEndTime :: !(Maybe Text),
    _sTimezone :: !(Maybe Text),
    _sStartTime :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Schedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sFrequency' - Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
--
-- * 'sQuietTime' - The default quiet time for the campaign. Quiet time is a specific time range when a campaign doesn't send messages to endpoints, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the campaign.     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the campaign. If any of the preceding conditions isn't met, the endpoint will receive messages from the campaign, even if quiet time is enabled.
--
-- * 'sEventFilter' - The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
--
-- * 'sIsLocalTime' - Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
--
-- * 'sEndTime' - The scheduled time, in ISO 8601 format, when the campaign ended or will end.
--
-- * 'sTimezone' - The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
--
-- * 'sStartTime' - The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
schedule ::
  -- | 'sStartTime'
  Text ->
  Schedule
schedule pStartTime_ =
  Schedule'
    { _sFrequency = Nothing,
      _sQuietTime = Nothing,
      _sEventFilter = Nothing,
      _sIsLocalTime = Nothing,
      _sEndTime = Nothing,
      _sTimezone = Nothing,
      _sStartTime = pStartTime_
    }

-- | Specifies how often the campaign is sent or whether the campaign is sent in response to a specific event.
sFrequency :: Lens' Schedule (Maybe Frequency)
sFrequency = lens _sFrequency (\s a -> s {_sFrequency = a})

-- | The default quiet time for the campaign. Quiet time is a specific time range when a campaign doesn't send messages to endpoints, if all the following conditions are met:     * The EndpointDemographic.Timezone property of the endpoint is set to a valid value.     * The current time in the endpoint's time zone is later than or equal to the time specified by the QuietTime.Start property for the campaign.     * The current time in the endpoint's time zone is earlier than or equal to the time specified by the QuietTime.End property for the campaign. If any of the preceding conditions isn't met, the endpoint will receive messages from the campaign, even if quiet time is enabled.
sQuietTime :: Lens' Schedule (Maybe QuietTime)
sQuietTime = lens _sQuietTime (\s a -> s {_sQuietTime = a})

-- | The type of event that causes the campaign to be sent, if the value of the Frequency property is EVENT.
sEventFilter :: Lens' Schedule (Maybe CampaignEventFilter)
sEventFilter = lens _sEventFilter (\s a -> s {_sEventFilter = a})

-- | Specifies whether the start and end times for the campaign schedule use each recipient's local time. To base the schedule on each recipient's local time, set this value to true.
sIsLocalTime :: Lens' Schedule (Maybe Bool)
sIsLocalTime = lens _sIsLocalTime (\s a -> s {_sIsLocalTime = a})

-- | The scheduled time, in ISO 8601 format, when the campaign ended or will end.
sEndTime :: Lens' Schedule (Maybe Text)
sEndTime = lens _sEndTime (\s a -> s {_sEndTime = a})

-- | The starting UTC offset for the campaign schedule, if the value of the IsLocalTime property is true. Valid values are: UTC, UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05,                   UTC+05:30, UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+09, UTC+09:30,                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+13, UTC-02, UTC-03, UTC-04, UTC-05, UTC-06,                   UTC-07, UTC-08, UTC-09, UTC-10, and UTC-11.
sTimezone :: Lens' Schedule (Maybe Text)
sTimezone = lens _sTimezone (\s a -> s {_sTimezone = a})

-- | The scheduled time when the campaign began or will begin. Valid values are: IMMEDIATE, to start the campaign immediately; or, a specific time in ISO 8601 format.
sStartTime :: Lens' Schedule Text
sStartTime = lens _sStartTime (\s a -> s {_sStartTime = a})

instance FromJSON Schedule where
  parseJSON =
    withObject
      "Schedule"
      ( \x ->
          Schedule'
            <$> (x .:? "Frequency")
            <*> (x .:? "QuietTime")
            <*> (x .:? "EventFilter")
            <*> (x .:? "IsLocalTime")
            <*> (x .:? "EndTime")
            <*> (x .:? "Timezone")
            <*> (x .: "StartTime")
      )

instance Hashable Schedule

instance NFData Schedule

instance ToJSON Schedule where
  toJSON Schedule' {..} =
    object
      ( catMaybes
          [ ("Frequency" .=) <$> _sFrequency,
            ("QuietTime" .=) <$> _sQuietTime,
            ("EventFilter" .=) <$> _sEventFilter,
            ("IsLocalTime" .=) <$> _sIsLocalTime,
            ("EndTime" .=) <$> _sEndTime,
            ("Timezone" .=) <$> _sTimezone,
            Just ("StartTime" .= _sStartTime)
          ]
      )
