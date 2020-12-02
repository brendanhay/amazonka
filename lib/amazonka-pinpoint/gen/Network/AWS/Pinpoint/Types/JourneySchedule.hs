{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JourneySchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JourneySchedule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the schedule settings for a journey.
--
--
--
-- /See:/ 'journeySchedule' smart constructor.
data JourneySchedule = JourneySchedule'
  { _jsStartTime ::
      !(Maybe POSIX),
    _jsEndTime :: !(Maybe POSIX),
    _jsTimezone :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JourneySchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jsStartTime' - The scheduled time, in ISO 8601 format, when the journey began or will begin.
--
-- * 'jsEndTime' - The scheduled time, in ISO 8601 format, when the journey ended or will end.
--
-- * 'jsTimezone' - The starting UTC offset for the journey schedule, if the value of the journey's LocalTime property is true. Valid values are: UTC,                   UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,                   UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09, UTC+09:30,                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13, UTC+13:45, UTC-02,                   UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06, UTC-07, UTC-08, UTC-09,                   UTC-09:30, UTC-10, and UTC-11.
journeySchedule ::
  JourneySchedule
journeySchedule =
  JourneySchedule'
    { _jsStartTime = Nothing,
      _jsEndTime = Nothing,
      _jsTimezone = Nothing
    }

-- | The scheduled time, in ISO 8601 format, when the journey began or will begin.
jsStartTime :: Lens' JourneySchedule (Maybe UTCTime)
jsStartTime = lens _jsStartTime (\s a -> s {_jsStartTime = a}) . mapping _Time

-- | The scheduled time, in ISO 8601 format, when the journey ended or will end.
jsEndTime :: Lens' JourneySchedule (Maybe UTCTime)
jsEndTime = lens _jsEndTime (\s a -> s {_jsEndTime = a}) . mapping _Time

-- | The starting UTC offset for the journey schedule, if the value of the journey's LocalTime property is true. Valid values are: UTC,                   UTC+01, UTC+02, UTC+03, UTC+03:30, UTC+04, UTC+04:30, UTC+05, UTC+05:30,                   UTC+05:45, UTC+06, UTC+06:30, UTC+07, UTC+08, UTC+08:45, UTC+09, UTC+09:30,                   UTC+10, UTC+10:30, UTC+11, UTC+12, UTC+12:45, UTC+13, UTC+13:45, UTC-02,                   UTC-02:30, UTC-03, UTC-03:30, UTC-04, UTC-05, UTC-06, UTC-07, UTC-08, UTC-09,                   UTC-09:30, UTC-10, and UTC-11.
jsTimezone :: Lens' JourneySchedule (Maybe Text)
jsTimezone = lens _jsTimezone (\s a -> s {_jsTimezone = a})

instance FromJSON JourneySchedule where
  parseJSON =
    withObject
      "JourneySchedule"
      ( \x ->
          JourneySchedule'
            <$> (x .:? "StartTime") <*> (x .:? "EndTime") <*> (x .:? "Timezone")
      )

instance Hashable JourneySchedule

instance NFData JourneySchedule

instance ToJSON JourneySchedule where
  toJSON JourneySchedule' {..} =
    object
      ( catMaybes
          [ ("StartTime" .=) <$> _jsStartTime,
            ("EndTime" .=) <$> _jsEndTime,
            ("Timezone" .=) <$> _jsTimezone
          ]
      )
