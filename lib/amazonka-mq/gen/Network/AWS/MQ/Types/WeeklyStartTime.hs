{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MQ.Types.WeeklyStartTime
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MQ.Types.WeeklyStartTime where

import Network.AWS.Lens
import Network.AWS.MQ.Types.DayOfWeek
import Network.AWS.Prelude

-- | The scheduled time period relative to UTC during which Amazon MQ begins to apply pending updates or patches to the broker.
--
-- /See:/ 'weeklyStartTime' smart constructor.
data WeeklyStartTime = WeeklyStartTime'
  { _wstTimeOfDay ::
      !(Maybe Text),
    _wstTimeZone :: !(Maybe Text),
    _wstDayOfWeek :: !(Maybe DayOfWeek)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WeeklyStartTime' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wstTimeOfDay' - Required. The time, in 24-hour format.
--
-- * 'wstTimeZone' - The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
--
-- * 'wstDayOfWeek' - Required. The day of the week.
weeklyStartTime ::
  WeeklyStartTime
weeklyStartTime =
  WeeklyStartTime'
    { _wstTimeOfDay = Nothing,
      _wstTimeZone = Nothing,
      _wstDayOfWeek = Nothing
    }

-- | Required. The time, in 24-hour format.
wstTimeOfDay :: Lens' WeeklyStartTime (Maybe Text)
wstTimeOfDay = lens _wstTimeOfDay (\s a -> s {_wstTimeOfDay = a})

-- | The time zone, UTC by default, in either the Country/City format, or the UTC offset format.
wstTimeZone :: Lens' WeeklyStartTime (Maybe Text)
wstTimeZone = lens _wstTimeZone (\s a -> s {_wstTimeZone = a})

-- | Required. The day of the week.
wstDayOfWeek :: Lens' WeeklyStartTime (Maybe DayOfWeek)
wstDayOfWeek = lens _wstDayOfWeek (\s a -> s {_wstDayOfWeek = a})

instance FromJSON WeeklyStartTime where
  parseJSON =
    withObject
      "WeeklyStartTime"
      ( \x ->
          WeeklyStartTime'
            <$> (x .:? "timeOfDay") <*> (x .:? "timeZone") <*> (x .:? "dayOfWeek")
      )

instance Hashable WeeklyStartTime

instance NFData WeeklyStartTime

instance ToJSON WeeklyStartTime where
  toJSON WeeklyStartTime' {..} =
    object
      ( catMaybes
          [ ("timeOfDay" .=) <$> _wstTimeOfDay,
            ("timeZone" .=) <$> _wstTimeZone,
            ("dayOfWeek" .=) <$> _wstDayOfWeek
          ]
      )
