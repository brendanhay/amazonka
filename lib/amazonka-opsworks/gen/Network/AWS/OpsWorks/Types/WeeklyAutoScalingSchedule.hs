{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.WeeklyAutoScalingSchedule where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a time-based instance's auto scaling schedule. The schedule consists of a set of key-value pairs.
--
--
--     * The key is the time period (a UTC hour) and must be an integer from 0 - 23.
--
--     * The value indicates whether the instance should be online or offline for the specified period, and must be set to "on" or "off"
--
--
--
-- The default setting for all time periods is off, so you use the following parameters primarily to specify the online periods. You don't have to explicitly specify offline periods unless you want to change an online period to an offline period.
--
-- The following example specifies that the instance should be online for four hours, from UTC 1200 - 1600. It will be off for the remainder of the day.
--
-- @{ "12":"on", "13":"on", "14":"on", "15":"on" } @
--
--
-- /See:/ 'weeklyAutoScalingSchedule' smart constructor.
data WeeklyAutoScalingSchedule = WeeklyAutoScalingSchedule'
  { _wassThursday ::
      !(Maybe (Map Text (Text))),
    _wassWednesday ::
      !(Maybe (Map Text (Text))),
    _wassSaturday ::
      !(Maybe (Map Text (Text))),
    _wassMonday ::
      !(Maybe (Map Text (Text))),
    _wassFriday ::
      !(Maybe (Map Text (Text))),
    _wassSunday ::
      !(Maybe (Map Text (Text))),
    _wassTuesday ::
      !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WeeklyAutoScalingSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wassThursday' - The schedule for Thursday.
--
-- * 'wassWednesday' - The schedule for Wednesday.
--
-- * 'wassSaturday' - The schedule for Saturday.
--
-- * 'wassMonday' - The schedule for Monday.
--
-- * 'wassFriday' - The schedule for Friday.
--
-- * 'wassSunday' - The schedule for Sunday.
--
-- * 'wassTuesday' - The schedule for Tuesday.
weeklyAutoScalingSchedule ::
  WeeklyAutoScalingSchedule
weeklyAutoScalingSchedule =
  WeeklyAutoScalingSchedule'
    { _wassThursday = Nothing,
      _wassWednesday = Nothing,
      _wassSaturday = Nothing,
      _wassMonday = Nothing,
      _wassFriday = Nothing,
      _wassSunday = Nothing,
      _wassTuesday = Nothing
    }

-- | The schedule for Thursday.
wassThursday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassThursday = lens _wassThursday (\s a -> s {_wassThursday = a}) . _Default . _Map

-- | The schedule for Wednesday.
wassWednesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassWednesday = lens _wassWednesday (\s a -> s {_wassWednesday = a}) . _Default . _Map

-- | The schedule for Saturday.
wassSaturday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassSaturday = lens _wassSaturday (\s a -> s {_wassSaturday = a}) . _Default . _Map

-- | The schedule for Monday.
wassMonday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassMonday = lens _wassMonday (\s a -> s {_wassMonday = a}) . _Default . _Map

-- | The schedule for Friday.
wassFriday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassFriday = lens _wassFriday (\s a -> s {_wassFriday = a}) . _Default . _Map

-- | The schedule for Sunday.
wassSunday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassSunday = lens _wassSunday (\s a -> s {_wassSunday = a}) . _Default . _Map

-- | The schedule for Tuesday.
wassTuesday :: Lens' WeeklyAutoScalingSchedule (HashMap Text (Text))
wassTuesday = lens _wassTuesday (\s a -> s {_wassTuesday = a}) . _Default . _Map

instance FromJSON WeeklyAutoScalingSchedule where
  parseJSON =
    withObject
      "WeeklyAutoScalingSchedule"
      ( \x ->
          WeeklyAutoScalingSchedule'
            <$> (x .:? "Thursday" .!= mempty)
            <*> (x .:? "Wednesday" .!= mempty)
            <*> (x .:? "Saturday" .!= mempty)
            <*> (x .:? "Monday" .!= mempty)
            <*> (x .:? "Friday" .!= mempty)
            <*> (x .:? "Sunday" .!= mempty)
            <*> (x .:? "Tuesday" .!= mempty)
      )

instance Hashable WeeklyAutoScalingSchedule

instance NFData WeeklyAutoScalingSchedule

instance ToJSON WeeklyAutoScalingSchedule where
  toJSON WeeklyAutoScalingSchedule' {..} =
    object
      ( catMaybes
          [ ("Thursday" .=) <$> _wassThursday,
            ("Wednesday" .=) <$> _wassWednesday,
            ("Saturday" .=) <$> _wassSaturday,
            ("Monday" .=) <$> _wassMonday,
            ("Friday" .=) <$> _wassFriday,
            ("Sunday" .=) <$> _wassSunday,
            ("Tuesday" .=) <$> _wassTuesday
          ]
      )
