{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceRecurrenceRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the recurring schedule for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstanceRecurrenceRequest' smart constructor.
data ScheduledInstanceRecurrenceRequest = ScheduledInstanceRecurrenceRequest'
  { _sirrFrequency ::
      !(Maybe Text),
    _sirrOccurrenceRelativeToEnd ::
      !(Maybe Bool),
    _sirrOccurrenceDays ::
      !(Maybe [Int]),
    _sirrOccurrenceUnit ::
      !(Maybe Text),
    _sirrInterval ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstanceRecurrenceRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirrFrequency' - The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- * 'sirrOccurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
--
-- * 'sirrOccurrenceDays' - The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
--
-- * 'sirrOccurrenceUnit' - The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
--
-- * 'sirrInterval' - The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
scheduledInstanceRecurrenceRequest ::
  ScheduledInstanceRecurrenceRequest
scheduledInstanceRecurrenceRequest =
  ScheduledInstanceRecurrenceRequest'
    { _sirrFrequency = Nothing,
      _sirrOccurrenceRelativeToEnd = Nothing,
      _sirrOccurrenceDays = Nothing,
      _sirrOccurrenceUnit = Nothing,
      _sirrInterval = Nothing
    }

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
sirrFrequency :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Text)
sirrFrequency = lens _sirrFrequency (\s a -> s {_sirrFrequency = a})

-- | Indicates whether the occurrence is relative to the end of the specified week or month. You can't specify this value with a daily schedule.
sirrOccurrenceRelativeToEnd :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Bool)
sirrOccurrenceRelativeToEnd = lens _sirrOccurrenceRelativeToEnd (\s a -> s {_sirrOccurrenceRelativeToEnd = a})

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday). You can't specify this value with a daily schedule. If the occurrence is relative to the end of the month, you can specify only a single day.
sirrOccurrenceDays :: Lens' ScheduledInstanceRecurrenceRequest [Int]
sirrOccurrenceDays = lens _sirrOccurrenceDays (\s a -> s {_sirrOccurrenceDays = a}) . _Default . _Coerce

-- | The unit for @OccurrenceDays@ (@DayOfWeek@ or @DayOfMonth@ ). This value is required for a monthly schedule. You can't specify @DayOfWeek@ with a weekly schedule. You can't specify this value with a daily schedule.
sirrOccurrenceUnit :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Text)
sirrOccurrenceUnit = lens _sirrOccurrenceUnit (\s a -> s {_sirrOccurrenceUnit = a})

-- | The interval quantity. The interval unit depends on the value of @Frequency@ . For example, every 2 weeks or every 2 months.
sirrInterval :: Lens' ScheduledInstanceRecurrenceRequest (Maybe Int)
sirrInterval = lens _sirrInterval (\s a -> s {_sirrInterval = a})

instance Hashable ScheduledInstanceRecurrenceRequest

instance NFData ScheduledInstanceRecurrenceRequest

instance ToQuery ScheduledInstanceRecurrenceRequest where
  toQuery ScheduledInstanceRecurrenceRequest' {..} =
    mconcat
      [ "Frequency" =: _sirrFrequency,
        "OccurrenceRelativeToEnd" =: _sirrOccurrenceRelativeToEnd,
        toQuery (toQueryList "OccurrenceDay" <$> _sirrOccurrenceDays),
        "OccurrenceUnit" =: _sirrOccurrenceUnit,
        "Interval" =: _sirrInterval
      ]
