{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstanceRecurrence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstanceRecurrence where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the recurring schedule for a Scheduled Instance.
--
--
--
-- /See:/ 'scheduledInstanceRecurrence' smart constructor.
data ScheduledInstanceRecurrence = ScheduledInstanceRecurrence'
  { _sirFrequency ::
      !(Maybe Text),
    _sirOccurrenceRelativeToEnd ::
      !(Maybe Bool),
    _sirOccurrenceUnit :: !(Maybe Text),
    _sirInterval :: !(Maybe Int),
    _sirOccurrenceDaySet ::
      !(Maybe [Int])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ScheduledInstanceRecurrence' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sirFrequency' - The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
--
-- * 'sirOccurrenceRelativeToEnd' - Indicates whether the occurrence is relative to the end of the specified week or month.
--
-- * 'sirOccurrenceUnit' - The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
--
-- * 'sirInterval' - The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
--
-- * 'sirOccurrenceDaySet' - The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
scheduledInstanceRecurrence ::
  ScheduledInstanceRecurrence
scheduledInstanceRecurrence =
  ScheduledInstanceRecurrence'
    { _sirFrequency = Nothing,
      _sirOccurrenceRelativeToEnd = Nothing,
      _sirOccurrenceUnit = Nothing,
      _sirInterval = Nothing,
      _sirOccurrenceDaySet = Nothing
    }

-- | The frequency (@Daily@ , @Weekly@ , or @Monthly@ ).
sirFrequency :: Lens' ScheduledInstanceRecurrence (Maybe Text)
sirFrequency = lens _sirFrequency (\s a -> s {_sirFrequency = a})

-- | Indicates whether the occurrence is relative to the end of the specified week or month.
sirOccurrenceRelativeToEnd :: Lens' ScheduledInstanceRecurrence (Maybe Bool)
sirOccurrenceRelativeToEnd = lens _sirOccurrenceRelativeToEnd (\s a -> s {_sirOccurrenceRelativeToEnd = a})

-- | The unit for @occurrenceDaySet@ (@DayOfWeek@ or @DayOfMonth@ ).
sirOccurrenceUnit :: Lens' ScheduledInstanceRecurrence (Maybe Text)
sirOccurrenceUnit = lens _sirOccurrenceUnit (\s a -> s {_sirOccurrenceUnit = a})

-- | The interval quantity. The interval unit depends on the value of @frequency@ . For example, every 2 weeks or every 2 months.
sirInterval :: Lens' ScheduledInstanceRecurrence (Maybe Int)
sirInterval = lens _sirInterval (\s a -> s {_sirInterval = a})

-- | The days. For a monthly schedule, this is one or more days of the month (1-31). For a weekly schedule, this is one or more days of the week (1-7, where 1 is Sunday).
sirOccurrenceDaySet :: Lens' ScheduledInstanceRecurrence [Int]
sirOccurrenceDaySet = lens _sirOccurrenceDaySet (\s a -> s {_sirOccurrenceDaySet = a}) . _Default . _Coerce

instance FromXML ScheduledInstanceRecurrence where
  parseXML x =
    ScheduledInstanceRecurrence'
      <$> (x .@? "frequency")
      <*> (x .@? "occurrenceRelativeToEnd")
      <*> (x .@? "occurrenceUnit")
      <*> (x .@? "interval")
      <*> (x .@? "occurrenceDaySet" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable ScheduledInstanceRecurrence

instance NFData ScheduledInstanceRecurrence
