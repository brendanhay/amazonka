{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AWSHealth.Types.DateTimeRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AWSHealth.Types.DateTimeRange where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A range of dates and times that is used by the <https://docs.aws.amazon.com/health/latest/APIReference/API_EventFilter.html EventFilter> and <https://docs.aws.amazon.com/health/latest/APIReference/API_EntityFilter.html EntityFilter> objects. If @from@ is set and @to@ is set: match items where the timestamp (@startTime@ , @endTime@ , or @lastUpdatedTime@ ) is between @from@ and @to@ inclusive. If @from@ is set and @to@ is not set: match items where the timestamp value is equal to or after @from@ . If @from@ is not set and @to@ is set: match items where the timestamp value is equal to or before @to@ .
--
--
--
-- /See:/ 'dateTimeRange' smart constructor.
data DateTimeRange = DateTimeRange'
  { _dtrTo :: !(Maybe POSIX),
    _dtrFrom :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DateTimeRange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrTo' - The ending date and time of a time range.
--
-- * 'dtrFrom' - The starting date and time of a time range.
dateTimeRange ::
  DateTimeRange
dateTimeRange =
  DateTimeRange' {_dtrTo = Nothing, _dtrFrom = Nothing}

-- | The ending date and time of a time range.
dtrTo :: Lens' DateTimeRange (Maybe UTCTime)
dtrTo = lens _dtrTo (\s a -> s {_dtrTo = a}) . mapping _Time

-- | The starting date and time of a time range.
dtrFrom :: Lens' DateTimeRange (Maybe UTCTime)
dtrFrom = lens _dtrFrom (\s a -> s {_dtrFrom = a}) . mapping _Time

instance Hashable DateTimeRange

instance NFData DateTimeRange

instance ToJSON DateTimeRange where
  toJSON DateTimeRange' {..} =
    object
      (catMaybes [("to" .=) <$> _dtrTo, ("from" .=) <$> _dtrFrom])
