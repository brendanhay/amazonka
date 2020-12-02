{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.TimeRangeFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.TimeRangeFilter where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Filters update actions from the service updates that are in available status during the time range.
--
--
--
-- /See:/ 'timeRangeFilter' smart constructor.
data TimeRangeFilter = TimeRangeFilter'
  { _trfStartTime ::
      !(Maybe ISO8601),
    _trfEndTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeRangeFilter' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'trfStartTime' - The start time of the time range filter
--
-- * 'trfEndTime' - The end time of the time range filter
timeRangeFilter ::
  TimeRangeFilter
timeRangeFilter =
  TimeRangeFilter' {_trfStartTime = Nothing, _trfEndTime = Nothing}

-- | The start time of the time range filter
trfStartTime :: Lens' TimeRangeFilter (Maybe UTCTime)
trfStartTime = lens _trfStartTime (\s a -> s {_trfStartTime = a}) . mapping _Time

-- | The end time of the time range filter
trfEndTime :: Lens' TimeRangeFilter (Maybe UTCTime)
trfEndTime = lens _trfEndTime (\s a -> s {_trfEndTime = a}) . mapping _Time

instance Hashable TimeRangeFilter

instance NFData TimeRangeFilter

instance ToQuery TimeRangeFilter where
  toQuery TimeRangeFilter' {..} =
    mconcat ["StartTime" =: _trfStartTime, "EndTime" =: _trfEndTime]
