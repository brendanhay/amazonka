{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SlotStartTimeRangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotStartTimeRangeRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the time period for a Scheduled Instance to start its first schedule.
--
--
--
-- /See:/ 'slotStartTimeRangeRequest' smart constructor.
data SlotStartTimeRangeRequest = SlotStartTimeRangeRequest'
  { _sstrrLatestTime ::
      !(Maybe ISO8601),
    _sstrrEarliestTime :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotStartTimeRangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sstrrLatestTime' - The latest date and time, in UTC, for the Scheduled Instance to start.
--
-- * 'sstrrEarliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
slotStartTimeRangeRequest ::
  SlotStartTimeRangeRequest
slotStartTimeRangeRequest =
  SlotStartTimeRangeRequest'
    { _sstrrLatestTime = Nothing,
      _sstrrEarliestTime = Nothing
    }

-- | The latest date and time, in UTC, for the Scheduled Instance to start.
sstrrLatestTime :: Lens' SlotStartTimeRangeRequest (Maybe UTCTime)
sstrrLatestTime = lens _sstrrLatestTime (\s a -> s {_sstrrLatestTime = a}) . mapping _Time

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
sstrrEarliestTime :: Lens' SlotStartTimeRangeRequest (Maybe UTCTime)
sstrrEarliestTime = lens _sstrrEarliestTime (\s a -> s {_sstrrEarliestTime = a}) . mapping _Time

instance Hashable SlotStartTimeRangeRequest

instance NFData SlotStartTimeRangeRequest

instance ToQuery SlotStartTimeRangeRequest where
  toQuery SlotStartTimeRangeRequest' {..} =
    mconcat
      [ "LatestTime" =: _sstrrLatestTime,
        "EarliestTime" =: _sstrrEarliestTime
      ]
