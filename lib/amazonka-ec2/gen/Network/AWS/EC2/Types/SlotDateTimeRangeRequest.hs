{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SlotDateTimeRangeRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SlotDateTimeRangeRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the time period for a Scheduled Instance to start its first schedule. The time period must span less than one day.
--
--
--
-- /See:/ 'slotDateTimeRangeRequest' smart constructor.
data SlotDateTimeRangeRequest = SlotDateTimeRangeRequest'
  { _sdtrrEarliestTime ::
      !ISO8601,
    _sdtrrLatestTime :: !ISO8601
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotDateTimeRangeRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdtrrEarliestTime' - The earliest date and time, in UTC, for the Scheduled Instance to start.
--
-- * 'sdtrrLatestTime' - The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
slotDateTimeRangeRequest ::
  -- | 'sdtrrEarliestTime'
  UTCTime ->
  -- | 'sdtrrLatestTime'
  UTCTime ->
  SlotDateTimeRangeRequest
slotDateTimeRangeRequest pEarliestTime_ pLatestTime_ =
  SlotDateTimeRangeRequest'
    { _sdtrrEarliestTime =
        _Time # pEarliestTime_,
      _sdtrrLatestTime = _Time # pLatestTime_
    }

-- | The earliest date and time, in UTC, for the Scheduled Instance to start.
sdtrrEarliestTime :: Lens' SlotDateTimeRangeRequest UTCTime
sdtrrEarliestTime = lens _sdtrrEarliestTime (\s a -> s {_sdtrrEarliestTime = a}) . _Time

-- | The latest date and time, in UTC, for the Scheduled Instance to start. This value must be later than or equal to the earliest date and at most three months in the future.
sdtrrLatestTime :: Lens' SlotDateTimeRangeRequest UTCTime
sdtrrLatestTime = lens _sdtrrLatestTime (\s a -> s {_sdtrrLatestTime = a}) . _Time

instance Hashable SlotDateTimeRangeRequest

instance NFData SlotDateTimeRangeRequest

instance ToQuery SlotDateTimeRangeRequest where
  toQuery SlotDateTimeRangeRequest' {..} =
    mconcat
      [ "EarliestTime" =: _sdtrrEarliestTime,
        "LatestTime" =: _sdtrrLatestTime
      ]
