{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.Types.Range
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatch.Types.Range where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies one range of days or times to exclude from use for training an anomaly detection model.
--
--
--
-- /See:/ 'range' smart constructor.
data Range = Range' {_rStartTime :: !ISO8601, _rEndTime :: !ISO8601}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Range' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rStartTime' - The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
--
-- * 'rEndTime' - The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
range ::
  -- | 'rStartTime'
  UTCTime ->
  -- | 'rEndTime'
  UTCTime ->
  Range
range pStartTime_ pEndTime_ =
  Range'
    { _rStartTime = _Time # pStartTime_,
      _rEndTime = _Time # pEndTime_
    }

-- | The start time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
rStartTime :: Lens' Range UTCTime
rStartTime = lens _rStartTime (\s a -> s {_rStartTime = a}) . _Time

-- | The end time of the range to exclude. The format is @yyyy-MM-dd'T'HH:mm:ss@ . For example, @2019-07-01T23:59:59@ .
rEndTime :: Lens' Range UTCTime
rEndTime = lens _rEndTime (\s a -> s {_rEndTime = a}) . _Time

instance FromXML Range where
  parseXML x = Range' <$> (x .@ "StartTime") <*> (x .@ "EndTime")

instance Hashable Range

instance NFData Range

instance ToQuery Range where
  toQuery Range' {..} =
    mconcat ["StartTime" =: _rStartTime, "EndTime" =: _rEndTime]
