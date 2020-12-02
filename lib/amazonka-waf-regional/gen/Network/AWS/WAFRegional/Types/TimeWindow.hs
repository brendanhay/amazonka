{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.TimeWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.TimeWindow where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | In a 'GetSampledRequests' request, the @StartTime@ and @EndTime@ objects specify the time range for which you want AWS WAF to return a sample of web requests.
--
--
-- You must specify the times in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ .
--
-- In a 'GetSampledRequests' response, the @StartTime@ and @EndTime@ objects specify the time range for which AWS WAF actually returned a sample of web requests. AWS WAF gets the specified number of requests from among the first 5,000 requests that your AWS resource receives during the specified time period. If your resource receives more than 5,000 requests during that period, AWS WAF stops sampling after the 5,000th request. In that case, @EndTime@ is the time that AWS WAF received the 5,000th request.
--
--
-- /See:/ 'timeWindow' smart constructor.
data TimeWindow = TimeWindow'
  { _twStartTime :: !POSIX,
    _twEndTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TimeWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'twStartTime' - The beginning of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
--
-- * 'twEndTime' - The end of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
timeWindow ::
  -- | 'twStartTime'
  UTCTime ->
  -- | 'twEndTime'
  UTCTime ->
  TimeWindow
timeWindow pStartTime_ pEndTime_ =
  TimeWindow'
    { _twStartTime = _Time # pStartTime_,
      _twEndTime = _Time # pEndTime_
    }

-- | The beginning of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
twStartTime :: Lens' TimeWindow UTCTime
twStartTime = lens _twStartTime (\s a -> s {_twStartTime = a}) . _Time

-- | The end of the time range from which you want @GetSampledRequests@ to return a sample of the requests that your AWS resource received. You must specify the date and time in Coordinated Universal Time (UTC) format. UTC format includes the special designator, @Z@ . For example, @"2016-09-27T14:50Z"@ . You can specify any time range in the previous three hours.
twEndTime :: Lens' TimeWindow UTCTime
twEndTime = lens _twEndTime (\s a -> s {_twEndTime = a}) . _Time

instance FromJSON TimeWindow where
  parseJSON =
    withObject
      "TimeWindow"
      (\x -> TimeWindow' <$> (x .: "StartTime") <*> (x .: "EndTime"))

instance Hashable TimeWindow

instance NFData TimeWindow

instance ToJSON TimeWindow where
  toJSON TimeWindow' {..} =
    object
      ( catMaybes
          [ Just ("StartTime" .= _twStartTime),
            Just ("EndTime" .= _twEndTime)
          ]
      )
