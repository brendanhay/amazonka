{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the user\'s sending statistics. The result is a list of data
-- points, representing the last two weeks of sending activity.
--
-- Each data point in the list contains statistics for a 15-minute
-- interval.
--
-- This action is throttled at one request per second.
--
-- <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetSendStatistics.html>
module Network.AWS.SES.GetSendStatistics
    (
    -- * Request
      GetSendStatistics
    -- ** Request constructor
    , getSendStatistics

    -- * Response
    , GetSendStatisticsResponse
    -- ** Response constructor
    , getSendStatisticsResponse
    -- ** Response lenses
    , gssrSendDataPoints
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.SES.Types

-- | /See:/ 'getSendStatistics' smart constructor.
data GetSendStatistics = GetSendStatistics' deriving (Eq, Read, Show)

-- | 'GetSendStatistics' smart constructor.
getSendStatistics :: GetSendStatistics
getSendStatistics = GetSendStatistics';

instance AWSRequest GetSendStatistics where
        type Sv GetSendStatistics = SES
        type Rs GetSendStatistics = GetSendStatisticsResponse
        request = post
        response
          = receiveXMLWrapper "GetSendStatisticsResult"
              (\ s h x ->
                 GetSendStatisticsResponse' <$>
                   (x .@? "SendDataPoints" .!@ mempty >>=
                      parseXMLList "member"))

instance ToHeaders GetSendStatistics where
        toHeaders = const mempty

instance ToPath GetSendStatistics where
        toPath = const "/"

instance ToQuery GetSendStatistics where
        toQuery
          = const
              (mconcat
                 ["Action" =: ("GetSendStatistics" :: ByteString),
                  "Version" =: ("2010-12-01" :: ByteString)])

-- | /See:/ 'getSendStatisticsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gssrSendDataPoints'
newtype GetSendStatisticsResponse = GetSendStatisticsResponse'{_gssrSendDataPoints :: [SendDataPoint]} deriving (Eq, Read, Show)

-- | 'GetSendStatisticsResponse' smart constructor.
getSendStatisticsResponse :: GetSendStatisticsResponse
getSendStatisticsResponse = GetSendStatisticsResponse'{_gssrSendDataPoints = mempty};

-- | A list of data points, each of which represents 15 minutes of activity.
gssrSendDataPoints :: Lens' GetSendStatisticsResponse [SendDataPoint]
gssrSendDataPoints = lens _gssrSendDataPoints (\ s a -> s{_gssrSendDataPoints = a});
