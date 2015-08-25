{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.GetSendStatistics
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the user\'s sending statistics. The result is a list of data
-- points, representing the last two weeks of sending activity.
--
-- Each data point in the list contains statistics for a 15-minute
-- interval.
--
-- This action is throttled at one request per second.
--
-- /See:/ <http://docs.aws.amazon.com/ses/latest/APIReference/API_GetSendStatistics.html AWS API Reference> for GetSendStatistics.
module Network.AWS.SES.GetSendStatistics
    (
    -- * Creating a Request
      getSendStatistics
    , GetSendStatistics

    -- * Destructuring the Response
    , getSendStatisticsResponse
    , GetSendStatisticsResponse
    -- * Response Lenses
    , gssrsSendDataPoints
    , gssrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SES.Types
import           Network.AWS.SES.Types.Product

-- | /See:/ 'getSendStatistics' smart constructor.
data GetSendStatistics =
    GetSendStatistics'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSendStatistics' with the minimum fields required to make a request.
--
getSendStatistics
    :: GetSendStatistics
getSendStatistics = GetSendStatistics'

instance AWSRequest GetSendStatistics where
        type Rs GetSendStatistics = GetSendStatisticsResponse
        request = postQuery sES
        response
          = receiveXMLWrapper "GetSendStatisticsResult"
              (\ s h x ->
                 GetSendStatisticsResponse' <$>
                   (x .@? "SendDataPoints" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

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

-- | Represents a list of 'SendDataPoint' items returned from a successful
-- 'GetSendStatistics' request. This list contains aggregated data from the
-- previous two weeks of sending activity.
--
-- /See:/ 'getSendStatisticsResponse' smart constructor.
data GetSendStatisticsResponse = GetSendStatisticsResponse'
    { _gssrsSendDataPoints :: !(Maybe [SendDataPoint])
    , _gssrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetSendStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssrsSendDataPoints'
--
-- * 'gssrsStatus'
getSendStatisticsResponse
    :: Int -- ^ 'gssrsStatus'
    -> GetSendStatisticsResponse
getSendStatisticsResponse pStatus_ =
    GetSendStatisticsResponse'
    { _gssrsSendDataPoints = Nothing
    , _gssrsStatus = pStatus_
    }

-- | A list of data points, each of which represents 15 minutes of activity.
gssrsSendDataPoints :: Lens' GetSendStatisticsResponse [SendDataPoint]
gssrsSendDataPoints = lens _gssrsSendDataPoints (\ s a -> s{_gssrsSendDataPoints = a}) . _Default . _Coerce;

-- | The response status code.
gssrsStatus :: Lens' GetSendStatisticsResponse Int
gssrsStatus = lens _gssrsStatus (\ s a -> s{_gssrsStatus = a});
