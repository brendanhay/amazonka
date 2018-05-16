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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides sending statistics for the Amazon SES account. The result is a list of data points, representing the last two weeks of sending activity. Each data point in the list contains statistics for a 15-minute period of time.
--
--
-- You can execute this operation no more than once per second.
--
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
    , gssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SES.Types
import Network.AWS.SES.Types.Product

-- | /See:/ 'getSendStatistics' smart constructor.
data GetSendStatistics =
  GetSendStatistics'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSendStatistics' with the minimum fields required to make a request.
--
getSendStatistics
    :: GetSendStatistics
getSendStatistics = GetSendStatistics'


instance AWSRequest GetSendStatistics where
        type Rs GetSendStatistics = GetSendStatisticsResponse
        request = postQuery ses
        response
          = receiveXMLWrapper "GetSendStatisticsResult"
              (\ s h x ->
                 GetSendStatisticsResponse' <$>
                   (x .@? "SendDataPoints" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable GetSendStatistics where

instance NFData GetSendStatistics where

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

-- | Represents a list of data points. This list contains aggregated data from the previous two weeks of your sending activity with Amazon SES.
--
--
--
-- /See:/ 'getSendStatisticsResponse' smart constructor.
data GetSendStatisticsResponse = GetSendStatisticsResponse'
  { _gssrsSendDataPoints :: !(Maybe [SendDataPoint])
  , _gssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetSendStatisticsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gssrsSendDataPoints' - A list of data points, each of which represents 15 minutes of activity.
--
-- * 'gssrsResponseStatus' - -- | The response status code.
getSendStatisticsResponse
    :: Int -- ^ 'gssrsResponseStatus'
    -> GetSendStatisticsResponse
getSendStatisticsResponse pResponseStatus_ =
  GetSendStatisticsResponse'
    {_gssrsSendDataPoints = Nothing, _gssrsResponseStatus = pResponseStatus_}


-- | A list of data points, each of which represents 15 minutes of activity.
gssrsSendDataPoints :: Lens' GetSendStatisticsResponse [SendDataPoint]
gssrsSendDataPoints = lens _gssrsSendDataPoints (\ s a -> s{_gssrsSendDataPoints = a}) . _Default . _Coerce

-- | -- | The response status code.
gssrsResponseStatus :: Lens' GetSendStatisticsResponse Int
gssrsResponseStatus = lens _gssrsResponseStatus (\ s a -> s{_gssrsResponseStatus = a})

instance NFData GetSendStatisticsResponse where
