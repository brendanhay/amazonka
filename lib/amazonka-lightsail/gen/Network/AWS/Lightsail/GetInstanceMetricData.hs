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
-- Module      : Network.AWS.Lightsail.GetInstanceMetricData
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data points for the specified Amazon Lightsail instance metric, given an instance name.
--
--
module Network.AWS.Lightsail.GetInstanceMetricData
    (
    -- * Creating a Request
      getInstanceMetricData
    , GetInstanceMetricData
    -- * Request Lenses
    , gimdInstanceName
    , gimdMetricName
    , gimdPeriod
    , gimdStartTime
    , gimdEndTime
    , gimdUnit
    , gimdStatistics

    -- * Destructuring the Response
    , getInstanceMetricDataResponse
    , GetInstanceMetricDataResponse
    -- * Response Lenses
    , gimdrsMetricName
    , gimdrsMetricData
    , gimdrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Lightsail.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getInstanceMetricData' smart constructor.
data GetInstanceMetricData = GetInstanceMetricData'
  { _gimdInstanceName :: !Text
  , _gimdMetricName   :: !InstanceMetricName
  , _gimdPeriod       :: !Nat
  , _gimdStartTime    :: !POSIX
  , _gimdEndTime      :: !POSIX
  , _gimdUnit         :: !MetricUnit
  , _gimdStatistics   :: ![MetricStatistic]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gimdInstanceName' - The name of the instance for which you want to get metrics data.
--
-- * 'gimdMetricName' - The metric name to get data about.
--
-- * 'gimdPeriod' - The time period for which you are requesting data.
--
-- * 'gimdStartTime' - The start time of the time period.
--
-- * 'gimdEndTime' - The end time of the time period.
--
-- * 'gimdUnit' - The unit. The list of valid values is below.
--
-- * 'gimdStatistics' - The instance statistics.
getInstanceMetricData
    :: Text -- ^ 'gimdInstanceName'
    -> InstanceMetricName -- ^ 'gimdMetricName'
    -> Natural -- ^ 'gimdPeriod'
    -> UTCTime -- ^ 'gimdStartTime'
    -> UTCTime -- ^ 'gimdEndTime'
    -> MetricUnit -- ^ 'gimdUnit'
    -> GetInstanceMetricData
getInstanceMetricData pInstanceName_ pMetricName_ pPeriod_ pStartTime_ pEndTime_ pUnit_ =
  GetInstanceMetricData'
    { _gimdInstanceName = pInstanceName_
    , _gimdMetricName = pMetricName_
    , _gimdPeriod = _Nat # pPeriod_
    , _gimdStartTime = _Time # pStartTime_
    , _gimdEndTime = _Time # pEndTime_
    , _gimdUnit = pUnit_
    , _gimdStatistics = mempty
    }


-- | The name of the instance for which you want to get metrics data.
gimdInstanceName :: Lens' GetInstanceMetricData Text
gimdInstanceName = lens _gimdInstanceName (\ s a -> s{_gimdInstanceName = a})

-- | The metric name to get data about.
gimdMetricName :: Lens' GetInstanceMetricData InstanceMetricName
gimdMetricName = lens _gimdMetricName (\ s a -> s{_gimdMetricName = a})

-- | The time period for which you are requesting data.
gimdPeriod :: Lens' GetInstanceMetricData Natural
gimdPeriod = lens _gimdPeriod (\ s a -> s{_gimdPeriod = a}) . _Nat

-- | The start time of the time period.
gimdStartTime :: Lens' GetInstanceMetricData UTCTime
gimdStartTime = lens _gimdStartTime (\ s a -> s{_gimdStartTime = a}) . _Time

-- | The end time of the time period.
gimdEndTime :: Lens' GetInstanceMetricData UTCTime
gimdEndTime = lens _gimdEndTime (\ s a -> s{_gimdEndTime = a}) . _Time

-- | The unit. The list of valid values is below.
gimdUnit :: Lens' GetInstanceMetricData MetricUnit
gimdUnit = lens _gimdUnit (\ s a -> s{_gimdUnit = a})

-- | The instance statistics.
gimdStatistics :: Lens' GetInstanceMetricData [MetricStatistic]
gimdStatistics = lens _gimdStatistics (\ s a -> s{_gimdStatistics = a}) . _Coerce

instance AWSRequest GetInstanceMetricData where
        type Rs GetInstanceMetricData =
             GetInstanceMetricDataResponse
        request = postJSON lightsail
        response
          = receiveJSON
              (\ s h x ->
                 GetInstanceMetricDataResponse' <$>
                   (x .?> "metricName") <*>
                     (x .?> "metricData" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable GetInstanceMetricData where

instance NFData GetInstanceMetricData where

instance ToHeaders GetInstanceMetricData where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Lightsail_20161128.GetInstanceMetricData" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetInstanceMetricData where
        toJSON GetInstanceMetricData'{..}
          = object
              (catMaybes
                 [Just ("instanceName" .= _gimdInstanceName),
                  Just ("metricName" .= _gimdMetricName),
                  Just ("period" .= _gimdPeriod),
                  Just ("startTime" .= _gimdStartTime),
                  Just ("endTime" .= _gimdEndTime),
                  Just ("unit" .= _gimdUnit),
                  Just ("statistics" .= _gimdStatistics)])

instance ToPath GetInstanceMetricData where
        toPath = const "/"

instance ToQuery GetInstanceMetricData where
        toQuery = const mempty

-- | /See:/ 'getInstanceMetricDataResponse' smart constructor.
data GetInstanceMetricDataResponse = GetInstanceMetricDataResponse'
  { _gimdrsMetricName     :: !(Maybe InstanceMetricName)
  , _gimdrsMetricData     :: !(Maybe [MetricDatapoint])
  , _gimdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetInstanceMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gimdrsMetricName' - The metric name to return data for.
--
-- * 'gimdrsMetricData' - An array of key-value pairs containing information about the results of your get instance metric data request.
--
-- * 'gimdrsResponseStatus' - -- | The response status code.
getInstanceMetricDataResponse
    :: Int -- ^ 'gimdrsResponseStatus'
    -> GetInstanceMetricDataResponse
getInstanceMetricDataResponse pResponseStatus_ =
  GetInstanceMetricDataResponse'
    { _gimdrsMetricName = Nothing
    , _gimdrsMetricData = Nothing
    , _gimdrsResponseStatus = pResponseStatus_
    }


-- | The metric name to return data for.
gimdrsMetricName :: Lens' GetInstanceMetricDataResponse (Maybe InstanceMetricName)
gimdrsMetricName = lens _gimdrsMetricName (\ s a -> s{_gimdrsMetricName = a})

-- | An array of key-value pairs containing information about the results of your get instance metric data request.
gimdrsMetricData :: Lens' GetInstanceMetricDataResponse [MetricDatapoint]
gimdrsMetricData = lens _gimdrsMetricData (\ s a -> s{_gimdrsMetricData = a}) . _Default . _Coerce

-- | -- | The response status code.
gimdrsResponseStatus :: Lens' GetInstanceMetricDataResponse Int
gimdrsResponseStatus = lens _gimdrsResponseStatus (\ s a -> s{_gimdrsResponseStatus = a})

instance NFData GetInstanceMetricDataResponse where
