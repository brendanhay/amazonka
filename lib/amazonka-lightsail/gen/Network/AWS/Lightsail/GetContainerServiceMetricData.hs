{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.GetContainerServiceMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the data points of a specific metric of your Amazon Lightsail container service.
--
--
-- Metrics report the utilization of your resources. Monitor and collect metric data regularly to maintain the reliability, availability, and performance of your resources.
module Network.AWS.Lightsail.GetContainerServiceMetricData
  ( -- * Creating a Request
    getContainerServiceMetricData,
    GetContainerServiceMetricData,

    -- * Request Lenses
    gcsmdServiceName,
    gcsmdMetricName,
    gcsmdStartTime,
    gcsmdEndTime,
    gcsmdPeriod,
    gcsmdStatistics,

    -- * Destructuring the Response
    getContainerServiceMetricDataResponse,
    GetContainerServiceMetricDataResponse,

    -- * Response Lenses
    gcsmdrsMetricName,
    gcsmdrsMetricData,
    gcsmdrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContainerServiceMetricData' smart constructor.
data GetContainerServiceMetricData = GetContainerServiceMetricData'
  { _gcsmdServiceName ::
      !Text,
    _gcsmdMetricName ::
      !ContainerServiceMetricName,
    _gcsmdStartTime :: !POSIX,
    _gcsmdEndTime :: !POSIX,
    _gcsmdPeriod :: !Nat,
    _gcsmdStatistics ::
      ![MetricStatistic]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContainerServiceMetricData' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsmdServiceName' - The name of the container service for which to get metric data.
--
-- * 'gcsmdMetricName' - The metric for which you want to return information. Valid container service metric names are listed below, along with the most useful statistics to include in your request, and the published unit value.     * @CPUUtilization@ - The average percentage of compute units that are currently in use across all nodes of the container service. This metric identifies the processing power required to run containers on each node of the container service. Statistics: The most useful statistics are @Maximum@ and @Average@ . Unit: The published unit is @Percent@ .     * @MemoryUtilization@ - The average percentage of available memory that is currently in use across all nodes of the container service. This metric identifies the memory required to run containers on each node of the container service. Statistics: The most useful statistics are @Maximum@ and @Average@ . Unit: The published unit is @Percent@ .
--
-- * 'gcsmdStartTime' - The start time of the time period.
--
-- * 'gcsmdEndTime' - The end time of the time period.
--
-- * 'gcsmdPeriod' - The granularity, in seconds, of the returned data points. All container service metric data is available in 5-minute (300 seconds) granularity.
--
-- * 'gcsmdStatistics' - The statistic for the metric. The following statistics are available:     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.     * @Average@ - The value of @Sum@ / @SampleCount@ during the specified period. By comparing this statistic with the @Minimum@ and @Maximum@ values, you can determine the full scope of a metric and how close the average use is to the @Minimum@ and @Maximum@ values. This comparison helps you to know when to increase or decrease your resources.     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
getContainerServiceMetricData ::
  -- | 'gcsmdServiceName'
  Text ->
  -- | 'gcsmdMetricName'
  ContainerServiceMetricName ->
  -- | 'gcsmdStartTime'
  UTCTime ->
  -- | 'gcsmdEndTime'
  UTCTime ->
  -- | 'gcsmdPeriod'
  Natural ->
  GetContainerServiceMetricData
getContainerServiceMetricData
  pServiceName_
  pMetricName_
  pStartTime_
  pEndTime_
  pPeriod_ =
    GetContainerServiceMetricData'
      { _gcsmdServiceName = pServiceName_,
        _gcsmdMetricName = pMetricName_,
        _gcsmdStartTime = _Time # pStartTime_,
        _gcsmdEndTime = _Time # pEndTime_,
        _gcsmdPeriod = _Nat # pPeriod_,
        _gcsmdStatistics = mempty
      }

-- | The name of the container service for which to get metric data.
gcsmdServiceName :: Lens' GetContainerServiceMetricData Text
gcsmdServiceName = lens _gcsmdServiceName (\s a -> s {_gcsmdServiceName = a})

-- | The metric for which you want to return information. Valid container service metric names are listed below, along with the most useful statistics to include in your request, and the published unit value.     * @CPUUtilization@ - The average percentage of compute units that are currently in use across all nodes of the container service. This metric identifies the processing power required to run containers on each node of the container service. Statistics: The most useful statistics are @Maximum@ and @Average@ . Unit: The published unit is @Percent@ .     * @MemoryUtilization@ - The average percentage of available memory that is currently in use across all nodes of the container service. This metric identifies the memory required to run containers on each node of the container service. Statistics: The most useful statistics are @Maximum@ and @Average@ . Unit: The published unit is @Percent@ .
gcsmdMetricName :: Lens' GetContainerServiceMetricData ContainerServiceMetricName
gcsmdMetricName = lens _gcsmdMetricName (\s a -> s {_gcsmdMetricName = a})

-- | The start time of the time period.
gcsmdStartTime :: Lens' GetContainerServiceMetricData UTCTime
gcsmdStartTime = lens _gcsmdStartTime (\s a -> s {_gcsmdStartTime = a}) . _Time

-- | The end time of the time period.
gcsmdEndTime :: Lens' GetContainerServiceMetricData UTCTime
gcsmdEndTime = lens _gcsmdEndTime (\s a -> s {_gcsmdEndTime = a}) . _Time

-- | The granularity, in seconds, of the returned data points. All container service metric data is available in 5-minute (300 seconds) granularity.
gcsmdPeriod :: Lens' GetContainerServiceMetricData Natural
gcsmdPeriod = lens _gcsmdPeriod (\s a -> s {_gcsmdPeriod = a}) . _Nat

-- | The statistic for the metric. The following statistics are available:     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.     * @Average@ - The value of @Sum@ / @SampleCount@ during the specified period. By comparing this statistic with the @Minimum@ and @Maximum@ values, you can determine the full scope of a metric and how close the average use is to the @Minimum@ and @Maximum@ values. This comparison helps you to know when to increase or decrease your resources.     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
gcsmdStatistics :: Lens' GetContainerServiceMetricData [MetricStatistic]
gcsmdStatistics = lens _gcsmdStatistics (\s a -> s {_gcsmdStatistics = a}) . _Coerce

instance AWSRequest GetContainerServiceMetricData where
  type
    Rs GetContainerServiceMetricData =
      GetContainerServiceMetricDataResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetContainerServiceMetricDataResponse'
            <$> (x .?> "metricName")
            <*> (x .?> "metricData" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable GetContainerServiceMetricData

instance NFData GetContainerServiceMetricData

instance ToHeaders GetContainerServiceMetricData where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetContainerServiceMetricData" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetContainerServiceMetricData where
  toJSON GetContainerServiceMetricData' {..} =
    object
      ( catMaybes
          [ Just ("serviceName" .= _gcsmdServiceName),
            Just ("metricName" .= _gcsmdMetricName),
            Just ("startTime" .= _gcsmdStartTime),
            Just ("endTime" .= _gcsmdEndTime),
            Just ("period" .= _gcsmdPeriod),
            Just ("statistics" .= _gcsmdStatistics)
          ]
      )

instance ToPath GetContainerServiceMetricData where
  toPath = const "/"

instance ToQuery GetContainerServiceMetricData where
  toQuery = const mempty

-- | /See:/ 'getContainerServiceMetricDataResponse' smart constructor.
data GetContainerServiceMetricDataResponse = GetContainerServiceMetricDataResponse'
  { _gcsmdrsMetricName ::
      !( Maybe
           ContainerServiceMetricName
       ),
    _gcsmdrsMetricData ::
      !( Maybe
           [MetricDatapoint]
       ),
    _gcsmdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContainerServiceMetricDataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcsmdrsMetricName' - The name of the metric returned.
--
-- * 'gcsmdrsMetricData' - An array of objects that describe the metric data returned.
--
-- * 'gcsmdrsResponseStatus' - -- | The response status code.
getContainerServiceMetricDataResponse ::
  -- | 'gcsmdrsResponseStatus'
  Int ->
  GetContainerServiceMetricDataResponse
getContainerServiceMetricDataResponse pResponseStatus_ =
  GetContainerServiceMetricDataResponse'
    { _gcsmdrsMetricName =
        Nothing,
      _gcsmdrsMetricData = Nothing,
      _gcsmdrsResponseStatus = pResponseStatus_
    }

-- | The name of the metric returned.
gcsmdrsMetricName :: Lens' GetContainerServiceMetricDataResponse (Maybe ContainerServiceMetricName)
gcsmdrsMetricName = lens _gcsmdrsMetricName (\s a -> s {_gcsmdrsMetricName = a})

-- | An array of objects that describe the metric data returned.
gcsmdrsMetricData :: Lens' GetContainerServiceMetricDataResponse [MetricDatapoint]
gcsmdrsMetricData = lens _gcsmdrsMetricData (\s a -> s {_gcsmdrsMetricData = a}) . _Default . _Coerce

-- | -- | The response status code.
gcsmdrsResponseStatus :: Lens' GetContainerServiceMetricDataResponse Int
gcsmdrsResponseStatus = lens _gcsmdrsResponseStatus (\s a -> s {_gcsmdrsResponseStatus = a})

instance NFData GetContainerServiceMetricDataResponse
