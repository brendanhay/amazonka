{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- Metrics report the utilization of your resources. Monitor and collect metric data regularly to maintain the reliability, availability, and performance of your resources.
module Network.AWS.Lightsail.GetContainerServiceMetricData
  ( -- * Creating a request
    GetContainerServiceMetricData (..),
    mkGetContainerServiceMetricData,

    -- ** Request lenses
    gcsmdStartTime,
    gcsmdPeriod,
    gcsmdMetricName,
    gcsmdEndTime,
    gcsmdServiceName,
    gcsmdStatistics,

    -- * Destructuring the response
    GetContainerServiceMetricDataResponse (..),
    mkGetContainerServiceMetricDataResponse,

    -- ** Response lenses
    gcsmdrsMetricName,
    gcsmdrsMetricData,
    gcsmdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetContainerServiceMetricData' smart constructor.
data GetContainerServiceMetricData = GetContainerServiceMetricData'
  { -- | The start time of the time period.
    startTime :: Lude.Timestamp,
    -- | The granularity, in seconds, of the returned data points.
    --
    -- All container service metric data is available in 5-minute (300 seconds) granularity.
    period :: Lude.Natural,
    -- | The metric for which you want to return information.
    --
    -- Valid container service metric names are listed below, along with the most useful statistics to include in your request, and the published unit value.
    --
    --     * @CPUUtilization@ - The average percentage of compute units that are currently in use across all nodes of the container service. This metric identifies the processing power required to run containers on each node of the container service.
    -- Statistics: The most useful statistics are @Maximum@ and @Average@ .
    -- Unit: The published unit is @Percent@ .
    --
    --
    --     * @MemoryUtilization@ - The average percentage of available memory that is currently in use across all nodes of the container service. This metric identifies the memory required to run containers on each node of the container service.
    -- Statistics: The most useful statistics are @Maximum@ and @Average@ .
    -- Unit: The published unit is @Percent@ .
    metricName :: ContainerServiceMetricName,
    -- | The end time of the time period.
    endTime :: Lude.Timestamp,
    -- | The name of the container service for which to get metric data.
    serviceName :: Lude.Text,
    -- | The statistic for the metric.
    --
    -- The following statistics are available:
    --
    --     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
    --
    --
    --     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
    --
    --
    --     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
    --
    --
    --     * @Average@ - The value of @Sum@ / @SampleCount@ during the specified period. By comparing this statistic with the @Minimum@ and @Maximum@ values, you can determine the full scope of a metric and how close the average use is to the @Minimum@ and @Maximum@ values. This comparison helps you to know when to increase or decrease your resources.
    --
    --
    --     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
    statistics :: [MetricStatistic]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerServiceMetricData' with the minimum fields required to make a request.
--
-- * 'startTime' - The start time of the time period.
-- * 'period' - The granularity, in seconds, of the returned data points.
--
-- All container service metric data is available in 5-minute (300 seconds) granularity.
-- * 'metricName' - The metric for which you want to return information.
--
-- Valid container service metric names are listed below, along with the most useful statistics to include in your request, and the published unit value.
--
--     * @CPUUtilization@ - The average percentage of compute units that are currently in use across all nodes of the container service. This metric identifies the processing power required to run containers on each node of the container service.
-- Statistics: The most useful statistics are @Maximum@ and @Average@ .
-- Unit: The published unit is @Percent@ .
--
--
--     * @MemoryUtilization@ - The average percentage of available memory that is currently in use across all nodes of the container service. This metric identifies the memory required to run containers on each node of the container service.
-- Statistics: The most useful statistics are @Maximum@ and @Average@ .
-- Unit: The published unit is @Percent@ .
--
--
-- * 'endTime' - The end time of the time period.
-- * 'serviceName' - The name of the container service for which to get metric data.
-- * 'statistics' - The statistic for the metric.
--
-- The following statistics are available:
--
--     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
--
--
--     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
--
--
--     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
--
--
--     * @Average@ - The value of @Sum@ / @SampleCount@ during the specified period. By comparing this statistic with the @Minimum@ and @Maximum@ values, you can determine the full scope of a metric and how close the average use is to the @Minimum@ and @Maximum@ values. This comparison helps you to know when to increase or decrease your resources.
--
--
--     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
mkGetContainerServiceMetricData ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'period'
  Lude.Natural ->
  -- | 'metricName'
  ContainerServiceMetricName ->
  -- | 'endTime'
  Lude.Timestamp ->
  -- | 'serviceName'
  Lude.Text ->
  GetContainerServiceMetricData
mkGetContainerServiceMetricData
  pStartTime_
  pPeriod_
  pMetricName_
  pEndTime_
  pServiceName_ =
    GetContainerServiceMetricData'
      { startTime = pStartTime_,
        period = pPeriod_,
        metricName = pMetricName_,
        endTime = pEndTime_,
        serviceName = pServiceName_,
        statistics = Lude.mempty
      }

-- | The start time of the time period.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdStartTime :: Lens.Lens' GetContainerServiceMetricData Lude.Timestamp
gcsmdStartTime = Lens.lens (startTime :: GetContainerServiceMetricData -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetContainerServiceMetricData)
{-# DEPRECATED gcsmdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The granularity, in seconds, of the returned data points.
--
-- All container service metric data is available in 5-minute (300 seconds) granularity.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdPeriod :: Lens.Lens' GetContainerServiceMetricData Lude.Natural
gcsmdPeriod = Lens.lens (period :: GetContainerServiceMetricData -> Lude.Natural) (\s a -> s {period = a} :: GetContainerServiceMetricData)
{-# DEPRECATED gcsmdPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The metric for which you want to return information.
--
-- Valid container service metric names are listed below, along with the most useful statistics to include in your request, and the published unit value.
--
--     * @CPUUtilization@ - The average percentage of compute units that are currently in use across all nodes of the container service. This metric identifies the processing power required to run containers on each node of the container service.
-- Statistics: The most useful statistics are @Maximum@ and @Average@ .
-- Unit: The published unit is @Percent@ .
--
--
--     * @MemoryUtilization@ - The average percentage of available memory that is currently in use across all nodes of the container service. This metric identifies the memory required to run containers on each node of the container service.
-- Statistics: The most useful statistics are @Maximum@ and @Average@ .
-- Unit: The published unit is @Percent@ .
--
--
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdMetricName :: Lens.Lens' GetContainerServiceMetricData ContainerServiceMetricName
gcsmdMetricName = Lens.lens (metricName :: GetContainerServiceMetricData -> ContainerServiceMetricName) (\s a -> s {metricName = a} :: GetContainerServiceMetricData)
{-# DEPRECATED gcsmdMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The end time of the time period.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdEndTime :: Lens.Lens' GetContainerServiceMetricData Lude.Timestamp
gcsmdEndTime = Lens.lens (endTime :: GetContainerServiceMetricData -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetContainerServiceMetricData)
{-# DEPRECATED gcsmdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The name of the container service for which to get metric data.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdServiceName :: Lens.Lens' GetContainerServiceMetricData Lude.Text
gcsmdServiceName = Lens.lens (serviceName :: GetContainerServiceMetricData -> Lude.Text) (\s a -> s {serviceName = a} :: GetContainerServiceMetricData)
{-# DEPRECATED gcsmdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

-- | The statistic for the metric.
--
-- The following statistics are available:
--
--     * @Minimum@ - The lowest value observed during the specified period. Use this value to determine low volumes of activity for your application.
--
--
--     * @Maximum@ - The highest value observed during the specified period. Use this value to determine high volumes of activity for your application.
--
--
--     * @Sum@ - All values submitted for the matching metric added together. You can use this statistic to determine the total volume of a metric.
--
--
--     * @Average@ - The value of @Sum@ / @SampleCount@ during the specified period. By comparing this statistic with the @Minimum@ and @Maximum@ values, you can determine the full scope of a metric and how close the average use is to the @Minimum@ and @Maximum@ values. This comparison helps you to know when to increase or decrease your resources.
--
--
--     * @SampleCount@ - The count, or number, of data points used for the statistical calculation.
--
--
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdStatistics :: Lens.Lens' GetContainerServiceMetricData [MetricStatistic]
gcsmdStatistics = Lens.lens (statistics :: GetContainerServiceMetricData -> [MetricStatistic]) (\s a -> s {statistics = a} :: GetContainerServiceMetricData)
{-# DEPRECATED gcsmdStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

instance Lude.AWSRequest GetContainerServiceMetricData where
  type
    Rs GetContainerServiceMetricData =
      GetContainerServiceMetricDataResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetContainerServiceMetricDataResponse'
            Lude.<$> (x Lude..?> "metricName")
            Lude.<*> (x Lude..?> "metricData" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetContainerServiceMetricData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "Lightsail_20161128.GetContainerServiceMetricData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetContainerServiceMetricData where
  toJSON GetContainerServiceMetricData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("startTime" Lude..= startTime),
            Lude.Just ("period" Lude..= period),
            Lude.Just ("metricName" Lude..= metricName),
            Lude.Just ("endTime" Lude..= endTime),
            Lude.Just ("serviceName" Lude..= serviceName),
            Lude.Just ("statistics" Lude..= statistics)
          ]
      )

instance Lude.ToPath GetContainerServiceMetricData where
  toPath = Lude.const "/"

instance Lude.ToQuery GetContainerServiceMetricData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetContainerServiceMetricDataResponse' smart constructor.
data GetContainerServiceMetricDataResponse = GetContainerServiceMetricDataResponse'
  { -- | The name of the metric returned.
    metricName :: Lude.Maybe ContainerServiceMetricName,
    -- | An array of objects that describe the metric data returned.
    metricData :: Lude.Maybe [MetricDatapoint],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetContainerServiceMetricDataResponse' with the minimum fields required to make a request.
--
-- * 'metricName' - The name of the metric returned.
-- * 'metricData' - An array of objects that describe the metric data returned.
-- * 'responseStatus' - The response status code.
mkGetContainerServiceMetricDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetContainerServiceMetricDataResponse
mkGetContainerServiceMetricDataResponse pResponseStatus_ =
  GetContainerServiceMetricDataResponse'
    { metricName = Lude.Nothing,
      metricData = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The name of the metric returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdrsMetricName :: Lens.Lens' GetContainerServiceMetricDataResponse (Lude.Maybe ContainerServiceMetricName)
gcsmdrsMetricName = Lens.lens (metricName :: GetContainerServiceMetricDataResponse -> Lude.Maybe ContainerServiceMetricName) (\s a -> s {metricName = a} :: GetContainerServiceMetricDataResponse)
{-# DEPRECATED gcsmdrsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | An array of objects that describe the metric data returned.
--
-- /Note:/ Consider using 'metricData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdrsMetricData :: Lens.Lens' GetContainerServiceMetricDataResponse (Lude.Maybe [MetricDatapoint])
gcsmdrsMetricData = Lens.lens (metricData :: GetContainerServiceMetricDataResponse -> Lude.Maybe [MetricDatapoint]) (\s a -> s {metricData = a} :: GetContainerServiceMetricDataResponse)
{-# DEPRECATED gcsmdrsMetricData "Use generic-lens or generic-optics with 'metricData' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdrsResponseStatus :: Lens.Lens' GetContainerServiceMetricDataResponse Lude.Int
gcsmdrsResponseStatus = Lens.lens (responseStatus :: GetContainerServiceMetricDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetContainerServiceMetricDataResponse)
{-# DEPRECATED gcsmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
