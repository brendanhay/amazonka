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
    gcsmdServiceName,
    gcsmdMetricName,
    gcsmdStartTime,
    gcsmdEndTime,
    gcsmdPeriod,
    gcsmdStatistics,

    -- * Destructuring the response
    GetContainerServiceMetricDataResponse (..),
    mkGetContainerServiceMetricDataResponse,

    -- ** Response lenses
    gcsmdrrsMetricData,
    gcsmdrrsMetricName,
    gcsmdrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetContainerServiceMetricData' smart constructor.
data GetContainerServiceMetricData = GetContainerServiceMetricData'
  { -- | The name of the container service for which to get metric data.
    serviceName :: Types.ContainerServiceName,
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
    metricName :: Types.ContainerServiceMetricName,
    -- | The start time of the time period.
    startTime :: Core.NominalDiffTime,
    -- | The end time of the time period.
    endTime :: Core.NominalDiffTime,
    -- | The granularity, in seconds, of the returned data points.
    --
    -- All container service metric data is available in 5-minute (300 seconds) granularity.
    period :: Core.Natural,
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
    statistics :: [Types.MetricStatistic]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContainerServiceMetricData' value with any optional fields omitted.
mkGetContainerServiceMetricData ::
  -- | 'serviceName'
  Types.ContainerServiceName ->
  -- | 'metricName'
  Types.ContainerServiceMetricName ->
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  -- | 'period'
  Core.Natural ->
  GetContainerServiceMetricData
mkGetContainerServiceMetricData
  serviceName
  metricName
  startTime
  endTime
  period =
    GetContainerServiceMetricData'
      { serviceName,
        metricName,
        startTime,
        endTime,
        period,
        statistics = Core.mempty
      }

-- | The name of the container service for which to get metric data.
--
-- /Note:/ Consider using 'serviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdServiceName :: Lens.Lens' GetContainerServiceMetricData Types.ContainerServiceName
gcsmdServiceName = Lens.field @"serviceName"
{-# DEPRECATED gcsmdServiceName "Use generic-lens or generic-optics with 'serviceName' instead." #-}

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
gcsmdMetricName :: Lens.Lens' GetContainerServiceMetricData Types.ContainerServiceMetricName
gcsmdMetricName = Lens.field @"metricName"
{-# DEPRECATED gcsmdMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The start time of the time period.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdStartTime :: Lens.Lens' GetContainerServiceMetricData Core.NominalDiffTime
gcsmdStartTime = Lens.field @"startTime"
{-# DEPRECATED gcsmdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end time of the time period.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdEndTime :: Lens.Lens' GetContainerServiceMetricData Core.NominalDiffTime
gcsmdEndTime = Lens.field @"endTime"
{-# DEPRECATED gcsmdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The granularity, in seconds, of the returned data points.
--
-- All container service metric data is available in 5-minute (300 seconds) granularity.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdPeriod :: Lens.Lens' GetContainerServiceMetricData Core.Natural
gcsmdPeriod = Lens.field @"period"
{-# DEPRECATED gcsmdPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

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
gcsmdStatistics :: Lens.Lens' GetContainerServiceMetricData [Types.MetricStatistic]
gcsmdStatistics = Lens.field @"statistics"
{-# DEPRECATED gcsmdStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

instance Core.FromJSON GetContainerServiceMetricData where
  toJSON GetContainerServiceMetricData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("serviceName" Core..= serviceName),
            Core.Just ("metricName" Core..= metricName),
            Core.Just ("startTime" Core..= startTime),
            Core.Just ("endTime" Core..= endTime),
            Core.Just ("period" Core..= period),
            Core.Just ("statistics" Core..= statistics)
          ]
      )

instance Core.AWSRequest GetContainerServiceMetricData where
  type
    Rs GetContainerServiceMetricData =
      GetContainerServiceMetricDataResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "Lightsail_20161128.GetContainerServiceMetricData"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetContainerServiceMetricDataResponse'
            Core.<$> (x Core..:? "metricData")
            Core.<*> (x Core..:? "metricName")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetContainerServiceMetricDataResponse' smart constructor.
data GetContainerServiceMetricDataResponse = GetContainerServiceMetricDataResponse'
  { -- | An array of objects that describe the metric data returned.
    metricData :: Core.Maybe [Types.MetricDatapoint],
    -- | The name of the metric returned.
    metricName :: Core.Maybe Types.ContainerServiceMetricName,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetContainerServiceMetricDataResponse' value with any optional fields omitted.
mkGetContainerServiceMetricDataResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetContainerServiceMetricDataResponse
mkGetContainerServiceMetricDataResponse responseStatus =
  GetContainerServiceMetricDataResponse'
    { metricData = Core.Nothing,
      metricName = Core.Nothing,
      responseStatus
    }

-- | An array of objects that describe the metric data returned.
--
-- /Note:/ Consider using 'metricData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdrrsMetricData :: Lens.Lens' GetContainerServiceMetricDataResponse (Core.Maybe [Types.MetricDatapoint])
gcsmdrrsMetricData = Lens.field @"metricData"
{-# DEPRECATED gcsmdrrsMetricData "Use generic-lens or generic-optics with 'metricData' instead." #-}

-- | The name of the metric returned.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdrrsMetricName :: Lens.Lens' GetContainerServiceMetricDataResponse (Core.Maybe Types.ContainerServiceMetricName)
gcsmdrrsMetricName = Lens.field @"metricName"
{-# DEPRECATED gcsmdrrsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcsmdrrsResponseStatus :: Lens.Lens' GetContainerServiceMetricDataResponse Core.Int
gcsmdrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcsmdrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
