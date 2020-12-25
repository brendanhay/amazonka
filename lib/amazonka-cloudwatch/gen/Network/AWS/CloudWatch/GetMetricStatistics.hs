{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetMetricStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets statistics for the specified metric.
--
-- The maximum number of data points returned from a single call is 1,440. If you request more than 1,440 data points, CloudWatch returns an error. To reduce the number of data points, you can narrow the specified time range and make multiple requests across adjacent time ranges, or you can increase the specified period. Data points are not returned in chronological order.
-- CloudWatch aggregates data points based on the length of the period that you specify. For example, if you request statistics with a one-hour period, CloudWatch aggregates all data points with time stamps that fall within each one-hour period. Therefore, the number of values aggregated by CloudWatch is larger than the number of data points returned.
-- CloudWatch needs raw data points to calculate percentile statistics. If you publish data using a statistic set instead, you can only retrieve percentile statistics for this data if one of the following conditions is true:
--
--     * The SampleCount value of the statistic set is 1.
--
--
--     * The Min and the Max values of the statistic set are equal.
--
--
-- Percentile statistics are not available for metrics when any of the metric values are negative numbers.
-- Amazon CloudWatch retains metric data as follows:
--
--     * Data points with a period of less than 60 seconds are available for 3 hours. These data points are high-resolution metrics and are available only for custom metrics that have been defined with a @StorageResolution@ of 1.
--
--
--     * Data points with a period of 60 seconds (1-minute) are available for 15 days.
--
--
--     * Data points with a period of 300 seconds (5-minute) are available for 63 days.
--
--
--     * Data points with a period of 3600 seconds (1 hour) are available for 455 days (15 months).
--
--
-- Data points that are initially published with a shorter period are aggregated together for long-term storage. For example, if you collect data using a period of 1 minute, the data remains available for 15 days with 1-minute resolution. After 15 days, this data is still available, but is aggregated and retrievable only with a resolution of 5 minutes. After 63 days, the data is further aggregated and is available with a resolution of 1 hour.
-- CloudWatch started retaining 5-minute and 1-hour metric data as of July 9, 2016.
-- For information about metrics and dimensions supported by AWS services, see the <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CW_Support_For_AWS.html Amazon CloudWatch Metrics and Dimensions Reference> in the /Amazon CloudWatch User Guide/ .
module Network.AWS.CloudWatch.GetMetricStatistics
  ( -- * Creating a request
    GetMetricStatistics (..),
    mkGetMetricStatistics,

    -- ** Request lenses
    gmsNamespace,
    gmsMetricName,
    gmsStartTime,
    gmsEndTime,
    gmsPeriod,
    gmsDimensions,
    gmsExtendedStatistics,
    gmsStatistics,
    gmsUnit,

    -- * Destructuring the response
    GetMetricStatisticsResponse (..),
    mkGetMetricStatisticsResponse,

    -- ** Response lenses
    gmsrrsDatapoints,
    gmsrrsLabel,
    gmsrrsResponseStatus,
  )
where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMetricStatistics' smart constructor.
data GetMetricStatistics = GetMetricStatistics'
  { -- | The namespace of the metric, with or without spaces.
    namespace :: Types.Namespace,
    -- | The name of the metric, with or without spaces.
    metricName :: Types.MetricName,
    -- | The time stamp that determines the first data point to return. Start times are evaluated relative to the time that CloudWatch receives the request.
    --
    -- The value specified is inclusive; results include data points with the specified time stamp. In a raw HTTP query, the time stamp must be in ISO 8601 UTC format (for example, 2016-10-03T23:00:00Z).
    -- CloudWatch rounds the specified time stamp as follows:
    --
    --     * Start time less than 15 days ago - Round down to the nearest whole minute. For example, 12:32:34 is rounded down to 12:32:00.
    --
    --
    --     * Start time between 15 and 63 days ago - Round down to the nearest 5-minute clock interval. For example, 12:32:34 is rounded down to 12:30:00.
    --
    --
    --     * Start time greater than 63 days ago - Round down to the nearest 1-hour clock interval. For example, 12:32:34 is rounded down to 12:00:00.
    --
    --
    -- If you set @Period@ to 5, 10, or 30, the start time of your request is rounded down to the nearest time that corresponds to even 5-, 10-, or 30-second divisions of a minute. For example, if you make a query at (HH:mm:ss) 01:05:23 for the previous 10-second period, the start time of your request is rounded down and you receive data from 01:05:10 to 01:05:20. If you make a query at 15:07:17 for the previous 5 minutes of data, using a period of 5 seconds, you receive data timestamped between 15:02:15 and 15:07:15.
    startTime :: Core.UTCTime,
    -- | The time stamp that determines the last data point to return.
    --
    -- The value specified is exclusive; results include data points up to the specified time stamp. In a raw HTTP query, the time stamp must be in ISO 8601 UTC format (for example, 2016-10-10T23:00:00Z).
    endTime :: Core.UTCTime,
    -- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ call that includes a @StorageResolution@ of 1 second.
    --
    -- If the @StartTime@ parameter specifies a time stamp that is greater than 3 hours ago, you must specify the period as follows or no data points in that time range is returned:
    --
    --     * Start time between 3 hours and 15 days ago - Use a multiple of 60 seconds (1 minute).
    --
    --
    --     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).
    --
    --
    --     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
    period :: Core.Natural,
    -- | The dimensions. If the metric contains multiple dimensions, you must include a value for each dimension. CloudWatch treats each unique combination of dimensions as a separate metric. If a specific combination of dimensions was not published, you can't retrieve statistics for it. You must specify the same dimensions that were used when the metrics were created. For an example, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations> in the /Amazon CloudWatch User Guide/ . For more information about specifying dimensions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics> in the /Amazon CloudWatch User Guide/ .
    dimensions :: Core.Maybe [Types.Dimension],
    -- | The percentile statistics. Specify values between p0.0 and p100. When calling @GetMetricStatistics@ , you must specify either @Statistics@ or @ExtendedStatistics@ , but not both. Percentile statistics are not available for metrics when any of the metric values are negative numbers.
    extendedStatistics :: Core.Maybe (Core.NonEmpty Types.ExtendedStatistic),
    -- | The metric statistics, other than percentile. For percentile statistics, use @ExtendedStatistics@ . When calling @GetMetricStatistics@ , you must specify either @Statistics@ or @ExtendedStatistics@ , but not both.
    statistics :: Core.Maybe (Core.NonEmpty Types.Statistic),
    -- | The unit for a given metric. If you omit @Unit@ , all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
    unit :: Core.Maybe Types.StandardUnit
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMetricStatistics' value with any optional fields omitted.
mkGetMetricStatistics ::
  -- | 'namespace'
  Types.Namespace ->
  -- | 'metricName'
  Types.MetricName ->
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  -- | 'period'
  Core.Natural ->
  GetMetricStatistics
mkGetMetricStatistics namespace metricName startTime endTime period =
  GetMetricStatistics'
    { namespace,
      metricName,
      startTime,
      endTime,
      period,
      dimensions = Core.Nothing,
      extendedStatistics = Core.Nothing,
      statistics = Core.Nothing,
      unit = Core.Nothing
    }

-- | The namespace of the metric, with or without spaces.
--
-- /Note:/ Consider using 'namespace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsNamespace :: Lens.Lens' GetMetricStatistics Types.Namespace
gmsNamespace = Lens.field @"namespace"
{-# DEPRECATED gmsNamespace "Use generic-lens or generic-optics with 'namespace' instead." #-}

-- | The name of the metric, with or without spaces.
--
-- /Note:/ Consider using 'metricName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsMetricName :: Lens.Lens' GetMetricStatistics Types.MetricName
gmsMetricName = Lens.field @"metricName"
{-# DEPRECATED gmsMetricName "Use generic-lens or generic-optics with 'metricName' instead." #-}

-- | The time stamp that determines the first data point to return. Start times are evaluated relative to the time that CloudWatch receives the request.
--
-- The value specified is inclusive; results include data points with the specified time stamp. In a raw HTTP query, the time stamp must be in ISO 8601 UTC format (for example, 2016-10-03T23:00:00Z).
-- CloudWatch rounds the specified time stamp as follows:
--
--     * Start time less than 15 days ago - Round down to the nearest whole minute. For example, 12:32:34 is rounded down to 12:32:00.
--
--
--     * Start time between 15 and 63 days ago - Round down to the nearest 5-minute clock interval. For example, 12:32:34 is rounded down to 12:30:00.
--
--
--     * Start time greater than 63 days ago - Round down to the nearest 1-hour clock interval. For example, 12:32:34 is rounded down to 12:00:00.
--
--
-- If you set @Period@ to 5, 10, or 30, the start time of your request is rounded down to the nearest time that corresponds to even 5-, 10-, or 30-second divisions of a minute. For example, if you make a query at (HH:mm:ss) 01:05:23 for the previous 10-second period, the start time of your request is rounded down and you receive data from 01:05:10 to 01:05:20. If you make a query at 15:07:17 for the previous 5 minutes of data, using a period of 5 seconds, you receive data timestamped between 15:02:15 and 15:07:15.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsStartTime :: Lens.Lens' GetMetricStatistics Core.UTCTime
gmsStartTime = Lens.field @"startTime"
{-# DEPRECATED gmsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The time stamp that determines the last data point to return.
--
-- The value specified is exclusive; results include data points up to the specified time stamp. In a raw HTTP query, the time stamp must be in ISO 8601 UTC format (for example, 2016-10-10T23:00:00Z).
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsEndTime :: Lens.Lens' GetMetricStatistics Core.UTCTime
gmsEndTime = Lens.field @"endTime"
{-# DEPRECATED gmsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The granularity, in seconds, of the returned data points. For metrics with regular resolution, a period can be as short as one minute (60 seconds) and must be a multiple of 60. For high-resolution metrics that are collected at intervals of less than one minute, the period can be 1, 5, 10, 30, 60, or any multiple of 60. High-resolution metrics are those metrics stored by a @PutMetricData@ call that includes a @StorageResolution@ of 1 second.
--
-- If the @StartTime@ parameter specifies a time stamp that is greater than 3 hours ago, you must specify the period as follows or no data points in that time range is returned:
--
--     * Start time between 3 hours and 15 days ago - Use a multiple of 60 seconds (1 minute).
--
--
--     * Start time between 15 and 63 days ago - Use a multiple of 300 seconds (5 minutes).
--
--
--     * Start time greater than 63 days ago - Use a multiple of 3600 seconds (1 hour).
--
--
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsPeriod :: Lens.Lens' GetMetricStatistics Core.Natural
gmsPeriod = Lens.field @"period"
{-# DEPRECATED gmsPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The dimensions. If the metric contains multiple dimensions, you must include a value for each dimension. CloudWatch treats each unique combination of dimensions as a separate metric. If a specific combination of dimensions was not published, you can't retrieve statistics for it. You must specify the same dimensions that were used when the metrics were created. For an example, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/cloudwatch_concepts.html#dimension-combinations Dimension Combinations> in the /Amazon CloudWatch User Guide/ . For more information about specifying dimensions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/publishingMetrics.html Publishing Metrics> in the /Amazon CloudWatch User Guide/ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsDimensions :: Lens.Lens' GetMetricStatistics (Core.Maybe [Types.Dimension])
gmsDimensions = Lens.field @"dimensions"
{-# DEPRECATED gmsDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The percentile statistics. Specify values between p0.0 and p100. When calling @GetMetricStatistics@ , you must specify either @Statistics@ or @ExtendedStatistics@ , but not both. Percentile statistics are not available for metrics when any of the metric values are negative numbers.
--
-- /Note:/ Consider using 'extendedStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsExtendedStatistics :: Lens.Lens' GetMetricStatistics (Core.Maybe (Core.NonEmpty Types.ExtendedStatistic))
gmsExtendedStatistics = Lens.field @"extendedStatistics"
{-# DEPRECATED gmsExtendedStatistics "Use generic-lens or generic-optics with 'extendedStatistics' instead." #-}

-- | The metric statistics, other than percentile. For percentile statistics, use @ExtendedStatistics@ . When calling @GetMetricStatistics@ , you must specify either @Statistics@ or @ExtendedStatistics@ , but not both.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsStatistics :: Lens.Lens' GetMetricStatistics (Core.Maybe (Core.NonEmpty Types.Statistic))
gmsStatistics = Lens.field @"statistics"
{-# DEPRECATED gmsStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | The unit for a given metric. If you omit @Unit@ , all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsUnit :: Lens.Lens' GetMetricStatistics (Core.Maybe Types.StandardUnit)
gmsUnit = Lens.field @"unit"
{-# DEPRECATED gmsUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Core.AWSRequest GetMetricStatistics where
  type Rs GetMetricStatistics = GetMetricStatisticsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "GetMetricStatistics")
                Core.<> (Core.pure ("Version", "2010-08-01"))
                Core.<> (Core.toQueryValue "Namespace" namespace)
                Core.<> (Core.toQueryValue "MetricName" metricName)
                Core.<> (Core.toQueryValue "StartTime" startTime)
                Core.<> (Core.toQueryValue "EndTime" endTime)
                Core.<> (Core.toQueryValue "Period" period)
                Core.<> ( Core.toQueryValue
                            "Dimensions"
                            (Core.toQueryList "member" Core.<$> dimensions)
                        )
                Core.<> ( Core.toQueryValue
                            "ExtendedStatistics"
                            (Core.toQueryList "member" Core.<$> extendedStatistics)
                        )
                Core.<> ( Core.toQueryValue
                            "Statistics"
                            (Core.toQueryList "member" Core.<$> statistics)
                        )
                Core.<> (Core.toQueryValue "Unit" Core.<$> unit)
            )
      }
  response =
    Response.receiveXMLWrapper
      "GetMetricStatisticsResult"
      ( \s h x ->
          GetMetricStatisticsResponse'
            Core.<$> (x Core..@? "Datapoints" Core..<@> Core.parseXMLList "member")
            Core.<*> (x Core..@? "Label")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetMetricStatisticsResponse' smart constructor.
data GetMetricStatisticsResponse = GetMetricStatisticsResponse'
  { -- | The data points for the specified metric.
    datapoints :: Core.Maybe [Types.Datapoint],
    -- | A label for the specified metric.
    label :: Core.Maybe Types.Label,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetMetricStatisticsResponse' value with any optional fields omitted.
mkGetMetricStatisticsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetMetricStatisticsResponse
mkGetMetricStatisticsResponse responseStatus =
  GetMetricStatisticsResponse'
    { datapoints = Core.Nothing,
      label = Core.Nothing,
      responseStatus
    }

-- | The data points for the specified metric.
--
-- /Note:/ Consider using 'datapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsDatapoints :: Lens.Lens' GetMetricStatisticsResponse (Core.Maybe [Types.Datapoint])
gmsrrsDatapoints = Lens.field @"datapoints"
{-# DEPRECATED gmsrrsDatapoints "Use generic-lens or generic-optics with 'datapoints' instead." #-}

-- | A label for the specified metric.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsLabel :: Lens.Lens' GetMetricStatisticsResponse (Core.Maybe Types.Label)
gmsrrsLabel = Lens.field @"label"
{-# DEPRECATED gmsrrsLabel "Use generic-lens or generic-optics with 'label' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsrrsResponseStatus :: Lens.Lens' GetMetricStatisticsResponse Core.Int
gmsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gmsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
