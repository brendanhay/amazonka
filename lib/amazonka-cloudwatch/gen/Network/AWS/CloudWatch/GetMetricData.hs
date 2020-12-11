{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.GetMetricData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use the @GetMetricData@ API to retrieve as many as 500 different metrics in a single request, with a total of as many as 100,800 data points. You can also optionally perform math expressions on the values of the returned statistics, to create new time series that represent new insights into your data. For example, using Lambda metrics, you could divide the Errors metric by the Invocations metric to get an error rate time series. For more information about metric math expressions, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/using-metric-math.html#metric-math-syntax Metric Math Syntax and Functions> in the /Amazon CloudWatch User Guide/ .
--
-- Calls to the @GetMetricData@ API have a different pricing structure than calls to @GetMetricStatistics@ . For more information about pricing, see <https://aws.amazon.com/cloudwatch/pricing/ Amazon CloudWatch Pricing> .
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
-- If you omit @Unit@ in your request, all data that was collected with any unit is returned, along with the corresponding units that were specified when the data was reported to CloudWatch. If you specify a unit, the operation returns only data that was collected with that unit specified. If you specify a unit that does not match the data collected, the results of the operation are null. CloudWatch does not perform unit conversions.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatch.GetMetricData
  ( -- * Creating a request
    GetMetricData (..),
    mkGetMetricData,

    -- ** Request lenses
    gmdMaxDatapoints,
    gmdNextToken,
    gmdScanBy,
    gmdMetricDataQueries,
    gmdStartTime,
    gmdEndTime,

    -- * Destructuring the response
    GetMetricDataResponse (..),
    mkGetMetricDataResponse,

    -- ** Response lenses
    gmdrsMetricDataResults,
    gmdrsNextToken,
    gmdrsMessages,
    gmdrsResponseStatus,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { maxDatapoints ::
      Lude.Maybe Lude.Int,
    nextToken :: Lude.Maybe Lude.Text,
    scanBy :: Lude.Maybe ScanBy,
    metricDataQueries :: [MetricDataQuery],
    startTime :: Lude.ISO8601,
    endTime :: Lude.ISO8601
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricData' with the minimum fields required to make a request.
--
-- * 'endTime' - The time stamp indicating the latest data to be returned.
--
-- The value specified is exclusive; results include data points up to the specified time stamp.
-- For better performance, specify @StartTime@ and @EndTime@ values that align with the value of the metric's @Period@ and sync up with the beginning and end of an hour. For example, if the @Period@ of a metric is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@ .
-- * 'maxDatapoints' - The maximum number of data points the request should return before paginating. If you omit this, the default of 100,800 is used.
-- * 'metricDataQueries' - The metric queries to be returned. A single @GetMetricData@ call can include as many as 500 @MetricDataQuery@ structures. Each of these structures can specify either a metric to retrieve, or a math expression to perform on retrieved data.
-- * 'nextToken' - Include this value, if it was returned by the previous @GetMetricData@ operation, to get the next set of data points.
-- * 'scanBy' - The order in which data points should be returned. @TimestampDescending@ returns the newest data first and paginates when the @MaxDatapoints@ limit is reached. @TimestampAscending@ returns the oldest data first and paginates when the @MaxDatapoints@ limit is reached.
-- * 'startTime' - The time stamp indicating the earliest data to be returned.
--
-- The value specified is inclusive; results include data points with the specified time stamp.
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
-- For better performance, specify @StartTime@ and @EndTime@ values that align with the value of the metric's @Period@ and sync up with the beginning and end of an hour. For example, if the @Period@ of a metric is 5 minutes, specifying 12:05 or 12:30 as @StartTime@ can get a faster response from CloudWatch than setting 12:07 or 12:29 as the @StartTime@ .
mkGetMetricData ::
  -- | 'startTime'
  Lude.ISO8601 ->
  -- | 'endTime'
  Lude.ISO8601 ->
  GetMetricData
mkGetMetricData pStartTime_ pEndTime_ =
  GetMetricData'
    { maxDatapoints = Lude.Nothing,
      nextToken = Lude.Nothing,
      scanBy = Lude.Nothing,
      metricDataQueries = Lude.mempty,
      startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | The maximum number of data points the request should return before paginating. If you omit this, the default of 100,800 is used.
--
-- /Note:/ Consider using 'maxDatapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdMaxDatapoints :: Lens.Lens' GetMetricData (Lude.Maybe Lude.Int)
gmdMaxDatapoints = Lens.lens (maxDatapoints :: GetMetricData -> Lude.Maybe Lude.Int) (\s a -> s {maxDatapoints = a} :: GetMetricData)
{-# DEPRECATED gmdMaxDatapoints "Use generic-lens or generic-optics with 'maxDatapoints' instead." #-}

-- | Include this value, if it was returned by the previous @GetMetricData@ operation, to get the next set of data points.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdNextToken :: Lens.Lens' GetMetricData (Lude.Maybe Lude.Text)
gmdNextToken = Lens.lens (nextToken :: GetMetricData -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMetricData)
{-# DEPRECATED gmdNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The order in which data points should be returned. @TimestampDescending@ returns the newest data first and paginates when the @MaxDatapoints@ limit is reached. @TimestampAscending@ returns the oldest data first and paginates when the @MaxDatapoints@ limit is reached.
--
-- /Note:/ Consider using 'scanBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdScanBy :: Lens.Lens' GetMetricData (Lude.Maybe ScanBy)
gmdScanBy = Lens.lens (scanBy :: GetMetricData -> Lude.Maybe ScanBy) (\s a -> s {scanBy = a} :: GetMetricData)
{-# DEPRECATED gmdScanBy "Use generic-lens or generic-optics with 'scanBy' instead." #-}

-- | The metric queries to be returned. A single @GetMetricData@ call can include as many as 500 @MetricDataQuery@ structures. Each of these structures can specify either a metric to retrieve, or a math expression to perform on retrieved data.
--
-- /Note:/ Consider using 'metricDataQueries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdMetricDataQueries :: Lens.Lens' GetMetricData [MetricDataQuery]
gmdMetricDataQueries = Lens.lens (metricDataQueries :: GetMetricData -> [MetricDataQuery]) (\s a -> s {metricDataQueries = a} :: GetMetricData)
{-# DEPRECATED gmdMetricDataQueries "Use generic-lens or generic-optics with 'metricDataQueries' instead." #-}

-- | The time stamp indicating the earliest data to be returned.
--
-- The value specified is inclusive; results include data points with the specified time stamp.
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
-- For better performance, specify @StartTime@ and @EndTime@ values that align with the value of the metric's @Period@ and sync up with the beginning and end of an hour. For example, if the @Period@ of a metric is 5 minutes, specifying 12:05 or 12:30 as @StartTime@ can get a faster response from CloudWatch than setting 12:07 or 12:29 as the @StartTime@ .
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdStartTime :: Lens.Lens' GetMetricData Lude.ISO8601
gmdStartTime = Lens.lens (startTime :: GetMetricData -> Lude.ISO8601) (\s a -> s {startTime = a} :: GetMetricData)
{-# DEPRECATED gmdStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The time stamp indicating the latest data to be returned.
--
-- The value specified is exclusive; results include data points up to the specified time stamp.
-- For better performance, specify @StartTime@ and @EndTime@ values that align with the value of the metric's @Period@ and sync up with the beginning and end of an hour. For example, if the @Period@ of a metric is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdEndTime :: Lens.Lens' GetMetricData Lude.ISO8601
gmdEndTime = Lens.lens (endTime :: GetMetricData -> Lude.ISO8601) (\s a -> s {endTime = a} :: GetMetricData)
{-# DEPRECATED gmdEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Page.AWSPager GetMetricData where
  page rq rs
    | Page.stop (rs Lens.^. gmdrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gmdrsMetricDataResults) = Lude.Nothing
    | Page.stop (rs Lens.^. gmdrsMessages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gmdNextToken Lens..~ rs Lens.^. gmdrsNextToken

instance Lude.AWSRequest GetMetricData where
  type Rs GetMetricData = GetMetricDataResponse
  request = Req.postQuery cloudWatchService
  response =
    Res.receiveXMLWrapper
      "GetMetricDataResult"
      ( \s h x ->
          GetMetricDataResponse'
            Lude.<$> ( x Lude..@? "MetricDataResults" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "NextToken")
            Lude.<*> ( x Lude..@? "Messages" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMetricData where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetMetricData where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMetricData where
  toQuery GetMetricData' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("GetMetricData" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "MaxDatapoints" Lude.=: maxDatapoints,
        "NextToken" Lude.=: nextToken,
        "ScanBy" Lude.=: scanBy,
        "MetricDataQueries"
          Lude.=: Lude.toQueryList "member" metricDataQueries,
        "StartTime" Lude.=: startTime,
        "EndTime" Lude.=: endTime
      ]

-- | /See:/ 'mkGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { metricDataResults ::
      Lude.Maybe [MetricDataResult],
    nextToken :: Lude.Maybe Lude.Text,
    messages :: Lude.Maybe [MessageData],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMetricDataResponse' with the minimum fields required to make a request.
--
-- * 'messages' - Contains a message about this @GetMetricData@ operation, if the operation results in such a message. An example of a message that might be returned is @Maximum number of allowed metrics exceeded@ . If there is a message, as much of the operation as possible is still executed.
--
-- A message appears here only if it is related to the global @GetMetricData@ operation. Any message about a specific metric returned by the operation appears in the @MetricDataResult@ object returned for that metric.
-- * 'metricDataResults' - The metrics that are returned, including the metric name, namespace, and dimensions.
-- * 'nextToken' - A token that marks the next batch of returned results.
-- * 'responseStatus' - The response status code.
mkGetMetricDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMetricDataResponse
mkGetMetricDataResponse pResponseStatus_ =
  GetMetricDataResponse'
    { metricDataResults = Lude.Nothing,
      nextToken = Lude.Nothing,
      messages = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The metrics that are returned, including the metric name, namespace, and dimensions.
--
-- /Note:/ Consider using 'metricDataResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMetricDataResults :: Lens.Lens' GetMetricDataResponse (Lude.Maybe [MetricDataResult])
gmdrsMetricDataResults = Lens.lens (metricDataResults :: GetMetricDataResponse -> Lude.Maybe [MetricDataResult]) (\s a -> s {metricDataResults = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsMetricDataResults "Use generic-lens or generic-optics with 'metricDataResults' instead." #-}

-- | A token that marks the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsNextToken :: Lens.Lens' GetMetricDataResponse (Lude.Maybe Lude.Text)
gmdrsNextToken = Lens.lens (nextToken :: GetMetricDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Contains a message about this @GetMetricData@ operation, if the operation results in such a message. An example of a message that might be returned is @Maximum number of allowed metrics exceeded@ . If there is a message, as much of the operation as possible is still executed.
--
-- A message appears here only if it is related to the global @GetMetricData@ operation. Any message about a specific metric returned by the operation appears in the @MetricDataResult@ object returned for that metric.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMessages :: Lens.Lens' GetMetricDataResponse (Lude.Maybe [MessageData])
gmdrsMessages = Lens.lens (messages :: GetMetricDataResponse -> Lude.Maybe [MessageData]) (\s a -> s {messages = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsMessages "Use generic-lens or generic-optics with 'messages' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsResponseStatus :: Lens.Lens' GetMetricDataResponse Lude.Int
gmdrsResponseStatus = Lens.lens (responseStatus :: GetMetricDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMetricDataResponse)
{-# DEPRECATED gmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
