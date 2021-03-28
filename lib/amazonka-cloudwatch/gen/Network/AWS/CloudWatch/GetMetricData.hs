{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetMetricData (..)
    , mkGetMetricData
    -- ** Request lenses
    , gmdMetricDataQueries
    , gmdStartTime
    , gmdEndTime
    , gmdMaxDatapoints
    , gmdNextToken
    , gmdScanBy

    -- * Destructuring the response
    , GetMetricDataResponse (..)
    , mkGetMetricDataResponse
    -- ** Response lenses
    , gmdrrsMessages
    , gmdrrsMetricDataResults
    , gmdrrsNextToken
    , gmdrrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatch.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetMetricData' smart constructor.
data GetMetricData = GetMetricData'
  { metricDataQueries :: [Types.MetricDataQuery]
    -- ^ The metric queries to be returned. A single @GetMetricData@ call can include as many as 500 @MetricDataQuery@ structures. Each of these structures can specify either a metric to retrieve, or a math expression to perform on retrieved data. 
  , startTime :: Core.UTCTime
    -- ^ The time stamp indicating the earliest data to be returned.
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
  , endTime :: Core.UTCTime
    -- ^ The time stamp indicating the latest data to be returned.
--
-- The value specified is exclusive; results include data points up to the specified time stamp.
-- For better performance, specify @StartTime@ and @EndTime@ values that align with the value of the metric's @Period@ and sync up with the beginning and end of an hour. For example, if the @Period@ of a metric is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@ .
  , maxDatapoints :: Core.Maybe Core.Int
    -- ^ The maximum number of data points the request should return before paginating. If you omit this, the default of 100,800 is used.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ Include this value, if it was returned by the previous @GetMetricData@ operation, to get the next set of data points.
  , scanBy :: Core.Maybe Types.ScanBy
    -- ^ The order in which data points should be returned. @TimestampDescending@ returns the newest data first and paginates when the @MaxDatapoints@ limit is reached. @TimestampAscending@ returns the oldest data first and paginates when the @MaxDatapoints@ limit is reached.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMetricData' value with any optional fields omitted.
mkGetMetricData
    :: Core.UTCTime -- ^ 'startTime'
    -> Core.UTCTime -- ^ 'endTime'
    -> GetMetricData
mkGetMetricData startTime endTime
  = GetMetricData'{metricDataQueries = Core.mempty, startTime,
                   endTime, maxDatapoints = Core.Nothing, nextToken = Core.Nothing,
                   scanBy = Core.Nothing}

-- | The metric queries to be returned. A single @GetMetricData@ call can include as many as 500 @MetricDataQuery@ structures. Each of these structures can specify either a metric to retrieve, or a math expression to perform on retrieved data. 
--
-- /Note:/ Consider using 'metricDataQueries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdMetricDataQueries :: Lens.Lens' GetMetricData [Types.MetricDataQuery]
gmdMetricDataQueries = Lens.field @"metricDataQueries"
{-# INLINEABLE gmdMetricDataQueries #-}
{-# DEPRECATED metricDataQueries "Use generic-lens or generic-optics with 'metricDataQueries' instead"  #-}

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
gmdStartTime :: Lens.Lens' GetMetricData Core.UTCTime
gmdStartTime = Lens.field @"startTime"
{-# INLINEABLE gmdStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The time stamp indicating the latest data to be returned.
--
-- The value specified is exclusive; results include data points up to the specified time stamp.
-- For better performance, specify @StartTime@ and @EndTime@ values that align with the value of the metric's @Period@ and sync up with the beginning and end of an hour. For example, if the @Period@ of a metric is 5 minutes, specifying 12:05 or 12:30 as @EndTime@ can get a faster response from CloudWatch than setting 12:07 or 12:29 as the @EndTime@ .
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdEndTime :: Lens.Lens' GetMetricData Core.UTCTime
gmdEndTime = Lens.field @"endTime"
{-# INLINEABLE gmdEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | The maximum number of data points the request should return before paginating. If you omit this, the default of 100,800 is used.
--
-- /Note:/ Consider using 'maxDatapoints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdMaxDatapoints :: Lens.Lens' GetMetricData (Core.Maybe Core.Int)
gmdMaxDatapoints = Lens.field @"maxDatapoints"
{-# INLINEABLE gmdMaxDatapoints #-}
{-# DEPRECATED maxDatapoints "Use generic-lens or generic-optics with 'maxDatapoints' instead"  #-}

-- | Include this value, if it was returned by the previous @GetMetricData@ operation, to get the next set of data points.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdNextToken :: Lens.Lens' GetMetricData (Core.Maybe Types.NextToken)
gmdNextToken = Lens.field @"nextToken"
{-# INLINEABLE gmdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The order in which data points should be returned. @TimestampDescending@ returns the newest data first and paginates when the @MaxDatapoints@ limit is reached. @TimestampAscending@ returns the oldest data first and paginates when the @MaxDatapoints@ limit is reached.
--
-- /Note:/ Consider using 'scanBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdScanBy :: Lens.Lens' GetMetricData (Core.Maybe Types.ScanBy)
gmdScanBy = Lens.field @"scanBy"
{-# INLINEABLE gmdScanBy #-}
{-# DEPRECATED scanBy "Use generic-lens or generic-optics with 'scanBy' instead"  #-}

instance Core.ToQuery GetMetricData where
        toQuery GetMetricData{..}
          = Core.toQueryPair "Action" ("GetMetricData" :: Core.Text) Core.<>
              Core.toQueryPair "Version" ("2010-08-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "MetricDataQueries"
                (Core.toQueryList "member" metricDataQueries)
              Core.<> Core.toQueryPair "StartTime" startTime
              Core.<> Core.toQueryPair "EndTime" endTime
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxDatapoints")
                maxDatapoints
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "ScanBy") scanBy

instance Core.ToHeaders GetMetricData where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest GetMetricData where
        type Rs GetMetricData = GetMetricDataResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "GetMetricDataResult"
              (\ s h x ->
                 GetMetricDataResponse' Core.<$>
                   (x Core..@? "Messages" Core..<@> Core.parseXMLList "member")
                     Core.<*>
                     x Core..@? "MetricDataResults" Core..<@> Core.parseXMLList "member"
                     Core.<*> x Core..@? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetMetricData where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"metricDataResults" Core.. Lens._Just)
            = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"messages" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetMetricDataResponse' smart constructor.
data GetMetricDataResponse = GetMetricDataResponse'
  { messages :: Core.Maybe [Types.MessageData]
    -- ^ Contains a message about this @GetMetricData@ operation, if the operation results in such a message. An example of a message that might be returned is @Maximum number of allowed metrics exceeded@ . If there is a message, as much of the operation as possible is still executed.
--
-- A message appears here only if it is related to the global @GetMetricData@ operation. Any message about a specific metric returned by the operation appears in the @MetricDataResult@ object returned for that metric.
  , metricDataResults :: Core.Maybe [Types.MetricDataResult]
    -- ^ The metrics that are returned, including the metric name, namespace, and dimensions.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that marks the next batch of returned results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetMetricDataResponse' value with any optional fields omitted.
mkGetMetricDataResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetMetricDataResponse
mkGetMetricDataResponse responseStatus
  = GetMetricDataResponse'{messages = Core.Nothing,
                           metricDataResults = Core.Nothing, nextToken = Core.Nothing,
                           responseStatus}

-- | Contains a message about this @GetMetricData@ operation, if the operation results in such a message. An example of a message that might be returned is @Maximum number of allowed metrics exceeded@ . If there is a message, as much of the operation as possible is still executed.
--
-- A message appears here only if it is related to the global @GetMetricData@ operation. Any message about a specific metric returned by the operation appears in the @MetricDataResult@ object returned for that metric.
--
-- /Note:/ Consider using 'messages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsMessages :: Lens.Lens' GetMetricDataResponse (Core.Maybe [Types.MessageData])
gmdrrsMessages = Lens.field @"messages"
{-# INLINEABLE gmdrrsMessages #-}
{-# DEPRECATED messages "Use generic-lens or generic-optics with 'messages' instead"  #-}

-- | The metrics that are returned, including the metric name, namespace, and dimensions.
--
-- /Note:/ Consider using 'metricDataResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsMetricDataResults :: Lens.Lens' GetMetricDataResponse (Core.Maybe [Types.MetricDataResult])
gmdrrsMetricDataResults = Lens.field @"metricDataResults"
{-# INLINEABLE gmdrrsMetricDataResults #-}
{-# DEPRECATED metricDataResults "Use generic-lens or generic-optics with 'metricDataResults' instead"  #-}

-- | A token that marks the next batch of returned results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsNextToken :: Lens.Lens' GetMetricDataResponse (Core.Maybe Types.NextToken)
gmdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gmdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrrsResponseStatus :: Lens.Lens' GetMetricDataResponse Core.Int
gmdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gmdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
