{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetTraceSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves IDs and annotations for traces available for a specified time frame using an optional filter. To get the full traces, pass the trace IDs to @BatchGetTraces@ .
--
-- A filter expression can target traced requests that hit specific service nodes or edges, have errors, or come from a known user. For example, the following filter expression targets traces that pass through @api.example.com@ :
-- @service("api.example.com")@ 
-- This filter expression finds traces that have an annotation named @account@ with the value @12345@ :
-- @annotation.account = "12345"@ 
-- For a full list of indexed fields and keywords that you can use in filter expressions, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-console-filters.html Using Filter Expressions> in the /AWS X-Ray Developer Guide/ .
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTraceSummaries
    (
    -- * Creating a request
      GetTraceSummaries (..)
    , mkGetTraceSummaries
    -- ** Request lenses
    , gtsStartTime
    , gtsEndTime
    , gtsFilterExpression
    , gtsNextToken
    , gtsSampling
    , gtsSamplingStrategy
    , gtsTimeRangeType

    -- * Destructuring the response
    , GetTraceSummariesResponse (..)
    , mkGetTraceSummariesResponse
    -- ** Response lenses
    , gtsrrsApproximateTime
    , gtsrrsNextToken
    , gtsrrsTraceSummaries
    , gtsrrsTracesProcessedCount
    , gtsrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetTraceSummaries' smart constructor.
data GetTraceSummaries = GetTraceSummaries'
  { startTime :: Core.NominalDiffTime
    -- ^ The start of the time frame for which to retrieve traces.
  , endTime :: Core.NominalDiffTime
    -- ^ The end of the time frame for which to retrieve traces.
  , filterExpression :: Core.Maybe Types.FilterExpression
    -- ^ Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
  , nextToken :: Core.Maybe Core.Text
    -- ^ Specify the pagination token returned by a previous request to retrieve the next page of results.
  , sampling :: Core.Maybe Core.Bool
    -- ^ Set to @true@ to get summaries for only a subset of available traces.
  , samplingStrategy :: Core.Maybe Types.SamplingStrategy
    -- ^ A parameter to indicate whether to enable sampling on trace summaries. Input parameters are Name and Value.
  , timeRangeType :: Core.Maybe Types.TimeRangeType
    -- ^ A parameter to indicate whether to query trace summaries by TraceId or Event time.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTraceSummaries' value with any optional fields omitted.
mkGetTraceSummaries
    :: Core.NominalDiffTime -- ^ 'startTime'
    -> Core.NominalDiffTime -- ^ 'endTime'
    -> GetTraceSummaries
mkGetTraceSummaries startTime endTime
  = GetTraceSummaries'{startTime, endTime,
                       filterExpression = Core.Nothing, nextToken = Core.Nothing,
                       sampling = Core.Nothing, samplingStrategy = Core.Nothing,
                       timeRangeType = Core.Nothing}

-- | The start of the time frame for which to retrieve traces.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStartTime :: Lens.Lens' GetTraceSummaries Core.NominalDiffTime
gtsStartTime = Lens.field @"startTime"
{-# INLINEABLE gtsStartTime #-}
{-# DEPRECATED startTime "Use generic-lens or generic-optics with 'startTime' instead"  #-}

-- | The end of the time frame for which to retrieve traces.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsEndTime :: Lens.Lens' GetTraceSummaries Core.NominalDiffTime
gtsEndTime = Lens.field @"endTime"
{-# INLINEABLE gtsEndTime #-}
{-# DEPRECATED endTime "Use generic-lens or generic-optics with 'endTime' instead"  #-}

-- | Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsFilterExpression :: Lens.Lens' GetTraceSummaries (Core.Maybe Types.FilterExpression)
gtsFilterExpression = Lens.field @"filterExpression"
{-# INLINEABLE gtsFilterExpression #-}
{-# DEPRECATED filterExpression "Use generic-lens or generic-optics with 'filterExpression' instead"  #-}

-- | Specify the pagination token returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsNextToken :: Lens.Lens' GetTraceSummaries (Core.Maybe Core.Text)
gtsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Set to @true@ to get summaries for only a subset of available traces.
--
-- /Note:/ Consider using 'sampling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsSampling :: Lens.Lens' GetTraceSummaries (Core.Maybe Core.Bool)
gtsSampling = Lens.field @"sampling"
{-# INLINEABLE gtsSampling #-}
{-# DEPRECATED sampling "Use generic-lens or generic-optics with 'sampling' instead"  #-}

-- | A parameter to indicate whether to enable sampling on trace summaries. Input parameters are Name and Value.
--
-- /Note:/ Consider using 'samplingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsSamplingStrategy :: Lens.Lens' GetTraceSummaries (Core.Maybe Types.SamplingStrategy)
gtsSamplingStrategy = Lens.field @"samplingStrategy"
{-# INLINEABLE gtsSamplingStrategy #-}
{-# DEPRECATED samplingStrategy "Use generic-lens or generic-optics with 'samplingStrategy' instead"  #-}

-- | A parameter to indicate whether to query trace summaries by TraceId or Event time.
--
-- /Note:/ Consider using 'timeRangeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTimeRangeType :: Lens.Lens' GetTraceSummaries (Core.Maybe Types.TimeRangeType)
gtsTimeRangeType = Lens.field @"timeRangeType"
{-# INLINEABLE gtsTimeRangeType #-}
{-# DEPRECATED timeRangeType "Use generic-lens or generic-optics with 'timeRangeType' instead"  #-}

instance Core.ToQuery GetTraceSummaries where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetTraceSummaries where
        toHeaders _ = Core.pure Core.mempty

instance Core.FromJSON GetTraceSummaries where
        toJSON GetTraceSummaries{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("StartTime" Core..= startTime),
                  Core.Just ("EndTime" Core..= endTime),
                  ("FilterExpression" Core..=) Core.<$> filterExpression,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Sampling" Core..=) Core.<$> sampling,
                  ("SamplingStrategy" Core..=) Core.<$> samplingStrategy,
                  ("TimeRangeType" Core..=) Core.<$> timeRangeType])

instance Core.AWSRequest GetTraceSummaries where
        type Rs GetTraceSummaries = GetTraceSummariesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/TraceSummaries",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetTraceSummariesResponse' Core.<$>
                   (x Core..:? "ApproximateTime") Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "TraceSummaries"
                     Core.<*> x Core..:? "TracesProcessedCount"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetTraceSummaries where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"traceSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetTraceSummariesResponse' smart constructor.
data GetTraceSummariesResponse = GetTraceSummariesResponse'
  { approximateTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The start time of this page of results.
  , nextToken :: Core.Maybe Core.Text
    -- ^ If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most recent results, closest to the end of the time frame.
  , traceSummaries :: Core.Maybe [Types.TraceSummary]
    -- ^ Trace IDs and annotations for traces that were found in the specified time frame.
  , tracesProcessedCount :: Core.Maybe Core.Integer
    -- ^ The total number of traces processed, including traces that did not match the specified filter expression.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetTraceSummariesResponse' value with any optional fields omitted.
mkGetTraceSummariesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetTraceSummariesResponse
mkGetTraceSummariesResponse responseStatus
  = GetTraceSummariesResponse'{approximateTime = Core.Nothing,
                               nextToken = Core.Nothing, traceSummaries = Core.Nothing,
                               tracesProcessedCount = Core.Nothing, responseStatus}

-- | The start time of this page of results.
--
-- /Note:/ Consider using 'approximateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsApproximateTime :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe Core.NominalDiffTime)
gtsrrsApproximateTime = Lens.field @"approximateTime"
{-# INLINEABLE gtsrrsApproximateTime #-}
{-# DEPRECATED approximateTime "Use generic-lens or generic-optics with 'approximateTime' instead"  #-}

-- | If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most recent results, closest to the end of the time frame.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsNextToken :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe Core.Text)
gtsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gtsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Trace IDs and annotations for traces that were found in the specified time frame.
--
-- /Note:/ Consider using 'traceSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsTraceSummaries :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe [Types.TraceSummary])
gtsrrsTraceSummaries = Lens.field @"traceSummaries"
{-# INLINEABLE gtsrrsTraceSummaries #-}
{-# DEPRECATED traceSummaries "Use generic-lens or generic-optics with 'traceSummaries' instead"  #-}

-- | The total number of traces processed, including traces that did not match the specified filter expression.
--
-- /Note:/ Consider using 'tracesProcessedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsTracesProcessedCount :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe Core.Integer)
gtsrrsTracesProcessedCount = Lens.field @"tracesProcessedCount"
{-# INLINEABLE gtsrrsTracesProcessedCount #-}
{-# DEPRECATED tracesProcessedCount "Use generic-lens or generic-optics with 'tracesProcessedCount' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrrsResponseStatus :: Lens.Lens' GetTraceSummariesResponse Core.Int
gtsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gtsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
