{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetTraceSummaries (..),
    mkGetTraceSummaries,

    -- ** Request lenses
    gtsFilterExpression,
    gtsStartTime,
    gtsNextToken,
    gtsTimeRangeType,
    gtsEndTime,
    gtsSamplingStrategy,
    gtsSampling,

    -- * Destructuring the response
    GetTraceSummariesResponse (..),
    mkGetTraceSummariesResponse,

    -- ** Response lenses
    gtsrsTracesProcessedCount,
    gtsrsNextToken,
    gtsrsApproximateTime,
    gtsrsTraceSummaries,
    gtsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetTraceSummaries' smart constructor.
data GetTraceSummaries = GetTraceSummaries'
  { -- | Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
    filterExpression :: Lude.Maybe Lude.Text,
    -- | The start of the time frame for which to retrieve traces.
    startTime :: Lude.Timestamp,
    -- | Specify the pagination token returned by a previous request to retrieve the next page of results.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A parameter to indicate whether to query trace summaries by TraceId or Event time.
    timeRangeType :: Lude.Maybe TimeRangeType,
    -- | The end of the time frame for which to retrieve traces.
    endTime :: Lude.Timestamp,
    -- | A parameter to indicate whether to enable sampling on trace summaries. Input parameters are Name and Value.
    samplingStrategy :: Lude.Maybe SamplingStrategy,
    -- | Set to @true@ to get summaries for only a subset of available traces.
    sampling :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTraceSummaries' with the minimum fields required to make a request.
--
-- * 'filterExpression' - Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
-- * 'startTime' - The start of the time frame for which to retrieve traces.
-- * 'nextToken' - Specify the pagination token returned by a previous request to retrieve the next page of results.
-- * 'timeRangeType' - A parameter to indicate whether to query trace summaries by TraceId or Event time.
-- * 'endTime' - The end of the time frame for which to retrieve traces.
-- * 'samplingStrategy' - A parameter to indicate whether to enable sampling on trace summaries. Input parameters are Name and Value.
-- * 'sampling' - Set to @true@ to get summaries for only a subset of available traces.
mkGetTraceSummaries ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  GetTraceSummaries
mkGetTraceSummaries pStartTime_ pEndTime_ =
  GetTraceSummaries'
    { filterExpression = Lude.Nothing,
      startTime = pStartTime_,
      nextToken = Lude.Nothing,
      timeRangeType = Lude.Nothing,
      endTime = pEndTime_,
      samplingStrategy = Lude.Nothing,
      sampling = Lude.Nothing
    }

-- | Specify a filter expression to retrieve trace summaries for services or requests that meet certain requirements.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsFilterExpression :: Lens.Lens' GetTraceSummaries (Lude.Maybe Lude.Text)
gtsFilterExpression = Lens.lens (filterExpression :: GetTraceSummaries -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: GetTraceSummaries)
{-# DEPRECATED gtsFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | The start of the time frame for which to retrieve traces.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsStartTime :: Lens.Lens' GetTraceSummaries Lude.Timestamp
gtsStartTime = Lens.lens (startTime :: GetTraceSummaries -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetTraceSummaries)
{-# DEPRECATED gtsStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | Specify the pagination token returned by a previous request to retrieve the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsNextToken :: Lens.Lens' GetTraceSummaries (Lude.Maybe Lude.Text)
gtsNextToken = Lens.lens (nextToken :: GetTraceSummaries -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTraceSummaries)
{-# DEPRECATED gtsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A parameter to indicate whether to query trace summaries by TraceId or Event time.
--
-- /Note:/ Consider using 'timeRangeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsTimeRangeType :: Lens.Lens' GetTraceSummaries (Lude.Maybe TimeRangeType)
gtsTimeRangeType = Lens.lens (timeRangeType :: GetTraceSummaries -> Lude.Maybe TimeRangeType) (\s a -> s {timeRangeType = a} :: GetTraceSummaries)
{-# DEPRECATED gtsTimeRangeType "Use generic-lens or generic-optics with 'timeRangeType' instead." #-}

-- | The end of the time frame for which to retrieve traces.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsEndTime :: Lens.Lens' GetTraceSummaries Lude.Timestamp
gtsEndTime = Lens.lens (endTime :: GetTraceSummaries -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetTraceSummaries)
{-# DEPRECATED gtsEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A parameter to indicate whether to enable sampling on trace summaries. Input parameters are Name and Value.
--
-- /Note:/ Consider using 'samplingStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsSamplingStrategy :: Lens.Lens' GetTraceSummaries (Lude.Maybe SamplingStrategy)
gtsSamplingStrategy = Lens.lens (samplingStrategy :: GetTraceSummaries -> Lude.Maybe SamplingStrategy) (\s a -> s {samplingStrategy = a} :: GetTraceSummaries)
{-# DEPRECATED gtsSamplingStrategy "Use generic-lens or generic-optics with 'samplingStrategy' instead." #-}

-- | Set to @true@ to get summaries for only a subset of available traces.
--
-- /Note:/ Consider using 'sampling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsSampling :: Lens.Lens' GetTraceSummaries (Lude.Maybe Lude.Bool)
gtsSampling = Lens.lens (sampling :: GetTraceSummaries -> Lude.Maybe Lude.Bool) (\s a -> s {sampling = a} :: GetTraceSummaries)
{-# DEPRECATED gtsSampling "Use generic-lens or generic-optics with 'sampling' instead." #-}

instance Page.AWSPager GetTraceSummaries where
  page rq rs
    | Page.stop (rs Lens.^. gtsrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtsrsTraceSummaries) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtsNextToken Lens..~ rs Lens.^. gtsrsNextToken

instance Lude.AWSRequest GetTraceSummaries where
  type Rs GetTraceSummaries = GetTraceSummariesResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTraceSummariesResponse'
            Lude.<$> (x Lude..?> "TracesProcessedCount")
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "ApproximateTime")
            Lude.<*> (x Lude..?> "TraceSummaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTraceSummaries where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetTraceSummaries where
  toJSON GetTraceSummaries' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FilterExpression" Lude..=) Lude.<$> filterExpression,
            Lude.Just ("StartTime" Lude..= startTime),
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("TimeRangeType" Lude..=) Lude.<$> timeRangeType,
            Lude.Just ("EndTime" Lude..= endTime),
            ("SamplingStrategy" Lude..=) Lude.<$> samplingStrategy,
            ("Sampling" Lude..=) Lude.<$> sampling
          ]
      )

instance Lude.ToPath GetTraceSummaries where
  toPath = Lude.const "/TraceSummaries"

instance Lude.ToQuery GetTraceSummaries where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTraceSummariesResponse' smart constructor.
data GetTraceSummariesResponse = GetTraceSummariesResponse'
  { -- | The total number of traces processed, including traces that did not match the specified filter expression.
    tracesProcessedCount :: Lude.Maybe Lude.Integer,
    -- | If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most recent results, closest to the end of the time frame.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The start time of this page of results.
    approximateTime :: Lude.Maybe Lude.Timestamp,
    -- | Trace IDs and annotations for traces that were found in the specified time frame.
    traceSummaries :: Lude.Maybe [TraceSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTraceSummariesResponse' with the minimum fields required to make a request.
--
-- * 'tracesProcessedCount' - The total number of traces processed, including traces that did not match the specified filter expression.
-- * 'nextToken' - If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most recent results, closest to the end of the time frame.
-- * 'approximateTime' - The start time of this page of results.
-- * 'traceSummaries' - Trace IDs and annotations for traces that were found in the specified time frame.
-- * 'responseStatus' - The response status code.
mkGetTraceSummariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTraceSummariesResponse
mkGetTraceSummariesResponse pResponseStatus_ =
  GetTraceSummariesResponse'
    { tracesProcessedCount = Lude.Nothing,
      nextToken = Lude.Nothing,
      approximateTime = Lude.Nothing,
      traceSummaries = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The total number of traces processed, including traces that did not match the specified filter expression.
--
-- /Note:/ Consider using 'tracesProcessedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsTracesProcessedCount :: Lens.Lens' GetTraceSummariesResponse (Lude.Maybe Lude.Integer)
gtsrsTracesProcessedCount = Lens.lens (tracesProcessedCount :: GetTraceSummariesResponse -> Lude.Maybe Lude.Integer) (\s a -> s {tracesProcessedCount = a} :: GetTraceSummariesResponse)
{-# DEPRECATED gtsrsTracesProcessedCount "Use generic-lens or generic-optics with 'tracesProcessedCount' instead." #-}

-- | If the requested time frame contained more than one page of results, you can use this token to retrieve the next page. The first page contains the most recent results, closest to the end of the time frame.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsNextToken :: Lens.Lens' GetTraceSummariesResponse (Lude.Maybe Lude.Text)
gtsrsNextToken = Lens.lens (nextToken :: GetTraceSummariesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTraceSummariesResponse)
{-# DEPRECATED gtsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The start time of this page of results.
--
-- /Note:/ Consider using 'approximateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsApproximateTime :: Lens.Lens' GetTraceSummariesResponse (Lude.Maybe Lude.Timestamp)
gtsrsApproximateTime = Lens.lens (approximateTime :: GetTraceSummariesResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {approximateTime = a} :: GetTraceSummariesResponse)
{-# DEPRECATED gtsrsApproximateTime "Use generic-lens or generic-optics with 'approximateTime' instead." #-}

-- | Trace IDs and annotations for traces that were found in the specified time frame.
--
-- /Note:/ Consider using 'traceSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsTraceSummaries :: Lens.Lens' GetTraceSummariesResponse (Lude.Maybe [TraceSummary])
gtsrsTraceSummaries = Lens.lens (traceSummaries :: GetTraceSummariesResponse -> Lude.Maybe [TraceSummary]) (\s a -> s {traceSummaries = a} :: GetTraceSummariesResponse)
{-# DEPRECATED gtsrsTraceSummaries "Use generic-lens or generic-optics with 'traceSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsrsResponseStatus :: Lens.Lens' GetTraceSummariesResponse Lude.Int
gtsrsResponseStatus = Lens.lens (responseStatus :: GetTraceSummariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTraceSummariesResponse)
{-# DEPRECATED gtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
