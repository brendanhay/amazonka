{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetTraceSummaries
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves IDs and annotations for traces available for a specified time
-- frame using an optional filter. To get the full traces, pass the trace
-- IDs to @BatchGetTraces@.
--
-- A filter expression can target traced requests that hit specific service
-- nodes or edges, have errors, or come from a known user. For example, the
-- following filter expression targets traces that pass through
-- @api.example.com@:
--
-- @service(\"api.example.com\")@
--
-- This filter expression finds traces that have an annotation named
-- @account@ with the value @12345@:
--
-- @annotation.account = \"12345\"@
--
-- For a full list of indexed fields and keywords that you can use in
-- filter expressions, see
-- <https://docs.aws.amazon.com/xray/latest/devguide/xray-console-filters.html Using Filter Expressions>
-- in the /AWS X-Ray Developer Guide/.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTraceSummaries
  ( -- * Creating a Request
    GetTraceSummaries (..),
    newGetTraceSummaries,

    -- * Request Lenses
    getTraceSummaries_nextToken,
    getTraceSummaries_filterExpression,
    getTraceSummaries_timeRangeType,
    getTraceSummaries_sampling,
    getTraceSummaries_samplingStrategy,
    getTraceSummaries_startTime,
    getTraceSummaries_endTime,

    -- * Destructuring the Response
    GetTraceSummariesResponse (..),
    newGetTraceSummariesResponse,

    -- * Response Lenses
    getTraceSummariesResponse_nextToken,
    getTraceSummariesResponse_tracesProcessedCount,
    getTraceSummariesResponse_traceSummaries,
    getTraceSummariesResponse_approximateTime,
    getTraceSummariesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetTraceSummaries' smart constructor.
data GetTraceSummaries = GetTraceSummaries'
  { -- | Specify the pagination token returned by a previous request to retrieve
    -- the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Specify a filter expression to retrieve trace summaries for services or
    -- requests that meet certain requirements.
    filterExpression :: Core.Maybe Core.Text,
    -- | A parameter to indicate whether to query trace summaries by TraceId or
    -- Event time.
    timeRangeType :: Core.Maybe TimeRangeType,
    -- | Set to @true@ to get summaries for only a subset of available traces.
    sampling :: Core.Maybe Core.Bool,
    -- | A parameter to indicate whether to enable sampling on trace summaries.
    -- Input parameters are Name and Value.
    samplingStrategy :: Core.Maybe SamplingStrategy,
    -- | The start of the time frame for which to retrieve traces.
    startTime :: Core.POSIX,
    -- | The end of the time frame for which to retrieve traces.
    endTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTraceSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTraceSummaries_nextToken' - Specify the pagination token returned by a previous request to retrieve
-- the next page of results.
--
-- 'filterExpression', 'getTraceSummaries_filterExpression' - Specify a filter expression to retrieve trace summaries for services or
-- requests that meet certain requirements.
--
-- 'timeRangeType', 'getTraceSummaries_timeRangeType' - A parameter to indicate whether to query trace summaries by TraceId or
-- Event time.
--
-- 'sampling', 'getTraceSummaries_sampling' - Set to @true@ to get summaries for only a subset of available traces.
--
-- 'samplingStrategy', 'getTraceSummaries_samplingStrategy' - A parameter to indicate whether to enable sampling on trace summaries.
-- Input parameters are Name and Value.
--
-- 'startTime', 'getTraceSummaries_startTime' - The start of the time frame for which to retrieve traces.
--
-- 'endTime', 'getTraceSummaries_endTime' - The end of the time frame for which to retrieve traces.
newGetTraceSummaries ::
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  GetTraceSummaries
newGetTraceSummaries pStartTime_ pEndTime_ =
  GetTraceSummaries'
    { nextToken = Core.Nothing,
      filterExpression = Core.Nothing,
      timeRangeType = Core.Nothing,
      sampling = Core.Nothing,
      samplingStrategy = Core.Nothing,
      startTime = Core._Time Lens.# pStartTime_,
      endTime = Core._Time Lens.# pEndTime_
    }

-- | Specify the pagination token returned by a previous request to retrieve
-- the next page of results.
getTraceSummaries_nextToken :: Lens.Lens' GetTraceSummaries (Core.Maybe Core.Text)
getTraceSummaries_nextToken = Lens.lens (\GetTraceSummaries' {nextToken} -> nextToken) (\s@GetTraceSummaries' {} a -> s {nextToken = a} :: GetTraceSummaries)

-- | Specify a filter expression to retrieve trace summaries for services or
-- requests that meet certain requirements.
getTraceSummaries_filterExpression :: Lens.Lens' GetTraceSummaries (Core.Maybe Core.Text)
getTraceSummaries_filterExpression = Lens.lens (\GetTraceSummaries' {filterExpression} -> filterExpression) (\s@GetTraceSummaries' {} a -> s {filterExpression = a} :: GetTraceSummaries)

-- | A parameter to indicate whether to query trace summaries by TraceId or
-- Event time.
getTraceSummaries_timeRangeType :: Lens.Lens' GetTraceSummaries (Core.Maybe TimeRangeType)
getTraceSummaries_timeRangeType = Lens.lens (\GetTraceSummaries' {timeRangeType} -> timeRangeType) (\s@GetTraceSummaries' {} a -> s {timeRangeType = a} :: GetTraceSummaries)

-- | Set to @true@ to get summaries for only a subset of available traces.
getTraceSummaries_sampling :: Lens.Lens' GetTraceSummaries (Core.Maybe Core.Bool)
getTraceSummaries_sampling = Lens.lens (\GetTraceSummaries' {sampling} -> sampling) (\s@GetTraceSummaries' {} a -> s {sampling = a} :: GetTraceSummaries)

-- | A parameter to indicate whether to enable sampling on trace summaries.
-- Input parameters are Name and Value.
getTraceSummaries_samplingStrategy :: Lens.Lens' GetTraceSummaries (Core.Maybe SamplingStrategy)
getTraceSummaries_samplingStrategy = Lens.lens (\GetTraceSummaries' {samplingStrategy} -> samplingStrategy) (\s@GetTraceSummaries' {} a -> s {samplingStrategy = a} :: GetTraceSummaries)

-- | The start of the time frame for which to retrieve traces.
getTraceSummaries_startTime :: Lens.Lens' GetTraceSummaries Core.UTCTime
getTraceSummaries_startTime = Lens.lens (\GetTraceSummaries' {startTime} -> startTime) (\s@GetTraceSummaries' {} a -> s {startTime = a} :: GetTraceSummaries) Core.. Core._Time

-- | The end of the time frame for which to retrieve traces.
getTraceSummaries_endTime :: Lens.Lens' GetTraceSummaries Core.UTCTime
getTraceSummaries_endTime = Lens.lens (\GetTraceSummaries' {endTime} -> endTime) (\s@GetTraceSummaries' {} a -> s {endTime = a} :: GetTraceSummaries) Core.. Core._Time

instance Core.AWSPager GetTraceSummaries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTraceSummariesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTraceSummariesResponse_traceSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTraceSummaries_nextToken
          Lens..~ rs
          Lens.^? getTraceSummariesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetTraceSummaries where
  type
    AWSResponse GetTraceSummaries =
      GetTraceSummariesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTraceSummariesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "TracesProcessedCount")
            Core.<*> (x Core..?> "TraceSummaries" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "ApproximateTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTraceSummaries

instance Core.NFData GetTraceSummaries

instance Core.ToHeaders GetTraceSummaries where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetTraceSummaries where
  toJSON GetTraceSummaries' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("FilterExpression" Core..=)
              Core.<$> filterExpression,
            ("TimeRangeType" Core..=) Core.<$> timeRangeType,
            ("Sampling" Core..=) Core.<$> sampling,
            ("SamplingStrategy" Core..=)
              Core.<$> samplingStrategy,
            Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.ToPath GetTraceSummaries where
  toPath = Core.const "/TraceSummaries"

instance Core.ToQuery GetTraceSummaries where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTraceSummariesResponse' smart constructor.
data GetTraceSummariesResponse = GetTraceSummariesResponse'
  { -- | If the requested time frame contained more than one page of results, you
    -- can use this token to retrieve the next page. The first page contains
    -- the most recent results, closest to the end of the time frame.
    nextToken :: Core.Maybe Core.Text,
    -- | The total number of traces processed, including traces that did not
    -- match the specified filter expression.
    tracesProcessedCount :: Core.Maybe Core.Integer,
    -- | Trace IDs and annotations for traces that were found in the specified
    -- time frame.
    traceSummaries :: Core.Maybe [TraceSummary],
    -- | The start time of this page of results.
    approximateTime :: Core.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTraceSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTraceSummariesResponse_nextToken' - If the requested time frame contained more than one page of results, you
-- can use this token to retrieve the next page. The first page contains
-- the most recent results, closest to the end of the time frame.
--
-- 'tracesProcessedCount', 'getTraceSummariesResponse_tracesProcessedCount' - The total number of traces processed, including traces that did not
-- match the specified filter expression.
--
-- 'traceSummaries', 'getTraceSummariesResponse_traceSummaries' - Trace IDs and annotations for traces that were found in the specified
-- time frame.
--
-- 'approximateTime', 'getTraceSummariesResponse_approximateTime' - The start time of this page of results.
--
-- 'httpStatus', 'getTraceSummariesResponse_httpStatus' - The response's http status code.
newGetTraceSummariesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTraceSummariesResponse
newGetTraceSummariesResponse pHttpStatus_ =
  GetTraceSummariesResponse'
    { nextToken =
        Core.Nothing,
      tracesProcessedCount = Core.Nothing,
      traceSummaries = Core.Nothing,
      approximateTime = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the requested time frame contained more than one page of results, you
-- can use this token to retrieve the next page. The first page contains
-- the most recent results, closest to the end of the time frame.
getTraceSummariesResponse_nextToken :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe Core.Text)
getTraceSummariesResponse_nextToken = Lens.lens (\GetTraceSummariesResponse' {nextToken} -> nextToken) (\s@GetTraceSummariesResponse' {} a -> s {nextToken = a} :: GetTraceSummariesResponse)

-- | The total number of traces processed, including traces that did not
-- match the specified filter expression.
getTraceSummariesResponse_tracesProcessedCount :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe Core.Integer)
getTraceSummariesResponse_tracesProcessedCount = Lens.lens (\GetTraceSummariesResponse' {tracesProcessedCount} -> tracesProcessedCount) (\s@GetTraceSummariesResponse' {} a -> s {tracesProcessedCount = a} :: GetTraceSummariesResponse)

-- | Trace IDs and annotations for traces that were found in the specified
-- time frame.
getTraceSummariesResponse_traceSummaries :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe [TraceSummary])
getTraceSummariesResponse_traceSummaries = Lens.lens (\GetTraceSummariesResponse' {traceSummaries} -> traceSummaries) (\s@GetTraceSummariesResponse' {} a -> s {traceSummaries = a} :: GetTraceSummariesResponse) Core.. Lens.mapping Lens._Coerce

-- | The start time of this page of results.
getTraceSummariesResponse_approximateTime :: Lens.Lens' GetTraceSummariesResponse (Core.Maybe Core.UTCTime)
getTraceSummariesResponse_approximateTime = Lens.lens (\GetTraceSummariesResponse' {approximateTime} -> approximateTime) (\s@GetTraceSummariesResponse' {} a -> s {approximateTime = a} :: GetTraceSummariesResponse) Core.. Lens.mapping Core._Time

-- | The response's http status code.
getTraceSummariesResponse_httpStatus :: Lens.Lens' GetTraceSummariesResponse Core.Int
getTraceSummariesResponse_httpStatus = Lens.lens (\GetTraceSummariesResponse' {httpStatus} -> httpStatus) (\s@GetTraceSummariesResponse' {} a -> s {httpStatus = a} :: GetTraceSummariesResponse)

instance Core.NFData GetTraceSummariesResponse
