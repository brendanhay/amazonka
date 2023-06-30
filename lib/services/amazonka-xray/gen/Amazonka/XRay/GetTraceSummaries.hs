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
-- Module      : Amazonka.XRay.GetTraceSummaries
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- in the /Amazon Web Services X-Ray Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.XRay.GetTraceSummaries
  ( -- * Creating a Request
    GetTraceSummaries (..),
    newGetTraceSummaries,

    -- * Request Lenses
    getTraceSummaries_filterExpression,
    getTraceSummaries_nextToken,
    getTraceSummaries_sampling,
    getTraceSummaries_samplingStrategy,
    getTraceSummaries_timeRangeType,
    getTraceSummaries_startTime,
    getTraceSummaries_endTime,

    -- * Destructuring the Response
    GetTraceSummariesResponse (..),
    newGetTraceSummariesResponse,

    -- * Response Lenses
    getTraceSummariesResponse_approximateTime,
    getTraceSummariesResponse_nextToken,
    getTraceSummariesResponse_traceSummaries,
    getTraceSummariesResponse_tracesProcessedCount,
    getTraceSummariesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetTraceSummaries' smart constructor.
data GetTraceSummaries = GetTraceSummaries'
  { -- | Specify a filter expression to retrieve trace summaries for services or
    -- requests that meet certain requirements.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | Specify the pagination token returned by a previous request to retrieve
    -- the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set to @true@ to get summaries for only a subset of available traces.
    sampling :: Prelude.Maybe Prelude.Bool,
    -- | A parameter to indicate whether to enable sampling on trace summaries.
    -- Input parameters are Name and Value.
    samplingStrategy :: Prelude.Maybe SamplingStrategy,
    -- | A parameter to indicate whether to query trace summaries by TraceId or
    -- Event time.
    timeRangeType :: Prelude.Maybe TimeRangeType,
    -- | The start of the time frame for which to retrieve traces.
    startTime :: Data.POSIX,
    -- | The end of the time frame for which to retrieve traces.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTraceSummaries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterExpression', 'getTraceSummaries_filterExpression' - Specify a filter expression to retrieve trace summaries for services or
-- requests that meet certain requirements.
--
-- 'nextToken', 'getTraceSummaries_nextToken' - Specify the pagination token returned by a previous request to retrieve
-- the next page of results.
--
-- 'sampling', 'getTraceSummaries_sampling' - Set to @true@ to get summaries for only a subset of available traces.
--
-- 'samplingStrategy', 'getTraceSummaries_samplingStrategy' - A parameter to indicate whether to enable sampling on trace summaries.
-- Input parameters are Name and Value.
--
-- 'timeRangeType', 'getTraceSummaries_timeRangeType' - A parameter to indicate whether to query trace summaries by TraceId or
-- Event time.
--
-- 'startTime', 'getTraceSummaries_startTime' - The start of the time frame for which to retrieve traces.
--
-- 'endTime', 'getTraceSummaries_endTime' - The end of the time frame for which to retrieve traces.
newGetTraceSummaries ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetTraceSummaries
newGetTraceSummaries pStartTime_ pEndTime_ =
  GetTraceSummaries'
    { filterExpression =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sampling = Prelude.Nothing,
      samplingStrategy = Prelude.Nothing,
      timeRangeType = Prelude.Nothing,
      startTime = Data._Time Lens.# pStartTime_,
      endTime = Data._Time Lens.# pEndTime_
    }

-- | Specify a filter expression to retrieve trace summaries for services or
-- requests that meet certain requirements.
getTraceSummaries_filterExpression :: Lens.Lens' GetTraceSummaries (Prelude.Maybe Prelude.Text)
getTraceSummaries_filterExpression = Lens.lens (\GetTraceSummaries' {filterExpression} -> filterExpression) (\s@GetTraceSummaries' {} a -> s {filterExpression = a} :: GetTraceSummaries)

-- | Specify the pagination token returned by a previous request to retrieve
-- the next page of results.
getTraceSummaries_nextToken :: Lens.Lens' GetTraceSummaries (Prelude.Maybe Prelude.Text)
getTraceSummaries_nextToken = Lens.lens (\GetTraceSummaries' {nextToken} -> nextToken) (\s@GetTraceSummaries' {} a -> s {nextToken = a} :: GetTraceSummaries)

-- | Set to @true@ to get summaries for only a subset of available traces.
getTraceSummaries_sampling :: Lens.Lens' GetTraceSummaries (Prelude.Maybe Prelude.Bool)
getTraceSummaries_sampling = Lens.lens (\GetTraceSummaries' {sampling} -> sampling) (\s@GetTraceSummaries' {} a -> s {sampling = a} :: GetTraceSummaries)

-- | A parameter to indicate whether to enable sampling on trace summaries.
-- Input parameters are Name and Value.
getTraceSummaries_samplingStrategy :: Lens.Lens' GetTraceSummaries (Prelude.Maybe SamplingStrategy)
getTraceSummaries_samplingStrategy = Lens.lens (\GetTraceSummaries' {samplingStrategy} -> samplingStrategy) (\s@GetTraceSummaries' {} a -> s {samplingStrategy = a} :: GetTraceSummaries)

-- | A parameter to indicate whether to query trace summaries by TraceId or
-- Event time.
getTraceSummaries_timeRangeType :: Lens.Lens' GetTraceSummaries (Prelude.Maybe TimeRangeType)
getTraceSummaries_timeRangeType = Lens.lens (\GetTraceSummaries' {timeRangeType} -> timeRangeType) (\s@GetTraceSummaries' {} a -> s {timeRangeType = a} :: GetTraceSummaries)

-- | The start of the time frame for which to retrieve traces.
getTraceSummaries_startTime :: Lens.Lens' GetTraceSummaries Prelude.UTCTime
getTraceSummaries_startTime = Lens.lens (\GetTraceSummaries' {startTime} -> startTime) (\s@GetTraceSummaries' {} a -> s {startTime = a} :: GetTraceSummaries) Prelude.. Data._Time

-- | The end of the time frame for which to retrieve traces.
getTraceSummaries_endTime :: Lens.Lens' GetTraceSummaries Prelude.UTCTime
getTraceSummaries_endTime = Lens.lens (\GetTraceSummaries' {endTime} -> endTime) (\s@GetTraceSummaries' {} a -> s {endTime = a} :: GetTraceSummaries) Prelude.. Data._Time

instance Core.AWSPager GetTraceSummaries where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTraceSummariesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTraceSummariesResponse_traceSummaries
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getTraceSummaries_nextToken
          Lens..~ rs
          Lens.^? getTraceSummariesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetTraceSummaries where
  type
    AWSResponse GetTraceSummaries =
      GetTraceSummariesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTraceSummariesResponse'
            Prelude.<$> (x Data..?> "ApproximateTime")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TraceSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "TracesProcessedCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTraceSummaries where
  hashWithSalt _salt GetTraceSummaries' {..} =
    _salt
      `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sampling
      `Prelude.hashWithSalt` samplingStrategy
      `Prelude.hashWithSalt` timeRangeType
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData GetTraceSummaries where
  rnf GetTraceSummaries' {..} =
    Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sampling
      `Prelude.seq` Prelude.rnf samplingStrategy
      `Prelude.seq` Prelude.rnf timeRangeType
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders GetTraceSummaries where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetTraceSummaries where
  toJSON GetTraceSummaries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilterExpression" Data..=)
              Prelude.<$> filterExpression,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Sampling" Data..=) Prelude.<$> sampling,
            ("SamplingStrategy" Data..=)
              Prelude.<$> samplingStrategy,
            ("TimeRangeType" Data..=) Prelude.<$> timeRangeType,
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath GetTraceSummaries where
  toPath = Prelude.const "/TraceSummaries"

instance Data.ToQuery GetTraceSummaries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTraceSummariesResponse' smart constructor.
data GetTraceSummariesResponse = GetTraceSummariesResponse'
  { -- | The start time of this page of results.
    approximateTime :: Prelude.Maybe Data.POSIX,
    -- | If the requested time frame contained more than one page of results, you
    -- can use this token to retrieve the next page. The first page contains
    -- the most recent results, closest to the end of the time frame.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Trace IDs and annotations for traces that were found in the specified
    -- time frame.
    traceSummaries :: Prelude.Maybe [TraceSummary],
    -- | The total number of traces processed, including traces that did not
    -- match the specified filter expression.
    tracesProcessedCount :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTraceSummariesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateTime', 'getTraceSummariesResponse_approximateTime' - The start time of this page of results.
--
-- 'nextToken', 'getTraceSummariesResponse_nextToken' - If the requested time frame contained more than one page of results, you
-- can use this token to retrieve the next page. The first page contains
-- the most recent results, closest to the end of the time frame.
--
-- 'traceSummaries', 'getTraceSummariesResponse_traceSummaries' - Trace IDs and annotations for traces that were found in the specified
-- time frame.
--
-- 'tracesProcessedCount', 'getTraceSummariesResponse_tracesProcessedCount' - The total number of traces processed, including traces that did not
-- match the specified filter expression.
--
-- 'httpStatus', 'getTraceSummariesResponse_httpStatus' - The response's http status code.
newGetTraceSummariesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTraceSummariesResponse
newGetTraceSummariesResponse pHttpStatus_ =
  GetTraceSummariesResponse'
    { approximateTime =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      traceSummaries = Prelude.Nothing,
      tracesProcessedCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The start time of this page of results.
getTraceSummariesResponse_approximateTime :: Lens.Lens' GetTraceSummariesResponse (Prelude.Maybe Prelude.UTCTime)
getTraceSummariesResponse_approximateTime = Lens.lens (\GetTraceSummariesResponse' {approximateTime} -> approximateTime) (\s@GetTraceSummariesResponse' {} a -> s {approximateTime = a} :: GetTraceSummariesResponse) Prelude.. Lens.mapping Data._Time

-- | If the requested time frame contained more than one page of results, you
-- can use this token to retrieve the next page. The first page contains
-- the most recent results, closest to the end of the time frame.
getTraceSummariesResponse_nextToken :: Lens.Lens' GetTraceSummariesResponse (Prelude.Maybe Prelude.Text)
getTraceSummariesResponse_nextToken = Lens.lens (\GetTraceSummariesResponse' {nextToken} -> nextToken) (\s@GetTraceSummariesResponse' {} a -> s {nextToken = a} :: GetTraceSummariesResponse)

-- | Trace IDs and annotations for traces that were found in the specified
-- time frame.
getTraceSummariesResponse_traceSummaries :: Lens.Lens' GetTraceSummariesResponse (Prelude.Maybe [TraceSummary])
getTraceSummariesResponse_traceSummaries = Lens.lens (\GetTraceSummariesResponse' {traceSummaries} -> traceSummaries) (\s@GetTraceSummariesResponse' {} a -> s {traceSummaries = a} :: GetTraceSummariesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of traces processed, including traces that did not
-- match the specified filter expression.
getTraceSummariesResponse_tracesProcessedCount :: Lens.Lens' GetTraceSummariesResponse (Prelude.Maybe Prelude.Integer)
getTraceSummariesResponse_tracesProcessedCount = Lens.lens (\GetTraceSummariesResponse' {tracesProcessedCount} -> tracesProcessedCount) (\s@GetTraceSummariesResponse' {} a -> s {tracesProcessedCount = a} :: GetTraceSummariesResponse)

-- | The response's http status code.
getTraceSummariesResponse_httpStatus :: Lens.Lens' GetTraceSummariesResponse Prelude.Int
getTraceSummariesResponse_httpStatus = Lens.lens (\GetTraceSummariesResponse' {httpStatus} -> httpStatus) (\s@GetTraceSummariesResponse' {} a -> s {httpStatus = a} :: GetTraceSummariesResponse)

instance Prelude.NFData GetTraceSummariesResponse where
  rnf GetTraceSummariesResponse' {..} =
    Prelude.rnf approximateTime
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf traceSummaries
      `Prelude.seq` Prelude.rnf tracesProcessedCount
      `Prelude.seq` Prelude.rnf httpStatus
