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
-- Module      : Network.AWS.XRay.GetTimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an aggregation of service statistics defined by a specific time
-- range.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTimeSeriesServiceStatistics
  ( -- * Creating a Request
    GetTimeSeriesServiceStatistics (..),
    newGetTimeSeriesServiceStatistics,

    -- * Request Lenses
    getTimeSeriesServiceStatistics_nextToken,
    getTimeSeriesServiceStatistics_entitySelectorExpression,
    getTimeSeriesServiceStatistics_groupName,
    getTimeSeriesServiceStatistics_forecastStatistics,
    getTimeSeriesServiceStatistics_period,
    getTimeSeriesServiceStatistics_groupARN,
    getTimeSeriesServiceStatistics_startTime,
    getTimeSeriesServiceStatistics_endTime,

    -- * Destructuring the Response
    GetTimeSeriesServiceStatisticsResponse (..),
    newGetTimeSeriesServiceStatisticsResponse,

    -- * Response Lenses
    getTimeSeriesServiceStatisticsResponse_nextToken,
    getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics,
    getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions,
    getTimeSeriesServiceStatisticsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.XRay.Types

-- | /See:/ 'newGetTimeSeriesServiceStatistics' smart constructor.
data GetTimeSeriesServiceStatistics = GetTimeSeriesServiceStatistics'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter expression defining entities that will be aggregated for
    -- statistics. Supports ID, service, and edge functions. If no selector
    -- expression is specified, edge statistics are returned.
    entitySelectorExpression :: Core.Maybe Core.Text,
    -- | The case-sensitive name of the group for which to pull statistics from.
    groupName :: Core.Maybe Core.Text,
    -- | The forecasted high and low fault count values. Forecast enabled
    -- requests require the EntitySelectorExpression ID be provided.
    forecastStatistics :: Core.Maybe Core.Bool,
    -- | Aggregation period in seconds.
    period :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the group for which to pull statistics
    -- from.
    groupARN :: Core.Maybe Core.Text,
    -- | The start of the time frame for which to aggregate statistics.
    startTime :: Core.POSIX,
    -- | The end of the time frame for which to aggregate statistics.
    endTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTimeSeriesServiceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTimeSeriesServiceStatistics_nextToken' - Pagination token.
--
-- 'entitySelectorExpression', 'getTimeSeriesServiceStatistics_entitySelectorExpression' - A filter expression defining entities that will be aggregated for
-- statistics. Supports ID, service, and edge functions. If no selector
-- expression is specified, edge statistics are returned.
--
-- 'groupName', 'getTimeSeriesServiceStatistics_groupName' - The case-sensitive name of the group for which to pull statistics from.
--
-- 'forecastStatistics', 'getTimeSeriesServiceStatistics_forecastStatistics' - The forecasted high and low fault count values. Forecast enabled
-- requests require the EntitySelectorExpression ID be provided.
--
-- 'period', 'getTimeSeriesServiceStatistics_period' - Aggregation period in seconds.
--
-- 'groupARN', 'getTimeSeriesServiceStatistics_groupARN' - The Amazon Resource Name (ARN) of the group for which to pull statistics
-- from.
--
-- 'startTime', 'getTimeSeriesServiceStatistics_startTime' - The start of the time frame for which to aggregate statistics.
--
-- 'endTime', 'getTimeSeriesServiceStatistics_endTime' - The end of the time frame for which to aggregate statistics.
newGetTimeSeriesServiceStatistics ::
  -- | 'startTime'
  Core.UTCTime ->
  -- | 'endTime'
  Core.UTCTime ->
  GetTimeSeriesServiceStatistics
newGetTimeSeriesServiceStatistics
  pStartTime_
  pEndTime_ =
    GetTimeSeriesServiceStatistics'
      { nextToken =
          Core.Nothing,
        entitySelectorExpression = Core.Nothing,
        groupName = Core.Nothing,
        forecastStatistics = Core.Nothing,
        period = Core.Nothing,
        groupARN = Core.Nothing,
        startTime = Core._Time Lens.# pStartTime_,
        endTime = Core._Time Lens.# pEndTime_
      }

-- | Pagination token.
getTimeSeriesServiceStatistics_nextToken :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Text)
getTimeSeriesServiceStatistics_nextToken = Lens.lens (\GetTimeSeriesServiceStatistics' {nextToken} -> nextToken) (\s@GetTimeSeriesServiceStatistics' {} a -> s {nextToken = a} :: GetTimeSeriesServiceStatistics)

-- | A filter expression defining entities that will be aggregated for
-- statistics. Supports ID, service, and edge functions. If no selector
-- expression is specified, edge statistics are returned.
getTimeSeriesServiceStatistics_entitySelectorExpression :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Text)
getTimeSeriesServiceStatistics_entitySelectorExpression = Lens.lens (\GetTimeSeriesServiceStatistics' {entitySelectorExpression} -> entitySelectorExpression) (\s@GetTimeSeriesServiceStatistics' {} a -> s {entitySelectorExpression = a} :: GetTimeSeriesServiceStatistics)

-- | The case-sensitive name of the group for which to pull statistics from.
getTimeSeriesServiceStatistics_groupName :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Text)
getTimeSeriesServiceStatistics_groupName = Lens.lens (\GetTimeSeriesServiceStatistics' {groupName} -> groupName) (\s@GetTimeSeriesServiceStatistics' {} a -> s {groupName = a} :: GetTimeSeriesServiceStatistics)

-- | The forecasted high and low fault count values. Forecast enabled
-- requests require the EntitySelectorExpression ID be provided.
getTimeSeriesServiceStatistics_forecastStatistics :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Bool)
getTimeSeriesServiceStatistics_forecastStatistics = Lens.lens (\GetTimeSeriesServiceStatistics' {forecastStatistics} -> forecastStatistics) (\s@GetTimeSeriesServiceStatistics' {} a -> s {forecastStatistics = a} :: GetTimeSeriesServiceStatistics)

-- | Aggregation period in seconds.
getTimeSeriesServiceStatistics_period :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Int)
getTimeSeriesServiceStatistics_period = Lens.lens (\GetTimeSeriesServiceStatistics' {period} -> period) (\s@GetTimeSeriesServiceStatistics' {} a -> s {period = a} :: GetTimeSeriesServiceStatistics)

-- | The Amazon Resource Name (ARN) of the group for which to pull statistics
-- from.
getTimeSeriesServiceStatistics_groupARN :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Text)
getTimeSeriesServiceStatistics_groupARN = Lens.lens (\GetTimeSeriesServiceStatistics' {groupARN} -> groupARN) (\s@GetTimeSeriesServiceStatistics' {} a -> s {groupARN = a} :: GetTimeSeriesServiceStatistics)

-- | The start of the time frame for which to aggregate statistics.
getTimeSeriesServiceStatistics_startTime :: Lens.Lens' GetTimeSeriesServiceStatistics Core.UTCTime
getTimeSeriesServiceStatistics_startTime = Lens.lens (\GetTimeSeriesServiceStatistics' {startTime} -> startTime) (\s@GetTimeSeriesServiceStatistics' {} a -> s {startTime = a} :: GetTimeSeriesServiceStatistics) Core.. Core._Time

-- | The end of the time frame for which to aggregate statistics.
getTimeSeriesServiceStatistics_endTime :: Lens.Lens' GetTimeSeriesServiceStatistics Core.UTCTime
getTimeSeriesServiceStatistics_endTime = Lens.lens (\GetTimeSeriesServiceStatistics' {endTime} -> endTime) (\s@GetTimeSeriesServiceStatistics' {} a -> s {endTime = a} :: GetTimeSeriesServiceStatistics) Core.. Core._Time

instance Core.AWSPager GetTimeSeriesServiceStatistics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTimeSeriesServiceStatisticsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTimeSeriesServiceStatistics_nextToken
          Lens..~ rs
          Lens.^? getTimeSeriesServiceStatisticsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetTimeSeriesServiceStatistics
  where
  type
    AWSResponse GetTimeSeriesServiceStatistics =
      GetTimeSeriesServiceStatisticsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTimeSeriesServiceStatisticsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "TimeSeriesServiceStatistics"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "ContainsOldGroupVersions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTimeSeriesServiceStatistics

instance Core.NFData GetTimeSeriesServiceStatistics

instance
  Core.ToHeaders
    GetTimeSeriesServiceStatistics
  where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON GetTimeSeriesServiceStatistics where
  toJSON GetTimeSeriesServiceStatistics' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("EntitySelectorExpression" Core..=)
              Core.<$> entitySelectorExpression,
            ("GroupName" Core..=) Core.<$> groupName,
            ("ForecastStatistics" Core..=)
              Core.<$> forecastStatistics,
            ("Period" Core..=) Core.<$> period,
            ("GroupARN" Core..=) Core.<$> groupARN,
            Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime)
          ]
      )

instance Core.ToPath GetTimeSeriesServiceStatistics where
  toPath = Core.const "/TimeSeriesServiceStatistics"

instance Core.ToQuery GetTimeSeriesServiceStatistics where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTimeSeriesServiceStatisticsResponse' smart constructor.
data GetTimeSeriesServiceStatisticsResponse = GetTimeSeriesServiceStatisticsResponse'
  { -- | Pagination token.
    nextToken :: Core.Maybe Core.Text,
    -- | The collection of statistics.
    timeSeriesServiceStatistics :: Core.Maybe [TimeSeriesServiceStatistics],
    -- | A flag indicating whether or not a group\'s filter expression has been
    -- consistent, or if a returned aggregation might show statistics from an
    -- older version of the group\'s filter expression.
    containsOldGroupVersions :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTimeSeriesServiceStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTimeSeriesServiceStatisticsResponse_nextToken' - Pagination token.
--
-- 'timeSeriesServiceStatistics', 'getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics' - The collection of statistics.
--
-- 'containsOldGroupVersions', 'getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions' - A flag indicating whether or not a group\'s filter expression has been
-- consistent, or if a returned aggregation might show statistics from an
-- older version of the group\'s filter expression.
--
-- 'httpStatus', 'getTimeSeriesServiceStatisticsResponse_httpStatus' - The response's http status code.
newGetTimeSeriesServiceStatisticsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTimeSeriesServiceStatisticsResponse
newGetTimeSeriesServiceStatisticsResponse
  pHttpStatus_ =
    GetTimeSeriesServiceStatisticsResponse'
      { nextToken =
          Core.Nothing,
        timeSeriesServiceStatistics =
          Core.Nothing,
        containsOldGroupVersions =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Pagination token.
getTimeSeriesServiceStatisticsResponse_nextToken :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Core.Maybe Core.Text)
getTimeSeriesServiceStatisticsResponse_nextToken = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {nextToken} -> nextToken) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {nextToken = a} :: GetTimeSeriesServiceStatisticsResponse)

-- | The collection of statistics.
getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Core.Maybe [TimeSeriesServiceStatistics])
getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {timeSeriesServiceStatistics} -> timeSeriesServiceStatistics) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {timeSeriesServiceStatistics = a} :: GetTimeSeriesServiceStatisticsResponse) Core.. Lens.mapping Lens._Coerce

-- | A flag indicating whether or not a group\'s filter expression has been
-- consistent, or if a returned aggregation might show statistics from an
-- older version of the group\'s filter expression.
getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Core.Maybe Core.Bool)
getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {containsOldGroupVersions} -> containsOldGroupVersions) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {containsOldGroupVersions = a} :: GetTimeSeriesServiceStatisticsResponse)

-- | The response's http status code.
getTimeSeriesServiceStatisticsResponse_httpStatus :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse Core.Int
getTimeSeriesServiceStatisticsResponse_httpStatus = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {httpStatus = a} :: GetTimeSeriesServiceStatisticsResponse)

instance
  Core.NFData
    GetTimeSeriesServiceStatisticsResponse
