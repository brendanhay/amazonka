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
-- Module      : Amazonka.XRay.GetTimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an aggregation of service statistics defined by a specific time
-- range.
--
-- This operation returns paginated results.
module Amazonka.XRay.GetTimeSeriesServiceStatistics
  ( -- * Creating a Request
    GetTimeSeriesServiceStatistics (..),
    newGetTimeSeriesServiceStatistics,

    -- * Request Lenses
    getTimeSeriesServiceStatistics_entitySelectorExpression,
    getTimeSeriesServiceStatistics_forecastStatistics,
    getTimeSeriesServiceStatistics_groupARN,
    getTimeSeriesServiceStatistics_groupName,
    getTimeSeriesServiceStatistics_nextToken,
    getTimeSeriesServiceStatistics_period,
    getTimeSeriesServiceStatistics_startTime,
    getTimeSeriesServiceStatistics_endTime,

    -- * Destructuring the Response
    GetTimeSeriesServiceStatisticsResponse (..),
    newGetTimeSeriesServiceStatisticsResponse,

    -- * Response Lenses
    getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions,
    getTimeSeriesServiceStatisticsResponse_nextToken,
    getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics,
    getTimeSeriesServiceStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.XRay.Types

-- | /See:/ 'newGetTimeSeriesServiceStatistics' smart constructor.
data GetTimeSeriesServiceStatistics = GetTimeSeriesServiceStatistics'
  { -- | A filter expression defining entities that will be aggregated for
    -- statistics. Supports ID, service, and edge functions. If no selector
    -- expression is specified, edge statistics are returned.
    entitySelectorExpression :: Prelude.Maybe Prelude.Text,
    -- | The forecasted high and low fault count values. Forecast enabled
    -- requests require the EntitySelectorExpression ID be provided.
    forecastStatistics :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the group for which to pull statistics
    -- from.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The case-sensitive name of the group for which to pull statistics from.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Aggregation period in seconds.
    period :: Prelude.Maybe Prelude.Int,
    -- | The start of the time frame for which to aggregate statistics.
    startTime :: Data.POSIX,
    -- | The end of the time frame for which to aggregate statistics.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTimeSeriesServiceStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entitySelectorExpression', 'getTimeSeriesServiceStatistics_entitySelectorExpression' - A filter expression defining entities that will be aggregated for
-- statistics. Supports ID, service, and edge functions. If no selector
-- expression is specified, edge statistics are returned.
--
-- 'forecastStatistics', 'getTimeSeriesServiceStatistics_forecastStatistics' - The forecasted high and low fault count values. Forecast enabled
-- requests require the EntitySelectorExpression ID be provided.
--
-- 'groupARN', 'getTimeSeriesServiceStatistics_groupARN' - The Amazon Resource Name (ARN) of the group for which to pull statistics
-- from.
--
-- 'groupName', 'getTimeSeriesServiceStatistics_groupName' - The case-sensitive name of the group for which to pull statistics from.
--
-- 'nextToken', 'getTimeSeriesServiceStatistics_nextToken' - Pagination token.
--
-- 'period', 'getTimeSeriesServiceStatistics_period' - Aggregation period in seconds.
--
-- 'startTime', 'getTimeSeriesServiceStatistics_startTime' - The start of the time frame for which to aggregate statistics.
--
-- 'endTime', 'getTimeSeriesServiceStatistics_endTime' - The end of the time frame for which to aggregate statistics.
newGetTimeSeriesServiceStatistics ::
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  GetTimeSeriesServiceStatistics
newGetTimeSeriesServiceStatistics
  pStartTime_
  pEndTime_ =
    GetTimeSeriesServiceStatistics'
      { entitySelectorExpression =
          Prelude.Nothing,
        forecastStatistics = Prelude.Nothing,
        groupARN = Prelude.Nothing,
        groupName = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        period = Prelude.Nothing,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | A filter expression defining entities that will be aggregated for
-- statistics. Supports ID, service, and edge functions. If no selector
-- expression is specified, edge statistics are returned.
getTimeSeriesServiceStatistics_entitySelectorExpression :: Lens.Lens' GetTimeSeriesServiceStatistics (Prelude.Maybe Prelude.Text)
getTimeSeriesServiceStatistics_entitySelectorExpression = Lens.lens (\GetTimeSeriesServiceStatistics' {entitySelectorExpression} -> entitySelectorExpression) (\s@GetTimeSeriesServiceStatistics' {} a -> s {entitySelectorExpression = a} :: GetTimeSeriesServiceStatistics)

-- | The forecasted high and low fault count values. Forecast enabled
-- requests require the EntitySelectorExpression ID be provided.
getTimeSeriesServiceStatistics_forecastStatistics :: Lens.Lens' GetTimeSeriesServiceStatistics (Prelude.Maybe Prelude.Bool)
getTimeSeriesServiceStatistics_forecastStatistics = Lens.lens (\GetTimeSeriesServiceStatistics' {forecastStatistics} -> forecastStatistics) (\s@GetTimeSeriesServiceStatistics' {} a -> s {forecastStatistics = a} :: GetTimeSeriesServiceStatistics)

-- | The Amazon Resource Name (ARN) of the group for which to pull statistics
-- from.
getTimeSeriesServiceStatistics_groupARN :: Lens.Lens' GetTimeSeriesServiceStatistics (Prelude.Maybe Prelude.Text)
getTimeSeriesServiceStatistics_groupARN = Lens.lens (\GetTimeSeriesServiceStatistics' {groupARN} -> groupARN) (\s@GetTimeSeriesServiceStatistics' {} a -> s {groupARN = a} :: GetTimeSeriesServiceStatistics)

-- | The case-sensitive name of the group for which to pull statistics from.
getTimeSeriesServiceStatistics_groupName :: Lens.Lens' GetTimeSeriesServiceStatistics (Prelude.Maybe Prelude.Text)
getTimeSeriesServiceStatistics_groupName = Lens.lens (\GetTimeSeriesServiceStatistics' {groupName} -> groupName) (\s@GetTimeSeriesServiceStatistics' {} a -> s {groupName = a} :: GetTimeSeriesServiceStatistics)

-- | Pagination token.
getTimeSeriesServiceStatistics_nextToken :: Lens.Lens' GetTimeSeriesServiceStatistics (Prelude.Maybe Prelude.Text)
getTimeSeriesServiceStatistics_nextToken = Lens.lens (\GetTimeSeriesServiceStatistics' {nextToken} -> nextToken) (\s@GetTimeSeriesServiceStatistics' {} a -> s {nextToken = a} :: GetTimeSeriesServiceStatistics)

-- | Aggregation period in seconds.
getTimeSeriesServiceStatistics_period :: Lens.Lens' GetTimeSeriesServiceStatistics (Prelude.Maybe Prelude.Int)
getTimeSeriesServiceStatistics_period = Lens.lens (\GetTimeSeriesServiceStatistics' {period} -> period) (\s@GetTimeSeriesServiceStatistics' {} a -> s {period = a} :: GetTimeSeriesServiceStatistics)

-- | The start of the time frame for which to aggregate statistics.
getTimeSeriesServiceStatistics_startTime :: Lens.Lens' GetTimeSeriesServiceStatistics Prelude.UTCTime
getTimeSeriesServiceStatistics_startTime = Lens.lens (\GetTimeSeriesServiceStatistics' {startTime} -> startTime) (\s@GetTimeSeriesServiceStatistics' {} a -> s {startTime = a} :: GetTimeSeriesServiceStatistics) Prelude.. Data._Time

-- | The end of the time frame for which to aggregate statistics.
getTimeSeriesServiceStatistics_endTime :: Lens.Lens' GetTimeSeriesServiceStatistics Prelude.UTCTime
getTimeSeriesServiceStatistics_endTime = Lens.lens (\GetTimeSeriesServiceStatistics' {endTime} -> endTime) (\s@GetTimeSeriesServiceStatistics' {} a -> s {endTime = a} :: GetTimeSeriesServiceStatistics) Prelude.. Data._Time

instance Core.AWSPager GetTimeSeriesServiceStatistics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTimeSeriesServiceStatisticsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTimeSeriesServiceStatistics_nextToken
          Lens..~ rs
          Lens.^? getTimeSeriesServiceStatisticsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTimeSeriesServiceStatistics
  where
  type
    AWSResponse GetTimeSeriesServiceStatistics =
      GetTimeSeriesServiceStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTimeSeriesServiceStatisticsResponse'
            Prelude.<$> (x Data..?> "ContainsOldGroupVersions")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "TimeSeriesServiceStatistics"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTimeSeriesServiceStatistics
  where
  hashWithSalt
    _salt
    GetTimeSeriesServiceStatistics' {..} =
      _salt
        `Prelude.hashWithSalt` entitySelectorExpression
        `Prelude.hashWithSalt` forecastStatistics
        `Prelude.hashWithSalt` groupARN
        `Prelude.hashWithSalt` groupName
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` period
        `Prelude.hashWithSalt` startTime
        `Prelude.hashWithSalt` endTime

instance
  Prelude.NFData
    GetTimeSeriesServiceStatistics
  where
  rnf GetTimeSeriesServiceStatistics' {..} =
    Prelude.rnf entitySelectorExpression
      `Prelude.seq` Prelude.rnf forecastStatistics
      `Prelude.seq` Prelude.rnf groupARN
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance
  Data.ToHeaders
    GetTimeSeriesServiceStatistics
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetTimeSeriesServiceStatistics where
  toJSON GetTimeSeriesServiceStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EntitySelectorExpression" Data..=)
              Prelude.<$> entitySelectorExpression,
            ("ForecastStatistics" Data..=)
              Prelude.<$> forecastStatistics,
            ("GroupARN" Data..=) Prelude.<$> groupARN,
            ("GroupName" Data..=) Prelude.<$> groupName,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("Period" Data..=) Prelude.<$> period,
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath GetTimeSeriesServiceStatistics where
  toPath = Prelude.const "/TimeSeriesServiceStatistics"

instance Data.ToQuery GetTimeSeriesServiceStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTimeSeriesServiceStatisticsResponse' smart constructor.
data GetTimeSeriesServiceStatisticsResponse = GetTimeSeriesServiceStatisticsResponse'
  { -- | A flag indicating whether or not a group\'s filter expression has been
    -- consistent, or if a returned aggregation might show statistics from an
    -- older version of the group\'s filter expression.
    containsOldGroupVersions :: Prelude.Maybe Prelude.Bool,
    -- | Pagination token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The collection of statistics.
    timeSeriesServiceStatistics :: Prelude.Maybe [TimeSeriesServiceStatistics],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTimeSeriesServiceStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containsOldGroupVersions', 'getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions' - A flag indicating whether or not a group\'s filter expression has been
-- consistent, or if a returned aggregation might show statistics from an
-- older version of the group\'s filter expression.
--
-- 'nextToken', 'getTimeSeriesServiceStatisticsResponse_nextToken' - Pagination token.
--
-- 'timeSeriesServiceStatistics', 'getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics' - The collection of statistics.
--
-- 'httpStatus', 'getTimeSeriesServiceStatisticsResponse_httpStatus' - The response's http status code.
newGetTimeSeriesServiceStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTimeSeriesServiceStatisticsResponse
newGetTimeSeriesServiceStatisticsResponse
  pHttpStatus_ =
    GetTimeSeriesServiceStatisticsResponse'
      { containsOldGroupVersions =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        timeSeriesServiceStatistics =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A flag indicating whether or not a group\'s filter expression has been
-- consistent, or if a returned aggregation might show statistics from an
-- older version of the group\'s filter expression.
getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Prelude.Maybe Prelude.Bool)
getTimeSeriesServiceStatisticsResponse_containsOldGroupVersions = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {containsOldGroupVersions} -> containsOldGroupVersions) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {containsOldGroupVersions = a} :: GetTimeSeriesServiceStatisticsResponse)

-- | Pagination token.
getTimeSeriesServiceStatisticsResponse_nextToken :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Prelude.Maybe Prelude.Text)
getTimeSeriesServiceStatisticsResponse_nextToken = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {nextToken} -> nextToken) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {nextToken = a} :: GetTimeSeriesServiceStatisticsResponse)

-- | The collection of statistics.
getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Prelude.Maybe [TimeSeriesServiceStatistics])
getTimeSeriesServiceStatisticsResponse_timeSeriesServiceStatistics = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {timeSeriesServiceStatistics} -> timeSeriesServiceStatistics) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {timeSeriesServiceStatistics = a} :: GetTimeSeriesServiceStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTimeSeriesServiceStatisticsResponse_httpStatus :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse Prelude.Int
getTimeSeriesServiceStatisticsResponse_httpStatus = Lens.lens (\GetTimeSeriesServiceStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetTimeSeriesServiceStatisticsResponse' {} a -> s {httpStatus = a} :: GetTimeSeriesServiceStatisticsResponse)

instance
  Prelude.NFData
    GetTimeSeriesServiceStatisticsResponse
  where
  rnf GetTimeSeriesServiceStatisticsResponse' {..} =
    Prelude.rnf containsOldGroupVersions
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timeSeriesServiceStatistics
      `Prelude.seq` Prelude.rnf httpStatus
