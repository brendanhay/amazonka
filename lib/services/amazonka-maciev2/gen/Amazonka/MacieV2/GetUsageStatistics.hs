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
-- Module      : Amazonka.MacieV2.GetUsageStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) quotas and aggregated usage data for one or more
-- accounts.
--
-- This operation returns paginated results.
module Amazonka.MacieV2.GetUsageStatistics
  ( -- * Creating a Request
    GetUsageStatistics (..),
    newGetUsageStatistics,

    -- * Request Lenses
    getUsageStatistics_nextToken,
    getUsageStatistics_timeRange,
    getUsageStatistics_filterBy,
    getUsageStatistics_sortBy,
    getUsageStatistics_maxResults,

    -- * Destructuring the Response
    GetUsageStatisticsResponse (..),
    newGetUsageStatisticsResponse,

    -- * Response Lenses
    getUsageStatisticsResponse_records,
    getUsageStatisticsResponse_nextToken,
    getUsageStatisticsResponse_timeRange,
    getUsageStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetUsageStatistics' smart constructor.
data GetUsageStatistics = GetUsageStatistics'
  { -- | The nextToken string that specifies which page of results to return in a
    -- paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The inclusive time period to query usage data for. Valid values are:
    -- MONTH_TO_DATE, for the current calendar month to date; and,
    -- PAST_30_DAYS, for the preceding 30 days. If you don\'t specify a value,
    -- Amazon Macie provides usage data for the preceding 30 days.
    timeRange :: Prelude.Maybe TimeRange,
    -- | An array of objects, one for each condition to use to filter the query
    -- results. If you specify more than one condition, Amazon Macie uses an
    -- AND operator to join the conditions.
    filterBy :: Prelude.Maybe [UsageStatisticsFilter],
    -- | The criteria to use to sort the query results.
    sortBy :: Prelude.Maybe UsageStatisticsSortBy,
    -- | The maximum number of items to include in each page of the response.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getUsageStatistics_nextToken' - The nextToken string that specifies which page of results to return in a
-- paginated response.
--
-- 'timeRange', 'getUsageStatistics_timeRange' - The inclusive time period to query usage data for. Valid values are:
-- MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days. If you don\'t specify a value,
-- Amazon Macie provides usage data for the preceding 30 days.
--
-- 'filterBy', 'getUsageStatistics_filterBy' - An array of objects, one for each condition to use to filter the query
-- results. If you specify more than one condition, Amazon Macie uses an
-- AND operator to join the conditions.
--
-- 'sortBy', 'getUsageStatistics_sortBy' - The criteria to use to sort the query results.
--
-- 'maxResults', 'getUsageStatistics_maxResults' - The maximum number of items to include in each page of the response.
newGetUsageStatistics ::
  GetUsageStatistics
newGetUsageStatistics =
  GetUsageStatistics'
    { nextToken = Prelude.Nothing,
      timeRange = Prelude.Nothing,
      filterBy = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The nextToken string that specifies which page of results to return in a
-- paginated response.
getUsageStatistics_nextToken :: Lens.Lens' GetUsageStatistics (Prelude.Maybe Prelude.Text)
getUsageStatistics_nextToken = Lens.lens (\GetUsageStatistics' {nextToken} -> nextToken) (\s@GetUsageStatistics' {} a -> s {nextToken = a} :: GetUsageStatistics)

-- | The inclusive time period to query usage data for. Valid values are:
-- MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days. If you don\'t specify a value,
-- Amazon Macie provides usage data for the preceding 30 days.
getUsageStatistics_timeRange :: Lens.Lens' GetUsageStatistics (Prelude.Maybe TimeRange)
getUsageStatistics_timeRange = Lens.lens (\GetUsageStatistics' {timeRange} -> timeRange) (\s@GetUsageStatistics' {} a -> s {timeRange = a} :: GetUsageStatistics)

-- | An array of objects, one for each condition to use to filter the query
-- results. If you specify more than one condition, Amazon Macie uses an
-- AND operator to join the conditions.
getUsageStatistics_filterBy :: Lens.Lens' GetUsageStatistics (Prelude.Maybe [UsageStatisticsFilter])
getUsageStatistics_filterBy = Lens.lens (\GetUsageStatistics' {filterBy} -> filterBy) (\s@GetUsageStatistics' {} a -> s {filterBy = a} :: GetUsageStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The criteria to use to sort the query results.
getUsageStatistics_sortBy :: Lens.Lens' GetUsageStatistics (Prelude.Maybe UsageStatisticsSortBy)
getUsageStatistics_sortBy = Lens.lens (\GetUsageStatistics' {sortBy} -> sortBy) (\s@GetUsageStatistics' {} a -> s {sortBy = a} :: GetUsageStatistics)

-- | The maximum number of items to include in each page of the response.
getUsageStatistics_maxResults :: Lens.Lens' GetUsageStatistics (Prelude.Maybe Prelude.Int)
getUsageStatistics_maxResults = Lens.lens (\GetUsageStatistics' {maxResults} -> maxResults) (\s@GetUsageStatistics' {} a -> s {maxResults = a} :: GetUsageStatistics)

instance Core.AWSPager GetUsageStatistics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getUsageStatisticsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getUsageStatisticsResponse_records
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getUsageStatistics_nextToken
          Lens..~ rs
          Lens.^? getUsageStatisticsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetUsageStatistics where
  type
    AWSResponse GetUsageStatistics =
      GetUsageStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetUsageStatisticsResponse'
            Prelude.<$> (x Core..?> "records" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "timeRange")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetUsageStatistics where
  hashWithSalt _salt GetUsageStatistics' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` timeRange
      `Prelude.hashWithSalt` filterBy
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetUsageStatistics where
  rnf GetUsageStatistics' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timeRange
      `Prelude.seq` Prelude.rnf filterBy
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetUsageStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetUsageStatistics where
  toJSON GetUsageStatistics' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("timeRange" Core..=) Prelude.<$> timeRange,
            ("filterBy" Core..=) Prelude.<$> filterBy,
            ("sortBy" Core..=) Prelude.<$> sortBy,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetUsageStatistics where
  toPath = Prelude.const "/usage/statistics"

instance Core.ToQuery GetUsageStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetUsageStatisticsResponse' smart constructor.
data GetUsageStatisticsResponse = GetUsageStatisticsResponse'
  { -- | An array of objects that contains the results of the query. Each object
    -- contains the data for an account that meets the filter criteria
    -- specified in the request.
    records :: Prelude.Maybe [UsageRecord],
    -- | The string to use in a subsequent request to get the next page of
    -- results in a paginated response. This value is null if there are no
    -- additional pages.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The inclusive time period that the usage data applies to. Possible
    -- values are: MONTH_TO_DATE, for the current calendar month to date; and,
    -- PAST_30_DAYS, for the preceding 30 days.
    timeRange :: Prelude.Maybe TimeRange,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetUsageStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'records', 'getUsageStatisticsResponse_records' - An array of objects that contains the results of the query. Each object
-- contains the data for an account that meets the filter criteria
-- specified in the request.
--
-- 'nextToken', 'getUsageStatisticsResponse_nextToken' - The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
--
-- 'timeRange', 'getUsageStatisticsResponse_timeRange' - The inclusive time period that the usage data applies to. Possible
-- values are: MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days.
--
-- 'httpStatus', 'getUsageStatisticsResponse_httpStatus' - The response's http status code.
newGetUsageStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetUsageStatisticsResponse
newGetUsageStatisticsResponse pHttpStatus_ =
  GetUsageStatisticsResponse'
    { records =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      timeRange = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that contains the results of the query. Each object
-- contains the data for an account that meets the filter criteria
-- specified in the request.
getUsageStatisticsResponse_records :: Lens.Lens' GetUsageStatisticsResponse (Prelude.Maybe [UsageRecord])
getUsageStatisticsResponse_records = Lens.lens (\GetUsageStatisticsResponse' {records} -> records) (\s@GetUsageStatisticsResponse' {} a -> s {records = a} :: GetUsageStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string to use in a subsequent request to get the next page of
-- results in a paginated response. This value is null if there are no
-- additional pages.
getUsageStatisticsResponse_nextToken :: Lens.Lens' GetUsageStatisticsResponse (Prelude.Maybe Prelude.Text)
getUsageStatisticsResponse_nextToken = Lens.lens (\GetUsageStatisticsResponse' {nextToken} -> nextToken) (\s@GetUsageStatisticsResponse' {} a -> s {nextToken = a} :: GetUsageStatisticsResponse)

-- | The inclusive time period that the usage data applies to. Possible
-- values are: MONTH_TO_DATE, for the current calendar month to date; and,
-- PAST_30_DAYS, for the preceding 30 days.
getUsageStatisticsResponse_timeRange :: Lens.Lens' GetUsageStatisticsResponse (Prelude.Maybe TimeRange)
getUsageStatisticsResponse_timeRange = Lens.lens (\GetUsageStatisticsResponse' {timeRange} -> timeRange) (\s@GetUsageStatisticsResponse' {} a -> s {timeRange = a} :: GetUsageStatisticsResponse)

-- | The response's http status code.
getUsageStatisticsResponse_httpStatus :: Lens.Lens' GetUsageStatisticsResponse Prelude.Int
getUsageStatisticsResponse_httpStatus = Lens.lens (\GetUsageStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetUsageStatisticsResponse' {} a -> s {httpStatus = a} :: GetUsageStatisticsResponse)

instance Prelude.NFData GetUsageStatisticsResponse where
  rnf GetUsageStatisticsResponse' {..} =
    Prelude.rnf records
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf timeRange
      `Prelude.seq` Prelude.rnf httpStatus
