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
-- Module      : Amazonka.CodeGuruProfiler.ListProfileTimes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the start times of the available aggregated profiles of a
-- profiling group for an aggregation period within the specified time
-- range.
--
-- This operation returns paginated results.
module Amazonka.CodeGuruProfiler.ListProfileTimes
  ( -- * Creating a Request
    ListProfileTimes (..),
    newListProfileTimes,

    -- * Request Lenses
    listProfileTimes_nextToken,
    listProfileTimes_maxResults,
    listProfileTimes_orderBy,
    listProfileTimes_endTime,
    listProfileTimes_period,
    listProfileTimes_profilingGroupName,
    listProfileTimes_startTime,

    -- * Destructuring the Response
    ListProfileTimesResponse (..),
    newListProfileTimesResponse,

    -- * Response Lenses
    listProfileTimesResponse_nextToken,
    listProfileTimesResponse_httpStatus,
    listProfileTimesResponse_profileTimes,
  )
where

import Amazonka.CodeGuruProfiler.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The structure representing the listProfileTimesRequest.
--
-- /See:/ 'newListProfileTimes' smart constructor.
data ListProfileTimes = ListProfileTimes'
  { -- | The @nextToken@ value returned from a previous paginated
    -- @ListProfileTimes@ request where @maxResults@ was used and the results
    -- exceeded the value of that parameter. Pagination continues from the end
    -- of the previous results that returned the @nextToken@ value.
    --
    -- This token should be treated as an opaque identifier that is only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of profile time results returned by
    -- @ListProfileTimes@ in paginated output. When this parameter is used,
    -- @ListProfileTimes@ only returns @maxResults@ results in a single page
    -- with a @nextToken@ response element. The remaining results of the
    -- initial request can be seen by sending another @ListProfileTimes@
    -- request with the returned @nextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The order (ascending or descending by start time of the profile) to use
    -- when listing profiles. Defaults to @TIMESTAMP_DESCENDING@.
    orderBy :: Prelude.Maybe OrderBy,
    -- | The end time of the time range from which to list the profiles.
    endTime :: Core.POSIX,
    -- | The aggregation period. This specifies the period during which an
    -- aggregation profile collects posted agent profiles for a profiling
    -- group. There are 3 valid values.
    --
    -- -   @P1D@ — 1 day
    --
    -- -   @PT1H@ — 1 hour
    --
    -- -   @PT5M@ — 5 minutes
    period :: AggregationPeriod,
    -- | The name of the profiling group.
    profilingGroupName :: Prelude.Text,
    -- | The start time of the time range from which to list the profiles.
    startTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileTimes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfileTimes_nextToken' - The @nextToken@ value returned from a previous paginated
-- @ListProfileTimes@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'listProfileTimes_maxResults' - The maximum number of profile time results returned by
-- @ListProfileTimes@ in paginated output. When this parameter is used,
-- @ListProfileTimes@ only returns @maxResults@ results in a single page
-- with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListProfileTimes@
-- request with the returned @nextToken@ value.
--
-- 'orderBy', 'listProfileTimes_orderBy' - The order (ascending or descending by start time of the profile) to use
-- when listing profiles. Defaults to @TIMESTAMP_DESCENDING@.
--
-- 'endTime', 'listProfileTimes_endTime' - The end time of the time range from which to list the profiles.
--
-- 'period', 'listProfileTimes_period' - The aggregation period. This specifies the period during which an
-- aggregation profile collects posted agent profiles for a profiling
-- group. There are 3 valid values.
--
-- -   @P1D@ — 1 day
--
-- -   @PT1H@ — 1 hour
--
-- -   @PT5M@ — 5 minutes
--
-- 'profilingGroupName', 'listProfileTimes_profilingGroupName' - The name of the profiling group.
--
-- 'startTime', 'listProfileTimes_startTime' - The start time of the time range from which to list the profiles.
newListProfileTimes ::
  -- | 'endTime'
  Prelude.UTCTime ->
  -- | 'period'
  AggregationPeriod ->
  -- | 'profilingGroupName'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  ListProfileTimes
newListProfileTimes
  pEndTime_
  pPeriod_
  pProfilingGroupName_
  pStartTime_ =
    ListProfileTimes'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        orderBy = Prelude.Nothing,
        endTime = Core._Time Lens.# pEndTime_,
        period = pPeriod_,
        profilingGroupName = pProfilingGroupName_,
        startTime = Core._Time Lens.# pStartTime_
      }

-- | The @nextToken@ value returned from a previous paginated
-- @ListProfileTimes@ request where @maxResults@ was used and the results
-- exceeded the value of that parameter. Pagination continues from the end
-- of the previous results that returned the @nextToken@ value.
--
-- This token should be treated as an opaque identifier that is only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
listProfileTimes_nextToken :: Lens.Lens' ListProfileTimes (Prelude.Maybe Prelude.Text)
listProfileTimes_nextToken = Lens.lens (\ListProfileTimes' {nextToken} -> nextToken) (\s@ListProfileTimes' {} a -> s {nextToken = a} :: ListProfileTimes)

-- | The maximum number of profile time results returned by
-- @ListProfileTimes@ in paginated output. When this parameter is used,
-- @ListProfileTimes@ only returns @maxResults@ results in a single page
-- with a @nextToken@ response element. The remaining results of the
-- initial request can be seen by sending another @ListProfileTimes@
-- request with the returned @nextToken@ value.
listProfileTimes_maxResults :: Lens.Lens' ListProfileTimes (Prelude.Maybe Prelude.Natural)
listProfileTimes_maxResults = Lens.lens (\ListProfileTimes' {maxResults} -> maxResults) (\s@ListProfileTimes' {} a -> s {maxResults = a} :: ListProfileTimes)

-- | The order (ascending or descending by start time of the profile) to use
-- when listing profiles. Defaults to @TIMESTAMP_DESCENDING@.
listProfileTimes_orderBy :: Lens.Lens' ListProfileTimes (Prelude.Maybe OrderBy)
listProfileTimes_orderBy = Lens.lens (\ListProfileTimes' {orderBy} -> orderBy) (\s@ListProfileTimes' {} a -> s {orderBy = a} :: ListProfileTimes)

-- | The end time of the time range from which to list the profiles.
listProfileTimes_endTime :: Lens.Lens' ListProfileTimes Prelude.UTCTime
listProfileTimes_endTime = Lens.lens (\ListProfileTimes' {endTime} -> endTime) (\s@ListProfileTimes' {} a -> s {endTime = a} :: ListProfileTimes) Prelude.. Core._Time

-- | The aggregation period. This specifies the period during which an
-- aggregation profile collects posted agent profiles for a profiling
-- group. There are 3 valid values.
--
-- -   @P1D@ — 1 day
--
-- -   @PT1H@ — 1 hour
--
-- -   @PT5M@ — 5 minutes
listProfileTimes_period :: Lens.Lens' ListProfileTimes AggregationPeriod
listProfileTimes_period = Lens.lens (\ListProfileTimes' {period} -> period) (\s@ListProfileTimes' {} a -> s {period = a} :: ListProfileTimes)

-- | The name of the profiling group.
listProfileTimes_profilingGroupName :: Lens.Lens' ListProfileTimes Prelude.Text
listProfileTimes_profilingGroupName = Lens.lens (\ListProfileTimes' {profilingGroupName} -> profilingGroupName) (\s@ListProfileTimes' {} a -> s {profilingGroupName = a} :: ListProfileTimes)

-- | The start time of the time range from which to list the profiles.
listProfileTimes_startTime :: Lens.Lens' ListProfileTimes Prelude.UTCTime
listProfileTimes_startTime = Lens.lens (\ListProfileTimes' {startTime} -> startTime) (\s@ListProfileTimes' {} a -> s {startTime = a} :: ListProfileTimes) Prelude.. Core._Time

instance Core.AWSPager ListProfileTimes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProfileTimesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listProfileTimesResponse_profileTimes) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProfileTimes_nextToken
          Lens..~ rs
          Lens.^? listProfileTimesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProfileTimes where
  type
    AWSResponse ListProfileTimes =
      ListProfileTimesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProfileTimesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "profileTimes" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListProfileTimes where
  hashWithSalt _salt ListProfileTimes' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` orderBy
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` profilingGroupName
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData ListProfileTimes where
  rnf ListProfileTimes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf profilingGroupName
      `Prelude.seq` Prelude.rnf startTime

instance Core.ToHeaders ListProfileTimes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListProfileTimes where
  toPath ListProfileTimes' {..} =
    Prelude.mconcat
      [ "/profilingGroups/",
        Core.toBS profilingGroupName,
        "/profileTimes"
      ]

instance Core.ToQuery ListProfileTimes where
  toQuery ListProfileTimes' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults,
        "orderBy" Core.=: orderBy,
        "endTime" Core.=: endTime,
        "period" Core.=: period,
        "startTime" Core.=: startTime
      ]

-- | The structure representing the listProfileTimesResponse.
--
-- /See:/ 'newListProfileTimesResponse' smart constructor.
data ListProfileTimesResponse = ListProfileTimesResponse'
  { -- | The @nextToken@ value to include in a future @ListProfileTimes@ request.
    -- When the results of a @ListProfileTimes@ request exceed @maxResults@,
    -- this value can be used to retrieve the next page of results. This value
    -- is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of start times of the available profiles for the aggregation
    -- period in the specified time range.
    profileTimes :: [ProfileTime]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProfileTimesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProfileTimesResponse_nextToken' - The @nextToken@ value to include in a future @ListProfileTimes@ request.
-- When the results of a @ListProfileTimes@ request exceed @maxResults@,
-- this value can be used to retrieve the next page of results. This value
-- is @null@ when there are no more results to return.
--
-- 'httpStatus', 'listProfileTimesResponse_httpStatus' - The response's http status code.
--
-- 'profileTimes', 'listProfileTimesResponse_profileTimes' - The list of start times of the available profiles for the aggregation
-- period in the specified time range.
newListProfileTimesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProfileTimesResponse
newListProfileTimesResponse pHttpStatus_ =
  ListProfileTimesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      profileTimes = Prelude.mempty
    }

-- | The @nextToken@ value to include in a future @ListProfileTimes@ request.
-- When the results of a @ListProfileTimes@ request exceed @maxResults@,
-- this value can be used to retrieve the next page of results. This value
-- is @null@ when there are no more results to return.
listProfileTimesResponse_nextToken :: Lens.Lens' ListProfileTimesResponse (Prelude.Maybe Prelude.Text)
listProfileTimesResponse_nextToken = Lens.lens (\ListProfileTimesResponse' {nextToken} -> nextToken) (\s@ListProfileTimesResponse' {} a -> s {nextToken = a} :: ListProfileTimesResponse)

-- | The response's http status code.
listProfileTimesResponse_httpStatus :: Lens.Lens' ListProfileTimesResponse Prelude.Int
listProfileTimesResponse_httpStatus = Lens.lens (\ListProfileTimesResponse' {httpStatus} -> httpStatus) (\s@ListProfileTimesResponse' {} a -> s {httpStatus = a} :: ListProfileTimesResponse)

-- | The list of start times of the available profiles for the aggregation
-- period in the specified time range.
listProfileTimesResponse_profileTimes :: Lens.Lens' ListProfileTimesResponse [ProfileTime]
listProfileTimesResponse_profileTimes = Lens.lens (\ListProfileTimesResponse' {profileTimes} -> profileTimes) (\s@ListProfileTimesResponse' {} a -> s {profileTimes = a} :: ListProfileTimesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProfileTimesResponse where
  rnf ListProfileTimesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf profileTimes
