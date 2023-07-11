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
-- Module      : Amazonka.CloudTrail.ListQueries
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of queries and query statuses for the past seven days.
-- You must specify an ARN value for @EventDataStore@. Optionally, to
-- shorten the list of results, you can specify a time range, formatted as
-- timestamps, by adding @StartTime@ and @EndTime@ parameters, and a
-- @QueryStatus@ value. Valid values for @QueryStatus@ include @QUEUED@,
-- @RUNNING@, @FINISHED@, @FAILED@, @TIMED_OUT@, or @CANCELLED@.
module Amazonka.CloudTrail.ListQueries
  ( -- * Creating a Request
    ListQueries (..),
    newListQueries,

    -- * Request Lenses
    listQueries_endTime,
    listQueries_maxResults,
    listQueries_nextToken,
    listQueries_queryStatus,
    listQueries_startTime,
    listQueries_eventDataStore,

    -- * Destructuring the Response
    ListQueriesResponse (..),
    newListQueriesResponse,

    -- * Response Lenses
    listQueriesResponse_nextToken,
    listQueriesResponse_queries,
    listQueriesResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListQueries' smart constructor.
data ListQueries = ListQueries'
  { -- | Use with @StartTime@ to bound a @ListQueries@ request, and limit its
    -- results to only those queries run within a specified time period.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of queries to show on a page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token you can use to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status of queries that you want to return in results. Valid values
    -- for @QueryStatus@ include @QUEUED@, @RUNNING@, @FINISHED@, @FAILED@,
    -- @TIMED_OUT@, or @CANCELLED@.
    queryStatus :: Prelude.Maybe QueryStatus,
    -- | Use with @EndTime@ to bound a @ListQueries@ request, and limit its
    -- results to only those queries run within a specified time period.
    startTime :: Prelude.Maybe Data.POSIX,
    -- | The ARN (or the ID suffix of the ARN) of an event data store on which
    -- queries were run.
    eventDataStore :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueries' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'listQueries_endTime' - Use with @StartTime@ to bound a @ListQueries@ request, and limit its
-- results to only those queries run within a specified time period.
--
-- 'maxResults', 'listQueries_maxResults' - The maximum number of queries to show on a page.
--
-- 'nextToken', 'listQueries_nextToken' - A token you can use to get the next page of results.
--
-- 'queryStatus', 'listQueries_queryStatus' - The status of queries that you want to return in results. Valid values
-- for @QueryStatus@ include @QUEUED@, @RUNNING@, @FINISHED@, @FAILED@,
-- @TIMED_OUT@, or @CANCELLED@.
--
-- 'startTime', 'listQueries_startTime' - Use with @EndTime@ to bound a @ListQueries@ request, and limit its
-- results to only those queries run within a specified time period.
--
-- 'eventDataStore', 'listQueries_eventDataStore' - The ARN (or the ID suffix of the ARN) of an event data store on which
-- queries were run.
newListQueries ::
  -- | 'eventDataStore'
  Prelude.Text ->
  ListQueries
newListQueries pEventDataStore_ =
  ListQueries'
    { endTime = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryStatus = Prelude.Nothing,
      startTime = Prelude.Nothing,
      eventDataStore = pEventDataStore_
    }

-- | Use with @StartTime@ to bound a @ListQueries@ request, and limit its
-- results to only those queries run within a specified time period.
listQueries_endTime :: Lens.Lens' ListQueries (Prelude.Maybe Prelude.UTCTime)
listQueries_endTime = Lens.lens (\ListQueries' {endTime} -> endTime) (\s@ListQueries' {} a -> s {endTime = a} :: ListQueries) Prelude.. Lens.mapping Data._Time

-- | The maximum number of queries to show on a page.
listQueries_maxResults :: Lens.Lens' ListQueries (Prelude.Maybe Prelude.Natural)
listQueries_maxResults = Lens.lens (\ListQueries' {maxResults} -> maxResults) (\s@ListQueries' {} a -> s {maxResults = a} :: ListQueries)

-- | A token you can use to get the next page of results.
listQueries_nextToken :: Lens.Lens' ListQueries (Prelude.Maybe Prelude.Text)
listQueries_nextToken = Lens.lens (\ListQueries' {nextToken} -> nextToken) (\s@ListQueries' {} a -> s {nextToken = a} :: ListQueries)

-- | The status of queries that you want to return in results. Valid values
-- for @QueryStatus@ include @QUEUED@, @RUNNING@, @FINISHED@, @FAILED@,
-- @TIMED_OUT@, or @CANCELLED@.
listQueries_queryStatus :: Lens.Lens' ListQueries (Prelude.Maybe QueryStatus)
listQueries_queryStatus = Lens.lens (\ListQueries' {queryStatus} -> queryStatus) (\s@ListQueries' {} a -> s {queryStatus = a} :: ListQueries)

-- | Use with @EndTime@ to bound a @ListQueries@ request, and limit its
-- results to only those queries run within a specified time period.
listQueries_startTime :: Lens.Lens' ListQueries (Prelude.Maybe Prelude.UTCTime)
listQueries_startTime = Lens.lens (\ListQueries' {startTime} -> startTime) (\s@ListQueries' {} a -> s {startTime = a} :: ListQueries) Prelude.. Lens.mapping Data._Time

-- | The ARN (or the ID suffix of the ARN) of an event data store on which
-- queries were run.
listQueries_eventDataStore :: Lens.Lens' ListQueries Prelude.Text
listQueries_eventDataStore = Lens.lens (\ListQueries' {eventDataStore} -> eventDataStore) (\s@ListQueries' {} a -> s {eventDataStore = a} :: ListQueries)

instance Core.AWSRequest ListQueries where
  type AWSResponse ListQueries = ListQueriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListQueriesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Queries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListQueries where
  hashWithSalt _salt ListQueries' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryStatus
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` eventDataStore

instance Prelude.NFData ListQueries where
  rnf ListQueries' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryStatus
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf eventDataStore

instance Data.ToHeaders ListQueries where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.ListQueries" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListQueries where
  toJSON ListQueries' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("QueryStatus" Data..=) Prelude.<$> queryStatus,
            ("StartTime" Data..=) Prelude.<$> startTime,
            Prelude.Just
              ("EventDataStore" Data..= eventDataStore)
          ]
      )

instance Data.ToPath ListQueries where
  toPath = Prelude.const "/"

instance Data.ToQuery ListQueries where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListQueriesResponse' smart constructor.
data ListQueriesResponse = ListQueriesResponse'
  { -- | A token you can use to get the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Lists matching query results, and shows query ID, status, and creation
    -- time of each query.
    queries :: Prelude.Maybe [Query],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListQueriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listQueriesResponse_nextToken' - A token you can use to get the next page of results.
--
-- 'queries', 'listQueriesResponse_queries' - Lists matching query results, and shows query ID, status, and creation
-- time of each query.
--
-- 'httpStatus', 'listQueriesResponse_httpStatus' - The response's http status code.
newListQueriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListQueriesResponse
newListQueriesResponse pHttpStatus_ =
  ListQueriesResponse'
    { nextToken = Prelude.Nothing,
      queries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token you can use to get the next page of results.
listQueriesResponse_nextToken :: Lens.Lens' ListQueriesResponse (Prelude.Maybe Prelude.Text)
listQueriesResponse_nextToken = Lens.lens (\ListQueriesResponse' {nextToken} -> nextToken) (\s@ListQueriesResponse' {} a -> s {nextToken = a} :: ListQueriesResponse)

-- | Lists matching query results, and shows query ID, status, and creation
-- time of each query.
listQueriesResponse_queries :: Lens.Lens' ListQueriesResponse (Prelude.Maybe [Query])
listQueriesResponse_queries = Lens.lens (\ListQueriesResponse' {queries} -> queries) (\s@ListQueriesResponse' {} a -> s {queries = a} :: ListQueriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listQueriesResponse_httpStatus :: Lens.Lens' ListQueriesResponse Prelude.Int
listQueriesResponse_httpStatus = Lens.lens (\ListQueriesResponse' {httpStatus} -> httpStatus) (\s@ListQueriesResponse' {} a -> s {httpStatus = a} :: ListQueriesResponse)

instance Prelude.NFData ListQueriesResponse where
  rnf ListQueriesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queries
      `Prelude.seq` Prelude.rnf httpStatus
