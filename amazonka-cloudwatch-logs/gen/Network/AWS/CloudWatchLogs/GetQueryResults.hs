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
-- Module      : Network.AWS.CloudWatchLogs.GetQueryResults
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results from the specified query.
--
-- Only the fields requested in the query are returned, along with a
-- @\@ptr@ field, which is the identifier for the log record. You can use
-- the value of @\@ptr@ in a
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_GetLogRecord.html GetLogRecord>
-- operation to get the full log record.
--
-- @GetQueryResults@ does not start a query execution. To run a query, use
-- <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_StartQuery.html StartQuery>.
--
-- If the value of the @Status@ field in the output is @Running@, this
-- operation returns only partial results. If you see a value of
-- @Scheduled@ or @Running@ for the status, you can retry the operation
-- later to see the final results.
module Network.AWS.CloudWatchLogs.GetQueryResults
  ( -- * Creating a Request
    GetQueryResults (..),
    newGetQueryResults,

    -- * Request Lenses
    getQueryResults_queryId,

    -- * Destructuring the Response
    GetQueryResultsResponse (..),
    newGetQueryResultsResponse,

    -- * Response Lenses
    getQueryResultsResponse_status,
    getQueryResultsResponse_statistics,
    getQueryResultsResponse_results,
    getQueryResultsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetQueryResults' smart constructor.
data GetQueryResults = GetQueryResults'
  { -- | The ID number of the query.
    queryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetQueryResults' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryId', 'getQueryResults_queryId' - The ID number of the query.
newGetQueryResults ::
  -- | 'queryId'
  Core.Text ->
  GetQueryResults
newGetQueryResults pQueryId_ =
  GetQueryResults' {queryId = pQueryId_}

-- | The ID number of the query.
getQueryResults_queryId :: Lens.Lens' GetQueryResults Core.Text
getQueryResults_queryId = Lens.lens (\GetQueryResults' {queryId} -> queryId) (\s@GetQueryResults' {} a -> s {queryId = a} :: GetQueryResults)

instance Core.AWSRequest GetQueryResults where
  type
    AWSResponse GetQueryResults =
      GetQueryResultsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryResultsResponse'
            Core.<$> (x Core..?> "status")
            Core.<*> (x Core..?> "statistics")
            Core.<*> (x Core..?> "results" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetQueryResults

instance Core.NFData GetQueryResults

instance Core.ToHeaders GetQueryResults where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Logs_20140328.GetQueryResults" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetQueryResults where
  toJSON GetQueryResults' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("queryId" Core..= queryId)]
      )

instance Core.ToPath GetQueryResults where
  toPath = Core.const "/"

instance Core.ToQuery GetQueryResults where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { -- | The status of the most recent running of the query. Possible values are
    -- @Cancelled@, @Complete@, @Failed@, @Running@, @Scheduled@, @Timeout@,
    -- and @Unknown@.
    --
    -- Queries time out after 15 minutes of execution. To avoid having your
    -- queries time out, reduce the time range being searched or partition your
    -- query into a number of queries.
    status :: Core.Maybe QueryStatus,
    -- | Includes the number of log events scanned by the query, the number of
    -- log events that matched the query criteria, and the total number of
    -- bytes in the log events that were scanned. These values reflect the full
    -- raw results of the query.
    statistics :: Core.Maybe QueryStatistics,
    -- | The log events that matched the query criteria during the most recent
    -- time it ran.
    --
    -- The @results@ value is an array of arrays. Each log event is one object
    -- in the top-level array. Each of these log event objects is an array of
    -- @field@\/@value@ pairs.
    results :: Core.Maybe [[ResultField]],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetQueryResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getQueryResultsResponse_status' - The status of the most recent running of the query. Possible values are
-- @Cancelled@, @Complete@, @Failed@, @Running@, @Scheduled@, @Timeout@,
-- and @Unknown@.
--
-- Queries time out after 15 minutes of execution. To avoid having your
-- queries time out, reduce the time range being searched or partition your
-- query into a number of queries.
--
-- 'statistics', 'getQueryResultsResponse_statistics' - Includes the number of log events scanned by the query, the number of
-- log events that matched the query criteria, and the total number of
-- bytes in the log events that were scanned. These values reflect the full
-- raw results of the query.
--
-- 'results', 'getQueryResultsResponse_results' - The log events that matched the query criteria during the most recent
-- time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object
-- in the top-level array. Each of these log event objects is an array of
-- @field@\/@value@ pairs.
--
-- 'httpStatus', 'getQueryResultsResponse_httpStatus' - The response's http status code.
newGetQueryResultsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetQueryResultsResponse
newGetQueryResultsResponse pHttpStatus_ =
  GetQueryResultsResponse'
    { status = Core.Nothing,
      statistics = Core.Nothing,
      results = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the most recent running of the query. Possible values are
-- @Cancelled@, @Complete@, @Failed@, @Running@, @Scheduled@, @Timeout@,
-- and @Unknown@.
--
-- Queries time out after 15 minutes of execution. To avoid having your
-- queries time out, reduce the time range being searched or partition your
-- query into a number of queries.
getQueryResultsResponse_status :: Lens.Lens' GetQueryResultsResponse (Core.Maybe QueryStatus)
getQueryResultsResponse_status = Lens.lens (\GetQueryResultsResponse' {status} -> status) (\s@GetQueryResultsResponse' {} a -> s {status = a} :: GetQueryResultsResponse)

-- | Includes the number of log events scanned by the query, the number of
-- log events that matched the query criteria, and the total number of
-- bytes in the log events that were scanned. These values reflect the full
-- raw results of the query.
getQueryResultsResponse_statistics :: Lens.Lens' GetQueryResultsResponse (Core.Maybe QueryStatistics)
getQueryResultsResponse_statistics = Lens.lens (\GetQueryResultsResponse' {statistics} -> statistics) (\s@GetQueryResultsResponse' {} a -> s {statistics = a} :: GetQueryResultsResponse)

-- | The log events that matched the query criteria during the most recent
-- time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object
-- in the top-level array. Each of these log event objects is an array of
-- @field@\/@value@ pairs.
getQueryResultsResponse_results :: Lens.Lens' GetQueryResultsResponse (Core.Maybe [[ResultField]])
getQueryResultsResponse_results = Lens.lens (\GetQueryResultsResponse' {results} -> results) (\s@GetQueryResultsResponse' {} a -> s {results = a} :: GetQueryResultsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getQueryResultsResponse_httpStatus :: Lens.Lens' GetQueryResultsResponse Core.Int
getQueryResultsResponse_httpStatus = Lens.lens (\GetQueryResultsResponse' {httpStatus} -> httpStatus) (\s@GetQueryResultsResponse' {} a -> s {httpStatus = a} :: GetQueryResultsResponse)

instance Core.NFData GetQueryResultsResponse
