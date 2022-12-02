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
-- Module      : Amazonka.CloudWatchLogs.GetQueryResults
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CloudWatchLogs.GetQueryResults
  ( -- * Creating a Request
    GetQueryResults (..),
    newGetQueryResults,

    -- * Request Lenses
    getQueryResults_queryId,

    -- * Destructuring the Response
    GetQueryResultsResponse (..),
    newGetQueryResultsResponse,

    -- * Response Lenses
    getQueryResultsResponse_statistics,
    getQueryResultsResponse_status,
    getQueryResultsResponse_results,
    getQueryResultsResponse_httpStatus,
  )
where

import Amazonka.CloudWatchLogs.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueryResults' smart constructor.
data GetQueryResults = GetQueryResults'
  { -- | The ID number of the query.
    queryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetQueryResults
newGetQueryResults pQueryId_ =
  GetQueryResults' {queryId = pQueryId_}

-- | The ID number of the query.
getQueryResults_queryId :: Lens.Lens' GetQueryResults Prelude.Text
getQueryResults_queryId = Lens.lens (\GetQueryResults' {queryId} -> queryId) (\s@GetQueryResults' {} a -> s {queryId = a} :: GetQueryResults)

instance Core.AWSRequest GetQueryResults where
  type
    AWSResponse GetQueryResults =
      GetQueryResultsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQueryResultsResponse'
            Prelude.<$> (x Data..?> "statistics")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "results" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueryResults where
  hashWithSalt _salt GetQueryResults' {..} =
    _salt `Prelude.hashWithSalt` queryId

instance Prelude.NFData GetQueryResults where
  rnf GetQueryResults' {..} = Prelude.rnf queryId

instance Data.ToHeaders GetQueryResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Logs_20140328.GetQueryResults" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQueryResults where
  toJSON GetQueryResults' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("queryId" Data..= queryId)]
      )

instance Data.ToPath GetQueryResults where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueryResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { -- | Includes the number of log events scanned by the query, the number of
    -- log events that matched the query criteria, and the total number of
    -- bytes in the log events that were scanned. These values reflect the full
    -- raw results of the query.
    statistics :: Prelude.Maybe QueryStatistics,
    -- | The status of the most recent running of the query. Possible values are
    -- @Cancelled@, @Complete@, @Failed@, @Running@, @Scheduled@, @Timeout@,
    -- and @Unknown@.
    --
    -- Queries time out after 15 minutes of execution. To avoid having your
    -- queries time out, reduce the time range being searched or partition your
    -- query into a number of queries.
    status :: Prelude.Maybe QueryStatus,
    -- | The log events that matched the query criteria during the most recent
    -- time it ran.
    --
    -- The @results@ value is an array of arrays. Each log event is one object
    -- in the top-level array. Each of these log event objects is an array of
    -- @field@\/@value@ pairs.
    results :: Prelude.Maybe [[ResultField]],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQueryResultsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statistics', 'getQueryResultsResponse_statistics' - Includes the number of log events scanned by the query, the number of
-- log events that matched the query criteria, and the total number of
-- bytes in the log events that were scanned. These values reflect the full
-- raw results of the query.
--
-- 'status', 'getQueryResultsResponse_status' - The status of the most recent running of the query. Possible values are
-- @Cancelled@, @Complete@, @Failed@, @Running@, @Scheduled@, @Timeout@,
-- and @Unknown@.
--
-- Queries time out after 15 minutes of execution. To avoid having your
-- queries time out, reduce the time range being searched or partition your
-- query into a number of queries.
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
  Prelude.Int ->
  GetQueryResultsResponse
newGetQueryResultsResponse pHttpStatus_ =
  GetQueryResultsResponse'
    { statistics =
        Prelude.Nothing,
      status = Prelude.Nothing,
      results = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Includes the number of log events scanned by the query, the number of
-- log events that matched the query criteria, and the total number of
-- bytes in the log events that were scanned. These values reflect the full
-- raw results of the query.
getQueryResultsResponse_statistics :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe QueryStatistics)
getQueryResultsResponse_statistics = Lens.lens (\GetQueryResultsResponse' {statistics} -> statistics) (\s@GetQueryResultsResponse' {} a -> s {statistics = a} :: GetQueryResultsResponse)

-- | The status of the most recent running of the query. Possible values are
-- @Cancelled@, @Complete@, @Failed@, @Running@, @Scheduled@, @Timeout@,
-- and @Unknown@.
--
-- Queries time out after 15 minutes of execution. To avoid having your
-- queries time out, reduce the time range being searched or partition your
-- query into a number of queries.
getQueryResultsResponse_status :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe QueryStatus)
getQueryResultsResponse_status = Lens.lens (\GetQueryResultsResponse' {status} -> status) (\s@GetQueryResultsResponse' {} a -> s {status = a} :: GetQueryResultsResponse)

-- | The log events that matched the query criteria during the most recent
-- time it ran.
--
-- The @results@ value is an array of arrays. Each log event is one object
-- in the top-level array. Each of these log event objects is an array of
-- @field@\/@value@ pairs.
getQueryResultsResponse_results :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe [[ResultField]])
getQueryResultsResponse_results = Lens.lens (\GetQueryResultsResponse' {results} -> results) (\s@GetQueryResultsResponse' {} a -> s {results = a} :: GetQueryResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getQueryResultsResponse_httpStatus :: Lens.Lens' GetQueryResultsResponse Prelude.Int
getQueryResultsResponse_httpStatus = Lens.lens (\GetQueryResultsResponse' {httpStatus} -> httpStatus) (\s@GetQueryResultsResponse' {} a -> s {httpStatus = a} :: GetQueryResultsResponse)

instance Prelude.NFData GetQueryResultsResponse where
  rnf GetQueryResultsResponse' {..} =
    Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf results
      `Prelude.seq` Prelude.rnf httpStatus
