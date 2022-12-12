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
-- Module      : Amazonka.CloudTrail.GetQueryResults
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets event data results of a query. You must specify the @QueryID@ value
-- returned by the @StartQuery@ operation, and an ARN for @EventDataStore@.
module Amazonka.CloudTrail.GetQueryResults
  ( -- * Creating a Request
    GetQueryResults (..),
    newGetQueryResults,

    -- * Request Lenses
    getQueryResults_eventDataStore,
    getQueryResults_maxQueryResults,
    getQueryResults_nextToken,
    getQueryResults_queryId,

    -- * Destructuring the Response
    GetQueryResultsResponse (..),
    newGetQueryResultsResponse,

    -- * Response Lenses
    getQueryResultsResponse_errorMessage,
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_queryResultRows,
    getQueryResultsResponse_queryStatistics,
    getQueryResultsResponse_queryStatus,
    getQueryResultsResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueryResults' smart constructor.
data GetQueryResults = GetQueryResults'
  { -- | The ARN (or ID suffix of the ARN) of the event data store against which
    -- the query was run.
    eventDataStore :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of query results to display on a single page.
    maxQueryResults :: Prelude.Maybe Prelude.Natural,
    -- | A token you can use to get the next page of query results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query for which you want to get results.
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
-- 'eventDataStore', 'getQueryResults_eventDataStore' - The ARN (or ID suffix of the ARN) of the event data store against which
-- the query was run.
--
-- 'maxQueryResults', 'getQueryResults_maxQueryResults' - The maximum number of query results to display on a single page.
--
-- 'nextToken', 'getQueryResults_nextToken' - A token you can use to get the next page of query results.
--
-- 'queryId', 'getQueryResults_queryId' - The ID of the query for which you want to get results.
newGetQueryResults ::
  -- | 'queryId'
  Prelude.Text ->
  GetQueryResults
newGetQueryResults pQueryId_ =
  GetQueryResults'
    { eventDataStore = Prelude.Nothing,
      maxQueryResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryId = pQueryId_
    }

-- | The ARN (or ID suffix of the ARN) of the event data store against which
-- the query was run.
getQueryResults_eventDataStore :: Lens.Lens' GetQueryResults (Prelude.Maybe Prelude.Text)
getQueryResults_eventDataStore = Lens.lens (\GetQueryResults' {eventDataStore} -> eventDataStore) (\s@GetQueryResults' {} a -> s {eventDataStore = a} :: GetQueryResults)

-- | The maximum number of query results to display on a single page.
getQueryResults_maxQueryResults :: Lens.Lens' GetQueryResults (Prelude.Maybe Prelude.Natural)
getQueryResults_maxQueryResults = Lens.lens (\GetQueryResults' {maxQueryResults} -> maxQueryResults) (\s@GetQueryResults' {} a -> s {maxQueryResults = a} :: GetQueryResults)

-- | A token you can use to get the next page of query results.
getQueryResults_nextToken :: Lens.Lens' GetQueryResults (Prelude.Maybe Prelude.Text)
getQueryResults_nextToken = Lens.lens (\GetQueryResults' {nextToken} -> nextToken) (\s@GetQueryResults' {} a -> s {nextToken = a} :: GetQueryResults)

-- | The ID of the query for which you want to get results.
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
            Prelude.<$> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "QueryResultRows"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "QueryStatistics")
            Prelude.<*> (x Data..?> "QueryStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueryResults where
  hashWithSalt _salt GetQueryResults' {..} =
    _salt `Prelude.hashWithSalt` eventDataStore
      `Prelude.hashWithSalt` maxQueryResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryId

instance Prelude.NFData GetQueryResults where
  rnf GetQueryResults' {..} =
    Prelude.rnf eventDataStore
      `Prelude.seq` Prelude.rnf maxQueryResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryId

instance Data.ToHeaders GetQueryResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetQueryResults" ::
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
          [ ("EventDataStore" Data..=)
              Prelude.<$> eventDataStore,
            ("MaxQueryResults" Data..=)
              Prelude.<$> maxQueryResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("QueryId" Data..= queryId)
          ]
      )

instance Data.ToPath GetQueryResults where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueryResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { -- | The error message returned if a query failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | A token you can use to get the next page of query results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Contains the individual event results of the query.
    queryResultRows :: Prelude.Maybe [[Prelude.HashMap Prelude.Text Prelude.Text]],
    -- | Shows the count of query results.
    queryStatistics :: Prelude.Maybe QueryStatistics,
    -- | The status of the query. Values include @QUEUED@, @RUNNING@, @FINISHED@,
    -- @FAILED@, @TIMED_OUT@, or @CANCELLED@.
    queryStatus :: Prelude.Maybe QueryStatus,
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
-- 'errorMessage', 'getQueryResultsResponse_errorMessage' - The error message returned if a query failed.
--
-- 'nextToken', 'getQueryResultsResponse_nextToken' - A token you can use to get the next page of query results.
--
-- 'queryResultRows', 'getQueryResultsResponse_queryResultRows' - Contains the individual event results of the query.
--
-- 'queryStatistics', 'getQueryResultsResponse_queryStatistics' - Shows the count of query results.
--
-- 'queryStatus', 'getQueryResultsResponse_queryStatus' - The status of the query. Values include @QUEUED@, @RUNNING@, @FINISHED@,
-- @FAILED@, @TIMED_OUT@, or @CANCELLED@.
--
-- 'httpStatus', 'getQueryResultsResponse_httpStatus' - The response's http status code.
newGetQueryResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQueryResultsResponse
newGetQueryResultsResponse pHttpStatus_ =
  GetQueryResultsResponse'
    { errorMessage =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryResultRows = Prelude.Nothing,
      queryStatistics = Prelude.Nothing,
      queryStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The error message returned if a query failed.
getQueryResultsResponse_errorMessage :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe Prelude.Text)
getQueryResultsResponse_errorMessage = Lens.lens (\GetQueryResultsResponse' {errorMessage} -> errorMessage) (\s@GetQueryResultsResponse' {} a -> s {errorMessage = a} :: GetQueryResultsResponse)

-- | A token you can use to get the next page of query results.
getQueryResultsResponse_nextToken :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe Prelude.Text)
getQueryResultsResponse_nextToken = Lens.lens (\GetQueryResultsResponse' {nextToken} -> nextToken) (\s@GetQueryResultsResponse' {} a -> s {nextToken = a} :: GetQueryResultsResponse)

-- | Contains the individual event results of the query.
getQueryResultsResponse_queryResultRows :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe [[Prelude.HashMap Prelude.Text Prelude.Text]])
getQueryResultsResponse_queryResultRows = Lens.lens (\GetQueryResultsResponse' {queryResultRows} -> queryResultRows) (\s@GetQueryResultsResponse' {} a -> s {queryResultRows = a} :: GetQueryResultsResponse) Prelude.. Lens.mapping Lens.coerced

-- | Shows the count of query results.
getQueryResultsResponse_queryStatistics :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe QueryStatistics)
getQueryResultsResponse_queryStatistics = Lens.lens (\GetQueryResultsResponse' {queryStatistics} -> queryStatistics) (\s@GetQueryResultsResponse' {} a -> s {queryStatistics = a} :: GetQueryResultsResponse)

-- | The status of the query. Values include @QUEUED@, @RUNNING@, @FINISHED@,
-- @FAILED@, @TIMED_OUT@, or @CANCELLED@.
getQueryResultsResponse_queryStatus :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe QueryStatus)
getQueryResultsResponse_queryStatus = Lens.lens (\GetQueryResultsResponse' {queryStatus} -> queryStatus) (\s@GetQueryResultsResponse' {} a -> s {queryStatus = a} :: GetQueryResultsResponse)

-- | The response's http status code.
getQueryResultsResponse_httpStatus :: Lens.Lens' GetQueryResultsResponse Prelude.Int
getQueryResultsResponse_httpStatus = Lens.lens (\GetQueryResultsResponse' {httpStatus} -> httpStatus) (\s@GetQueryResultsResponse' {} a -> s {httpStatus = a} :: GetQueryResultsResponse)

instance Prelude.NFData GetQueryResultsResponse where
  rnf GetQueryResultsResponse' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryResultRows
      `Prelude.seq` Prelude.rnf queryStatistics
      `Prelude.seq` Prelude.rnf queryStatus
      `Prelude.seq` Prelude.rnf httpStatus
