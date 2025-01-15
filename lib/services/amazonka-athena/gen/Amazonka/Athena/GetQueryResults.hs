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
-- Module      : Amazonka.Athena.GetQueryResults
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Streams the results of a single query execution specified by
-- @QueryExecutionId@ from the Athena query results location in Amazon S3.
-- For more information, see
-- <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results>
-- in the /Amazon Athena User Guide/. This request does not execute the
-- query but returns results. Use StartQueryExecution to run a query.
--
-- To stream query results successfully, the IAM principal with permission
-- to call @GetQueryResults@ also must have permissions to the Amazon S3
-- @GetObject@ action for the Athena query results location.
--
-- IAM principals with permission to the Amazon S3 @GetObject@ action for
-- the query results location are able to retrieve query results from
-- Amazon S3 even if permission to the @GetQueryResults@ action is denied.
-- To restrict user or role access, ensure that Amazon S3 permissions to
-- the Athena query location are denied.
--
-- This operation returns paginated results.
module Amazonka.Athena.GetQueryResults
  ( -- * Creating a Request
    GetQueryResults (..),
    newGetQueryResults,

    -- * Request Lenses
    getQueryResults_maxResults,
    getQueryResults_nextToken,
    getQueryResults_queryExecutionId,

    -- * Destructuring the Response
    GetQueryResultsResponse (..),
    newGetQueryResultsResponse,

    -- * Response Lenses
    getQueryResultsResponse_nextToken,
    getQueryResultsResponse_resultSet,
    getQueryResultsResponse_updateCount,
    getQueryResultsResponse_httpStatus,
  )
where

import Amazonka.Athena.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQueryResults' smart constructor.
data GetQueryResults = GetQueryResults'
  { -- | The maximum number of results (rows) to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the @NextToken@ from the response object of the
    -- previous page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of the query execution.
    queryExecutionId :: Prelude.Text
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
-- 'maxResults', 'getQueryResults_maxResults' - The maximum number of results (rows) to return in this request.
--
-- 'nextToken', 'getQueryResults_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
--
-- 'queryExecutionId', 'getQueryResults_queryExecutionId' - The unique ID of the query execution.
newGetQueryResults ::
  -- | 'queryExecutionId'
  Prelude.Text ->
  GetQueryResults
newGetQueryResults pQueryExecutionId_ =
  GetQueryResults'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryExecutionId = pQueryExecutionId_
    }

-- | The maximum number of results (rows) to return in this request.
getQueryResults_maxResults :: Lens.Lens' GetQueryResults (Prelude.Maybe Prelude.Natural)
getQueryResults_maxResults = Lens.lens (\GetQueryResults' {maxResults} -> maxResults) (\s@GetQueryResults' {} a -> s {maxResults = a} :: GetQueryResults)

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
getQueryResults_nextToken :: Lens.Lens' GetQueryResults (Prelude.Maybe Prelude.Text)
getQueryResults_nextToken = Lens.lens (\GetQueryResults' {nextToken} -> nextToken) (\s@GetQueryResults' {} a -> s {nextToken = a} :: GetQueryResults)

-- | The unique ID of the query execution.
getQueryResults_queryExecutionId :: Lens.Lens' GetQueryResults Prelude.Text
getQueryResults_queryExecutionId = Lens.lens (\GetQueryResults' {queryExecutionId} -> queryExecutionId) (\s@GetQueryResults' {} a -> s {queryExecutionId = a} :: GetQueryResults)

instance Core.AWSPager GetQueryResults where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getQueryResultsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getQueryResultsResponse_resultSet
            Prelude.. Lens._Just
            Prelude.. resultSet_rows
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getQueryResults_nextToken
              Lens..~ rs
              Lens.^? getQueryResultsResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "ResultSet")
            Prelude.<*> (x Data..?> "UpdateCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQueryResults where
  hashWithSalt _salt GetQueryResults' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryExecutionId

instance Prelude.NFData GetQueryResults where
  rnf GetQueryResults' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf queryExecutionId

instance Data.ToHeaders GetQueryResults where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonAthena.GetQueryResults" ::
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
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just
              ("QueryExecutionId" Data..= queryExecutionId)
          ]
      )

instance Data.ToPath GetQueryResults where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQueryResults where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQueryResultsResponse' smart constructor.
data GetQueryResultsResponse = GetQueryResultsResponse'
  { -- | A token generated by the Athena service that specifies where to continue
    -- pagination if a previous request was truncated. To obtain the next set
    -- of pages, pass in the @NextToken@ from the response object of the
    -- previous page call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The results of the query execution.
    resultSet :: Prelude.Maybe ResultSet,
    -- | The number of rows inserted with a @CREATE TABLE AS SELECT@ statement.
    updateCount :: Prelude.Maybe Prelude.Integer,
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
-- 'nextToken', 'getQueryResultsResponse_nextToken' - A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
--
-- 'resultSet', 'getQueryResultsResponse_resultSet' - The results of the query execution.
--
-- 'updateCount', 'getQueryResultsResponse_updateCount' - The number of rows inserted with a @CREATE TABLE AS SELECT@ statement.
--
-- 'httpStatus', 'getQueryResultsResponse_httpStatus' - The response's http status code.
newGetQueryResultsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQueryResultsResponse
newGetQueryResultsResponse pHttpStatus_ =
  GetQueryResultsResponse'
    { nextToken =
        Prelude.Nothing,
      resultSet = Prelude.Nothing,
      updateCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token generated by the Athena service that specifies where to continue
-- pagination if a previous request was truncated. To obtain the next set
-- of pages, pass in the @NextToken@ from the response object of the
-- previous page call.
getQueryResultsResponse_nextToken :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe Prelude.Text)
getQueryResultsResponse_nextToken = Lens.lens (\GetQueryResultsResponse' {nextToken} -> nextToken) (\s@GetQueryResultsResponse' {} a -> s {nextToken = a} :: GetQueryResultsResponse)

-- | The results of the query execution.
getQueryResultsResponse_resultSet :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe ResultSet)
getQueryResultsResponse_resultSet = Lens.lens (\GetQueryResultsResponse' {resultSet} -> resultSet) (\s@GetQueryResultsResponse' {} a -> s {resultSet = a} :: GetQueryResultsResponse)

-- | The number of rows inserted with a @CREATE TABLE AS SELECT@ statement.
getQueryResultsResponse_updateCount :: Lens.Lens' GetQueryResultsResponse (Prelude.Maybe Prelude.Integer)
getQueryResultsResponse_updateCount = Lens.lens (\GetQueryResultsResponse' {updateCount} -> updateCount) (\s@GetQueryResultsResponse' {} a -> s {updateCount = a} :: GetQueryResultsResponse)

-- | The response's http status code.
getQueryResultsResponse_httpStatus :: Lens.Lens' GetQueryResultsResponse Prelude.Int
getQueryResultsResponse_httpStatus = Lens.lens (\GetQueryResultsResponse' {httpStatus} -> httpStatus) (\s@GetQueryResultsResponse' {} a -> s {httpStatus = a} :: GetQueryResultsResponse)

instance Prelude.NFData GetQueryResultsResponse where
  rnf GetQueryResultsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf resultSet `Prelude.seq`
        Prelude.rnf updateCount `Prelude.seq`
          Prelude.rnf httpStatus
