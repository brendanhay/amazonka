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
-- Module      : Amazonka.TimeStreamQuery.Query
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- @Query@ is a synchronous operation that enables you to run a query
-- against your Amazon Timestream data. @Query@ will time out after 60
-- seconds. You must update the default timeout in the SDK to support a
-- timeout of 60 seconds. See the
-- <https://docs.aws.amazon.com/timestream/latest/developerguide/code-samples.run-query.html code sample>
-- for details.
--
-- Your query request will fail in the following cases:
--
-- -   If you submit a @Query@ request with the same client token outside
--     of the 5-minute idempotency window.
--
-- -   If you submit a @Query@ request with the same client token, but
--     change other parameters, within the 5-minute idempotency window.
--
-- -   If the size of the row (including the query metadata) exceeds 1 MB,
--     then the query will fail with the following error message:
--
--     @Query aborted as max page response size has been exceeded by the output result row@
--
-- -   If the IAM principal of the query initiator and the result reader
--     are not the same and\/or the query initiator and the result reader
--     do not have the same query string in the query requests, the query
--     will fail with an @Invalid pagination token@ error.
--
-- This operation returns paginated results.
module Amazonka.TimeStreamQuery.Query
  ( -- * Creating a Request
    Query (..),
    newQuery,

    -- * Request Lenses
    query_clientToken,
    query_maxRows,
    query_nextToken,
    query_queryString,

    -- * Destructuring the Response
    QueryResponse (..),
    newQueryResponse,

    -- * Response Lenses
    queryResponse_nextToken,
    queryResponse_queryStatus,
    queryResponse_httpStatus,
    queryResponse_queryId,
    queryResponse_rows,
    queryResponse_columnInfo,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.TimeStreamQuery.Types

-- | /See:/ 'newQuery' smart constructor.
data Query = Query'
  { -- | Unique, case-sensitive string of up to 64 ASCII characters specified
    -- when a @Query@ request is made. Providing a @ClientToken@ makes the call
    -- to @Query@ /idempotent/. This means that running the same query
    -- repeatedly will produce the same result. In other words, making multiple
    -- identical @Query@ requests has the same effect as making a single
    -- request. When using @ClientToken@ in a query, note the following:
    --
    -- -   If the Query API is instantiated without a @ClientToken@, the Query
    --     SDK generates a @ClientToken@ on your behalf.
    --
    -- -   If the @Query@ invocation only contains the @ClientToken@ but does
    --     not include a @NextToken@, that invocation of @Query@ is assumed to
    --     be a new query run.
    --
    -- -   If the invocation contains @NextToken@, that particular invocation
    --     is assumed to be a subsequent invocation of a prior call to the
    --     Query API, and a result set is returned.
    --
    -- -   After 4 hours, any request with the same @ClientToken@ is treated as
    --     a new request.
    clientToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The total number of rows to be returned in the @Query@ output. The
    -- initial run of @Query@ with a @MaxRows@ value specified will return the
    -- result set of the query in two cases:
    --
    -- -   The size of the result is less than @1MB@.
    --
    -- -   The number of rows in the result set is less than the value of
    --     @maxRows@.
    --
    -- Otherwise, the initial invocation of @Query@ only returns a @NextToken@,
    -- which can then be used in subsequent calls to fetch the result set. To
    -- resume pagination, provide the @NextToken@ value in the subsequent
    -- command.
    --
    -- If the row size is large (e.g. a row has many columns), Timestream may
    -- return fewer rows to keep the response size from exceeding the 1 MB
    -- limit. If @MaxRows@ is not provided, Timestream will send the necessary
    -- number of rows to meet the 1 MB limit.
    maxRows :: Prelude.Maybe Prelude.Natural,
    -- | A pagination token used to return a set of results. When the @Query@ API
    -- is invoked using @NextToken@, that particular invocation is assumed to
    -- be a subsequent invocation of a prior call to @Query@, and a result set
    -- is returned. However, if the @Query@ invocation only contains the
    -- @ClientToken@, that invocation of @Query@ is assumed to be a new query
    -- run.
    --
    -- Note the following when using NextToken in a query:
    --
    -- -   A pagination token can be used for up to five @Query@ invocations,
    --     OR for a duration of up to 1 hour – whichever comes first.
    --
    -- -   Using the same @NextToken@ will return the same set of records. To
    --     keep paginating through the result set, you must to use the most
    --     recent @nextToken@.
    --
    -- -   Suppose a @Query@ invocation returns two @NextToken@ values,
    --     @TokenA@ and @TokenB@. If @TokenB@ is used in a subsequent @Query@
    --     invocation, then @TokenA@ is invalidated and cannot be reused.
    --
    -- -   To request a previous result set from a query after pagination has
    --     begun, you must re-invoke the Query API.
    --
    -- -   The latest @NextToken@ should be used to paginate until @null@ is
    --     returned, at which point a new @NextToken@ should be used.
    --
    -- -   If the IAM principal of the query initiator and the result reader
    --     are not the same and\/or the query initiator and the result reader
    --     do not have the same query string in the query requests, the query
    --     will fail with an @Invalid pagination token@ error.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The query to be run by Timestream.
    queryString :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Query' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'query_clientToken' - Unique, case-sensitive string of up to 64 ASCII characters specified
-- when a @Query@ request is made. Providing a @ClientToken@ makes the call
-- to @Query@ /idempotent/. This means that running the same query
-- repeatedly will produce the same result. In other words, making multiple
-- identical @Query@ requests has the same effect as making a single
-- request. When using @ClientToken@ in a query, note the following:
--
-- -   If the Query API is instantiated without a @ClientToken@, the Query
--     SDK generates a @ClientToken@ on your behalf.
--
-- -   If the @Query@ invocation only contains the @ClientToken@ but does
--     not include a @NextToken@, that invocation of @Query@ is assumed to
--     be a new query run.
--
-- -   If the invocation contains @NextToken@, that particular invocation
--     is assumed to be a subsequent invocation of a prior call to the
--     Query API, and a result set is returned.
--
-- -   After 4 hours, any request with the same @ClientToken@ is treated as
--     a new request.
--
-- 'maxRows', 'query_maxRows' - The total number of rows to be returned in the @Query@ output. The
-- initial run of @Query@ with a @MaxRows@ value specified will return the
-- result set of the query in two cases:
--
-- -   The size of the result is less than @1MB@.
--
-- -   The number of rows in the result set is less than the value of
--     @maxRows@.
--
-- Otherwise, the initial invocation of @Query@ only returns a @NextToken@,
-- which can then be used in subsequent calls to fetch the result set. To
-- resume pagination, provide the @NextToken@ value in the subsequent
-- command.
--
-- If the row size is large (e.g. a row has many columns), Timestream may
-- return fewer rows to keep the response size from exceeding the 1 MB
-- limit. If @MaxRows@ is not provided, Timestream will send the necessary
-- number of rows to meet the 1 MB limit.
--
-- 'nextToken', 'query_nextToken' - A pagination token used to return a set of results. When the @Query@ API
-- is invoked using @NextToken@, that particular invocation is assumed to
-- be a subsequent invocation of a prior call to @Query@, and a result set
-- is returned. However, if the @Query@ invocation only contains the
-- @ClientToken@, that invocation of @Query@ is assumed to be a new query
-- run.
--
-- Note the following when using NextToken in a query:
--
-- -   A pagination token can be used for up to five @Query@ invocations,
--     OR for a duration of up to 1 hour – whichever comes first.
--
-- -   Using the same @NextToken@ will return the same set of records. To
--     keep paginating through the result set, you must to use the most
--     recent @nextToken@.
--
-- -   Suppose a @Query@ invocation returns two @NextToken@ values,
--     @TokenA@ and @TokenB@. If @TokenB@ is used in a subsequent @Query@
--     invocation, then @TokenA@ is invalidated and cannot be reused.
--
-- -   To request a previous result set from a query after pagination has
--     begun, you must re-invoke the Query API.
--
-- -   The latest @NextToken@ should be used to paginate until @null@ is
--     returned, at which point a new @NextToken@ should be used.
--
-- -   If the IAM principal of the query initiator and the result reader
--     are not the same and\/or the query initiator and the result reader
--     do not have the same query string in the query requests, the query
--     will fail with an @Invalid pagination token@ error.
--
-- 'queryString', 'query_queryString' - The query to be run by Timestream.
newQuery ::
  -- | 'queryString'
  Prelude.Text ->
  Query
newQuery pQueryString_ =
  Query'
    { clientToken = Prelude.Nothing,
      maxRows = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      queryString = Data._Sensitive Lens.# pQueryString_
    }

-- | Unique, case-sensitive string of up to 64 ASCII characters specified
-- when a @Query@ request is made. Providing a @ClientToken@ makes the call
-- to @Query@ /idempotent/. This means that running the same query
-- repeatedly will produce the same result. In other words, making multiple
-- identical @Query@ requests has the same effect as making a single
-- request. When using @ClientToken@ in a query, note the following:
--
-- -   If the Query API is instantiated without a @ClientToken@, the Query
--     SDK generates a @ClientToken@ on your behalf.
--
-- -   If the @Query@ invocation only contains the @ClientToken@ but does
--     not include a @NextToken@, that invocation of @Query@ is assumed to
--     be a new query run.
--
-- -   If the invocation contains @NextToken@, that particular invocation
--     is assumed to be a subsequent invocation of a prior call to the
--     Query API, and a result set is returned.
--
-- -   After 4 hours, any request with the same @ClientToken@ is treated as
--     a new request.
query_clientToken :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_clientToken = Lens.lens (\Query' {clientToken} -> clientToken) (\s@Query' {} a -> s {clientToken = a} :: Query) Prelude.. Lens.mapping Data._Sensitive

-- | The total number of rows to be returned in the @Query@ output. The
-- initial run of @Query@ with a @MaxRows@ value specified will return the
-- result set of the query in two cases:
--
-- -   The size of the result is less than @1MB@.
--
-- -   The number of rows in the result set is less than the value of
--     @maxRows@.
--
-- Otherwise, the initial invocation of @Query@ only returns a @NextToken@,
-- which can then be used in subsequent calls to fetch the result set. To
-- resume pagination, provide the @NextToken@ value in the subsequent
-- command.
--
-- If the row size is large (e.g. a row has many columns), Timestream may
-- return fewer rows to keep the response size from exceeding the 1 MB
-- limit. If @MaxRows@ is not provided, Timestream will send the necessary
-- number of rows to meet the 1 MB limit.
query_maxRows :: Lens.Lens' Query (Prelude.Maybe Prelude.Natural)
query_maxRows = Lens.lens (\Query' {maxRows} -> maxRows) (\s@Query' {} a -> s {maxRows = a} :: Query)

-- | A pagination token used to return a set of results. When the @Query@ API
-- is invoked using @NextToken@, that particular invocation is assumed to
-- be a subsequent invocation of a prior call to @Query@, and a result set
-- is returned. However, if the @Query@ invocation only contains the
-- @ClientToken@, that invocation of @Query@ is assumed to be a new query
-- run.
--
-- Note the following when using NextToken in a query:
--
-- -   A pagination token can be used for up to five @Query@ invocations,
--     OR for a duration of up to 1 hour – whichever comes first.
--
-- -   Using the same @NextToken@ will return the same set of records. To
--     keep paginating through the result set, you must to use the most
--     recent @nextToken@.
--
-- -   Suppose a @Query@ invocation returns two @NextToken@ values,
--     @TokenA@ and @TokenB@. If @TokenB@ is used in a subsequent @Query@
--     invocation, then @TokenA@ is invalidated and cannot be reused.
--
-- -   To request a previous result set from a query after pagination has
--     begun, you must re-invoke the Query API.
--
-- -   The latest @NextToken@ should be used to paginate until @null@ is
--     returned, at which point a new @NextToken@ should be used.
--
-- -   If the IAM principal of the query initiator and the result reader
--     are not the same and\/or the query initiator and the result reader
--     do not have the same query string in the query requests, the query
--     will fail with an @Invalid pagination token@ error.
query_nextToken :: Lens.Lens' Query (Prelude.Maybe Prelude.Text)
query_nextToken = Lens.lens (\Query' {nextToken} -> nextToken) (\s@Query' {} a -> s {nextToken = a} :: Query)

-- | The query to be run by Timestream.
query_queryString :: Lens.Lens' Query Prelude.Text
query_queryString = Lens.lens (\Query' {queryString} -> queryString) (\s@Query' {} a -> s {queryString = a} :: Query) Prelude.. Data._Sensitive

instance Core.AWSPager Query where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? queryResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. queryResponse_rows) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& query_nextToken
          Lens..~ rs
          Lens.^? queryResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest Query where
  type AWSResponse Query = QueryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "QueryStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "QueryId")
            Prelude.<*> (x Data..?> "Rows" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "ColumnInfo" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable Query where
  hashWithSalt _salt Query' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` maxRows
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` queryString

instance Prelude.NFData Query where
  rnf Query' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf maxRows
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryString

instance Data.ToHeaders Query where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("Timestream_20181101.Query" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON Query where
  toJSON Query' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("MaxRows" Data..=) Prelude.<$> maxRows,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("QueryString" Data..= queryString)
          ]
      )

instance Data.ToPath Query where
  toPath = Prelude.const "/"

instance Data.ToQuery Query where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQueryResponse' smart constructor.
data QueryResponse = QueryResponse'
  { -- | A pagination token that can be used again on a @Query@ call to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the status of the query, including progress and bytes
    -- scanned.
    queryStatus :: Prelude.Maybe QueryStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A unique ID for the given query.
    queryId :: Prelude.Text,
    -- | The result set rows returned by the query.
    rows :: [Row],
    -- | The column data types of the returned result set.
    columnInfo :: [ColumnInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'queryResponse_nextToken' - A pagination token that can be used again on a @Query@ call to get the
-- next set of results.
--
-- 'queryStatus', 'queryResponse_queryStatus' - Information about the status of the query, including progress and bytes
-- scanned.
--
-- 'httpStatus', 'queryResponse_httpStatus' - The response's http status code.
--
-- 'queryId', 'queryResponse_queryId' - A unique ID for the given query.
--
-- 'rows', 'queryResponse_rows' - The result set rows returned by the query.
--
-- 'columnInfo', 'queryResponse_columnInfo' - The column data types of the returned result set.
newQueryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'queryId'
  Prelude.Text ->
  QueryResponse
newQueryResponse pHttpStatus_ pQueryId_ =
  QueryResponse'
    { nextToken = Prelude.Nothing,
      queryStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      queryId = pQueryId_,
      rows = Prelude.mempty,
      columnInfo = Prelude.mempty
    }

-- | A pagination token that can be used again on a @Query@ call to get the
-- next set of results.
queryResponse_nextToken :: Lens.Lens' QueryResponse (Prelude.Maybe Prelude.Text)
queryResponse_nextToken = Lens.lens (\QueryResponse' {nextToken} -> nextToken) (\s@QueryResponse' {} a -> s {nextToken = a} :: QueryResponse)

-- | Information about the status of the query, including progress and bytes
-- scanned.
queryResponse_queryStatus :: Lens.Lens' QueryResponse (Prelude.Maybe QueryStatus)
queryResponse_queryStatus = Lens.lens (\QueryResponse' {queryStatus} -> queryStatus) (\s@QueryResponse' {} a -> s {queryStatus = a} :: QueryResponse)

-- | The response's http status code.
queryResponse_httpStatus :: Lens.Lens' QueryResponse Prelude.Int
queryResponse_httpStatus = Lens.lens (\QueryResponse' {httpStatus} -> httpStatus) (\s@QueryResponse' {} a -> s {httpStatus = a} :: QueryResponse)

-- | A unique ID for the given query.
queryResponse_queryId :: Lens.Lens' QueryResponse Prelude.Text
queryResponse_queryId = Lens.lens (\QueryResponse' {queryId} -> queryId) (\s@QueryResponse' {} a -> s {queryId = a} :: QueryResponse)

-- | The result set rows returned by the query.
queryResponse_rows :: Lens.Lens' QueryResponse [Row]
queryResponse_rows = Lens.lens (\QueryResponse' {rows} -> rows) (\s@QueryResponse' {} a -> s {rows = a} :: QueryResponse) Prelude.. Lens.coerced

-- | The column data types of the returned result set.
queryResponse_columnInfo :: Lens.Lens' QueryResponse [ColumnInfo]
queryResponse_columnInfo = Lens.lens (\QueryResponse' {columnInfo} -> columnInfo) (\s@QueryResponse' {} a -> s {columnInfo = a} :: QueryResponse) Prelude.. Lens.coerced

instance Prelude.NFData QueryResponse where
  rnf QueryResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf queryStatus
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf queryId
      `Prelude.seq` Prelude.rnf rows
      `Prelude.seq` Prelude.rnf columnInfo
