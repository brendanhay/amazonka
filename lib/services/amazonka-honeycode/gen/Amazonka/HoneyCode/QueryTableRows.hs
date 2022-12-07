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
-- Module      : Amazonka.HoneyCode.QueryTableRows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The QueryTableRows API allows you to use a filter formula to query for
-- specific rows in a table.
--
-- This operation returns paginated results.
module Amazonka.HoneyCode.QueryTableRows
  ( -- * Creating a Request
    QueryTableRows (..),
    newQueryTableRows,

    -- * Request Lenses
    queryTableRows_nextToken,
    queryTableRows_maxResults,
    queryTableRows_workbookId,
    queryTableRows_tableId,
    queryTableRows_filterFormula,

    -- * Destructuring the Response
    QueryTableRowsResponse (..),
    newQueryTableRowsResponse,

    -- * Response Lenses
    queryTableRowsResponse_nextToken,
    queryTableRowsResponse_httpStatus,
    queryTableRowsResponse_columnIds,
    queryTableRowsResponse_rows,
    queryTableRowsResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newQueryTableRows' smart constructor.
data QueryTableRows = QueryTableRows'
  { -- | This parameter is optional. If a nextToken is not specified, the API
    -- returns the first page of data.
    --
    -- Pagination tokens expire after 1 hour. If you use a token that was
    -- returned more than an hour back, the API will throw ValidationException.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of rows to return in each page of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the workbook whose table rows are being queried.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table whose rows are being queried.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text,
    -- | An object that represents a filter formula along with the id of the
    -- context row under which the filter function needs to evaluate.
    filterFormula :: Filter
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryTableRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'queryTableRows_nextToken' - This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
--
-- 'maxResults', 'queryTableRows_maxResults' - The maximum number of rows to return in each page of the results.
--
-- 'workbookId', 'queryTableRows_workbookId' - The ID of the workbook whose table rows are being queried.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'queryTableRows_tableId' - The ID of the table whose rows are being queried.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'filterFormula', 'queryTableRows_filterFormula' - An object that represents a filter formula along with the id of the
-- context row under which the filter function needs to evaluate.
newQueryTableRows ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  -- | 'filterFormula'
  Filter ->
  QueryTableRows
newQueryTableRows
  pWorkbookId_
  pTableId_
  pFilterFormula_ =
    QueryTableRows'
      { nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        workbookId = pWorkbookId_,
        tableId = pTableId_,
        filterFormula = pFilterFormula_
      }

-- | This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
queryTableRows_nextToken :: Lens.Lens' QueryTableRows (Prelude.Maybe Prelude.Text)
queryTableRows_nextToken = Lens.lens (\QueryTableRows' {nextToken} -> nextToken) (\s@QueryTableRows' {} a -> s {nextToken = a} :: QueryTableRows)

-- | The maximum number of rows to return in each page of the results.
queryTableRows_maxResults :: Lens.Lens' QueryTableRows (Prelude.Maybe Prelude.Natural)
queryTableRows_maxResults = Lens.lens (\QueryTableRows' {maxResults} -> maxResults) (\s@QueryTableRows' {} a -> s {maxResults = a} :: QueryTableRows)

-- | The ID of the workbook whose table rows are being queried.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
queryTableRows_workbookId :: Lens.Lens' QueryTableRows Prelude.Text
queryTableRows_workbookId = Lens.lens (\QueryTableRows' {workbookId} -> workbookId) (\s@QueryTableRows' {} a -> s {workbookId = a} :: QueryTableRows)

-- | The ID of the table whose rows are being queried.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
queryTableRows_tableId :: Lens.Lens' QueryTableRows Prelude.Text
queryTableRows_tableId = Lens.lens (\QueryTableRows' {tableId} -> tableId) (\s@QueryTableRows' {} a -> s {tableId = a} :: QueryTableRows)

-- | An object that represents a filter formula along with the id of the
-- context row under which the filter function needs to evaluate.
queryTableRows_filterFormula :: Lens.Lens' QueryTableRows Filter
queryTableRows_filterFormula = Lens.lens (\QueryTableRows' {filterFormula} -> filterFormula) (\s@QueryTableRows' {} a -> s {filterFormula = a} :: QueryTableRows)

instance Core.AWSPager QueryTableRows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? queryTableRowsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. queryTableRowsResponse_rows) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& queryTableRows_nextToken
          Lens..~ rs
          Lens.^? queryTableRowsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest QueryTableRows where
  type
    AWSResponse QueryTableRows =
      QueryTableRowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          QueryTableRowsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "columnIds")
            Prelude.<*> (x Data..?> "rows" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable QueryTableRows where
  hashWithSalt _salt QueryTableRows' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` filterFormula

instance Prelude.NFData QueryTableRows where
  rnf QueryTableRows' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf filterFormula

instance Data.ToHeaders QueryTableRows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON QueryTableRows where
  toJSON QueryTableRows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("filterFormula" Data..= filterFormula)
          ]
      )

instance Data.ToPath QueryTableRows where
  toPath QueryTableRows' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/rows/query"
      ]

instance Data.ToQuery QueryTableRows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newQueryTableRowsResponse' smart constructor.
data QueryTableRowsResponse = QueryTableRowsResponse'
  { -- | Provides the pagination token to load the next page if there are more
    -- results matching the request. If a pagination token is not present in
    -- the response, it means that all data matching the request has been
    -- loaded.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of columns in the table whose row data is returned in the
    -- result.
    columnIds :: Prelude.NonEmpty Prelude.Text,
    -- | The list of rows in the table that match the query filter.
    rows :: [TableRow],
    -- | Indicates the cursor of the workbook at which the data returned by this
    -- request is read. Workbook cursor keeps increasing with every update and
    -- the increments are not sequential.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueryTableRowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'queryTableRowsResponse_nextToken' - Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
--
-- 'httpStatus', 'queryTableRowsResponse_httpStatus' - The response's http status code.
--
-- 'columnIds', 'queryTableRowsResponse_columnIds' - The list of columns in the table whose row data is returned in the
-- result.
--
-- 'rows', 'queryTableRowsResponse_rows' - The list of rows in the table that match the query filter.
--
-- 'workbookCursor', 'queryTableRowsResponse_workbookCursor' - Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
newQueryTableRowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'columnIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  QueryTableRowsResponse
newQueryTableRowsResponse
  pHttpStatus_
  pColumnIds_
  pWorkbookCursor_ =
    QueryTableRowsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        columnIds = Lens.coerced Lens.# pColumnIds_,
        rows = Prelude.mempty,
        workbookCursor = pWorkbookCursor_
      }

-- | Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
queryTableRowsResponse_nextToken :: Lens.Lens' QueryTableRowsResponse (Prelude.Maybe Prelude.Text)
queryTableRowsResponse_nextToken = Lens.lens (\QueryTableRowsResponse' {nextToken} -> nextToken) (\s@QueryTableRowsResponse' {} a -> s {nextToken = a} :: QueryTableRowsResponse)

-- | The response's http status code.
queryTableRowsResponse_httpStatus :: Lens.Lens' QueryTableRowsResponse Prelude.Int
queryTableRowsResponse_httpStatus = Lens.lens (\QueryTableRowsResponse' {httpStatus} -> httpStatus) (\s@QueryTableRowsResponse' {} a -> s {httpStatus = a} :: QueryTableRowsResponse)

-- | The list of columns in the table whose row data is returned in the
-- result.
queryTableRowsResponse_columnIds :: Lens.Lens' QueryTableRowsResponse (Prelude.NonEmpty Prelude.Text)
queryTableRowsResponse_columnIds = Lens.lens (\QueryTableRowsResponse' {columnIds} -> columnIds) (\s@QueryTableRowsResponse' {} a -> s {columnIds = a} :: QueryTableRowsResponse) Prelude.. Lens.coerced

-- | The list of rows in the table that match the query filter.
queryTableRowsResponse_rows :: Lens.Lens' QueryTableRowsResponse [TableRow]
queryTableRowsResponse_rows = Lens.lens (\QueryTableRowsResponse' {rows} -> rows) (\s@QueryTableRowsResponse' {} a -> s {rows = a} :: QueryTableRowsResponse) Prelude.. Lens.coerced

-- | Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
queryTableRowsResponse_workbookCursor :: Lens.Lens' QueryTableRowsResponse Prelude.Integer
queryTableRowsResponse_workbookCursor = Lens.lens (\QueryTableRowsResponse' {workbookCursor} -> workbookCursor) (\s@QueryTableRowsResponse' {} a -> s {workbookCursor = a} :: QueryTableRowsResponse)

instance Prelude.NFData QueryTableRowsResponse where
  rnf QueryTableRowsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf columnIds
      `Prelude.seq` Prelude.rnf rows
      `Prelude.seq` Prelude.rnf workbookCursor
