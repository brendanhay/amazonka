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
-- Module      : Amazonka.HoneyCode.ListTableRows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListTableRows API allows you to retrieve a list of all the rows in a
-- table in a workbook.
--
-- This operation returns paginated results.
module Amazonka.HoneyCode.ListTableRows
  ( -- * Creating a Request
    ListTableRows (..),
    newListTableRows,

    -- * Request Lenses
    listTableRows_maxResults,
    listTableRows_nextToken,
    listTableRows_rowIds,
    listTableRows_workbookId,
    listTableRows_tableId,

    -- * Destructuring the Response
    ListTableRowsResponse (..),
    newListTableRowsResponse,

    -- * Response Lenses
    listTableRowsResponse_nextToken,
    listTableRowsResponse_rowIdsNotFound,
    listTableRowsResponse_httpStatus,
    listTableRowsResponse_columnIds,
    listTableRowsResponse_rows,
    listTableRowsResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTableRows' smart constructor.
data ListTableRows = ListTableRows'
  { -- | The maximum number of rows to return in each page of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | This parameter is optional. If a nextToken is not specified, the API
    -- returns the first page of data.
    --
    -- Pagination tokens expire after 1 hour. If you use a token that was
    -- returned more than an hour back, the API will throw ValidationException.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | This parameter is optional. If one or more row ids are specified in this
    -- list, then only the specified row ids are returned in the result. If no
    -- row ids are specified here, then all the rows in the table are returned.
    rowIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the workbook that contains the table whose rows are being
    -- retrieved.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table whose rows are being retrieved.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listTableRows_maxResults' - The maximum number of rows to return in each page of the results.
--
-- 'nextToken', 'listTableRows_nextToken' - This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
--
-- 'rowIds', 'listTableRows_rowIds' - This parameter is optional. If one or more row ids are specified in this
-- list, then only the specified row ids are returned in the result. If no
-- row ids are specified here, then all the rows in the table are returned.
--
-- 'workbookId', 'listTableRows_workbookId' - The ID of the workbook that contains the table whose rows are being
-- retrieved.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'listTableRows_tableId' - The ID of the table whose rows are being retrieved.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
newListTableRows ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  ListTableRows
newListTableRows pWorkbookId_ pTableId_ =
  ListTableRows'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      rowIds = Prelude.Nothing,
      workbookId = pWorkbookId_,
      tableId = pTableId_
    }

-- | The maximum number of rows to return in each page of the results.
listTableRows_maxResults :: Lens.Lens' ListTableRows (Prelude.Maybe Prelude.Natural)
listTableRows_maxResults = Lens.lens (\ListTableRows' {maxResults} -> maxResults) (\s@ListTableRows' {} a -> s {maxResults = a} :: ListTableRows)

-- | This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
listTableRows_nextToken :: Lens.Lens' ListTableRows (Prelude.Maybe Prelude.Text)
listTableRows_nextToken = Lens.lens (\ListTableRows' {nextToken} -> nextToken) (\s@ListTableRows' {} a -> s {nextToken = a} :: ListTableRows)

-- | This parameter is optional. If one or more row ids are specified in this
-- list, then only the specified row ids are returned in the result. If no
-- row ids are specified here, then all the rows in the table are returned.
listTableRows_rowIds :: Lens.Lens' ListTableRows (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listTableRows_rowIds = Lens.lens (\ListTableRows' {rowIds} -> rowIds) (\s@ListTableRows' {} a -> s {rowIds = a} :: ListTableRows) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the workbook that contains the table whose rows are being
-- retrieved.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
listTableRows_workbookId :: Lens.Lens' ListTableRows Prelude.Text
listTableRows_workbookId = Lens.lens (\ListTableRows' {workbookId} -> workbookId) (\s@ListTableRows' {} a -> s {workbookId = a} :: ListTableRows)

-- | The ID of the table whose rows are being retrieved.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
listTableRows_tableId :: Lens.Lens' ListTableRows Prelude.Text
listTableRows_tableId = Lens.lens (\ListTableRows' {tableId} -> tableId) (\s@ListTableRows' {} a -> s {tableId = a} :: ListTableRows)

instance Core.AWSPager ListTableRows where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTableRowsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop (rs Lens.^. listTableRowsResponse_rows) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listTableRows_nextToken
              Lens..~ rs
              Lens.^? listTableRowsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListTableRows where
  type
    AWSResponse ListTableRows =
      ListTableRowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTableRowsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "rowIdsNotFound")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "columnIds")
            Prelude.<*> (x Data..?> "rows" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable ListTableRows where
  hashWithSalt _salt ListTableRows' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` rowIds
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId

instance Prelude.NFData ListTableRows where
  rnf ListTableRows' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf rowIds `Prelude.seq`
          Prelude.rnf workbookId `Prelude.seq`
            Prelude.rnf tableId

instance Data.ToHeaders ListTableRows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTableRows where
  toJSON ListTableRows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("rowIds" Data..=) Prelude.<$> rowIds
          ]
      )

instance Data.ToPath ListTableRows where
  toPath ListTableRows' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/rows/list"
      ]

instance Data.ToQuery ListTableRows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTableRowsResponse' smart constructor.
data ListTableRowsResponse = ListTableRowsResponse'
  { -- | Provides the pagination token to load the next page if there are more
    -- results matching the request. If a pagination token is not present in
    -- the response, it means that all data matching the request has been
    -- loaded.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of row ids included in the request that were not found in the
    -- table.
    rowIdsNotFound :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of columns in the table whose row data is returned in the
    -- result.
    columnIds :: Prelude.NonEmpty Prelude.Text,
    -- | The list of rows in the table. Note that this result is paginated, so
    -- this list contains a maximum of 100 rows.
    rows :: [TableRow],
    -- | Indicates the cursor of the workbook at which the data returned by this
    -- request is read. Workbook cursor keeps increasing with every update and
    -- the increments are not sequential.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableRowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTableRowsResponse_nextToken' - Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
--
-- 'rowIdsNotFound', 'listTableRowsResponse_rowIdsNotFound' - The list of row ids included in the request that were not found in the
-- table.
--
-- 'httpStatus', 'listTableRowsResponse_httpStatus' - The response's http status code.
--
-- 'columnIds', 'listTableRowsResponse_columnIds' - The list of columns in the table whose row data is returned in the
-- result.
--
-- 'rows', 'listTableRowsResponse_rows' - The list of rows in the table. Note that this result is paginated, so
-- this list contains a maximum of 100 rows.
--
-- 'workbookCursor', 'listTableRowsResponse_workbookCursor' - Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
newListTableRowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'columnIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  ListTableRowsResponse
newListTableRowsResponse
  pHttpStatus_
  pColumnIds_
  pWorkbookCursor_ =
    ListTableRowsResponse'
      { nextToken = Prelude.Nothing,
        rowIdsNotFound = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        columnIds = Lens.coerced Lens.# pColumnIds_,
        rows = Prelude.mempty,
        workbookCursor = pWorkbookCursor_
      }

-- | Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
listTableRowsResponse_nextToken :: Lens.Lens' ListTableRowsResponse (Prelude.Maybe Prelude.Text)
listTableRowsResponse_nextToken = Lens.lens (\ListTableRowsResponse' {nextToken} -> nextToken) (\s@ListTableRowsResponse' {} a -> s {nextToken = a} :: ListTableRowsResponse)

-- | The list of row ids included in the request that were not found in the
-- table.
listTableRowsResponse_rowIdsNotFound :: Lens.Lens' ListTableRowsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
listTableRowsResponse_rowIdsNotFound = Lens.lens (\ListTableRowsResponse' {rowIdsNotFound} -> rowIdsNotFound) (\s@ListTableRowsResponse' {} a -> s {rowIdsNotFound = a} :: ListTableRowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTableRowsResponse_httpStatus :: Lens.Lens' ListTableRowsResponse Prelude.Int
listTableRowsResponse_httpStatus = Lens.lens (\ListTableRowsResponse' {httpStatus} -> httpStatus) (\s@ListTableRowsResponse' {} a -> s {httpStatus = a} :: ListTableRowsResponse)

-- | The list of columns in the table whose row data is returned in the
-- result.
listTableRowsResponse_columnIds :: Lens.Lens' ListTableRowsResponse (Prelude.NonEmpty Prelude.Text)
listTableRowsResponse_columnIds = Lens.lens (\ListTableRowsResponse' {columnIds} -> columnIds) (\s@ListTableRowsResponse' {} a -> s {columnIds = a} :: ListTableRowsResponse) Prelude.. Lens.coerced

-- | The list of rows in the table. Note that this result is paginated, so
-- this list contains a maximum of 100 rows.
listTableRowsResponse_rows :: Lens.Lens' ListTableRowsResponse [TableRow]
listTableRowsResponse_rows = Lens.lens (\ListTableRowsResponse' {rows} -> rows) (\s@ListTableRowsResponse' {} a -> s {rows = a} :: ListTableRowsResponse) Prelude.. Lens.coerced

-- | Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
listTableRowsResponse_workbookCursor :: Lens.Lens' ListTableRowsResponse Prelude.Integer
listTableRowsResponse_workbookCursor = Lens.lens (\ListTableRowsResponse' {workbookCursor} -> workbookCursor) (\s@ListTableRowsResponse' {} a -> s {workbookCursor = a} :: ListTableRowsResponse)

instance Prelude.NFData ListTableRowsResponse where
  rnf ListTableRowsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf rowIdsNotFound `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf columnIds `Prelude.seq`
            Prelude.rnf rows `Prelude.seq`
              Prelude.rnf workbookCursor
