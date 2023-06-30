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
-- Module      : Amazonka.HoneyCode.ListTableColumns
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListTableColumns API allows you to retrieve a list of all the
-- columns in a table in a workbook.
--
-- This operation returns paginated results.
module Amazonka.HoneyCode.ListTableColumns
  ( -- * Creating a Request
    ListTableColumns (..),
    newListTableColumns,

    -- * Request Lenses
    listTableColumns_nextToken,
    listTableColumns_workbookId,
    listTableColumns_tableId,

    -- * Destructuring the Response
    ListTableColumnsResponse (..),
    newListTableColumnsResponse,

    -- * Response Lenses
    listTableColumnsResponse_nextToken,
    listTableColumnsResponse_workbookCursor,
    listTableColumnsResponse_httpStatus,
    listTableColumnsResponse_tableColumns,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTableColumns' smart constructor.
data ListTableColumns = ListTableColumns'
  { -- | This parameter is optional. If a nextToken is not specified, the API
    -- returns the first page of data.
    --
    -- Pagination tokens expire after 1 hour. If you use a token that was
    -- returned more than an hour back, the API will throw ValidationException.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workbook that contains the table whose columns are being
    -- retrieved.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table whose columns are being retrieved.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableColumns' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTableColumns_nextToken' - This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
--
-- 'workbookId', 'listTableColumns_workbookId' - The ID of the workbook that contains the table whose columns are being
-- retrieved.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'listTableColumns_tableId' - The ID of the table whose columns are being retrieved.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
newListTableColumns ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  ListTableColumns
newListTableColumns pWorkbookId_ pTableId_ =
  ListTableColumns'
    { nextToken = Prelude.Nothing,
      workbookId = pWorkbookId_,
      tableId = pTableId_
    }

-- | This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
listTableColumns_nextToken :: Lens.Lens' ListTableColumns (Prelude.Maybe Prelude.Text)
listTableColumns_nextToken = Lens.lens (\ListTableColumns' {nextToken} -> nextToken) (\s@ListTableColumns' {} a -> s {nextToken = a} :: ListTableColumns)

-- | The ID of the workbook that contains the table whose columns are being
-- retrieved.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
listTableColumns_workbookId :: Lens.Lens' ListTableColumns Prelude.Text
listTableColumns_workbookId = Lens.lens (\ListTableColumns' {workbookId} -> workbookId) (\s@ListTableColumns' {} a -> s {workbookId = a} :: ListTableColumns)

-- | The ID of the table whose columns are being retrieved.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
listTableColumns_tableId :: Lens.Lens' ListTableColumns Prelude.Text
listTableColumns_tableId = Lens.lens (\ListTableColumns' {tableId} -> tableId) (\s@ListTableColumns' {} a -> s {tableId = a} :: ListTableColumns)

instance Core.AWSPager ListTableColumns where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTableColumnsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listTableColumnsResponse_tableColumns) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listTableColumns_nextToken
          Lens..~ rs
          Lens.^? listTableColumnsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListTableColumns where
  type
    AWSResponse ListTableColumns =
      ListTableColumnsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTableColumnsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "workbookCursor")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "tableColumns" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTableColumns where
  hashWithSalt _salt ListTableColumns' {..} =
    _salt
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId

instance Prelude.NFData ListTableColumns where
  rnf ListTableColumns' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId

instance Data.ToHeaders ListTableColumns where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTableColumns where
  toPath ListTableColumns' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/columns"
      ]

instance Data.ToQuery ListTableColumns where
  toQuery ListTableColumns' {..} =
    Prelude.mconcat ["nextToken" Data.=: nextToken]

-- | /See:/ 'newListTableColumnsResponse' smart constructor.
data ListTableColumnsResponse = ListTableColumnsResponse'
  { -- | Provides the pagination token to load the next page if there are more
    -- results matching the request. If a pagination token is not present in
    -- the response, it means that all data matching the request has been
    -- loaded.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates the cursor of the workbook at which the data returned by this
    -- request is read. Workbook cursor keeps increasing with every update and
    -- the increments are not sequential.
    workbookCursor :: Prelude.Maybe Prelude.Integer,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The list of columns in the table.
    tableColumns :: [TableColumn]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTableColumnsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTableColumnsResponse_nextToken' - Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
--
-- 'workbookCursor', 'listTableColumnsResponse_workbookCursor' - Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
--
-- 'httpStatus', 'listTableColumnsResponse_httpStatus' - The response's http status code.
--
-- 'tableColumns', 'listTableColumnsResponse_tableColumns' - The list of columns in the table.
newListTableColumnsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTableColumnsResponse
newListTableColumnsResponse pHttpStatus_ =
  ListTableColumnsResponse'
    { nextToken =
        Prelude.Nothing,
      workbookCursor = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tableColumns = Prelude.mempty
    }

-- | Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
listTableColumnsResponse_nextToken :: Lens.Lens' ListTableColumnsResponse (Prelude.Maybe Prelude.Text)
listTableColumnsResponse_nextToken = Lens.lens (\ListTableColumnsResponse' {nextToken} -> nextToken) (\s@ListTableColumnsResponse' {} a -> s {nextToken = a} :: ListTableColumnsResponse)

-- | Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
listTableColumnsResponse_workbookCursor :: Lens.Lens' ListTableColumnsResponse (Prelude.Maybe Prelude.Integer)
listTableColumnsResponse_workbookCursor = Lens.lens (\ListTableColumnsResponse' {workbookCursor} -> workbookCursor) (\s@ListTableColumnsResponse' {} a -> s {workbookCursor = a} :: ListTableColumnsResponse)

-- | The response's http status code.
listTableColumnsResponse_httpStatus :: Lens.Lens' ListTableColumnsResponse Prelude.Int
listTableColumnsResponse_httpStatus = Lens.lens (\ListTableColumnsResponse' {httpStatus} -> httpStatus) (\s@ListTableColumnsResponse' {} a -> s {httpStatus = a} :: ListTableColumnsResponse)

-- | The list of columns in the table.
listTableColumnsResponse_tableColumns :: Lens.Lens' ListTableColumnsResponse [TableColumn]
listTableColumnsResponse_tableColumns = Lens.lens (\ListTableColumnsResponse' {tableColumns} -> tableColumns) (\s@ListTableColumnsResponse' {} a -> s {tableColumns = a} :: ListTableColumnsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTableColumnsResponse where
  rnf ListTableColumnsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workbookCursor
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tableColumns
