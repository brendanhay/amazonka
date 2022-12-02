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
-- Module      : Amazonka.HoneyCode.ListTables
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListTables API allows you to retrieve a list of all the tables in a
-- workbook.
--
-- This operation returns paginated results.
module Amazonka.HoneyCode.ListTables
  ( -- * Creating a Request
    ListTables (..),
    newListTables,

    -- * Request Lenses
    listTables_nextToken,
    listTables_maxResults,
    listTables_workbookId,

    -- * Destructuring the Response
    ListTablesResponse (..),
    newListTablesResponse,

    -- * Response Lenses
    listTablesResponse_nextToken,
    listTablesResponse_workbookCursor,
    listTablesResponse_httpStatus,
    listTablesResponse_tables,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListTables' smart constructor.
data ListTables = ListTables'
  { -- | This parameter is optional. If a nextToken is not specified, the API
    -- returns the first page of data.
    --
    -- Pagination tokens expire after 1 hour. If you use a token that was
    -- returned more than an hour back, the API will throw ValidationException.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of tables to return in each page of the results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the workbook whose tables are being retrieved.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTables' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTables_nextToken' - This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
--
-- 'maxResults', 'listTables_maxResults' - The maximum number of tables to return in each page of the results.
--
-- 'workbookId', 'listTables_workbookId' - The ID of the workbook whose tables are being retrieved.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
newListTables ::
  -- | 'workbookId'
  Prelude.Text ->
  ListTables
newListTables pWorkbookId_ =
  ListTables'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      workbookId = pWorkbookId_
    }

-- | This parameter is optional. If a nextToken is not specified, the API
-- returns the first page of data.
--
-- Pagination tokens expire after 1 hour. If you use a token that was
-- returned more than an hour back, the API will throw ValidationException.
listTables_nextToken :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Text)
listTables_nextToken = Lens.lens (\ListTables' {nextToken} -> nextToken) (\s@ListTables' {} a -> s {nextToken = a} :: ListTables)

-- | The maximum number of tables to return in each page of the results.
listTables_maxResults :: Lens.Lens' ListTables (Prelude.Maybe Prelude.Natural)
listTables_maxResults = Lens.lens (\ListTables' {maxResults} -> maxResults) (\s@ListTables' {} a -> s {maxResults = a} :: ListTables)

-- | The ID of the workbook whose tables are being retrieved.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
listTables_workbookId :: Lens.Lens' ListTables Prelude.Text
listTables_workbookId = Lens.lens (\ListTables' {workbookId} -> workbookId) (\s@ListTables' {} a -> s {workbookId = a} :: ListTables)

instance Core.AWSPager ListTables where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTablesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop (rs Lens.^. listTablesResponse_tables) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTables_nextToken
          Lens..~ rs
          Lens.^? listTablesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTables where
  type AWSResponse ListTables = ListTablesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTablesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "workbookCursor")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "tables" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListTables where
  hashWithSalt _salt ListTables' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` workbookId

instance Prelude.NFData ListTables where
  rnf ListTables' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf workbookId

instance Data.ToHeaders ListTables where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListTables where
  toPath ListTables' {..} =
    Prelude.mconcat
      ["/workbooks/", Data.toBS workbookId, "/tables"]

instance Data.ToQuery ListTables where
  toQuery ListTables' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "maxResults" Data.=: maxResults
      ]

-- | /See:/ 'newListTablesResponse' smart constructor.
data ListTablesResponse = ListTablesResponse'
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
    -- | The list of tables in the workbook.
    tables :: [Table]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTablesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTablesResponse_nextToken' - Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
--
-- 'workbookCursor', 'listTablesResponse_workbookCursor' - Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
--
-- 'httpStatus', 'listTablesResponse_httpStatus' - The response's http status code.
--
-- 'tables', 'listTablesResponse_tables' - The list of tables in the workbook.
newListTablesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTablesResponse
newListTablesResponse pHttpStatus_ =
  ListTablesResponse'
    { nextToken = Prelude.Nothing,
      workbookCursor = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      tables = Prelude.mempty
    }

-- | Provides the pagination token to load the next page if there are more
-- results matching the request. If a pagination token is not present in
-- the response, it means that all data matching the request has been
-- loaded.
listTablesResponse_nextToken :: Lens.Lens' ListTablesResponse (Prelude.Maybe Prelude.Text)
listTablesResponse_nextToken = Lens.lens (\ListTablesResponse' {nextToken} -> nextToken) (\s@ListTablesResponse' {} a -> s {nextToken = a} :: ListTablesResponse)

-- | Indicates the cursor of the workbook at which the data returned by this
-- request is read. Workbook cursor keeps increasing with every update and
-- the increments are not sequential.
listTablesResponse_workbookCursor :: Lens.Lens' ListTablesResponse (Prelude.Maybe Prelude.Integer)
listTablesResponse_workbookCursor = Lens.lens (\ListTablesResponse' {workbookCursor} -> workbookCursor) (\s@ListTablesResponse' {} a -> s {workbookCursor = a} :: ListTablesResponse)

-- | The response's http status code.
listTablesResponse_httpStatus :: Lens.Lens' ListTablesResponse Prelude.Int
listTablesResponse_httpStatus = Lens.lens (\ListTablesResponse' {httpStatus} -> httpStatus) (\s@ListTablesResponse' {} a -> s {httpStatus = a} :: ListTablesResponse)

-- | The list of tables in the workbook.
listTablesResponse_tables :: Lens.Lens' ListTablesResponse [Table]
listTablesResponse_tables = Lens.lens (\ListTablesResponse' {tables} -> tables) (\s@ListTablesResponse' {} a -> s {tables = a} :: ListTablesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListTablesResponse where
  rnf ListTablesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workbookCursor
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf tables
