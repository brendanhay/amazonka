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
-- Module      : Amazonka.HoneyCode.BatchCreateTableRows
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The BatchCreateTableRows API allows you to create one or more rows at
-- the end of a table in a workbook. The API allows you to specify the
-- values to set in some or all of the columns in the new rows.
--
-- If a column is not explicitly set in a specific row, then the column
-- level formula specified in the table will be applied to the new row. If
-- there is no column level formula but the last row of the table has a
-- formula, then that formula will be copied down to the new row. If there
-- is no column level formula and no formula in the last row of the table,
-- then that column will be left blank for the new rows.
module Amazonka.HoneyCode.BatchCreateTableRows
  ( -- * Creating a Request
    BatchCreateTableRows (..),
    newBatchCreateTableRows,

    -- * Request Lenses
    batchCreateTableRows_clientRequestToken,
    batchCreateTableRows_workbookId,
    batchCreateTableRows_tableId,
    batchCreateTableRows_rowsToCreate,

    -- * Destructuring the Response
    BatchCreateTableRowsResponse (..),
    newBatchCreateTableRowsResponse,

    -- * Response Lenses
    batchCreateTableRowsResponse_failedBatchItems,
    batchCreateTableRowsResponse_httpStatus,
    batchCreateTableRowsResponse_workbookCursor,
    batchCreateTableRowsResponse_createdRows,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreateTableRows' smart constructor.
data BatchCreateTableRows = BatchCreateTableRows'
  { -- | The request token for performing the batch create operation. Request
    -- tokens help to identify duplicate requests. If a call times out or fails
    -- due to a transient error like a failed network connection, you can retry
    -- the call with the same request token. The service ensures that if the
    -- first call using that request token is successfully performed, the
    -- second call will not perform the operation again.
    --
    -- Note that request tokens are valid only for a few minutes. You cannot
    -- use request tokens to dedupe requests spanning hours or days.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workbook where the new rows are being added.
    --
    -- If a workbook with the specified ID could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table where the new rows are being added.
    --
    -- If a table with the specified ID could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text,
    -- | The list of rows to create at the end of the table. Each item in this
    -- list needs to have a batch item id to uniquely identify the element in
    -- the request and the cells to create for that row. You need to specify at
    -- least one item in this list.
    --
    -- Note that if one of the column ids in any of the rows in the request
    -- does not exist in the table, then the request fails and no updates are
    -- made to the table.
    rowsToCreate :: Prelude.NonEmpty CreateRowData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateTableRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'batchCreateTableRows_clientRequestToken' - The request token for performing the batch create operation. Request
-- tokens help to identify duplicate requests. If a call times out or fails
-- due to a transient error like a failed network connection, you can retry
-- the call with the same request token. The service ensures that if the
-- first call using that request token is successfully performed, the
-- second call will not perform the operation again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
--
-- 'workbookId', 'batchCreateTableRows_workbookId' - The ID of the workbook where the new rows are being added.
--
-- If a workbook with the specified ID could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'batchCreateTableRows_tableId' - The ID of the table where the new rows are being added.
--
-- If a table with the specified ID could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'rowsToCreate', 'batchCreateTableRows_rowsToCreate' - The list of rows to create at the end of the table. Each item in this
-- list needs to have a batch item id to uniquely identify the element in
-- the request and the cells to create for that row. You need to specify at
-- least one item in this list.
--
-- Note that if one of the column ids in any of the rows in the request
-- does not exist in the table, then the request fails and no updates are
-- made to the table.
newBatchCreateTableRows ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  -- | 'rowsToCreate'
  Prelude.NonEmpty CreateRowData ->
  BatchCreateTableRows
newBatchCreateTableRows
  pWorkbookId_
  pTableId_
  pRowsToCreate_ =
    BatchCreateTableRows'
      { clientRequestToken =
          Prelude.Nothing,
        workbookId = pWorkbookId_,
        tableId = pTableId_,
        rowsToCreate = Lens.coerced Lens.# pRowsToCreate_
      }

-- | The request token for performing the batch create operation. Request
-- tokens help to identify duplicate requests. If a call times out or fails
-- due to a transient error like a failed network connection, you can retry
-- the call with the same request token. The service ensures that if the
-- first call using that request token is successfully performed, the
-- second call will not perform the operation again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
batchCreateTableRows_clientRequestToken :: Lens.Lens' BatchCreateTableRows (Prelude.Maybe Prelude.Text)
batchCreateTableRows_clientRequestToken = Lens.lens (\BatchCreateTableRows' {clientRequestToken} -> clientRequestToken) (\s@BatchCreateTableRows' {} a -> s {clientRequestToken = a} :: BatchCreateTableRows)

-- | The ID of the workbook where the new rows are being added.
--
-- If a workbook with the specified ID could not be found, this API throws
-- ResourceNotFoundException.
batchCreateTableRows_workbookId :: Lens.Lens' BatchCreateTableRows Prelude.Text
batchCreateTableRows_workbookId = Lens.lens (\BatchCreateTableRows' {workbookId} -> workbookId) (\s@BatchCreateTableRows' {} a -> s {workbookId = a} :: BatchCreateTableRows)

-- | The ID of the table where the new rows are being added.
--
-- If a table with the specified ID could not be found, this API throws
-- ResourceNotFoundException.
batchCreateTableRows_tableId :: Lens.Lens' BatchCreateTableRows Prelude.Text
batchCreateTableRows_tableId = Lens.lens (\BatchCreateTableRows' {tableId} -> tableId) (\s@BatchCreateTableRows' {} a -> s {tableId = a} :: BatchCreateTableRows)

-- | The list of rows to create at the end of the table. Each item in this
-- list needs to have a batch item id to uniquely identify the element in
-- the request and the cells to create for that row. You need to specify at
-- least one item in this list.
--
-- Note that if one of the column ids in any of the rows in the request
-- does not exist in the table, then the request fails and no updates are
-- made to the table.
batchCreateTableRows_rowsToCreate :: Lens.Lens' BatchCreateTableRows (Prelude.NonEmpty CreateRowData)
batchCreateTableRows_rowsToCreate = Lens.lens (\BatchCreateTableRows' {rowsToCreate} -> rowsToCreate) (\s@BatchCreateTableRows' {} a -> s {rowsToCreate = a} :: BatchCreateTableRows) Prelude.. Lens.coerced

instance Core.AWSRequest BatchCreateTableRows where
  type
    AWSResponse BatchCreateTableRows =
      BatchCreateTableRowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreateTableRowsResponse'
            Prelude.<$> ( x Data..?> "failedBatchItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workbookCursor")
            Prelude.<*> (x Data..?> "createdRows" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable BatchCreateTableRows where
  hashWithSalt _salt BatchCreateTableRows' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` rowsToCreate

instance Prelude.NFData BatchCreateTableRows where
  rnf BatchCreateTableRows' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf rowsToCreate

instance Data.ToHeaders BatchCreateTableRows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchCreateTableRows where
  toJSON BatchCreateTableRows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("rowsToCreate" Data..= rowsToCreate)
          ]
      )

instance Data.ToPath BatchCreateTableRows where
  toPath BatchCreateTableRows' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/rows/batchcreate"
      ]

instance Data.ToQuery BatchCreateTableRows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreateTableRowsResponse' smart constructor.
data BatchCreateTableRowsResponse = BatchCreateTableRowsResponse'
  { -- | The list of batch items in the request that could not be added to the
    -- table. Each element in this list contains one item from the request that
    -- could not be added to the table along with the reason why that item
    -- could not be added.
    failedBatchItems :: Prelude.Maybe [FailedBatchItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The updated workbook cursor after adding the new rows at the end of the
    -- table.
    workbookCursor :: Prelude.Integer,
    -- | The map of batch item id to the row id that was created for that item.
    createdRows :: Prelude.HashMap Prelude.Text Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreateTableRowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatchItems', 'batchCreateTableRowsResponse_failedBatchItems' - The list of batch items in the request that could not be added to the
-- table. Each element in this list contains one item from the request that
-- could not be added to the table along with the reason why that item
-- could not be added.
--
-- 'httpStatus', 'batchCreateTableRowsResponse_httpStatus' - The response's http status code.
--
-- 'workbookCursor', 'batchCreateTableRowsResponse_workbookCursor' - The updated workbook cursor after adding the new rows at the end of the
-- table.
--
-- 'createdRows', 'batchCreateTableRowsResponse_createdRows' - The map of batch item id to the row id that was created for that item.
newBatchCreateTableRowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  BatchCreateTableRowsResponse
newBatchCreateTableRowsResponse
  pHttpStatus_
  pWorkbookCursor_ =
    BatchCreateTableRowsResponse'
      { failedBatchItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        workbookCursor = pWorkbookCursor_,
        createdRows = Prelude.mempty
      }

-- | The list of batch items in the request that could not be added to the
-- table. Each element in this list contains one item from the request that
-- could not be added to the table along with the reason why that item
-- could not be added.
batchCreateTableRowsResponse_failedBatchItems :: Lens.Lens' BatchCreateTableRowsResponse (Prelude.Maybe [FailedBatchItem])
batchCreateTableRowsResponse_failedBatchItems = Lens.lens (\BatchCreateTableRowsResponse' {failedBatchItems} -> failedBatchItems) (\s@BatchCreateTableRowsResponse' {} a -> s {failedBatchItems = a} :: BatchCreateTableRowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreateTableRowsResponse_httpStatus :: Lens.Lens' BatchCreateTableRowsResponse Prelude.Int
batchCreateTableRowsResponse_httpStatus = Lens.lens (\BatchCreateTableRowsResponse' {httpStatus} -> httpStatus) (\s@BatchCreateTableRowsResponse' {} a -> s {httpStatus = a} :: BatchCreateTableRowsResponse)

-- | The updated workbook cursor after adding the new rows at the end of the
-- table.
batchCreateTableRowsResponse_workbookCursor :: Lens.Lens' BatchCreateTableRowsResponse Prelude.Integer
batchCreateTableRowsResponse_workbookCursor = Lens.lens (\BatchCreateTableRowsResponse' {workbookCursor} -> workbookCursor) (\s@BatchCreateTableRowsResponse' {} a -> s {workbookCursor = a} :: BatchCreateTableRowsResponse)

-- | The map of batch item id to the row id that was created for that item.
batchCreateTableRowsResponse_createdRows :: Lens.Lens' BatchCreateTableRowsResponse (Prelude.HashMap Prelude.Text Prelude.Text)
batchCreateTableRowsResponse_createdRows = Lens.lens (\BatchCreateTableRowsResponse' {createdRows} -> createdRows) (\s@BatchCreateTableRowsResponse' {} a -> s {createdRows = a} :: BatchCreateTableRowsResponse) Prelude.. Lens.coerced

instance Prelude.NFData BatchCreateTableRowsResponse where
  rnf BatchCreateTableRowsResponse' {..} =
    Prelude.rnf failedBatchItems
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workbookCursor
      `Prelude.seq` Prelude.rnf createdRows
