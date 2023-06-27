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
-- Module      : Amazonka.HoneyCode.BatchUpdateTableRows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The BatchUpdateTableRows API allows you to update one or more rows in a
-- table in a workbook.
--
-- You can specify the values to set in some or all of the columns in the
-- table for the specified rows. If a column is not explicitly specified in
-- a particular row, then that column will not be updated for that row. To
-- clear out the data in a specific cell, you need to set the value as an
-- empty string (\"\").
module Amazonka.HoneyCode.BatchUpdateTableRows
  ( -- * Creating a Request
    BatchUpdateTableRows (..),
    newBatchUpdateTableRows,

    -- * Request Lenses
    batchUpdateTableRows_clientRequestToken,
    batchUpdateTableRows_workbookId,
    batchUpdateTableRows_tableId,
    batchUpdateTableRows_rowsToUpdate,

    -- * Destructuring the Response
    BatchUpdateTableRowsResponse (..),
    newBatchUpdateTableRowsResponse,

    -- * Response Lenses
    batchUpdateTableRowsResponse_failedBatchItems,
    batchUpdateTableRowsResponse_httpStatus,
    batchUpdateTableRowsResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpdateTableRows' smart constructor.
data BatchUpdateTableRows = BatchUpdateTableRows'
  { -- | The request token for performing the update action. Request tokens help
    -- to identify duplicate requests. If a call times out or fails due to a
    -- transient error like a failed network connection, you can retry the call
    -- with the same request token. The service ensures that if the first call
    -- using that request token is successfully performed, the second call will
    -- not perform the action again.
    --
    -- Note that request tokens are valid only for a few minutes. You cannot
    -- use request tokens to dedupe requests spanning hours or days.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workbook where the rows are being updated.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table where the rows are being updated.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text,
    -- | The list of rows to update in the table. Each item in this list needs to
    -- contain the row id to update along with the map of column id to cell
    -- values for each column in that row that needs to be updated. You need to
    -- specify at least one row in this list, and for each row, you need to
    -- specify at least one column to update.
    --
    -- Note that if one of the row or column ids in the request does not exist
    -- in the table, then the request fails and no updates are made to the
    -- table.
    rowsToUpdate :: Prelude.NonEmpty UpdateRowData
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateTableRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'batchUpdateTableRows_clientRequestToken' - The request token for performing the update action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
--
-- 'workbookId', 'batchUpdateTableRows_workbookId' - The ID of the workbook where the rows are being updated.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'batchUpdateTableRows_tableId' - The ID of the table where the rows are being updated.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'rowsToUpdate', 'batchUpdateTableRows_rowsToUpdate' - The list of rows to update in the table. Each item in this list needs to
-- contain the row id to update along with the map of column id to cell
-- values for each column in that row that needs to be updated. You need to
-- specify at least one row in this list, and for each row, you need to
-- specify at least one column to update.
--
-- Note that if one of the row or column ids in the request does not exist
-- in the table, then the request fails and no updates are made to the
-- table.
newBatchUpdateTableRows ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  -- | 'rowsToUpdate'
  Prelude.NonEmpty UpdateRowData ->
  BatchUpdateTableRows
newBatchUpdateTableRows
  pWorkbookId_
  pTableId_
  pRowsToUpdate_ =
    BatchUpdateTableRows'
      { clientRequestToken =
          Prelude.Nothing,
        workbookId = pWorkbookId_,
        tableId = pTableId_,
        rowsToUpdate = Lens.coerced Lens.# pRowsToUpdate_
      }

-- | The request token for performing the update action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
batchUpdateTableRows_clientRequestToken :: Lens.Lens' BatchUpdateTableRows (Prelude.Maybe Prelude.Text)
batchUpdateTableRows_clientRequestToken = Lens.lens (\BatchUpdateTableRows' {clientRequestToken} -> clientRequestToken) (\s@BatchUpdateTableRows' {} a -> s {clientRequestToken = a} :: BatchUpdateTableRows)

-- | The ID of the workbook where the rows are being updated.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
batchUpdateTableRows_workbookId :: Lens.Lens' BatchUpdateTableRows Prelude.Text
batchUpdateTableRows_workbookId = Lens.lens (\BatchUpdateTableRows' {workbookId} -> workbookId) (\s@BatchUpdateTableRows' {} a -> s {workbookId = a} :: BatchUpdateTableRows)

-- | The ID of the table where the rows are being updated.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
batchUpdateTableRows_tableId :: Lens.Lens' BatchUpdateTableRows Prelude.Text
batchUpdateTableRows_tableId = Lens.lens (\BatchUpdateTableRows' {tableId} -> tableId) (\s@BatchUpdateTableRows' {} a -> s {tableId = a} :: BatchUpdateTableRows)

-- | The list of rows to update in the table. Each item in this list needs to
-- contain the row id to update along with the map of column id to cell
-- values for each column in that row that needs to be updated. You need to
-- specify at least one row in this list, and for each row, you need to
-- specify at least one column to update.
--
-- Note that if one of the row or column ids in the request does not exist
-- in the table, then the request fails and no updates are made to the
-- table.
batchUpdateTableRows_rowsToUpdate :: Lens.Lens' BatchUpdateTableRows (Prelude.NonEmpty UpdateRowData)
batchUpdateTableRows_rowsToUpdate = Lens.lens (\BatchUpdateTableRows' {rowsToUpdate} -> rowsToUpdate) (\s@BatchUpdateTableRows' {} a -> s {rowsToUpdate = a} :: BatchUpdateTableRows) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpdateTableRows where
  type
    AWSResponse BatchUpdateTableRows =
      BatchUpdateTableRowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpdateTableRowsResponse'
            Prelude.<$> ( x
                            Data..?> "failedBatchItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable BatchUpdateTableRows where
  hashWithSalt _salt BatchUpdateTableRows' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` rowsToUpdate

instance Prelude.NFData BatchUpdateTableRows where
  rnf BatchUpdateTableRows' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf rowsToUpdate

instance Data.ToHeaders BatchUpdateTableRows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpdateTableRows where
  toJSON BatchUpdateTableRows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("rowsToUpdate" Data..= rowsToUpdate)
          ]
      )

instance Data.ToPath BatchUpdateTableRows where
  toPath BatchUpdateTableRows' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/rows/batchupdate"
      ]

instance Data.ToQuery BatchUpdateTableRows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpdateTableRowsResponse' smart constructor.
data BatchUpdateTableRowsResponse = BatchUpdateTableRowsResponse'
  { -- | The list of batch items in the request that could not be updated in the
    -- table. Each element in this list contains one item from the request that
    -- could not be updated in the table along with the reason why that item
    -- could not be updated.
    failedBatchItems :: Prelude.Maybe [FailedBatchItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The updated workbook cursor after adding the new rows at the end of the
    -- table.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpdateTableRowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatchItems', 'batchUpdateTableRowsResponse_failedBatchItems' - The list of batch items in the request that could not be updated in the
-- table. Each element in this list contains one item from the request that
-- could not be updated in the table along with the reason why that item
-- could not be updated.
--
-- 'httpStatus', 'batchUpdateTableRowsResponse_httpStatus' - The response's http status code.
--
-- 'workbookCursor', 'batchUpdateTableRowsResponse_workbookCursor' - The updated workbook cursor after adding the new rows at the end of the
-- table.
newBatchUpdateTableRowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  BatchUpdateTableRowsResponse
newBatchUpdateTableRowsResponse
  pHttpStatus_
  pWorkbookCursor_ =
    BatchUpdateTableRowsResponse'
      { failedBatchItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        workbookCursor = pWorkbookCursor_
      }

-- | The list of batch items in the request that could not be updated in the
-- table. Each element in this list contains one item from the request that
-- could not be updated in the table along with the reason why that item
-- could not be updated.
batchUpdateTableRowsResponse_failedBatchItems :: Lens.Lens' BatchUpdateTableRowsResponse (Prelude.Maybe [FailedBatchItem])
batchUpdateTableRowsResponse_failedBatchItems = Lens.lens (\BatchUpdateTableRowsResponse' {failedBatchItems} -> failedBatchItems) (\s@BatchUpdateTableRowsResponse' {} a -> s {failedBatchItems = a} :: BatchUpdateTableRowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpdateTableRowsResponse_httpStatus :: Lens.Lens' BatchUpdateTableRowsResponse Prelude.Int
batchUpdateTableRowsResponse_httpStatus = Lens.lens (\BatchUpdateTableRowsResponse' {httpStatus} -> httpStatus) (\s@BatchUpdateTableRowsResponse' {} a -> s {httpStatus = a} :: BatchUpdateTableRowsResponse)

-- | The updated workbook cursor after adding the new rows at the end of the
-- table.
batchUpdateTableRowsResponse_workbookCursor :: Lens.Lens' BatchUpdateTableRowsResponse Prelude.Integer
batchUpdateTableRowsResponse_workbookCursor = Lens.lens (\BatchUpdateTableRowsResponse' {workbookCursor} -> workbookCursor) (\s@BatchUpdateTableRowsResponse' {} a -> s {workbookCursor = a} :: BatchUpdateTableRowsResponse)

instance Prelude.NFData BatchUpdateTableRowsResponse where
  rnf BatchUpdateTableRowsResponse' {..} =
    Prelude.rnf failedBatchItems
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workbookCursor
