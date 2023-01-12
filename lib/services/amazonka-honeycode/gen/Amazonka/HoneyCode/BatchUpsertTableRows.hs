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
-- Module      : Amazonka.HoneyCode.BatchUpsertTableRows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The BatchUpsertTableRows API allows you to upsert one or more rows in a
-- table. The upsert operation takes a filter expression as input and
-- evaluates it to find matching rows on the destination table. If matching
-- rows are found, it will update the cells in the matching rows to new
-- values specified in the request. If no matching rows are found, a new
-- row is added at the end of the table and the cells in that row are set
-- to the new values specified in the request.
--
-- You can specify the values to set in some or all of the columns in the
-- table for the matching or newly appended rows. If a column is not
-- explicitly specified for a particular row, then that column will not be
-- updated for that row. To clear out the data in a specific cell, you need
-- to set the value as an empty string (\"\").
module Amazonka.HoneyCode.BatchUpsertTableRows
  ( -- * Creating a Request
    BatchUpsertTableRows (..),
    newBatchUpsertTableRows,

    -- * Request Lenses
    batchUpsertTableRows_clientRequestToken,
    batchUpsertTableRows_workbookId,
    batchUpsertTableRows_tableId,
    batchUpsertTableRows_rowsToUpsert,

    -- * Destructuring the Response
    BatchUpsertTableRowsResponse (..),
    newBatchUpsertTableRowsResponse,

    -- * Response Lenses
    batchUpsertTableRowsResponse_failedBatchItems,
    batchUpsertTableRowsResponse_httpStatus,
    batchUpsertTableRowsResponse_rows,
    batchUpsertTableRowsResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchUpsertTableRows' smart constructor.
data BatchUpsertTableRows = BatchUpsertTableRows'
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
    -- | The ID of the workbook where the rows are being upserted.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table where the rows are being upserted.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text,
    -- | The list of rows to upsert in the table. Each item in this list needs to
    -- have a batch item id to uniquely identify the element in the request, a
    -- filter expression to find the rows to update for that element and the
    -- cell values to set for each column in the upserted rows. You need to
    -- specify at least one item in this list.
    --
    -- Note that if one of the filter formulas in the request fails to evaluate
    -- because of an error or one of the column ids in any of the rows does not
    -- exist in the table, then the request fails and no updates are made to
    -- the table.
    rowsToUpsert :: [UpsertRowData]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpsertTableRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'batchUpsertTableRows_clientRequestToken' - The request token for performing the update action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
--
-- 'workbookId', 'batchUpsertTableRows_workbookId' - The ID of the workbook where the rows are being upserted.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'batchUpsertTableRows_tableId' - The ID of the table where the rows are being upserted.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'rowsToUpsert', 'batchUpsertTableRows_rowsToUpsert' - The list of rows to upsert in the table. Each item in this list needs to
-- have a batch item id to uniquely identify the element in the request, a
-- filter expression to find the rows to update for that element and the
-- cell values to set for each column in the upserted rows. You need to
-- specify at least one item in this list.
--
-- Note that if one of the filter formulas in the request fails to evaluate
-- because of an error or one of the column ids in any of the rows does not
-- exist in the table, then the request fails and no updates are made to
-- the table.
newBatchUpsertTableRows ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  BatchUpsertTableRows
newBatchUpsertTableRows pWorkbookId_ pTableId_ =
  BatchUpsertTableRows'
    { clientRequestToken =
        Prelude.Nothing,
      workbookId = pWorkbookId_,
      tableId = pTableId_,
      rowsToUpsert = Prelude.mempty
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
batchUpsertTableRows_clientRequestToken :: Lens.Lens' BatchUpsertTableRows (Prelude.Maybe Prelude.Text)
batchUpsertTableRows_clientRequestToken = Lens.lens (\BatchUpsertTableRows' {clientRequestToken} -> clientRequestToken) (\s@BatchUpsertTableRows' {} a -> s {clientRequestToken = a} :: BatchUpsertTableRows)

-- | The ID of the workbook where the rows are being upserted.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
batchUpsertTableRows_workbookId :: Lens.Lens' BatchUpsertTableRows Prelude.Text
batchUpsertTableRows_workbookId = Lens.lens (\BatchUpsertTableRows' {workbookId} -> workbookId) (\s@BatchUpsertTableRows' {} a -> s {workbookId = a} :: BatchUpsertTableRows)

-- | The ID of the table where the rows are being upserted.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
batchUpsertTableRows_tableId :: Lens.Lens' BatchUpsertTableRows Prelude.Text
batchUpsertTableRows_tableId = Lens.lens (\BatchUpsertTableRows' {tableId} -> tableId) (\s@BatchUpsertTableRows' {} a -> s {tableId = a} :: BatchUpsertTableRows)

-- | The list of rows to upsert in the table. Each item in this list needs to
-- have a batch item id to uniquely identify the element in the request, a
-- filter expression to find the rows to update for that element and the
-- cell values to set for each column in the upserted rows. You need to
-- specify at least one item in this list.
--
-- Note that if one of the filter formulas in the request fails to evaluate
-- because of an error or one of the column ids in any of the rows does not
-- exist in the table, then the request fails and no updates are made to
-- the table.
batchUpsertTableRows_rowsToUpsert :: Lens.Lens' BatchUpsertTableRows [UpsertRowData]
batchUpsertTableRows_rowsToUpsert = Lens.lens (\BatchUpsertTableRows' {rowsToUpsert} -> rowsToUpsert) (\s@BatchUpsertTableRows' {} a -> s {rowsToUpsert = a} :: BatchUpsertTableRows) Prelude.. Lens.coerced

instance Core.AWSRequest BatchUpsertTableRows where
  type
    AWSResponse BatchUpsertTableRows =
      BatchUpsertTableRowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchUpsertTableRowsResponse'
            Prelude.<$> ( x Data..?> "failedBatchItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "rows" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable BatchUpsertTableRows where
  hashWithSalt _salt BatchUpsertTableRows' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` rowsToUpsert

instance Prelude.NFData BatchUpsertTableRows where
  rnf BatchUpsertTableRows' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf rowsToUpsert

instance Data.ToHeaders BatchUpsertTableRows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchUpsertTableRows where
  toJSON BatchUpsertTableRows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("rowsToUpsert" Data..= rowsToUpsert)
          ]
      )

instance Data.ToPath BatchUpsertTableRows where
  toPath BatchUpsertTableRows' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/rows/batchupsert"
      ]

instance Data.ToQuery BatchUpsertTableRows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchUpsertTableRowsResponse' smart constructor.
data BatchUpsertTableRowsResponse = BatchUpsertTableRowsResponse'
  { -- | The list of batch items in the request that could not be updated or
    -- appended in the table. Each element in this list contains one item from
    -- the request that could not be updated in the table along with the reason
    -- why that item could not be updated or appended.
    failedBatchItems :: Prelude.Maybe [FailedBatchItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A map with the batch item id as the key and the result of the upsert
    -- operation as the value. The result of the upsert operation specifies
    -- whether existing rows were updated or a new row was appended, along with
    -- the list of row ids that were affected.
    rows :: Prelude.HashMap Prelude.Text UpsertRowsResult,
    -- | The updated workbook cursor after updating or appending rows in the
    -- table.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchUpsertTableRowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatchItems', 'batchUpsertTableRowsResponse_failedBatchItems' - The list of batch items in the request that could not be updated or
-- appended in the table. Each element in this list contains one item from
-- the request that could not be updated in the table along with the reason
-- why that item could not be updated or appended.
--
-- 'httpStatus', 'batchUpsertTableRowsResponse_httpStatus' - The response's http status code.
--
-- 'rows', 'batchUpsertTableRowsResponse_rows' - A map with the batch item id as the key and the result of the upsert
-- operation as the value. The result of the upsert operation specifies
-- whether existing rows were updated or a new row was appended, along with
-- the list of row ids that were affected.
--
-- 'workbookCursor', 'batchUpsertTableRowsResponse_workbookCursor' - The updated workbook cursor after updating or appending rows in the
-- table.
newBatchUpsertTableRowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  BatchUpsertTableRowsResponse
newBatchUpsertTableRowsResponse
  pHttpStatus_
  pWorkbookCursor_ =
    BatchUpsertTableRowsResponse'
      { failedBatchItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        rows = Prelude.mempty,
        workbookCursor = pWorkbookCursor_
      }

-- | The list of batch items in the request that could not be updated or
-- appended in the table. Each element in this list contains one item from
-- the request that could not be updated in the table along with the reason
-- why that item could not be updated or appended.
batchUpsertTableRowsResponse_failedBatchItems :: Lens.Lens' BatchUpsertTableRowsResponse (Prelude.Maybe [FailedBatchItem])
batchUpsertTableRowsResponse_failedBatchItems = Lens.lens (\BatchUpsertTableRowsResponse' {failedBatchItems} -> failedBatchItems) (\s@BatchUpsertTableRowsResponse' {} a -> s {failedBatchItems = a} :: BatchUpsertTableRowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchUpsertTableRowsResponse_httpStatus :: Lens.Lens' BatchUpsertTableRowsResponse Prelude.Int
batchUpsertTableRowsResponse_httpStatus = Lens.lens (\BatchUpsertTableRowsResponse' {httpStatus} -> httpStatus) (\s@BatchUpsertTableRowsResponse' {} a -> s {httpStatus = a} :: BatchUpsertTableRowsResponse)

-- | A map with the batch item id as the key and the result of the upsert
-- operation as the value. The result of the upsert operation specifies
-- whether existing rows were updated or a new row was appended, along with
-- the list of row ids that were affected.
batchUpsertTableRowsResponse_rows :: Lens.Lens' BatchUpsertTableRowsResponse (Prelude.HashMap Prelude.Text UpsertRowsResult)
batchUpsertTableRowsResponse_rows = Lens.lens (\BatchUpsertTableRowsResponse' {rows} -> rows) (\s@BatchUpsertTableRowsResponse' {} a -> s {rows = a} :: BatchUpsertTableRowsResponse) Prelude.. Lens.coerced

-- | The updated workbook cursor after updating or appending rows in the
-- table.
batchUpsertTableRowsResponse_workbookCursor :: Lens.Lens' BatchUpsertTableRowsResponse Prelude.Integer
batchUpsertTableRowsResponse_workbookCursor = Lens.lens (\BatchUpsertTableRowsResponse' {workbookCursor} -> workbookCursor) (\s@BatchUpsertTableRowsResponse' {} a -> s {workbookCursor = a} :: BatchUpsertTableRowsResponse)

instance Prelude.NFData BatchUpsertTableRowsResponse where
  rnf BatchUpsertTableRowsResponse' {..} =
    Prelude.rnf failedBatchItems
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf rows
      `Prelude.seq` Prelude.rnf workbookCursor
