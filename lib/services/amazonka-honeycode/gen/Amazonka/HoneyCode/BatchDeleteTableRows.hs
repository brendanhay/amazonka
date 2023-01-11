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
-- Module      : Amazonka.HoneyCode.BatchDeleteTableRows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The BatchDeleteTableRows API allows you to delete one or more rows from
-- a table in a workbook. You need to specify the ids of the rows that you
-- want to delete from the table.
module Amazonka.HoneyCode.BatchDeleteTableRows
  ( -- * Creating a Request
    BatchDeleteTableRows (..),
    newBatchDeleteTableRows,

    -- * Request Lenses
    batchDeleteTableRows_clientRequestToken,
    batchDeleteTableRows_workbookId,
    batchDeleteTableRows_tableId,
    batchDeleteTableRows_rowIds,

    -- * Destructuring the Response
    BatchDeleteTableRowsResponse (..),
    newBatchDeleteTableRowsResponse,

    -- * Response Lenses
    batchDeleteTableRowsResponse_failedBatchItems,
    batchDeleteTableRowsResponse_httpStatus,
    batchDeleteTableRowsResponse_workbookCursor,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.HoneyCode.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteTableRows' smart constructor.
data BatchDeleteTableRows = BatchDeleteTableRows'
  { -- | The request token for performing the delete action. Request tokens help
    -- to identify duplicate requests. If a call times out or fails due to a
    -- transient error like a failed network connection, you can retry the call
    -- with the same request token. The service ensures that if the first call
    -- using that request token is successfully performed, the second call will
    -- not perform the action again.
    --
    -- Note that request tokens are valid only for a few minutes. You cannot
    -- use request tokens to dedupe requests spanning hours or days.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the workbook where the rows are being deleted.
    --
    -- If a workbook with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    workbookId :: Prelude.Text,
    -- | The ID of the table where the rows are being deleted.
    --
    -- If a table with the specified id could not be found, this API throws
    -- ResourceNotFoundException.
    tableId :: Prelude.Text,
    -- | The list of row ids to delete from the table. You need to specify at
    -- least one row id in this list.
    --
    -- Note that if one of the row ids provided in the request does not exist
    -- in the table, then the request fails and no rows are deleted from the
    -- table.
    rowIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteTableRows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'batchDeleteTableRows_clientRequestToken' - The request token for performing the delete action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
--
-- 'workbookId', 'batchDeleteTableRows_workbookId' - The ID of the workbook where the rows are being deleted.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'tableId', 'batchDeleteTableRows_tableId' - The ID of the table where the rows are being deleted.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
--
-- 'rowIds', 'batchDeleteTableRows_rowIds' - The list of row ids to delete from the table. You need to specify at
-- least one row id in this list.
--
-- Note that if one of the row ids provided in the request does not exist
-- in the table, then the request fails and no rows are deleted from the
-- table.
newBatchDeleteTableRows ::
  -- | 'workbookId'
  Prelude.Text ->
  -- | 'tableId'
  Prelude.Text ->
  -- | 'rowIds'
  Prelude.NonEmpty Prelude.Text ->
  BatchDeleteTableRows
newBatchDeleteTableRows
  pWorkbookId_
  pTableId_
  pRowIds_ =
    BatchDeleteTableRows'
      { clientRequestToken =
          Prelude.Nothing,
        workbookId = pWorkbookId_,
        tableId = pTableId_,
        rowIds = Lens.coerced Lens.# pRowIds_
      }

-- | The request token for performing the delete action. Request tokens help
-- to identify duplicate requests. If a call times out or fails due to a
-- transient error like a failed network connection, you can retry the call
-- with the same request token. The service ensures that if the first call
-- using that request token is successfully performed, the second call will
-- not perform the action again.
--
-- Note that request tokens are valid only for a few minutes. You cannot
-- use request tokens to dedupe requests spanning hours or days.
batchDeleteTableRows_clientRequestToken :: Lens.Lens' BatchDeleteTableRows (Prelude.Maybe Prelude.Text)
batchDeleteTableRows_clientRequestToken = Lens.lens (\BatchDeleteTableRows' {clientRequestToken} -> clientRequestToken) (\s@BatchDeleteTableRows' {} a -> s {clientRequestToken = a} :: BatchDeleteTableRows)

-- | The ID of the workbook where the rows are being deleted.
--
-- If a workbook with the specified id could not be found, this API throws
-- ResourceNotFoundException.
batchDeleteTableRows_workbookId :: Lens.Lens' BatchDeleteTableRows Prelude.Text
batchDeleteTableRows_workbookId = Lens.lens (\BatchDeleteTableRows' {workbookId} -> workbookId) (\s@BatchDeleteTableRows' {} a -> s {workbookId = a} :: BatchDeleteTableRows)

-- | The ID of the table where the rows are being deleted.
--
-- If a table with the specified id could not be found, this API throws
-- ResourceNotFoundException.
batchDeleteTableRows_tableId :: Lens.Lens' BatchDeleteTableRows Prelude.Text
batchDeleteTableRows_tableId = Lens.lens (\BatchDeleteTableRows' {tableId} -> tableId) (\s@BatchDeleteTableRows' {} a -> s {tableId = a} :: BatchDeleteTableRows)

-- | The list of row ids to delete from the table. You need to specify at
-- least one row id in this list.
--
-- Note that if one of the row ids provided in the request does not exist
-- in the table, then the request fails and no rows are deleted from the
-- table.
batchDeleteTableRows_rowIds :: Lens.Lens' BatchDeleteTableRows (Prelude.NonEmpty Prelude.Text)
batchDeleteTableRows_rowIds = Lens.lens (\BatchDeleteTableRows' {rowIds} -> rowIds) (\s@BatchDeleteTableRows' {} a -> s {rowIds = a} :: BatchDeleteTableRows) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteTableRows where
  type
    AWSResponse BatchDeleteTableRows =
      BatchDeleteTableRowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableRowsResponse'
            Prelude.<$> ( x Data..?> "failedBatchItems"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "workbookCursor")
      )

instance Prelude.Hashable BatchDeleteTableRows where
  hashWithSalt _salt BatchDeleteTableRows' {..} =
    _salt `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` workbookId
      `Prelude.hashWithSalt` tableId
      `Prelude.hashWithSalt` rowIds

instance Prelude.NFData BatchDeleteTableRows where
  rnf BatchDeleteTableRows' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf workbookId
      `Prelude.seq` Prelude.rnf tableId
      `Prelude.seq` Prelude.rnf rowIds

instance Data.ToHeaders BatchDeleteTableRows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteTableRows where
  toJSON BatchDeleteTableRows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("rowIds" Data..= rowIds)
          ]
      )

instance Data.ToPath BatchDeleteTableRows where
  toPath BatchDeleteTableRows' {..} =
    Prelude.mconcat
      [ "/workbooks/",
        Data.toBS workbookId,
        "/tables/",
        Data.toBS tableId,
        "/rows/batchdelete"
      ]

instance Data.ToQuery BatchDeleteTableRows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteTableRowsResponse' smart constructor.
data BatchDeleteTableRowsResponse = BatchDeleteTableRowsResponse'
  { -- | The list of row ids in the request that could not be deleted from the
    -- table. Each element in this list contains one row id from the request
    -- that could not be deleted along with the reason why that item could not
    -- be deleted.
    failedBatchItems :: Prelude.Maybe [FailedBatchItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The updated workbook cursor after deleting the rows from the table.
    workbookCursor :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteTableRowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedBatchItems', 'batchDeleteTableRowsResponse_failedBatchItems' - The list of row ids in the request that could not be deleted from the
-- table. Each element in this list contains one row id from the request
-- that could not be deleted along with the reason why that item could not
-- be deleted.
--
-- 'httpStatus', 'batchDeleteTableRowsResponse_httpStatus' - The response's http status code.
--
-- 'workbookCursor', 'batchDeleteTableRowsResponse_workbookCursor' - The updated workbook cursor after deleting the rows from the table.
newBatchDeleteTableRowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'workbookCursor'
  Prelude.Integer ->
  BatchDeleteTableRowsResponse
newBatchDeleteTableRowsResponse
  pHttpStatus_
  pWorkbookCursor_ =
    BatchDeleteTableRowsResponse'
      { failedBatchItems =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        workbookCursor = pWorkbookCursor_
      }

-- | The list of row ids in the request that could not be deleted from the
-- table. Each element in this list contains one row id from the request
-- that could not be deleted along with the reason why that item could not
-- be deleted.
batchDeleteTableRowsResponse_failedBatchItems :: Lens.Lens' BatchDeleteTableRowsResponse (Prelude.Maybe [FailedBatchItem])
batchDeleteTableRowsResponse_failedBatchItems = Lens.lens (\BatchDeleteTableRowsResponse' {failedBatchItems} -> failedBatchItems) (\s@BatchDeleteTableRowsResponse' {} a -> s {failedBatchItems = a} :: BatchDeleteTableRowsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteTableRowsResponse_httpStatus :: Lens.Lens' BatchDeleteTableRowsResponse Prelude.Int
batchDeleteTableRowsResponse_httpStatus = Lens.lens (\BatchDeleteTableRowsResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteTableRowsResponse' {} a -> s {httpStatus = a} :: BatchDeleteTableRowsResponse)

-- | The updated workbook cursor after deleting the rows from the table.
batchDeleteTableRowsResponse_workbookCursor :: Lens.Lens' BatchDeleteTableRowsResponse Prelude.Integer
batchDeleteTableRowsResponse_workbookCursor = Lens.lens (\BatchDeleteTableRowsResponse' {workbookCursor} -> workbookCursor) (\s@BatchDeleteTableRowsResponse' {} a -> s {workbookCursor = a} :: BatchDeleteTableRowsResponse)

instance Prelude.NFData BatchDeleteTableRowsResponse where
  rnf BatchDeleteTableRowsResponse' {..} =
    Prelude.rnf failedBatchItems
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf workbookCursor
