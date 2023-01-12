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
-- Module      : Amazonka.Glue.BatchDeleteTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes multiple tables at once.
--
-- After completing this operation, you no longer have access to the table
-- versions and partitions that belong to the deleted table. Glue deletes
-- these \"orphaned\" resources asynchronously in a timely manner, at the
-- discretion of the service.
--
-- To ensure the immediate deletion of all related resources, before
-- calling @BatchDeleteTable@, use @DeleteTableVersion@ or
-- @BatchDeleteTableVersion@, and @DeletePartition@ or
-- @BatchDeletePartition@, to delete any resources that belong to the
-- table.
module Amazonka.Glue.BatchDeleteTable
  ( -- * Creating a Request
    BatchDeleteTable (..),
    newBatchDeleteTable,

    -- * Request Lenses
    batchDeleteTable_catalogId,
    batchDeleteTable_transactionId,
    batchDeleteTable_databaseName,
    batchDeleteTable_tablesToDelete,

    -- * Destructuring the Response
    BatchDeleteTableResponse (..),
    newBatchDeleteTableResponse,

    -- * Response Lenses
    batchDeleteTableResponse_errors,
    batchDeleteTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteTable' smart constructor.
data BatchDeleteTable = BatchDeleteTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The transaction ID at which to delete the table contents.
    transactionId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which the tables to delete reside.
    -- For Hive compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | A list of the table to delete.
    tablesToDelete :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeleteTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'transactionId', 'batchDeleteTable_transactionId' - The transaction ID at which to delete the table contents.
--
-- 'databaseName', 'batchDeleteTable_databaseName' - The name of the catalog database in which the tables to delete reside.
-- For Hive compatibility, this name is entirely lowercase.
--
-- 'tablesToDelete', 'batchDeleteTable_tablesToDelete' - A list of the table to delete.
newBatchDeleteTable ::
  -- | 'databaseName'
  Prelude.Text ->
  BatchDeleteTable
newBatchDeleteTable pDatabaseName_ =
  BatchDeleteTable'
    { catalogId = Prelude.Nothing,
      transactionId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tablesToDelete = Prelude.mempty
    }

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the Amazon Web Services account ID is used by default.
batchDeleteTable_catalogId :: Lens.Lens' BatchDeleteTable (Prelude.Maybe Prelude.Text)
batchDeleteTable_catalogId = Lens.lens (\BatchDeleteTable' {catalogId} -> catalogId) (\s@BatchDeleteTable' {} a -> s {catalogId = a} :: BatchDeleteTable)

-- | The transaction ID at which to delete the table contents.
batchDeleteTable_transactionId :: Lens.Lens' BatchDeleteTable (Prelude.Maybe Prelude.Text)
batchDeleteTable_transactionId = Lens.lens (\BatchDeleteTable' {transactionId} -> transactionId) (\s@BatchDeleteTable' {} a -> s {transactionId = a} :: BatchDeleteTable)

-- | The name of the catalog database in which the tables to delete reside.
-- For Hive compatibility, this name is entirely lowercase.
batchDeleteTable_databaseName :: Lens.Lens' BatchDeleteTable Prelude.Text
batchDeleteTable_databaseName = Lens.lens (\BatchDeleteTable' {databaseName} -> databaseName) (\s@BatchDeleteTable' {} a -> s {databaseName = a} :: BatchDeleteTable)

-- | A list of the table to delete.
batchDeleteTable_tablesToDelete :: Lens.Lens' BatchDeleteTable [Prelude.Text]
batchDeleteTable_tablesToDelete = Lens.lens (\BatchDeleteTable' {tablesToDelete} -> tablesToDelete) (\s@BatchDeleteTable' {} a -> s {tablesToDelete = a} :: BatchDeleteTable) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteTable where
  type
    AWSResponse BatchDeleteTable =
      BatchDeleteTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteTable where
  hashWithSalt _salt BatchDeleteTable' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` transactionId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tablesToDelete

instance Prelude.NFData BatchDeleteTable where
  rnf BatchDeleteTable' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf transactionId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tablesToDelete

instance Data.ToHeaders BatchDeleteTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.BatchDeleteTable" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteTable where
  toJSON BatchDeleteTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("TransactionId" Data..=) Prelude.<$> transactionId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just
              ("TablesToDelete" Data..= tablesToDelete)
          ]
      )

instance Data.ToPath BatchDeleteTable where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteTableResponse' smart constructor.
data BatchDeleteTableResponse = BatchDeleteTableResponse'
  { -- | A list of errors encountered in attempting to delete the specified
    -- tables.
    errors :: Prelude.Maybe [TableError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteTableResponse_errors' - A list of errors encountered in attempting to delete the specified
-- tables.
--
-- 'httpStatus', 'batchDeleteTableResponse_httpStatus' - The response's http status code.
newBatchDeleteTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteTableResponse
newBatchDeleteTableResponse pHttpStatus_ =
  BatchDeleteTableResponse'
    { errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors encountered in attempting to delete the specified
-- tables.
batchDeleteTableResponse_errors :: Lens.Lens' BatchDeleteTableResponse (Prelude.Maybe [TableError])
batchDeleteTableResponse_errors = Lens.lens (\BatchDeleteTableResponse' {errors} -> errors) (\s@BatchDeleteTableResponse' {} a -> s {errors = a} :: BatchDeleteTableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteTableResponse_httpStatus :: Lens.Lens' BatchDeleteTableResponse Prelude.Int
batchDeleteTableResponse_httpStatus = Lens.lens (\BatchDeleteTableResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteTableResponse' {} a -> s {httpStatus = a} :: BatchDeleteTableResponse)

instance Prelude.NFData BatchDeleteTableResponse where
  rnf BatchDeleteTableResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
