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
-- Module      : Network.AWS.Glue.BatchDeleteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes multiple tables at once.
--
-- After completing this operation, you no longer have access to the table
-- versions and partitions that belong to the deleted table. AWS Glue
-- deletes these \"orphaned\" resources asynchronously in a timely manner,
-- at the discretion of the service.
--
-- To ensure the immediate deletion of all related resources, before
-- calling @BatchDeleteTable@, use @DeleteTableVersion@ or
-- @BatchDeleteTableVersion@, and @DeletePartition@ or
-- @BatchDeletePartition@, to delete any resources that belong to the
-- table.
module Network.AWS.Glue.BatchDeleteTable
  ( -- * Creating a Request
    BatchDeleteTable (..),
    newBatchDeleteTable,

    -- * Request Lenses
    batchDeleteTable_catalogId,
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeleteTable' smart constructor.
data BatchDeleteTable = BatchDeleteTable'
  { -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database in which the tables to delete reside.
    -- For Hive compatibility, this name is entirely lowercase.
    databaseName :: Core.Text,
    -- | A list of the table to delete.
    tablesToDelete :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeleteTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
--
-- 'databaseName', 'batchDeleteTable_databaseName' - The name of the catalog database in which the tables to delete reside.
-- For Hive compatibility, this name is entirely lowercase.
--
-- 'tablesToDelete', 'batchDeleteTable_tablesToDelete' - A list of the table to delete.
newBatchDeleteTable ::
  -- | 'databaseName'
  Core.Text ->
  BatchDeleteTable
newBatchDeleteTable pDatabaseName_ =
  BatchDeleteTable'
    { catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tablesToDelete = Core.mempty
    }

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the AWS account ID is used by default.
batchDeleteTable_catalogId :: Lens.Lens' BatchDeleteTable (Core.Maybe Core.Text)
batchDeleteTable_catalogId = Lens.lens (\BatchDeleteTable' {catalogId} -> catalogId) (\s@BatchDeleteTable' {} a -> s {catalogId = a} :: BatchDeleteTable)

-- | The name of the catalog database in which the tables to delete reside.
-- For Hive compatibility, this name is entirely lowercase.
batchDeleteTable_databaseName :: Lens.Lens' BatchDeleteTable Core.Text
batchDeleteTable_databaseName = Lens.lens (\BatchDeleteTable' {databaseName} -> databaseName) (\s@BatchDeleteTable' {} a -> s {databaseName = a} :: BatchDeleteTable)

-- | A list of the table to delete.
batchDeleteTable_tablesToDelete :: Lens.Lens' BatchDeleteTable [Core.Text]
batchDeleteTable_tablesToDelete = Lens.lens (\BatchDeleteTable' {tablesToDelete} -> tablesToDelete) (\s@BatchDeleteTable' {} a -> s {tablesToDelete = a} :: BatchDeleteTable) Core.. Lens._Coerce

instance Core.AWSRequest BatchDeleteTable where
  type
    AWSResponse BatchDeleteTable =
      BatchDeleteTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableResponse'
            Core.<$> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchDeleteTable

instance Core.NFData BatchDeleteTable

instance Core.ToHeaders BatchDeleteTable where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchDeleteTable" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDeleteTable where
  toJSON BatchDeleteTable' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TablesToDelete" Core..= tablesToDelete)
          ]
      )

instance Core.ToPath BatchDeleteTable where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeleteTable where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDeleteTableResponse' smart constructor.
data BatchDeleteTableResponse = BatchDeleteTableResponse'
  { -- | A list of errors encountered in attempting to delete the specified
    -- tables.
    errors :: Core.Maybe [TableError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  BatchDeleteTableResponse
newBatchDeleteTableResponse pHttpStatus_ =
  BatchDeleteTableResponse'
    { errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors encountered in attempting to delete the specified
-- tables.
batchDeleteTableResponse_errors :: Lens.Lens' BatchDeleteTableResponse (Core.Maybe [TableError])
batchDeleteTableResponse_errors = Lens.lens (\BatchDeleteTableResponse' {errors} -> errors) (\s@BatchDeleteTableResponse' {} a -> s {errors = a} :: BatchDeleteTableResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteTableResponse_httpStatus :: Lens.Lens' BatchDeleteTableResponse Core.Int
batchDeleteTableResponse_httpStatus = Lens.lens (\BatchDeleteTableResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteTableResponse' {} a -> s {httpStatus = a} :: BatchDeleteTableResponse)

instance Core.NFData BatchDeleteTableResponse
