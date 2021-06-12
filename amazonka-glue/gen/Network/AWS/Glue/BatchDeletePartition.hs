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
-- Module      : Network.AWS.Glue.BatchDeletePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more partitions in a batch operation.
module Network.AWS.Glue.BatchDeletePartition
  ( -- * Creating a Request
    BatchDeletePartition (..),
    newBatchDeletePartition,

    -- * Request Lenses
    batchDeletePartition_catalogId,
    batchDeletePartition_databaseName,
    batchDeletePartition_tableName,
    batchDeletePartition_partitionsToDelete,

    -- * Destructuring the Response
    BatchDeletePartitionResponse (..),
    newBatchDeletePartitionResponse,

    -- * Response Lenses
    batchDeletePartitionResponse_errors,
    batchDeletePartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeletePartition' smart constructor.
data BatchDeletePartition = BatchDeletePartition'
  { -- | The ID of the Data Catalog where the partition to be deleted resides. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database in which the table in question resides.
    databaseName :: Core.Text,
    -- | The name of the table that contains the partitions to be deleted.
    tableName :: Core.Text,
    -- | A list of @PartitionInput@ structures that define the partitions to be
    -- deleted.
    partitionsToDelete :: [PartitionValueList]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeletePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeletePartition_catalogId' - The ID of the Data Catalog where the partition to be deleted resides. If
-- none is provided, the AWS account ID is used by default.
--
-- 'databaseName', 'batchDeletePartition_databaseName' - The name of the catalog database in which the table in question resides.
--
-- 'tableName', 'batchDeletePartition_tableName' - The name of the table that contains the partitions to be deleted.
--
-- 'partitionsToDelete', 'batchDeletePartition_partitionsToDelete' - A list of @PartitionInput@ structures that define the partitions to be
-- deleted.
newBatchDeletePartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  BatchDeletePartition
newBatchDeletePartition pDatabaseName_ pTableName_ =
  BatchDeletePartition'
    { catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionsToDelete = Core.mempty
    }

-- | The ID of the Data Catalog where the partition to be deleted resides. If
-- none is provided, the AWS account ID is used by default.
batchDeletePartition_catalogId :: Lens.Lens' BatchDeletePartition (Core.Maybe Core.Text)
batchDeletePartition_catalogId = Lens.lens (\BatchDeletePartition' {catalogId} -> catalogId) (\s@BatchDeletePartition' {} a -> s {catalogId = a} :: BatchDeletePartition)

-- | The name of the catalog database in which the table in question resides.
batchDeletePartition_databaseName :: Lens.Lens' BatchDeletePartition Core.Text
batchDeletePartition_databaseName = Lens.lens (\BatchDeletePartition' {databaseName} -> databaseName) (\s@BatchDeletePartition' {} a -> s {databaseName = a} :: BatchDeletePartition)

-- | The name of the table that contains the partitions to be deleted.
batchDeletePartition_tableName :: Lens.Lens' BatchDeletePartition Core.Text
batchDeletePartition_tableName = Lens.lens (\BatchDeletePartition' {tableName} -> tableName) (\s@BatchDeletePartition' {} a -> s {tableName = a} :: BatchDeletePartition)

-- | A list of @PartitionInput@ structures that define the partitions to be
-- deleted.
batchDeletePartition_partitionsToDelete :: Lens.Lens' BatchDeletePartition [PartitionValueList]
batchDeletePartition_partitionsToDelete = Lens.lens (\BatchDeletePartition' {partitionsToDelete} -> partitionsToDelete) (\s@BatchDeletePartition' {} a -> s {partitionsToDelete = a} :: BatchDeletePartition) Core.. Lens._Coerce

instance Core.AWSRequest BatchDeletePartition where
  type
    AWSResponse BatchDeletePartition =
      BatchDeletePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeletePartitionResponse'
            Core.<$> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchDeletePartition

instance Core.NFData BatchDeletePartition

instance Core.ToHeaders BatchDeletePartition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchDeletePartition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDeletePartition where
  toJSON BatchDeletePartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ("PartitionsToDelete" Core..= partitionsToDelete)
          ]
      )

instance Core.ToPath BatchDeletePartition where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeletePartition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDeletePartitionResponse' smart constructor.
data BatchDeletePartitionResponse = BatchDeletePartitionResponse'
  { -- | The errors encountered when trying to delete the requested partitions.
    errors :: Core.Maybe [PartitionError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeletePartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeletePartitionResponse_errors' - The errors encountered when trying to delete the requested partitions.
--
-- 'httpStatus', 'batchDeletePartitionResponse_httpStatus' - The response's http status code.
newBatchDeletePartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchDeletePartitionResponse
newBatchDeletePartitionResponse pHttpStatus_ =
  BatchDeletePartitionResponse'
    { errors =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors encountered when trying to delete the requested partitions.
batchDeletePartitionResponse_errors :: Lens.Lens' BatchDeletePartitionResponse (Core.Maybe [PartitionError])
batchDeletePartitionResponse_errors = Lens.lens (\BatchDeletePartitionResponse' {errors} -> errors) (\s@BatchDeletePartitionResponse' {} a -> s {errors = a} :: BatchDeletePartitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeletePartitionResponse_httpStatus :: Lens.Lens' BatchDeletePartitionResponse Core.Int
batchDeletePartitionResponse_httpStatus = Lens.lens (\BatchDeletePartitionResponse' {httpStatus} -> httpStatus) (\s@BatchDeletePartitionResponse' {} a -> s {httpStatus = a} :: BatchDeletePartitionResponse)

instance Core.NFData BatchDeletePartitionResponse
