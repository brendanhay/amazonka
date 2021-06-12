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
-- Module      : Network.AWS.Glue.BatchGetPartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partitions in a batch request.
module Network.AWS.Glue.BatchGetPartition
  ( -- * Creating a Request
    BatchGetPartition (..),
    newBatchGetPartition,

    -- * Request Lenses
    batchGetPartition_catalogId,
    batchGetPartition_databaseName,
    batchGetPartition_tableName,
    batchGetPartition_partitionsToGet,

    -- * Destructuring the Response
    BatchGetPartitionResponse (..),
    newBatchGetPartitionResponse,

    -- * Response Lenses
    batchGetPartitionResponse_partitions,
    batchGetPartitionResponse_unprocessedKeys,
    batchGetPartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchGetPartition' smart constructor.
data BatchGetPartition = BatchGetPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Core.Text,
    -- | The name of the partitions\' table.
    tableName :: Core.Text,
    -- | A list of partition values identifying the partitions to retrieve.
    partitionsToGet :: [PartitionValueList]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchGetPartition_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'batchGetPartition_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'batchGetPartition_tableName' - The name of the partitions\' table.
--
-- 'partitionsToGet', 'batchGetPartition_partitionsToGet' - A list of partition values identifying the partitions to retrieve.
newBatchGetPartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  BatchGetPartition
newBatchGetPartition pDatabaseName_ pTableName_ =
  BatchGetPartition'
    { catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionsToGet = Core.mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
batchGetPartition_catalogId :: Lens.Lens' BatchGetPartition (Core.Maybe Core.Text)
batchGetPartition_catalogId = Lens.lens (\BatchGetPartition' {catalogId} -> catalogId) (\s@BatchGetPartition' {} a -> s {catalogId = a} :: BatchGetPartition)

-- | The name of the catalog database where the partitions reside.
batchGetPartition_databaseName :: Lens.Lens' BatchGetPartition Core.Text
batchGetPartition_databaseName = Lens.lens (\BatchGetPartition' {databaseName} -> databaseName) (\s@BatchGetPartition' {} a -> s {databaseName = a} :: BatchGetPartition)

-- | The name of the partitions\' table.
batchGetPartition_tableName :: Lens.Lens' BatchGetPartition Core.Text
batchGetPartition_tableName = Lens.lens (\BatchGetPartition' {tableName} -> tableName) (\s@BatchGetPartition' {} a -> s {tableName = a} :: BatchGetPartition)

-- | A list of partition values identifying the partitions to retrieve.
batchGetPartition_partitionsToGet :: Lens.Lens' BatchGetPartition [PartitionValueList]
batchGetPartition_partitionsToGet = Lens.lens (\BatchGetPartition' {partitionsToGet} -> partitionsToGet) (\s@BatchGetPartition' {} a -> s {partitionsToGet = a} :: BatchGetPartition) Core.. Lens._Coerce

instance Core.AWSRequest BatchGetPartition where
  type
    AWSResponse BatchGetPartition =
      BatchGetPartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetPartitionResponse'
            Core.<$> (x Core..?> "Partitions" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "UnprocessedKeys" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchGetPartition

instance Core.NFData BatchGetPartition

instance Core.ToHeaders BatchGetPartition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchGetPartition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchGetPartition where
  toJSON BatchGetPartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ("PartitionsToGet" Core..= partitionsToGet)
          ]
      )

instance Core.ToPath BatchGetPartition where
  toPath = Core.const "/"

instance Core.ToQuery BatchGetPartition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchGetPartitionResponse' smart constructor.
data BatchGetPartitionResponse = BatchGetPartitionResponse'
  { -- | A list of the requested partitions.
    partitions :: Core.Maybe [Partition],
    -- | A list of the partition values in the request for which partitions were
    -- not returned.
    unprocessedKeys :: Core.Maybe [PartitionValueList],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchGetPartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitions', 'batchGetPartitionResponse_partitions' - A list of the requested partitions.
--
-- 'unprocessedKeys', 'batchGetPartitionResponse_unprocessedKeys' - A list of the partition values in the request for which partitions were
-- not returned.
--
-- 'httpStatus', 'batchGetPartitionResponse_httpStatus' - The response's http status code.
newBatchGetPartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchGetPartitionResponse
newBatchGetPartitionResponse pHttpStatus_ =
  BatchGetPartitionResponse'
    { partitions =
        Core.Nothing,
      unprocessedKeys = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the requested partitions.
batchGetPartitionResponse_partitions :: Lens.Lens' BatchGetPartitionResponse (Core.Maybe [Partition])
batchGetPartitionResponse_partitions = Lens.lens (\BatchGetPartitionResponse' {partitions} -> partitions) (\s@BatchGetPartitionResponse' {} a -> s {partitions = a} :: BatchGetPartitionResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of the partition values in the request for which partitions were
-- not returned.
batchGetPartitionResponse_unprocessedKeys :: Lens.Lens' BatchGetPartitionResponse (Core.Maybe [PartitionValueList])
batchGetPartitionResponse_unprocessedKeys = Lens.lens (\BatchGetPartitionResponse' {unprocessedKeys} -> unprocessedKeys) (\s@BatchGetPartitionResponse' {} a -> s {unprocessedKeys = a} :: BatchGetPartitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchGetPartitionResponse_httpStatus :: Lens.Lens' BatchGetPartitionResponse Core.Int
batchGetPartitionResponse_httpStatus = Lens.lens (\BatchGetPartitionResponse' {httpStatus} -> httpStatus) (\s@BatchGetPartitionResponse' {} a -> s {httpStatus = a} :: BatchGetPartitionResponse)

instance Core.NFData BatchGetPartitionResponse
