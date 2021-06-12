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
-- Module      : Network.AWS.Glue.BatchCreatePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more partitions in a batch operation.
module Network.AWS.Glue.BatchCreatePartition
  ( -- * Creating a Request
    BatchCreatePartition (..),
    newBatchCreatePartition,

    -- * Request Lenses
    batchCreatePartition_catalogId,
    batchCreatePartition_databaseName,
    batchCreatePartition_tableName,
    batchCreatePartition_partitionInputList,

    -- * Destructuring the Response
    BatchCreatePartitionResponse (..),
    newBatchCreatePartitionResponse,

    -- * Response Lenses
    batchCreatePartitionResponse_errors,
    batchCreatePartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchCreatePartition' smart constructor.
data BatchCreatePartition = BatchCreatePartition'
  { -- | The ID of the catalog in which the partition is to be created.
    -- Currently, this should be the AWS account ID.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the metadata database in which the partition is to be
    -- created.
    databaseName :: Core.Text,
    -- | The name of the metadata table in which the partition is to be created.
    tableName :: Core.Text,
    -- | A list of @PartitionInput@ structures that define the partitions to be
    -- created.
    partitionInputList :: [PartitionInput]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchCreatePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchCreatePartition_catalogId' - The ID of the catalog in which the partition is to be created.
-- Currently, this should be the AWS account ID.
--
-- 'databaseName', 'batchCreatePartition_databaseName' - The name of the metadata database in which the partition is to be
-- created.
--
-- 'tableName', 'batchCreatePartition_tableName' - The name of the metadata table in which the partition is to be created.
--
-- 'partitionInputList', 'batchCreatePartition_partitionInputList' - A list of @PartitionInput@ structures that define the partitions to be
-- created.
newBatchCreatePartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  BatchCreatePartition
newBatchCreatePartition pDatabaseName_ pTableName_ =
  BatchCreatePartition'
    { catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionInputList = Core.mempty
    }

-- | The ID of the catalog in which the partition is to be created.
-- Currently, this should be the AWS account ID.
batchCreatePartition_catalogId :: Lens.Lens' BatchCreatePartition (Core.Maybe Core.Text)
batchCreatePartition_catalogId = Lens.lens (\BatchCreatePartition' {catalogId} -> catalogId) (\s@BatchCreatePartition' {} a -> s {catalogId = a} :: BatchCreatePartition)

-- | The name of the metadata database in which the partition is to be
-- created.
batchCreatePartition_databaseName :: Lens.Lens' BatchCreatePartition Core.Text
batchCreatePartition_databaseName = Lens.lens (\BatchCreatePartition' {databaseName} -> databaseName) (\s@BatchCreatePartition' {} a -> s {databaseName = a} :: BatchCreatePartition)

-- | The name of the metadata table in which the partition is to be created.
batchCreatePartition_tableName :: Lens.Lens' BatchCreatePartition Core.Text
batchCreatePartition_tableName = Lens.lens (\BatchCreatePartition' {tableName} -> tableName) (\s@BatchCreatePartition' {} a -> s {tableName = a} :: BatchCreatePartition)

-- | A list of @PartitionInput@ structures that define the partitions to be
-- created.
batchCreatePartition_partitionInputList :: Lens.Lens' BatchCreatePartition [PartitionInput]
batchCreatePartition_partitionInputList = Lens.lens (\BatchCreatePartition' {partitionInputList} -> partitionInputList) (\s@BatchCreatePartition' {} a -> s {partitionInputList = a} :: BatchCreatePartition) Core.. Lens._Coerce

instance Core.AWSRequest BatchCreatePartition where
  type
    AWSResponse BatchCreatePartition =
      BatchCreatePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreatePartitionResponse'
            Core.<$> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchCreatePartition

instance Core.NFData BatchCreatePartition

instance Core.ToHeaders BatchCreatePartition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.BatchCreatePartition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchCreatePartition where
  toJSON BatchCreatePartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ("PartitionInputList" Core..= partitionInputList)
          ]
      )

instance Core.ToPath BatchCreatePartition where
  toPath = Core.const "/"

instance Core.ToQuery BatchCreatePartition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchCreatePartitionResponse' smart constructor.
data BatchCreatePartitionResponse = BatchCreatePartitionResponse'
  { -- | The errors encountered when trying to create the requested partitions.
    errors :: Core.Maybe [PartitionError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchCreatePartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchCreatePartitionResponse_errors' - The errors encountered when trying to create the requested partitions.
--
-- 'httpStatus', 'batchCreatePartitionResponse_httpStatus' - The response's http status code.
newBatchCreatePartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  BatchCreatePartitionResponse
newBatchCreatePartitionResponse pHttpStatus_ =
  BatchCreatePartitionResponse'
    { errors =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors encountered when trying to create the requested partitions.
batchCreatePartitionResponse_errors :: Lens.Lens' BatchCreatePartitionResponse (Core.Maybe [PartitionError])
batchCreatePartitionResponse_errors = Lens.lens (\BatchCreatePartitionResponse' {errors} -> errors) (\s@BatchCreatePartitionResponse' {} a -> s {errors = a} :: BatchCreatePartitionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchCreatePartitionResponse_httpStatus :: Lens.Lens' BatchCreatePartitionResponse Core.Int
batchCreatePartitionResponse_httpStatus = Lens.lens (\BatchCreatePartitionResponse' {httpStatus} -> httpStatus) (\s@BatchCreatePartitionResponse' {} a -> s {httpStatus = a} :: BatchCreatePartitionResponse)

instance Core.NFData BatchCreatePartitionResponse
