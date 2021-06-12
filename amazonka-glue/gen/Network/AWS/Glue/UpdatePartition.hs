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
-- Module      : Network.AWS.Glue.UpdatePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a partition.
module Network.AWS.Glue.UpdatePartition
  ( -- * Creating a Request
    UpdatePartition (..),
    newUpdatePartition,

    -- * Request Lenses
    updatePartition_catalogId,
    updatePartition_databaseName,
    updatePartition_tableName,
    updatePartition_partitionValueList,
    updatePartition_partitionInput,

    -- * Destructuring the Response
    UpdatePartitionResponse (..),
    newUpdatePartitionResponse,

    -- * Response Lenses
    updatePartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdatePartition' smart constructor.
data UpdatePartition = UpdatePartition'
  { -- | The ID of the Data Catalog where the partition to be updated resides. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database in which the table in question resides.
    databaseName :: Core.Text,
    -- | The name of the table in which the partition to be updated is located.
    tableName :: Core.Text,
    -- | List of partition key values that define the partition to update.
    partitionValueList :: [Core.Text],
    -- | The new partition object to update the partition to.
    --
    -- The @Values@ property can\'t be changed. If you want to change the
    -- partition key values for a partition, delete and recreate the partition.
    partitionInput :: PartitionInput
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updatePartition_catalogId' - The ID of the Data Catalog where the partition to be updated resides. If
-- none is provided, the AWS account ID is used by default.
--
-- 'databaseName', 'updatePartition_databaseName' - The name of the catalog database in which the table in question resides.
--
-- 'tableName', 'updatePartition_tableName' - The name of the table in which the partition to be updated is located.
--
-- 'partitionValueList', 'updatePartition_partitionValueList' - List of partition key values that define the partition to update.
--
-- 'partitionInput', 'updatePartition_partitionInput' - The new partition object to update the partition to.
--
-- The @Values@ property can\'t be changed. If you want to change the
-- partition key values for a partition, delete and recreate the partition.
newUpdatePartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'partitionInput'
  PartitionInput ->
  UpdatePartition
newUpdatePartition
  pDatabaseName_
  pTableName_
  pPartitionInput_ =
    UpdatePartition'
      { catalogId = Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionValueList = Core.mempty,
        partitionInput = pPartitionInput_
      }

-- | The ID of the Data Catalog where the partition to be updated resides. If
-- none is provided, the AWS account ID is used by default.
updatePartition_catalogId :: Lens.Lens' UpdatePartition (Core.Maybe Core.Text)
updatePartition_catalogId = Lens.lens (\UpdatePartition' {catalogId} -> catalogId) (\s@UpdatePartition' {} a -> s {catalogId = a} :: UpdatePartition)

-- | The name of the catalog database in which the table in question resides.
updatePartition_databaseName :: Lens.Lens' UpdatePartition Core.Text
updatePartition_databaseName = Lens.lens (\UpdatePartition' {databaseName} -> databaseName) (\s@UpdatePartition' {} a -> s {databaseName = a} :: UpdatePartition)

-- | The name of the table in which the partition to be updated is located.
updatePartition_tableName :: Lens.Lens' UpdatePartition Core.Text
updatePartition_tableName = Lens.lens (\UpdatePartition' {tableName} -> tableName) (\s@UpdatePartition' {} a -> s {tableName = a} :: UpdatePartition)

-- | List of partition key values that define the partition to update.
updatePartition_partitionValueList :: Lens.Lens' UpdatePartition [Core.Text]
updatePartition_partitionValueList = Lens.lens (\UpdatePartition' {partitionValueList} -> partitionValueList) (\s@UpdatePartition' {} a -> s {partitionValueList = a} :: UpdatePartition) Core.. Lens._Coerce

-- | The new partition object to update the partition to.
--
-- The @Values@ property can\'t be changed. If you want to change the
-- partition key values for a partition, delete and recreate the partition.
updatePartition_partitionInput :: Lens.Lens' UpdatePartition PartitionInput
updatePartition_partitionInput = Lens.lens (\UpdatePartition' {partitionInput} -> partitionInput) (\s@UpdatePartition' {} a -> s {partitionInput = a} :: UpdatePartition)

instance Core.AWSRequest UpdatePartition where
  type
    AWSResponse UpdatePartition =
      UpdatePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePartitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdatePartition

instance Core.NFData UpdatePartition

instance Core.ToHeaders UpdatePartition where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdatePartition" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdatePartition where
  toJSON UpdatePartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ("PartitionValueList" Core..= partitionValueList),
            Core.Just ("PartitionInput" Core..= partitionInput)
          ]
      )

instance Core.ToPath UpdatePartition where
  toPath = Core.const "/"

instance Core.ToQuery UpdatePartition where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdatePartitionResponse' smart constructor.
data UpdatePartitionResponse = UpdatePartitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdatePartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePartitionResponse_httpStatus' - The response's http status code.
newUpdatePartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdatePartitionResponse
newUpdatePartitionResponse pHttpStatus_ =
  UpdatePartitionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updatePartitionResponse_httpStatus :: Lens.Lens' UpdatePartitionResponse Core.Int
updatePartitionResponse_httpStatus = Lens.lens (\UpdatePartitionResponse' {httpStatus} -> httpStatus) (\s@UpdatePartitionResponse' {} a -> s {httpStatus = a} :: UpdatePartitionResponse)

instance Core.NFData UpdatePartitionResponse
