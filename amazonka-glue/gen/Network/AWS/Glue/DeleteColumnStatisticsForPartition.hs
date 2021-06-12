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
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForPartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the partition column statistics of a column.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @DeletePartition@.
module Network.AWS.Glue.DeleteColumnStatisticsForPartition
  ( -- * Creating a Request
    DeleteColumnStatisticsForPartition (..),
    newDeleteColumnStatisticsForPartition,

    -- * Request Lenses
    deleteColumnStatisticsForPartition_catalogId,
    deleteColumnStatisticsForPartition_databaseName,
    deleteColumnStatisticsForPartition_tableName,
    deleteColumnStatisticsForPartition_partitionValues,
    deleteColumnStatisticsForPartition_columnName,

    -- * Destructuring the Response
    DeleteColumnStatisticsForPartitionResponse (..),
    newDeleteColumnStatisticsForPartitionResponse,

    -- * Response Lenses
    deleteColumnStatisticsForPartitionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteColumnStatisticsForPartition' smart constructor.
data DeleteColumnStatisticsForPartition = DeleteColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Core.Text,
    -- | The name of the partitions\' table.
    tableName :: Core.Text,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Core.Text],
    -- | Name of the column.
    columnName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteColumnStatisticsForPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteColumnStatisticsForPartition_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'deleteColumnStatisticsForPartition_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'deleteColumnStatisticsForPartition_tableName' - The name of the partitions\' table.
--
-- 'partitionValues', 'deleteColumnStatisticsForPartition_partitionValues' - A list of partition values identifying the partition.
--
-- 'columnName', 'deleteColumnStatisticsForPartition_columnName' - Name of the column.
newDeleteColumnStatisticsForPartition ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'columnName'
  Core.Text ->
  DeleteColumnStatisticsForPartition
newDeleteColumnStatisticsForPartition
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForPartition'
      { catalogId =
          Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionValues = Core.mempty,
        columnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
deleteColumnStatisticsForPartition_catalogId :: Lens.Lens' DeleteColumnStatisticsForPartition (Core.Maybe Core.Text)
deleteColumnStatisticsForPartition_catalogId = Lens.lens (\DeleteColumnStatisticsForPartition' {catalogId} -> catalogId) (\s@DeleteColumnStatisticsForPartition' {} a -> s {catalogId = a} :: DeleteColumnStatisticsForPartition)

-- | The name of the catalog database where the partitions reside.
deleteColumnStatisticsForPartition_databaseName :: Lens.Lens' DeleteColumnStatisticsForPartition Core.Text
deleteColumnStatisticsForPartition_databaseName = Lens.lens (\DeleteColumnStatisticsForPartition' {databaseName} -> databaseName) (\s@DeleteColumnStatisticsForPartition' {} a -> s {databaseName = a} :: DeleteColumnStatisticsForPartition)

-- | The name of the partitions\' table.
deleteColumnStatisticsForPartition_tableName :: Lens.Lens' DeleteColumnStatisticsForPartition Core.Text
deleteColumnStatisticsForPartition_tableName = Lens.lens (\DeleteColumnStatisticsForPartition' {tableName} -> tableName) (\s@DeleteColumnStatisticsForPartition' {} a -> s {tableName = a} :: DeleteColumnStatisticsForPartition)

-- | A list of partition values identifying the partition.
deleteColumnStatisticsForPartition_partitionValues :: Lens.Lens' DeleteColumnStatisticsForPartition [Core.Text]
deleteColumnStatisticsForPartition_partitionValues = Lens.lens (\DeleteColumnStatisticsForPartition' {partitionValues} -> partitionValues) (\s@DeleteColumnStatisticsForPartition' {} a -> s {partitionValues = a} :: DeleteColumnStatisticsForPartition) Core.. Lens._Coerce

-- | Name of the column.
deleteColumnStatisticsForPartition_columnName :: Lens.Lens' DeleteColumnStatisticsForPartition Core.Text
deleteColumnStatisticsForPartition_columnName = Lens.lens (\DeleteColumnStatisticsForPartition' {columnName} -> columnName) (\s@DeleteColumnStatisticsForPartition' {} a -> s {columnName = a} :: DeleteColumnStatisticsForPartition)

instance
  Core.AWSRequest
    DeleteColumnStatisticsForPartition
  where
  type
    AWSResponse DeleteColumnStatisticsForPartition =
      DeleteColumnStatisticsForPartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForPartitionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteColumnStatisticsForPartition

instance
  Core.NFData
    DeleteColumnStatisticsForPartition

instance
  Core.ToHeaders
    DeleteColumnStatisticsForPartition
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.DeleteColumnStatisticsForPartition" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeleteColumnStatisticsForPartition
  where
  toJSON DeleteColumnStatisticsForPartition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just
              ("PartitionValues" Core..= partitionValues),
            Core.Just ("ColumnName" Core..= columnName)
          ]
      )

instance
  Core.ToPath
    DeleteColumnStatisticsForPartition
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteColumnStatisticsForPartition
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteColumnStatisticsForPartitionResponse' smart constructor.
data DeleteColumnStatisticsForPartitionResponse = DeleteColumnStatisticsForPartitionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteColumnStatisticsForPartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteColumnStatisticsForPartitionResponse_httpStatus' - The response's http status code.
newDeleteColumnStatisticsForPartitionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteColumnStatisticsForPartitionResponse
newDeleteColumnStatisticsForPartitionResponse
  pHttpStatus_ =
    DeleteColumnStatisticsForPartitionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteColumnStatisticsForPartitionResponse_httpStatus :: Lens.Lens' DeleteColumnStatisticsForPartitionResponse Core.Int
deleteColumnStatisticsForPartitionResponse_httpStatus = Lens.lens (\DeleteColumnStatisticsForPartitionResponse' {httpStatus} -> httpStatus) (\s@DeleteColumnStatisticsForPartitionResponse' {} a -> s {httpStatus = a} :: DeleteColumnStatisticsForPartitionResponse)

instance
  Core.NFData
    DeleteColumnStatisticsForPartitionResponse
