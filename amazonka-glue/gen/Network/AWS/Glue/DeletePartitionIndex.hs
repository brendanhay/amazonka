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
-- Module      : Network.AWS.Glue.DeletePartitionIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition index from an existing table.
module Network.AWS.Glue.DeletePartitionIndex
  ( -- * Creating a Request
    DeletePartitionIndex (..),
    newDeletePartitionIndex,

    -- * Request Lenses
    deletePartitionIndex_catalogId,
    deletePartitionIndex_databaseName,
    deletePartitionIndex_tableName,
    deletePartitionIndex_indexName,

    -- * Destructuring the Response
    DeletePartitionIndexResponse (..),
    newDeletePartitionIndexResponse,

    -- * Response Lenses
    deletePartitionIndexResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePartitionIndex' smart constructor.
data DeletePartitionIndex = DeletePartitionIndex'
  { -- | The catalog ID where the table resides.
    catalogId :: Core.Maybe Core.Text,
    -- | Specifies the name of a database from which you want to delete a
    -- partition index.
    databaseName :: Core.Text,
    -- | Specifies the name of a table from which you want to delete a partition
    -- index.
    tableName :: Core.Text,
    -- | The name of the partition index to be deleted.
    indexName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePartitionIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deletePartitionIndex_catalogId' - The catalog ID where the table resides.
--
-- 'databaseName', 'deletePartitionIndex_databaseName' - Specifies the name of a database from which you want to delete a
-- partition index.
--
-- 'tableName', 'deletePartitionIndex_tableName' - Specifies the name of a table from which you want to delete a partition
-- index.
--
-- 'indexName', 'deletePartitionIndex_indexName' - The name of the partition index to be deleted.
newDeletePartitionIndex ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  -- | 'indexName'
  Core.Text ->
  DeletePartitionIndex
newDeletePartitionIndex
  pDatabaseName_
  pTableName_
  pIndexName_ =
    DeletePartitionIndex'
      { catalogId = Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        indexName = pIndexName_
      }

-- | The catalog ID where the table resides.
deletePartitionIndex_catalogId :: Lens.Lens' DeletePartitionIndex (Core.Maybe Core.Text)
deletePartitionIndex_catalogId = Lens.lens (\DeletePartitionIndex' {catalogId} -> catalogId) (\s@DeletePartitionIndex' {} a -> s {catalogId = a} :: DeletePartitionIndex)

-- | Specifies the name of a database from which you want to delete a
-- partition index.
deletePartitionIndex_databaseName :: Lens.Lens' DeletePartitionIndex Core.Text
deletePartitionIndex_databaseName = Lens.lens (\DeletePartitionIndex' {databaseName} -> databaseName) (\s@DeletePartitionIndex' {} a -> s {databaseName = a} :: DeletePartitionIndex)

-- | Specifies the name of a table from which you want to delete a partition
-- index.
deletePartitionIndex_tableName :: Lens.Lens' DeletePartitionIndex Core.Text
deletePartitionIndex_tableName = Lens.lens (\DeletePartitionIndex' {tableName} -> tableName) (\s@DeletePartitionIndex' {} a -> s {tableName = a} :: DeletePartitionIndex)

-- | The name of the partition index to be deleted.
deletePartitionIndex_indexName :: Lens.Lens' DeletePartitionIndex Core.Text
deletePartitionIndex_indexName = Lens.lens (\DeletePartitionIndex' {indexName} -> indexName) (\s@DeletePartitionIndex' {} a -> s {indexName = a} :: DeletePartitionIndex)

instance Core.AWSRequest DeletePartitionIndex where
  type
    AWSResponse DeletePartitionIndex =
      DeletePartitionIndexResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePartitionIndexResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePartitionIndex

instance Core.NFData DeletePartitionIndex

instance Core.ToHeaders DeletePartitionIndex where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.DeletePartitionIndex" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePartitionIndex where
  toJSON DeletePartitionIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("IndexName" Core..= indexName)
          ]
      )

instance Core.ToPath DeletePartitionIndex where
  toPath = Core.const "/"

instance Core.ToQuery DeletePartitionIndex where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePartitionIndexResponse' smart constructor.
data DeletePartitionIndexResponse = DeletePartitionIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePartitionIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePartitionIndexResponse_httpStatus' - The response's http status code.
newDeletePartitionIndexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeletePartitionIndexResponse
newDeletePartitionIndexResponse pHttpStatus_ =
  DeletePartitionIndexResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePartitionIndexResponse_httpStatus :: Lens.Lens' DeletePartitionIndexResponse Core.Int
deletePartitionIndexResponse_httpStatus = Lens.lens (\DeletePartitionIndexResponse' {httpStatus} -> httpStatus) (\s@DeletePartitionIndexResponse' {} a -> s {httpStatus = a} :: DeletePartitionIndexResponse)

instance Core.NFData DeletePartitionIndexResponse
