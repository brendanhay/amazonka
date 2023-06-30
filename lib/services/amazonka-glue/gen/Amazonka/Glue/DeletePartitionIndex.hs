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
-- Module      : Amazonka.Glue.DeletePartitionIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition index from an existing table.
module Amazonka.Glue.DeletePartitionIndex
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePartitionIndex' smart constructor.
data DeletePartitionIndex = DeletePartitionIndex'
  { -- | The catalog ID where the table resides.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the name of a database from which you want to delete a
    -- partition index.
    databaseName :: Prelude.Text,
    -- | Specifies the name of a table from which you want to delete a partition
    -- index.
    tableName :: Prelude.Text,
    -- | The name of the partition index to be deleted.
    indexName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'indexName'
  Prelude.Text ->
  DeletePartitionIndex
newDeletePartitionIndex
  pDatabaseName_
  pTableName_
  pIndexName_ =
    DeletePartitionIndex'
      { catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        indexName = pIndexName_
      }

-- | The catalog ID where the table resides.
deletePartitionIndex_catalogId :: Lens.Lens' DeletePartitionIndex (Prelude.Maybe Prelude.Text)
deletePartitionIndex_catalogId = Lens.lens (\DeletePartitionIndex' {catalogId} -> catalogId) (\s@DeletePartitionIndex' {} a -> s {catalogId = a} :: DeletePartitionIndex)

-- | Specifies the name of a database from which you want to delete a
-- partition index.
deletePartitionIndex_databaseName :: Lens.Lens' DeletePartitionIndex Prelude.Text
deletePartitionIndex_databaseName = Lens.lens (\DeletePartitionIndex' {databaseName} -> databaseName) (\s@DeletePartitionIndex' {} a -> s {databaseName = a} :: DeletePartitionIndex)

-- | Specifies the name of a table from which you want to delete a partition
-- index.
deletePartitionIndex_tableName :: Lens.Lens' DeletePartitionIndex Prelude.Text
deletePartitionIndex_tableName = Lens.lens (\DeletePartitionIndex' {tableName} -> tableName) (\s@DeletePartitionIndex' {} a -> s {tableName = a} :: DeletePartitionIndex)

-- | The name of the partition index to be deleted.
deletePartitionIndex_indexName :: Lens.Lens' DeletePartitionIndex Prelude.Text
deletePartitionIndex_indexName = Lens.lens (\DeletePartitionIndex' {indexName} -> indexName) (\s@DeletePartitionIndex' {} a -> s {indexName = a} :: DeletePartitionIndex)

instance Core.AWSRequest DeletePartitionIndex where
  type
    AWSResponse DeletePartitionIndex =
      DeletePartitionIndexResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePartitionIndexResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePartitionIndex where
  hashWithSalt _salt DeletePartitionIndex' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` indexName

instance Prelude.NFData DeletePartitionIndex where
  rnf DeletePartitionIndex' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf indexName

instance Data.ToHeaders DeletePartitionIndex where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.DeletePartitionIndex" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeletePartitionIndex where
  toJSON DeletePartitionIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("IndexName" Data..= indexName)
          ]
      )

instance Data.ToPath DeletePartitionIndex where
  toPath = Prelude.const "/"

instance Data.ToQuery DeletePartitionIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePartitionIndexResponse' smart constructor.
data DeletePartitionIndexResponse = DeletePartitionIndexResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeletePartitionIndexResponse
newDeletePartitionIndexResponse pHttpStatus_ =
  DeletePartitionIndexResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deletePartitionIndexResponse_httpStatus :: Lens.Lens' DeletePartitionIndexResponse Prelude.Int
deletePartitionIndexResponse_httpStatus = Lens.lens (\DeletePartitionIndexResponse' {httpStatus} -> httpStatus) (\s@DeletePartitionIndexResponse' {} a -> s {httpStatus = a} :: DeletePartitionIndexResponse)

instance Prelude.NFData DeletePartitionIndexResponse where
  rnf DeletePartitionIndexResponse' {..} =
    Prelude.rnf httpStatus
