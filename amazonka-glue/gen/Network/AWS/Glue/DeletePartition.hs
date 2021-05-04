{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.DeletePartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified partition.
module Network.AWS.Glue.DeletePartition
  ( -- * Creating a Request
    DeletePartition (..),
    newDeletePartition,

    -- * Request Lenses
    deletePartition_catalogId,
    deletePartition_databaseName,
    deletePartition_tableName,
    deletePartition_partitionValues,

    -- * Destructuring the Response
    DeletePartitionResponse (..),
    newDeletePartitionResponse,

    -- * Response Lenses
    deletePartitionResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePartition' smart constructor.
data DeletePartition = DeletePartition'
  { -- | The ID of the Data Catalog where the partition to be deleted resides. If
    -- none is provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which the table in question resides.
    databaseName :: Prelude.Text,
    -- | The name of the table that contains the partition to be deleted.
    tableName :: Prelude.Text,
    -- | The values that define the partition.
    partitionValues :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deletePartition_catalogId' - The ID of the Data Catalog where the partition to be deleted resides. If
-- none is provided, the AWS account ID is used by default.
--
-- 'databaseName', 'deletePartition_databaseName' - The name of the catalog database in which the table in question resides.
--
-- 'tableName', 'deletePartition_tableName' - The name of the table that contains the partition to be deleted.
--
-- 'partitionValues', 'deletePartition_partitionValues' - The values that define the partition.
newDeletePartition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DeletePartition
newDeletePartition pDatabaseName_ pTableName_ =
  DeletePartition'
    { catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionValues = Prelude.mempty
    }

-- | The ID of the Data Catalog where the partition to be deleted resides. If
-- none is provided, the AWS account ID is used by default.
deletePartition_catalogId :: Lens.Lens' DeletePartition (Prelude.Maybe Prelude.Text)
deletePartition_catalogId = Lens.lens (\DeletePartition' {catalogId} -> catalogId) (\s@DeletePartition' {} a -> s {catalogId = a} :: DeletePartition)

-- | The name of the catalog database in which the table in question resides.
deletePartition_databaseName :: Lens.Lens' DeletePartition Prelude.Text
deletePartition_databaseName = Lens.lens (\DeletePartition' {databaseName} -> databaseName) (\s@DeletePartition' {} a -> s {databaseName = a} :: DeletePartition)

-- | The name of the table that contains the partition to be deleted.
deletePartition_tableName :: Lens.Lens' DeletePartition Prelude.Text
deletePartition_tableName = Lens.lens (\DeletePartition' {tableName} -> tableName) (\s@DeletePartition' {} a -> s {tableName = a} :: DeletePartition)

-- | The values that define the partition.
deletePartition_partitionValues :: Lens.Lens' DeletePartition [Prelude.Text]
deletePartition_partitionValues = Lens.lens (\DeletePartition' {partitionValues} -> partitionValues) (\s@DeletePartition' {} a -> s {partitionValues = a} :: DeletePartition) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest DeletePartition where
  type Rs DeletePartition = DeletePartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePartitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePartition

instance Prelude.NFData DeletePartition

instance Prelude.ToHeaders DeletePartition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.DeletePartition" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeletePartition where
  toJSON DeletePartition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just
              ("PartitionValues" Prelude..= partitionValues)
          ]
      )

instance Prelude.ToPath DeletePartition where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePartition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePartitionResponse' smart constructor.
data DeletePartitionResponse = DeletePartitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePartitionResponse_httpStatus' - The response's http status code.
newDeletePartitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeletePartitionResponse
newDeletePartitionResponse pHttpStatus_ =
  DeletePartitionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePartitionResponse_httpStatus :: Lens.Lens' DeletePartitionResponse Prelude.Int
deletePartitionResponse_httpStatus = Lens.lens (\DeletePartitionResponse' {httpStatus} -> httpStatus) (\s@DeletePartitionResponse' {} a -> s {httpStatus = a} :: DeletePartitionResponse)

instance Prelude.NFData DeletePartitionResponse
