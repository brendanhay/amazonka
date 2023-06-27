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
-- Module      : Amazonka.Glue.BatchDeletePartition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more partitions in a batch operation.
module Amazonka.Glue.BatchDeletePartition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeletePartition' smart constructor.
data BatchDeletePartition = BatchDeletePartition'
  { -- | The ID of the Data Catalog where the partition to be deleted resides. If
    -- none is provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which the table in question resides.
    databaseName :: Prelude.Text,
    -- | The name of the table that contains the partitions to be deleted.
    tableName :: Prelude.Text,
    -- | A list of @PartitionInput@ structures that define the partitions to be
    -- deleted.
    partitionsToDelete :: [PartitionValueList]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeletePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeletePartition_catalogId' - The ID of the Data Catalog where the partition to be deleted resides. If
-- none is provided, the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'batchDeletePartition_databaseName' - The name of the catalog database in which the table in question resides.
--
-- 'tableName', 'batchDeletePartition_tableName' - The name of the table that contains the partitions to be deleted.
--
-- 'partitionsToDelete', 'batchDeletePartition_partitionsToDelete' - A list of @PartitionInput@ structures that define the partitions to be
-- deleted.
newBatchDeletePartition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  BatchDeletePartition
newBatchDeletePartition pDatabaseName_ pTableName_ =
  BatchDeletePartition'
    { catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionsToDelete = Prelude.mempty
    }

-- | The ID of the Data Catalog where the partition to be deleted resides. If
-- none is provided, the Amazon Web Services account ID is used by default.
batchDeletePartition_catalogId :: Lens.Lens' BatchDeletePartition (Prelude.Maybe Prelude.Text)
batchDeletePartition_catalogId = Lens.lens (\BatchDeletePartition' {catalogId} -> catalogId) (\s@BatchDeletePartition' {} a -> s {catalogId = a} :: BatchDeletePartition)

-- | The name of the catalog database in which the table in question resides.
batchDeletePartition_databaseName :: Lens.Lens' BatchDeletePartition Prelude.Text
batchDeletePartition_databaseName = Lens.lens (\BatchDeletePartition' {databaseName} -> databaseName) (\s@BatchDeletePartition' {} a -> s {databaseName = a} :: BatchDeletePartition)

-- | The name of the table that contains the partitions to be deleted.
batchDeletePartition_tableName :: Lens.Lens' BatchDeletePartition Prelude.Text
batchDeletePartition_tableName = Lens.lens (\BatchDeletePartition' {tableName} -> tableName) (\s@BatchDeletePartition' {} a -> s {tableName = a} :: BatchDeletePartition)

-- | A list of @PartitionInput@ structures that define the partitions to be
-- deleted.
batchDeletePartition_partitionsToDelete :: Lens.Lens' BatchDeletePartition [PartitionValueList]
batchDeletePartition_partitionsToDelete = Lens.lens (\BatchDeletePartition' {partitionsToDelete} -> partitionsToDelete) (\s@BatchDeletePartition' {} a -> s {partitionsToDelete = a} :: BatchDeletePartition) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeletePartition where
  type
    AWSResponse BatchDeletePartition =
      BatchDeletePartitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeletePartitionResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeletePartition where
  hashWithSalt _salt BatchDeletePartition' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` partitionsToDelete

instance Prelude.NFData BatchDeletePartition where
  rnf BatchDeletePartition' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionsToDelete

instance Data.ToHeaders BatchDeletePartition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.BatchDeletePartition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeletePartition where
  toJSON BatchDeletePartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionsToDelete" Data..= partitionsToDelete)
          ]
      )

instance Data.ToPath BatchDeletePartition where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeletePartition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeletePartitionResponse' smart constructor.
data BatchDeletePartitionResponse = BatchDeletePartitionResponse'
  { -- | The errors encountered when trying to delete the requested partitions.
    errors :: Prelude.Maybe [PartitionError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchDeletePartitionResponse
newBatchDeletePartitionResponse pHttpStatus_ =
  BatchDeletePartitionResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors encountered when trying to delete the requested partitions.
batchDeletePartitionResponse_errors :: Lens.Lens' BatchDeletePartitionResponse (Prelude.Maybe [PartitionError])
batchDeletePartitionResponse_errors = Lens.lens (\BatchDeletePartitionResponse' {errors} -> errors) (\s@BatchDeletePartitionResponse' {} a -> s {errors = a} :: BatchDeletePartitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeletePartitionResponse_httpStatus :: Lens.Lens' BatchDeletePartitionResponse Prelude.Int
batchDeletePartitionResponse_httpStatus = Lens.lens (\BatchDeletePartitionResponse' {httpStatus} -> httpStatus) (\s@BatchDeletePartitionResponse' {} a -> s {httpStatus = a} :: BatchDeletePartitionResponse)

instance Prelude.NFData BatchDeletePartitionResponse where
  rnf BatchDeletePartitionResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
