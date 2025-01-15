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
-- Module      : Amazonka.Glue.BatchCreatePartition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates one or more partitions in a batch operation.
module Amazonka.Glue.BatchCreatePartition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchCreatePartition' smart constructor.
data BatchCreatePartition = BatchCreatePartition'
  { -- | The ID of the catalog in which the partition is to be created.
    -- Currently, this should be the Amazon Web Services account ID.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the metadata database in which the partition is to be
    -- created.
    databaseName :: Prelude.Text,
    -- | The name of the metadata table in which the partition is to be created.
    tableName :: Prelude.Text,
    -- | A list of @PartitionInput@ structures that define the partitions to be
    -- created.
    partitionInputList :: [PartitionInput]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchCreatePartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchCreatePartition_catalogId' - The ID of the catalog in which the partition is to be created.
-- Currently, this should be the Amazon Web Services account ID.
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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  BatchCreatePartition
newBatchCreatePartition pDatabaseName_ pTableName_ =
  BatchCreatePartition'
    { catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionInputList = Prelude.mempty
    }

-- | The ID of the catalog in which the partition is to be created.
-- Currently, this should be the Amazon Web Services account ID.
batchCreatePartition_catalogId :: Lens.Lens' BatchCreatePartition (Prelude.Maybe Prelude.Text)
batchCreatePartition_catalogId = Lens.lens (\BatchCreatePartition' {catalogId} -> catalogId) (\s@BatchCreatePartition' {} a -> s {catalogId = a} :: BatchCreatePartition)

-- | The name of the metadata database in which the partition is to be
-- created.
batchCreatePartition_databaseName :: Lens.Lens' BatchCreatePartition Prelude.Text
batchCreatePartition_databaseName = Lens.lens (\BatchCreatePartition' {databaseName} -> databaseName) (\s@BatchCreatePartition' {} a -> s {databaseName = a} :: BatchCreatePartition)

-- | The name of the metadata table in which the partition is to be created.
batchCreatePartition_tableName :: Lens.Lens' BatchCreatePartition Prelude.Text
batchCreatePartition_tableName = Lens.lens (\BatchCreatePartition' {tableName} -> tableName) (\s@BatchCreatePartition' {} a -> s {tableName = a} :: BatchCreatePartition)

-- | A list of @PartitionInput@ structures that define the partitions to be
-- created.
batchCreatePartition_partitionInputList :: Lens.Lens' BatchCreatePartition [PartitionInput]
batchCreatePartition_partitionInputList = Lens.lens (\BatchCreatePartition' {partitionInputList} -> partitionInputList) (\s@BatchCreatePartition' {} a -> s {partitionInputList = a} :: BatchCreatePartition) Prelude.. Lens.coerced

instance Core.AWSRequest BatchCreatePartition where
  type
    AWSResponse BatchCreatePartition =
      BatchCreatePartitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchCreatePartitionResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchCreatePartition where
  hashWithSalt _salt BatchCreatePartition' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` partitionInputList

instance Prelude.NFData BatchCreatePartition where
  rnf BatchCreatePartition' {..} =
    Prelude.rnf catalogId `Prelude.seq`
      Prelude.rnf databaseName `Prelude.seq`
        Prelude.rnf tableName `Prelude.seq`
          Prelude.rnf partitionInputList

instance Data.ToHeaders BatchCreatePartition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.BatchCreatePartition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchCreatePartition where
  toJSON BatchCreatePartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionInputList" Data..= partitionInputList)
          ]
      )

instance Data.ToPath BatchCreatePartition where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchCreatePartition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchCreatePartitionResponse' smart constructor.
data BatchCreatePartitionResponse = BatchCreatePartitionResponse'
  { -- | The errors encountered when trying to create the requested partitions.
    errors :: Prelude.Maybe [PartitionError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchCreatePartitionResponse
newBatchCreatePartitionResponse pHttpStatus_ =
  BatchCreatePartitionResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The errors encountered when trying to create the requested partitions.
batchCreatePartitionResponse_errors :: Lens.Lens' BatchCreatePartitionResponse (Prelude.Maybe [PartitionError])
batchCreatePartitionResponse_errors = Lens.lens (\BatchCreatePartitionResponse' {errors} -> errors) (\s@BatchCreatePartitionResponse' {} a -> s {errors = a} :: BatchCreatePartitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchCreatePartitionResponse_httpStatus :: Lens.Lens' BatchCreatePartitionResponse Prelude.Int
batchCreatePartitionResponse_httpStatus = Lens.lens (\BatchCreatePartitionResponse' {httpStatus} -> httpStatus) (\s@BatchCreatePartitionResponse' {} a -> s {httpStatus = a} :: BatchCreatePartitionResponse)

instance Prelude.NFData BatchCreatePartitionResponse where
  rnf BatchCreatePartitionResponse' {..} =
    Prelude.rnf errors `Prelude.seq`
      Prelude.rnf httpStatus
