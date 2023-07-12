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
-- Module      : Amazonka.Glue.BatchGetPartition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partitions in a batch request.
module Amazonka.Glue.BatchGetPartition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetPartition' smart constructor.
data BatchGetPartition = BatchGetPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | A list of partition values identifying the partitions to retrieve.
    partitionsToGet :: [PartitionValueList]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchGetPartition_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'batchGetPartition_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'batchGetPartition_tableName' - The name of the partitions\' table.
--
-- 'partitionsToGet', 'batchGetPartition_partitionsToGet' - A list of partition values identifying the partitions to retrieve.
newBatchGetPartition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  BatchGetPartition
newBatchGetPartition pDatabaseName_ pTableName_ =
  BatchGetPartition'
    { catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      partitionsToGet = Prelude.mempty
    }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
batchGetPartition_catalogId :: Lens.Lens' BatchGetPartition (Prelude.Maybe Prelude.Text)
batchGetPartition_catalogId = Lens.lens (\BatchGetPartition' {catalogId} -> catalogId) (\s@BatchGetPartition' {} a -> s {catalogId = a} :: BatchGetPartition)

-- | The name of the catalog database where the partitions reside.
batchGetPartition_databaseName :: Lens.Lens' BatchGetPartition Prelude.Text
batchGetPartition_databaseName = Lens.lens (\BatchGetPartition' {databaseName} -> databaseName) (\s@BatchGetPartition' {} a -> s {databaseName = a} :: BatchGetPartition)

-- | The name of the partitions\' table.
batchGetPartition_tableName :: Lens.Lens' BatchGetPartition Prelude.Text
batchGetPartition_tableName = Lens.lens (\BatchGetPartition' {tableName} -> tableName) (\s@BatchGetPartition' {} a -> s {tableName = a} :: BatchGetPartition)

-- | A list of partition values identifying the partitions to retrieve.
batchGetPartition_partitionsToGet :: Lens.Lens' BatchGetPartition [PartitionValueList]
batchGetPartition_partitionsToGet = Lens.lens (\BatchGetPartition' {partitionsToGet} -> partitionsToGet) (\s@BatchGetPartition' {} a -> s {partitionsToGet = a} :: BatchGetPartition) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetPartition where
  type
    AWSResponse BatchGetPartition =
      BatchGetPartitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetPartitionResponse'
            Prelude.<$> (x Data..?> "Partitions" Core..!@ Prelude.mempty)
            Prelude.<*> ( x
                            Data..?> "UnprocessedKeys"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetPartition where
  hashWithSalt _salt BatchGetPartition' {..} =
    _salt
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` partitionsToGet

instance Prelude.NFData BatchGetPartition where
  rnf BatchGetPartition' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionsToGet

instance Data.ToHeaders BatchGetPartition where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.BatchGetPartition" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchGetPartition where
  toJSON BatchGetPartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionsToGet" Data..= partitionsToGet)
          ]
      )

instance Data.ToPath BatchGetPartition where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchGetPartition where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetPartitionResponse' smart constructor.
data BatchGetPartitionResponse = BatchGetPartitionResponse'
  { -- | A list of the requested partitions.
    partitions :: Prelude.Maybe [Partition],
    -- | A list of the partition values in the request for which partitions were
    -- not returned.
    unprocessedKeys :: Prelude.Maybe [PartitionValueList],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  BatchGetPartitionResponse
newBatchGetPartitionResponse pHttpStatus_ =
  BatchGetPartitionResponse'
    { partitions =
        Prelude.Nothing,
      unprocessedKeys = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of the requested partitions.
batchGetPartitionResponse_partitions :: Lens.Lens' BatchGetPartitionResponse (Prelude.Maybe [Partition])
batchGetPartitionResponse_partitions = Lens.lens (\BatchGetPartitionResponse' {partitions} -> partitions) (\s@BatchGetPartitionResponse' {} a -> s {partitions = a} :: BatchGetPartitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the partition values in the request for which partitions were
-- not returned.
batchGetPartitionResponse_unprocessedKeys :: Lens.Lens' BatchGetPartitionResponse (Prelude.Maybe [PartitionValueList])
batchGetPartitionResponse_unprocessedKeys = Lens.lens (\BatchGetPartitionResponse' {unprocessedKeys} -> unprocessedKeys) (\s@BatchGetPartitionResponse' {} a -> s {unprocessedKeys = a} :: BatchGetPartitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetPartitionResponse_httpStatus :: Lens.Lens' BatchGetPartitionResponse Prelude.Int
batchGetPartitionResponse_httpStatus = Lens.lens (\BatchGetPartitionResponse' {httpStatus} -> httpStatus) (\s@BatchGetPartitionResponse' {} a -> s {httpStatus = a} :: BatchGetPartitionResponse)

instance Prelude.NFData BatchGetPartitionResponse where
  rnf BatchGetPartitionResponse' {..} =
    Prelude.rnf partitions
      `Prelude.seq` Prelude.rnf unprocessedKeys
      `Prelude.seq` Prelude.rnf httpStatus
