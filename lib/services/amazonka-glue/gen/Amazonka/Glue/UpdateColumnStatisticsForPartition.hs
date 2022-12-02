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
-- Module      : Amazonka.Glue.UpdateColumnStatisticsForPartition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates partition statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @UpdatePartition@.
module Amazonka.Glue.UpdateColumnStatisticsForPartition
  ( -- * Creating a Request
    UpdateColumnStatisticsForPartition (..),
    newUpdateColumnStatisticsForPartition,

    -- * Request Lenses
    updateColumnStatisticsForPartition_catalogId,
    updateColumnStatisticsForPartition_databaseName,
    updateColumnStatisticsForPartition_tableName,
    updateColumnStatisticsForPartition_partitionValues,
    updateColumnStatisticsForPartition_columnStatisticsList,

    -- * Destructuring the Response
    UpdateColumnStatisticsForPartitionResponse (..),
    newUpdateColumnStatisticsForPartitionResponse,

    -- * Response Lenses
    updateColumnStatisticsForPartitionResponse_errors,
    updateColumnStatisticsForPartitionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateColumnStatisticsForPartition' smart constructor.
data UpdateColumnStatisticsForPartition = UpdateColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Prelude.Text],
    -- | A list of the column statistics.
    columnStatisticsList :: [ColumnStatistics]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateColumnStatisticsForPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateColumnStatisticsForPartition_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'updateColumnStatisticsForPartition_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'updateColumnStatisticsForPartition_tableName' - The name of the partitions\' table.
--
-- 'partitionValues', 'updateColumnStatisticsForPartition_partitionValues' - A list of partition values identifying the partition.
--
-- 'columnStatisticsList', 'updateColumnStatisticsForPartition_columnStatisticsList' - A list of the column statistics.
newUpdateColumnStatisticsForPartition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  UpdateColumnStatisticsForPartition
newUpdateColumnStatisticsForPartition
  pDatabaseName_
  pTableName_ =
    UpdateColumnStatisticsForPartition'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionValues = Prelude.mempty,
        columnStatisticsList = Prelude.mempty
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
updateColumnStatisticsForPartition_catalogId :: Lens.Lens' UpdateColumnStatisticsForPartition (Prelude.Maybe Prelude.Text)
updateColumnStatisticsForPartition_catalogId = Lens.lens (\UpdateColumnStatisticsForPartition' {catalogId} -> catalogId) (\s@UpdateColumnStatisticsForPartition' {} a -> s {catalogId = a} :: UpdateColumnStatisticsForPartition)

-- | The name of the catalog database where the partitions reside.
updateColumnStatisticsForPartition_databaseName :: Lens.Lens' UpdateColumnStatisticsForPartition Prelude.Text
updateColumnStatisticsForPartition_databaseName = Lens.lens (\UpdateColumnStatisticsForPartition' {databaseName} -> databaseName) (\s@UpdateColumnStatisticsForPartition' {} a -> s {databaseName = a} :: UpdateColumnStatisticsForPartition)

-- | The name of the partitions\' table.
updateColumnStatisticsForPartition_tableName :: Lens.Lens' UpdateColumnStatisticsForPartition Prelude.Text
updateColumnStatisticsForPartition_tableName = Lens.lens (\UpdateColumnStatisticsForPartition' {tableName} -> tableName) (\s@UpdateColumnStatisticsForPartition' {} a -> s {tableName = a} :: UpdateColumnStatisticsForPartition)

-- | A list of partition values identifying the partition.
updateColumnStatisticsForPartition_partitionValues :: Lens.Lens' UpdateColumnStatisticsForPartition [Prelude.Text]
updateColumnStatisticsForPartition_partitionValues = Lens.lens (\UpdateColumnStatisticsForPartition' {partitionValues} -> partitionValues) (\s@UpdateColumnStatisticsForPartition' {} a -> s {partitionValues = a} :: UpdateColumnStatisticsForPartition) Prelude.. Lens.coerced

-- | A list of the column statistics.
updateColumnStatisticsForPartition_columnStatisticsList :: Lens.Lens' UpdateColumnStatisticsForPartition [ColumnStatistics]
updateColumnStatisticsForPartition_columnStatisticsList = Lens.lens (\UpdateColumnStatisticsForPartition' {columnStatisticsList} -> columnStatisticsList) (\s@UpdateColumnStatisticsForPartition' {} a -> s {columnStatisticsList = a} :: UpdateColumnStatisticsForPartition) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateColumnStatisticsForPartition
  where
  type
    AWSResponse UpdateColumnStatisticsForPartition =
      UpdateColumnStatisticsForPartitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForPartitionResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateColumnStatisticsForPartition
  where
  hashWithSalt
    _salt
    UpdateColumnStatisticsForPartition' {..} =
      _salt `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` partitionValues
        `Prelude.hashWithSalt` columnStatisticsList

instance
  Prelude.NFData
    UpdateColumnStatisticsForPartition
  where
  rnf UpdateColumnStatisticsForPartition' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionValues
      `Prelude.seq` Prelude.rnf columnStatisticsList

instance
  Data.ToHeaders
    UpdateColumnStatisticsForPartition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.UpdateColumnStatisticsForPartition" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateColumnStatisticsForPartition
  where
  toJSON UpdateColumnStatisticsForPartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionValues" Data..= partitionValues),
            Prelude.Just
              ( "ColumnStatisticsList"
                  Data..= columnStatisticsList
              )
          ]
      )

instance
  Data.ToPath
    UpdateColumnStatisticsForPartition
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateColumnStatisticsForPartition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateColumnStatisticsForPartitionResponse' smart constructor.
data UpdateColumnStatisticsForPartitionResponse = UpdateColumnStatisticsForPartitionResponse'
  { -- | Error occurred during updating column statistics data.
    errors :: Prelude.Maybe [ColumnStatisticsError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateColumnStatisticsForPartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'updateColumnStatisticsForPartitionResponse_errors' - Error occurred during updating column statistics data.
--
-- 'httpStatus', 'updateColumnStatisticsForPartitionResponse_httpStatus' - The response's http status code.
newUpdateColumnStatisticsForPartitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateColumnStatisticsForPartitionResponse
newUpdateColumnStatisticsForPartitionResponse
  pHttpStatus_ =
    UpdateColumnStatisticsForPartitionResponse'
      { errors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Error occurred during updating column statistics data.
updateColumnStatisticsForPartitionResponse_errors :: Lens.Lens' UpdateColumnStatisticsForPartitionResponse (Prelude.Maybe [ColumnStatisticsError])
updateColumnStatisticsForPartitionResponse_errors = Lens.lens (\UpdateColumnStatisticsForPartitionResponse' {errors} -> errors) (\s@UpdateColumnStatisticsForPartitionResponse' {} a -> s {errors = a} :: UpdateColumnStatisticsForPartitionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateColumnStatisticsForPartitionResponse_httpStatus :: Lens.Lens' UpdateColumnStatisticsForPartitionResponse Prelude.Int
updateColumnStatisticsForPartitionResponse_httpStatus = Lens.lens (\UpdateColumnStatisticsForPartitionResponse' {httpStatus} -> httpStatus) (\s@UpdateColumnStatisticsForPartitionResponse' {} a -> s {httpStatus = a} :: UpdateColumnStatisticsForPartitionResponse)

instance
  Prelude.NFData
    UpdateColumnStatisticsForPartitionResponse
  where
  rnf UpdateColumnStatisticsForPartitionResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
