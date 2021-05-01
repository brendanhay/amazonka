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
-- Module      : Network.AWS.Glue.GetColumnStatisticsForPartition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves partition statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @GetPartition@.
module Network.AWS.Glue.GetColumnStatisticsForPartition
  ( -- * Creating a Request
    GetColumnStatisticsForPartition (..),
    newGetColumnStatisticsForPartition,

    -- * Request Lenses
    getColumnStatisticsForPartition_catalogId,
    getColumnStatisticsForPartition_databaseName,
    getColumnStatisticsForPartition_tableName,
    getColumnStatisticsForPartition_partitionValues,
    getColumnStatisticsForPartition_columnNames,

    -- * Destructuring the Response
    GetColumnStatisticsForPartitionResponse (..),
    newGetColumnStatisticsForPartitionResponse,

    -- * Response Lenses
    getColumnStatisticsForPartitionResponse_columnStatisticsList,
    getColumnStatisticsForPartitionResponse_errors,
    getColumnStatisticsForPartitionResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetColumnStatisticsForPartition' smart constructor.
data GetColumnStatisticsForPartition = GetColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Prelude.Text],
    -- | A list of the column names.
    columnNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetColumnStatisticsForPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getColumnStatisticsForPartition_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'getColumnStatisticsForPartition_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'getColumnStatisticsForPartition_tableName' - The name of the partitions\' table.
--
-- 'partitionValues', 'getColumnStatisticsForPartition_partitionValues' - A list of partition values identifying the partition.
--
-- 'columnNames', 'getColumnStatisticsForPartition_columnNames' - A list of the column names.
newGetColumnStatisticsForPartition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetColumnStatisticsForPartition
newGetColumnStatisticsForPartition
  pDatabaseName_
  pTableName_ =
    GetColumnStatisticsForPartition'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionValues = Prelude.mempty,
        columnNames = Prelude.mempty
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
getColumnStatisticsForPartition_catalogId :: Lens.Lens' GetColumnStatisticsForPartition (Prelude.Maybe Prelude.Text)
getColumnStatisticsForPartition_catalogId = Lens.lens (\GetColumnStatisticsForPartition' {catalogId} -> catalogId) (\s@GetColumnStatisticsForPartition' {} a -> s {catalogId = a} :: GetColumnStatisticsForPartition)

-- | The name of the catalog database where the partitions reside.
getColumnStatisticsForPartition_databaseName :: Lens.Lens' GetColumnStatisticsForPartition Prelude.Text
getColumnStatisticsForPartition_databaseName = Lens.lens (\GetColumnStatisticsForPartition' {databaseName} -> databaseName) (\s@GetColumnStatisticsForPartition' {} a -> s {databaseName = a} :: GetColumnStatisticsForPartition)

-- | The name of the partitions\' table.
getColumnStatisticsForPartition_tableName :: Lens.Lens' GetColumnStatisticsForPartition Prelude.Text
getColumnStatisticsForPartition_tableName = Lens.lens (\GetColumnStatisticsForPartition' {tableName} -> tableName) (\s@GetColumnStatisticsForPartition' {} a -> s {tableName = a} :: GetColumnStatisticsForPartition)

-- | A list of partition values identifying the partition.
getColumnStatisticsForPartition_partitionValues :: Lens.Lens' GetColumnStatisticsForPartition [Prelude.Text]
getColumnStatisticsForPartition_partitionValues = Lens.lens (\GetColumnStatisticsForPartition' {partitionValues} -> partitionValues) (\s@GetColumnStatisticsForPartition' {} a -> s {partitionValues = a} :: GetColumnStatisticsForPartition) Prelude.. Prelude._Coerce

-- | A list of the column names.
getColumnStatisticsForPartition_columnNames :: Lens.Lens' GetColumnStatisticsForPartition [Prelude.Text]
getColumnStatisticsForPartition_columnNames = Lens.lens (\GetColumnStatisticsForPartition' {columnNames} -> columnNames) (\s@GetColumnStatisticsForPartition' {} a -> s {columnNames = a} :: GetColumnStatisticsForPartition) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    GetColumnStatisticsForPartition
  where
  type
    Rs GetColumnStatisticsForPartition =
      GetColumnStatisticsForPartitionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetColumnStatisticsForPartitionResponse'
            Prelude.<$> ( x Prelude..?> "ColumnStatisticsList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "Errors" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetColumnStatisticsForPartition

instance
  Prelude.NFData
    GetColumnStatisticsForPartition

instance
  Prelude.ToHeaders
    GetColumnStatisticsForPartition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.GetColumnStatisticsForPartition" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    GetColumnStatisticsForPartition
  where
  toJSON GetColumnStatisticsForPartition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just
              ("PartitionValues" Prelude..= partitionValues),
            Prelude.Just ("ColumnNames" Prelude..= columnNames)
          ]
      )

instance
  Prelude.ToPath
    GetColumnStatisticsForPartition
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetColumnStatisticsForPartition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetColumnStatisticsForPartitionResponse' smart constructor.
data GetColumnStatisticsForPartitionResponse = GetColumnStatisticsForPartitionResponse'
  { -- | List of ColumnStatistics that failed to be retrieved.
    columnStatisticsList :: Prelude.Maybe [ColumnStatistics],
    -- | Error occurred during retrieving column statistics data.
    errors :: Prelude.Maybe [ColumnError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetColumnStatisticsForPartitionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnStatisticsList', 'getColumnStatisticsForPartitionResponse_columnStatisticsList' - List of ColumnStatistics that failed to be retrieved.
--
-- 'errors', 'getColumnStatisticsForPartitionResponse_errors' - Error occurred during retrieving column statistics data.
--
-- 'httpStatus', 'getColumnStatisticsForPartitionResponse_httpStatus' - The response's http status code.
newGetColumnStatisticsForPartitionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetColumnStatisticsForPartitionResponse
newGetColumnStatisticsForPartitionResponse
  pHttpStatus_ =
    GetColumnStatisticsForPartitionResponse'
      { columnStatisticsList =
          Prelude.Nothing,
        errors = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | List of ColumnStatistics that failed to be retrieved.
getColumnStatisticsForPartitionResponse_columnStatisticsList :: Lens.Lens' GetColumnStatisticsForPartitionResponse (Prelude.Maybe [ColumnStatistics])
getColumnStatisticsForPartitionResponse_columnStatisticsList = Lens.lens (\GetColumnStatisticsForPartitionResponse' {columnStatisticsList} -> columnStatisticsList) (\s@GetColumnStatisticsForPartitionResponse' {} a -> s {columnStatisticsList = a} :: GetColumnStatisticsForPartitionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Error occurred during retrieving column statistics data.
getColumnStatisticsForPartitionResponse_errors :: Lens.Lens' GetColumnStatisticsForPartitionResponse (Prelude.Maybe [ColumnError])
getColumnStatisticsForPartitionResponse_errors = Lens.lens (\GetColumnStatisticsForPartitionResponse' {errors} -> errors) (\s@GetColumnStatisticsForPartitionResponse' {} a -> s {errors = a} :: GetColumnStatisticsForPartitionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getColumnStatisticsForPartitionResponse_httpStatus :: Lens.Lens' GetColumnStatisticsForPartitionResponse Prelude.Int
getColumnStatisticsForPartitionResponse_httpStatus = Lens.lens (\GetColumnStatisticsForPartitionResponse' {httpStatus} -> httpStatus) (\s@GetColumnStatisticsForPartitionResponse' {} a -> s {httpStatus = a} :: GetColumnStatisticsForPartitionResponse)

instance
  Prelude.NFData
    GetColumnStatisticsForPartitionResponse
