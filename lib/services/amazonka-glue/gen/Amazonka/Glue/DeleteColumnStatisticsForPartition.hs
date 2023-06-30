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
-- Module      : Amazonka.Glue.DeleteColumnStatisticsForPartition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete the partition column statistics of a column.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @DeletePartition@.
module Amazonka.Glue.DeleteColumnStatisticsForPartition
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteColumnStatisticsForPartition' smart constructor.
data DeleteColumnStatisticsForPartition = DeleteColumnStatisticsForPartition'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | A list of partition values identifying the partition.
    partitionValues :: [Prelude.Text],
    -- | Name of the column.
    columnName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteColumnStatisticsForPartition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteColumnStatisticsForPartition_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'columnName'
  Prelude.Text ->
  DeleteColumnStatisticsForPartition
newDeleteColumnStatisticsForPartition
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForPartition'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        partitionValues = Prelude.mempty,
        columnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
deleteColumnStatisticsForPartition_catalogId :: Lens.Lens' DeleteColumnStatisticsForPartition (Prelude.Maybe Prelude.Text)
deleteColumnStatisticsForPartition_catalogId = Lens.lens (\DeleteColumnStatisticsForPartition' {catalogId} -> catalogId) (\s@DeleteColumnStatisticsForPartition' {} a -> s {catalogId = a} :: DeleteColumnStatisticsForPartition)

-- | The name of the catalog database where the partitions reside.
deleteColumnStatisticsForPartition_databaseName :: Lens.Lens' DeleteColumnStatisticsForPartition Prelude.Text
deleteColumnStatisticsForPartition_databaseName = Lens.lens (\DeleteColumnStatisticsForPartition' {databaseName} -> databaseName) (\s@DeleteColumnStatisticsForPartition' {} a -> s {databaseName = a} :: DeleteColumnStatisticsForPartition)

-- | The name of the partitions\' table.
deleteColumnStatisticsForPartition_tableName :: Lens.Lens' DeleteColumnStatisticsForPartition Prelude.Text
deleteColumnStatisticsForPartition_tableName = Lens.lens (\DeleteColumnStatisticsForPartition' {tableName} -> tableName) (\s@DeleteColumnStatisticsForPartition' {} a -> s {tableName = a} :: DeleteColumnStatisticsForPartition)

-- | A list of partition values identifying the partition.
deleteColumnStatisticsForPartition_partitionValues :: Lens.Lens' DeleteColumnStatisticsForPartition [Prelude.Text]
deleteColumnStatisticsForPartition_partitionValues = Lens.lens (\DeleteColumnStatisticsForPartition' {partitionValues} -> partitionValues) (\s@DeleteColumnStatisticsForPartition' {} a -> s {partitionValues = a} :: DeleteColumnStatisticsForPartition) Prelude.. Lens.coerced

-- | Name of the column.
deleteColumnStatisticsForPartition_columnName :: Lens.Lens' DeleteColumnStatisticsForPartition Prelude.Text
deleteColumnStatisticsForPartition_columnName = Lens.lens (\DeleteColumnStatisticsForPartition' {columnName} -> columnName) (\s@DeleteColumnStatisticsForPartition' {} a -> s {columnName = a} :: DeleteColumnStatisticsForPartition)

instance
  Core.AWSRequest
    DeleteColumnStatisticsForPartition
  where
  type
    AWSResponse DeleteColumnStatisticsForPartition =
      DeleteColumnStatisticsForPartitionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForPartitionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteColumnStatisticsForPartition
  where
  hashWithSalt
    _salt
    DeleteColumnStatisticsForPartition' {..} =
      _salt
        `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` partitionValues
        `Prelude.hashWithSalt` columnName

instance
  Prelude.NFData
    DeleteColumnStatisticsForPartition
  where
  rnf DeleteColumnStatisticsForPartition' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf partitionValues
      `Prelude.seq` Prelude.rnf columnName

instance
  Data.ToHeaders
    DeleteColumnStatisticsForPartition
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.DeleteColumnStatisticsForPartition" ::
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
    DeleteColumnStatisticsForPartition
  where
  toJSON DeleteColumnStatisticsForPartition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ("PartitionValues" Data..= partitionValues),
            Prelude.Just ("ColumnName" Data..= columnName)
          ]
      )

instance
  Data.ToPath
    DeleteColumnStatisticsForPartition
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DeleteColumnStatisticsForPartition
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteColumnStatisticsForPartitionResponse' smart constructor.
data DeleteColumnStatisticsForPartitionResponse = DeleteColumnStatisticsForPartitionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteColumnStatisticsForPartitionResponse
newDeleteColumnStatisticsForPartitionResponse
  pHttpStatus_ =
    DeleteColumnStatisticsForPartitionResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteColumnStatisticsForPartitionResponse_httpStatus :: Lens.Lens' DeleteColumnStatisticsForPartitionResponse Prelude.Int
deleteColumnStatisticsForPartitionResponse_httpStatus = Lens.lens (\DeleteColumnStatisticsForPartitionResponse' {httpStatus} -> httpStatus) (\s@DeleteColumnStatisticsForPartitionResponse' {} a -> s {httpStatus = a} :: DeleteColumnStatisticsForPartitionResponse)

instance
  Prelude.NFData
    DeleteColumnStatisticsForPartitionResponse
  where
  rnf DeleteColumnStatisticsForPartitionResponse' {..} =
    Prelude.rnf httpStatus
