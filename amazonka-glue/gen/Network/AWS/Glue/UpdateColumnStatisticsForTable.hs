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
-- Module      : Network.AWS.Glue.UpdateColumnStatisticsForTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @UpdateTable@.
module Network.AWS.Glue.UpdateColumnStatisticsForTable
  ( -- * Creating a Request
    UpdateColumnStatisticsForTable (..),
    newUpdateColumnStatisticsForTable,

    -- * Request Lenses
    updateColumnStatisticsForTable_catalogId,
    updateColumnStatisticsForTable_databaseName,
    updateColumnStatisticsForTable_tableName,
    updateColumnStatisticsForTable_columnStatisticsList,

    -- * Destructuring the Response
    UpdateColumnStatisticsForTableResponse (..),
    newUpdateColumnStatisticsForTableResponse,

    -- * Response Lenses
    updateColumnStatisticsForTableResponse_errors,
    updateColumnStatisticsForTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateColumnStatisticsForTable' smart constructor.
data UpdateColumnStatisticsForTable = UpdateColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | A list of the column statistics.
    columnStatisticsList :: [ColumnStatistics]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateColumnStatisticsForTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'updateColumnStatisticsForTable_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'updateColumnStatisticsForTable_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'updateColumnStatisticsForTable_tableName' - The name of the partitions\' table.
--
-- 'columnStatisticsList', 'updateColumnStatisticsForTable_columnStatisticsList' - A list of the column statistics.
newUpdateColumnStatisticsForTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  UpdateColumnStatisticsForTable
newUpdateColumnStatisticsForTable
  pDatabaseName_
  pTableName_ =
    UpdateColumnStatisticsForTable'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        columnStatisticsList = Prelude.mempty
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
updateColumnStatisticsForTable_catalogId :: Lens.Lens' UpdateColumnStatisticsForTable (Prelude.Maybe Prelude.Text)
updateColumnStatisticsForTable_catalogId = Lens.lens (\UpdateColumnStatisticsForTable' {catalogId} -> catalogId) (\s@UpdateColumnStatisticsForTable' {} a -> s {catalogId = a} :: UpdateColumnStatisticsForTable)

-- | The name of the catalog database where the partitions reside.
updateColumnStatisticsForTable_databaseName :: Lens.Lens' UpdateColumnStatisticsForTable Prelude.Text
updateColumnStatisticsForTable_databaseName = Lens.lens (\UpdateColumnStatisticsForTable' {databaseName} -> databaseName) (\s@UpdateColumnStatisticsForTable' {} a -> s {databaseName = a} :: UpdateColumnStatisticsForTable)

-- | The name of the partitions\' table.
updateColumnStatisticsForTable_tableName :: Lens.Lens' UpdateColumnStatisticsForTable Prelude.Text
updateColumnStatisticsForTable_tableName = Lens.lens (\UpdateColumnStatisticsForTable' {tableName} -> tableName) (\s@UpdateColumnStatisticsForTable' {} a -> s {tableName = a} :: UpdateColumnStatisticsForTable)

-- | A list of the column statistics.
updateColumnStatisticsForTable_columnStatisticsList :: Lens.Lens' UpdateColumnStatisticsForTable [ColumnStatistics]
updateColumnStatisticsForTable_columnStatisticsList = Lens.lens (\UpdateColumnStatisticsForTable' {columnStatisticsList} -> columnStatisticsList) (\s@UpdateColumnStatisticsForTable' {} a -> s {columnStatisticsList = a} :: UpdateColumnStatisticsForTable) Prelude.. Lens._Coerce

instance
  Core.AWSRequest
    UpdateColumnStatisticsForTable
  where
  type
    AWSResponse UpdateColumnStatisticsForTable =
      UpdateColumnStatisticsForTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForTableResponse'
            Prelude.<$> (x Core..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateColumnStatisticsForTable

instance
  Prelude.NFData
    UpdateColumnStatisticsForTable

instance
  Core.ToHeaders
    UpdateColumnStatisticsForTable
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.UpdateColumnStatisticsForTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateColumnStatisticsForTable where
  toJSON UpdateColumnStatisticsForTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just
              ( "ColumnStatisticsList"
                  Core..= columnStatisticsList
              )
          ]
      )

instance Core.ToPath UpdateColumnStatisticsForTable where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateColumnStatisticsForTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateColumnStatisticsForTableResponse' smart constructor.
data UpdateColumnStatisticsForTableResponse = UpdateColumnStatisticsForTableResponse'
  { -- | List of ColumnStatisticsErrors.
    errors :: Prelude.Maybe [ColumnStatisticsError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateColumnStatisticsForTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'updateColumnStatisticsForTableResponse_errors' - List of ColumnStatisticsErrors.
--
-- 'httpStatus', 'updateColumnStatisticsForTableResponse_httpStatus' - The response's http status code.
newUpdateColumnStatisticsForTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateColumnStatisticsForTableResponse
newUpdateColumnStatisticsForTableResponse
  pHttpStatus_ =
    UpdateColumnStatisticsForTableResponse'
      { errors =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | List of ColumnStatisticsErrors.
updateColumnStatisticsForTableResponse_errors :: Lens.Lens' UpdateColumnStatisticsForTableResponse (Prelude.Maybe [ColumnStatisticsError])
updateColumnStatisticsForTableResponse_errors = Lens.lens (\UpdateColumnStatisticsForTableResponse' {errors} -> errors) (\s@UpdateColumnStatisticsForTableResponse' {} a -> s {errors = a} :: UpdateColumnStatisticsForTableResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateColumnStatisticsForTableResponse_httpStatus :: Lens.Lens' UpdateColumnStatisticsForTableResponse Prelude.Int
updateColumnStatisticsForTableResponse_httpStatus = Lens.lens (\UpdateColumnStatisticsForTableResponse' {httpStatus} -> httpStatus) (\s@UpdateColumnStatisticsForTableResponse' {} a -> s {httpStatus = a} :: UpdateColumnStatisticsForTableResponse)

instance
  Prelude.NFData
    UpdateColumnStatisticsForTableResponse
