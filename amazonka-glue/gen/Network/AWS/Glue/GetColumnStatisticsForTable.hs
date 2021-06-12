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
-- Module      : Network.AWS.Glue.GetColumnStatisticsForTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @GetTable@.
module Network.AWS.Glue.GetColumnStatisticsForTable
  ( -- * Creating a Request
    GetColumnStatisticsForTable (..),
    newGetColumnStatisticsForTable,

    -- * Request Lenses
    getColumnStatisticsForTable_catalogId,
    getColumnStatisticsForTable_databaseName,
    getColumnStatisticsForTable_tableName,
    getColumnStatisticsForTable_columnNames,

    -- * Destructuring the Response
    GetColumnStatisticsForTableResponse (..),
    newGetColumnStatisticsForTableResponse,

    -- * Response Lenses
    getColumnStatisticsForTableResponse_columnStatisticsList,
    getColumnStatisticsForTableResponse_errors,
    getColumnStatisticsForTableResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetColumnStatisticsForTable' smart constructor.
data GetColumnStatisticsForTable = GetColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Core.Text,
    -- | The name of the partitions\' table.
    tableName :: Core.Text,
    -- | A list of the column names.
    columnNames :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetColumnStatisticsForTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getColumnStatisticsForTable_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'getColumnStatisticsForTable_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'getColumnStatisticsForTable_tableName' - The name of the partitions\' table.
--
-- 'columnNames', 'getColumnStatisticsForTable_columnNames' - A list of the column names.
newGetColumnStatisticsForTable ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GetColumnStatisticsForTable
newGetColumnStatisticsForTable
  pDatabaseName_
  pTableName_ =
    GetColumnStatisticsForTable'
      { catalogId =
          Core.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        columnNames = Core.mempty
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
getColumnStatisticsForTable_catalogId :: Lens.Lens' GetColumnStatisticsForTable (Core.Maybe Core.Text)
getColumnStatisticsForTable_catalogId = Lens.lens (\GetColumnStatisticsForTable' {catalogId} -> catalogId) (\s@GetColumnStatisticsForTable' {} a -> s {catalogId = a} :: GetColumnStatisticsForTable)

-- | The name of the catalog database where the partitions reside.
getColumnStatisticsForTable_databaseName :: Lens.Lens' GetColumnStatisticsForTable Core.Text
getColumnStatisticsForTable_databaseName = Lens.lens (\GetColumnStatisticsForTable' {databaseName} -> databaseName) (\s@GetColumnStatisticsForTable' {} a -> s {databaseName = a} :: GetColumnStatisticsForTable)

-- | The name of the partitions\' table.
getColumnStatisticsForTable_tableName :: Lens.Lens' GetColumnStatisticsForTable Core.Text
getColumnStatisticsForTable_tableName = Lens.lens (\GetColumnStatisticsForTable' {tableName} -> tableName) (\s@GetColumnStatisticsForTable' {} a -> s {tableName = a} :: GetColumnStatisticsForTable)

-- | A list of the column names.
getColumnStatisticsForTable_columnNames :: Lens.Lens' GetColumnStatisticsForTable [Core.Text]
getColumnStatisticsForTable_columnNames = Lens.lens (\GetColumnStatisticsForTable' {columnNames} -> columnNames) (\s@GetColumnStatisticsForTable' {} a -> s {columnNames = a} :: GetColumnStatisticsForTable) Core.. Lens._Coerce

instance Core.AWSRequest GetColumnStatisticsForTable where
  type
    AWSResponse GetColumnStatisticsForTable =
      GetColumnStatisticsForTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetColumnStatisticsForTableResponse'
            Core.<$> ( x Core..?> "ColumnStatisticsList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetColumnStatisticsForTable

instance Core.NFData GetColumnStatisticsForTable

instance Core.ToHeaders GetColumnStatisticsForTable where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetColumnStatisticsForTable" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetColumnStatisticsForTable where
  toJSON GetColumnStatisticsForTable' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("ColumnNames" Core..= columnNames)
          ]
      )

instance Core.ToPath GetColumnStatisticsForTable where
  toPath = Core.const "/"

instance Core.ToQuery GetColumnStatisticsForTable where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetColumnStatisticsForTableResponse' smart constructor.
data GetColumnStatisticsForTableResponse = GetColumnStatisticsForTableResponse'
  { -- | List of ColumnStatistics that failed to be retrieved.
    columnStatisticsList :: Core.Maybe [ColumnStatistics],
    -- | List of ColumnStatistics that failed to be retrieved.
    errors :: Core.Maybe [ColumnError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetColumnStatisticsForTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnStatisticsList', 'getColumnStatisticsForTableResponse_columnStatisticsList' - List of ColumnStatistics that failed to be retrieved.
--
-- 'errors', 'getColumnStatisticsForTableResponse_errors' - List of ColumnStatistics that failed to be retrieved.
--
-- 'httpStatus', 'getColumnStatisticsForTableResponse_httpStatus' - The response's http status code.
newGetColumnStatisticsForTableResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetColumnStatisticsForTableResponse
newGetColumnStatisticsForTableResponse pHttpStatus_ =
  GetColumnStatisticsForTableResponse'
    { columnStatisticsList =
        Core.Nothing,
      errors = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of ColumnStatistics that failed to be retrieved.
getColumnStatisticsForTableResponse_columnStatisticsList :: Lens.Lens' GetColumnStatisticsForTableResponse (Core.Maybe [ColumnStatistics])
getColumnStatisticsForTableResponse_columnStatisticsList = Lens.lens (\GetColumnStatisticsForTableResponse' {columnStatisticsList} -> columnStatisticsList) (\s@GetColumnStatisticsForTableResponse' {} a -> s {columnStatisticsList = a} :: GetColumnStatisticsForTableResponse) Core.. Lens.mapping Lens._Coerce

-- | List of ColumnStatistics that failed to be retrieved.
getColumnStatisticsForTableResponse_errors :: Lens.Lens' GetColumnStatisticsForTableResponse (Core.Maybe [ColumnError])
getColumnStatisticsForTableResponse_errors = Lens.lens (\GetColumnStatisticsForTableResponse' {errors} -> errors) (\s@GetColumnStatisticsForTableResponse' {} a -> s {errors = a} :: GetColumnStatisticsForTableResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getColumnStatisticsForTableResponse_httpStatus :: Lens.Lens' GetColumnStatisticsForTableResponse Core.Int
getColumnStatisticsForTableResponse_httpStatus = Lens.lens (\GetColumnStatisticsForTableResponse' {httpStatus} -> httpStatus) (\s@GetColumnStatisticsForTableResponse' {} a -> s {httpStatus = a} :: GetColumnStatisticsForTableResponse)

instance
  Core.NFData
    GetColumnStatisticsForTableResponse
