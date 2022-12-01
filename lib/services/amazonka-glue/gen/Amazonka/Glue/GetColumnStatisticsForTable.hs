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
-- Module      : Amazonka.Glue.GetColumnStatisticsForTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @GetTable@.
module Amazonka.Glue.GetColumnStatisticsForTable
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetColumnStatisticsForTable' smart constructor.
data GetColumnStatisticsForTable = GetColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | A list of the column names.
    columnNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetColumnStatisticsForTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getColumnStatisticsForTable_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'getColumnStatisticsForTable_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'getColumnStatisticsForTable_tableName' - The name of the partitions\' table.
--
-- 'columnNames', 'getColumnStatisticsForTable_columnNames' - A list of the column names.
newGetColumnStatisticsForTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetColumnStatisticsForTable
newGetColumnStatisticsForTable
  pDatabaseName_
  pTableName_ =
    GetColumnStatisticsForTable'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        columnNames = Prelude.mempty
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the Amazon Web Services account ID is used by default.
getColumnStatisticsForTable_catalogId :: Lens.Lens' GetColumnStatisticsForTable (Prelude.Maybe Prelude.Text)
getColumnStatisticsForTable_catalogId = Lens.lens (\GetColumnStatisticsForTable' {catalogId} -> catalogId) (\s@GetColumnStatisticsForTable' {} a -> s {catalogId = a} :: GetColumnStatisticsForTable)

-- | The name of the catalog database where the partitions reside.
getColumnStatisticsForTable_databaseName :: Lens.Lens' GetColumnStatisticsForTable Prelude.Text
getColumnStatisticsForTable_databaseName = Lens.lens (\GetColumnStatisticsForTable' {databaseName} -> databaseName) (\s@GetColumnStatisticsForTable' {} a -> s {databaseName = a} :: GetColumnStatisticsForTable)

-- | The name of the partitions\' table.
getColumnStatisticsForTable_tableName :: Lens.Lens' GetColumnStatisticsForTable Prelude.Text
getColumnStatisticsForTable_tableName = Lens.lens (\GetColumnStatisticsForTable' {tableName} -> tableName) (\s@GetColumnStatisticsForTable' {} a -> s {tableName = a} :: GetColumnStatisticsForTable)

-- | A list of the column names.
getColumnStatisticsForTable_columnNames :: Lens.Lens' GetColumnStatisticsForTable [Prelude.Text]
getColumnStatisticsForTable_columnNames = Lens.lens (\GetColumnStatisticsForTable' {columnNames} -> columnNames) (\s@GetColumnStatisticsForTable' {} a -> s {columnNames = a} :: GetColumnStatisticsForTable) Prelude.. Lens.coerced

instance Core.AWSRequest GetColumnStatisticsForTable where
  type
    AWSResponse GetColumnStatisticsForTable =
      GetColumnStatisticsForTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetColumnStatisticsForTableResponse'
            Prelude.<$> ( x Core..?> "ColumnStatisticsList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetColumnStatisticsForTable where
  hashWithSalt _salt GetColumnStatisticsForTable' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` columnNames

instance Prelude.NFData GetColumnStatisticsForTable where
  rnf GetColumnStatisticsForTable' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf columnNames

instance Core.ToHeaders GetColumnStatisticsForTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.GetColumnStatisticsForTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetColumnStatisticsForTable where
  toJSON GetColumnStatisticsForTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("ColumnNames" Core..= columnNames)
          ]
      )

instance Core.ToPath GetColumnStatisticsForTable where
  toPath = Prelude.const "/"

instance Core.ToQuery GetColumnStatisticsForTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetColumnStatisticsForTableResponse' smart constructor.
data GetColumnStatisticsForTableResponse = GetColumnStatisticsForTableResponse'
  { -- | List of ColumnStatistics that failed to be retrieved.
    columnStatisticsList :: Prelude.Maybe [ColumnStatistics],
    -- | List of ColumnStatistics that failed to be retrieved.
    errors :: Prelude.Maybe [ColumnError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetColumnStatisticsForTableResponse
newGetColumnStatisticsForTableResponse pHttpStatus_ =
  GetColumnStatisticsForTableResponse'
    { columnStatisticsList =
        Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of ColumnStatistics that failed to be retrieved.
getColumnStatisticsForTableResponse_columnStatisticsList :: Lens.Lens' GetColumnStatisticsForTableResponse (Prelude.Maybe [ColumnStatistics])
getColumnStatisticsForTableResponse_columnStatisticsList = Lens.lens (\GetColumnStatisticsForTableResponse' {columnStatisticsList} -> columnStatisticsList) (\s@GetColumnStatisticsForTableResponse' {} a -> s {columnStatisticsList = a} :: GetColumnStatisticsForTableResponse) Prelude.. Lens.mapping Lens.coerced

-- | List of ColumnStatistics that failed to be retrieved.
getColumnStatisticsForTableResponse_errors :: Lens.Lens' GetColumnStatisticsForTableResponse (Prelude.Maybe [ColumnError])
getColumnStatisticsForTableResponse_errors = Lens.lens (\GetColumnStatisticsForTableResponse' {errors} -> errors) (\s@GetColumnStatisticsForTableResponse' {} a -> s {errors = a} :: GetColumnStatisticsForTableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getColumnStatisticsForTableResponse_httpStatus :: Lens.Lens' GetColumnStatisticsForTableResponse Prelude.Int
getColumnStatisticsForTableResponse_httpStatus = Lens.lens (\GetColumnStatisticsForTableResponse' {httpStatus} -> httpStatus) (\s@GetColumnStatisticsForTableResponse' {} a -> s {httpStatus = a} :: GetColumnStatisticsForTableResponse)

instance
  Prelude.NFData
    GetColumnStatisticsForTableResponse
  where
  rnf GetColumnStatisticsForTableResponse' {..} =
    Prelude.rnf columnStatisticsList
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
