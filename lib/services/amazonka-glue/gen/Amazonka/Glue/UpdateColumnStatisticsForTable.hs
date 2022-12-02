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
-- Module      : Amazonka.Glue.UpdateColumnStatisticsForTable
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or updates table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @UpdateTable@.
module Amazonka.Glue.UpdateColumnStatisticsForTable
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateColumnStatisticsForTable' smart constructor.
data UpdateColumnStatisticsForTable = UpdateColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the Amazon Web Services account ID is used by default.
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
-- none is supplied, the Amazon Web Services account ID is used by default.
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
-- none is supplied, the Amazon Web Services account ID is used by default.
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
updateColumnStatisticsForTable_columnStatisticsList = Lens.lens (\UpdateColumnStatisticsForTable' {columnStatisticsList} -> columnStatisticsList) (\s@UpdateColumnStatisticsForTable' {} a -> s {columnStatisticsList = a} :: UpdateColumnStatisticsForTable) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateColumnStatisticsForTable
  where
  type
    AWSResponse UpdateColumnStatisticsForTable =
      UpdateColumnStatisticsForTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateColumnStatisticsForTableResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateColumnStatisticsForTable
  where
  hashWithSalt
    _salt
    UpdateColumnStatisticsForTable' {..} =
      _salt `Prelude.hashWithSalt` catalogId
        `Prelude.hashWithSalt` databaseName
        `Prelude.hashWithSalt` tableName
        `Prelude.hashWithSalt` columnStatisticsList

instance
  Prelude.NFData
    UpdateColumnStatisticsForTable
  where
  rnf UpdateColumnStatisticsForTable' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf columnStatisticsList

instance
  Data.ToHeaders
    UpdateColumnStatisticsForTable
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.UpdateColumnStatisticsForTable" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateColumnStatisticsForTable where
  toJSON UpdateColumnStatisticsForTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just
              ( "ColumnStatisticsList"
                  Data..= columnStatisticsList
              )
          ]
      )

instance Data.ToPath UpdateColumnStatisticsForTable where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateColumnStatisticsForTable where
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
updateColumnStatisticsForTableResponse_errors = Lens.lens (\UpdateColumnStatisticsForTableResponse' {errors} -> errors) (\s@UpdateColumnStatisticsForTableResponse' {} a -> s {errors = a} :: UpdateColumnStatisticsForTableResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateColumnStatisticsForTableResponse_httpStatus :: Lens.Lens' UpdateColumnStatisticsForTableResponse Prelude.Int
updateColumnStatisticsForTableResponse_httpStatus = Lens.lens (\UpdateColumnStatisticsForTableResponse' {httpStatus} -> httpStatus) (\s@UpdateColumnStatisticsForTableResponse' {} a -> s {httpStatus = a} :: UpdateColumnStatisticsForTableResponse)

instance
  Prelude.NFData
    UpdateColumnStatisticsForTableResponse
  where
  rnf UpdateColumnStatisticsForTableResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
