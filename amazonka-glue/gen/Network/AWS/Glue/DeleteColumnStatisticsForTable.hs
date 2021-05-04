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
-- Module      : Network.AWS.Glue.DeleteColumnStatisticsForTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves table statistics of columns.
--
-- The Identity and Access Management (IAM) permission required for this
-- operation is @DeleteTable@.
module Network.AWS.Glue.DeleteColumnStatisticsForTable
  ( -- * Creating a Request
    DeleteColumnStatisticsForTable (..),
    newDeleteColumnStatisticsForTable,

    -- * Request Lenses
    deleteColumnStatisticsForTable_catalogId,
    deleteColumnStatisticsForTable_databaseName,
    deleteColumnStatisticsForTable_tableName,
    deleteColumnStatisticsForTable_columnName,

    -- * Destructuring the Response
    DeleteColumnStatisticsForTableResponse (..),
    newDeleteColumnStatisticsForTableResponse,

    -- * Response Lenses
    deleteColumnStatisticsForTableResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteColumnStatisticsForTable' smart constructor.
data DeleteColumnStatisticsForTable = DeleteColumnStatisticsForTable'
  { -- | The ID of the Data Catalog where the partitions in question reside. If
    -- none is supplied, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database where the partitions reside.
    databaseName :: Prelude.Text,
    -- | The name of the partitions\' table.
    tableName :: Prelude.Text,
    -- | The name of the column.
    columnName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteColumnStatisticsForTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteColumnStatisticsForTable_catalogId' - The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
--
-- 'databaseName', 'deleteColumnStatisticsForTable_databaseName' - The name of the catalog database where the partitions reside.
--
-- 'tableName', 'deleteColumnStatisticsForTable_tableName' - The name of the partitions\' table.
--
-- 'columnName', 'deleteColumnStatisticsForTable_columnName' - The name of the column.
newDeleteColumnStatisticsForTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'columnName'
  Prelude.Text ->
  DeleteColumnStatisticsForTable
newDeleteColumnStatisticsForTable
  pDatabaseName_
  pTableName_
  pColumnName_ =
    DeleteColumnStatisticsForTable'
      { catalogId =
          Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        columnName = pColumnName_
      }

-- | The ID of the Data Catalog where the partitions in question reside. If
-- none is supplied, the AWS account ID is used by default.
deleteColumnStatisticsForTable_catalogId :: Lens.Lens' DeleteColumnStatisticsForTable (Prelude.Maybe Prelude.Text)
deleteColumnStatisticsForTable_catalogId = Lens.lens (\DeleteColumnStatisticsForTable' {catalogId} -> catalogId) (\s@DeleteColumnStatisticsForTable' {} a -> s {catalogId = a} :: DeleteColumnStatisticsForTable)

-- | The name of the catalog database where the partitions reside.
deleteColumnStatisticsForTable_databaseName :: Lens.Lens' DeleteColumnStatisticsForTable Prelude.Text
deleteColumnStatisticsForTable_databaseName = Lens.lens (\DeleteColumnStatisticsForTable' {databaseName} -> databaseName) (\s@DeleteColumnStatisticsForTable' {} a -> s {databaseName = a} :: DeleteColumnStatisticsForTable)

-- | The name of the partitions\' table.
deleteColumnStatisticsForTable_tableName :: Lens.Lens' DeleteColumnStatisticsForTable Prelude.Text
deleteColumnStatisticsForTable_tableName = Lens.lens (\DeleteColumnStatisticsForTable' {tableName} -> tableName) (\s@DeleteColumnStatisticsForTable' {} a -> s {tableName = a} :: DeleteColumnStatisticsForTable)

-- | The name of the column.
deleteColumnStatisticsForTable_columnName :: Lens.Lens' DeleteColumnStatisticsForTable Prelude.Text
deleteColumnStatisticsForTable_columnName = Lens.lens (\DeleteColumnStatisticsForTable' {columnName} -> columnName) (\s@DeleteColumnStatisticsForTable' {} a -> s {columnName = a} :: DeleteColumnStatisticsForTable)

instance
  Prelude.AWSRequest
    DeleteColumnStatisticsForTable
  where
  type
    Rs DeleteColumnStatisticsForTable =
      DeleteColumnStatisticsForTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteColumnStatisticsForTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteColumnStatisticsForTable

instance
  Prelude.NFData
    DeleteColumnStatisticsForTable

instance
  Prelude.ToHeaders
    DeleteColumnStatisticsForTable
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.DeleteColumnStatisticsForTable" ::
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
    DeleteColumnStatisticsForTable
  where
  toJSON DeleteColumnStatisticsForTable' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just ("ColumnName" Prelude..= columnName)
          ]
      )

instance
  Prelude.ToPath
    DeleteColumnStatisticsForTable
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteColumnStatisticsForTable
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteColumnStatisticsForTableResponse' smart constructor.
data DeleteColumnStatisticsForTableResponse = DeleteColumnStatisticsForTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteColumnStatisticsForTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteColumnStatisticsForTableResponse_httpStatus' - The response's http status code.
newDeleteColumnStatisticsForTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteColumnStatisticsForTableResponse
newDeleteColumnStatisticsForTableResponse
  pHttpStatus_ =
    DeleteColumnStatisticsForTableResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteColumnStatisticsForTableResponse_httpStatus :: Lens.Lens' DeleteColumnStatisticsForTableResponse Prelude.Int
deleteColumnStatisticsForTableResponse_httpStatus = Lens.lens (\DeleteColumnStatisticsForTableResponse' {httpStatus} -> httpStatus) (\s@DeleteColumnStatisticsForTableResponse' {} a -> s {httpStatus = a} :: DeleteColumnStatisticsForTableResponse)

instance
  Prelude.NFData
    DeleteColumnStatisticsForTableResponse
