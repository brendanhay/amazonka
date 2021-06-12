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
-- Module      : Network.AWS.Athena.GetTableMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns table metadata for the specified catalog, database, and table.
module Network.AWS.Athena.GetTableMetadata
  ( -- * Creating a Request
    GetTableMetadata (..),
    newGetTableMetadata,

    -- * Request Lenses
    getTableMetadata_catalogName,
    getTableMetadata_databaseName,
    getTableMetadata_tableName,

    -- * Destructuring the Response
    GetTableMetadataResponse (..),
    newGetTableMetadataResponse,

    -- * Response Lenses
    getTableMetadataResponse_tableMetadata,
    getTableMetadataResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTableMetadata' smart constructor.
data GetTableMetadata = GetTableMetadata'
  { -- | The name of the data catalog that contains the database and table
    -- metadata to return.
    catalogName :: Core.Text,
    -- | The name of the database that contains the table metadata to return.
    databaseName :: Core.Text,
    -- | The name of the table for which metadata is returned.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTableMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogName', 'getTableMetadata_catalogName' - The name of the data catalog that contains the database and table
-- metadata to return.
--
-- 'databaseName', 'getTableMetadata_databaseName' - The name of the database that contains the table metadata to return.
--
-- 'tableName', 'getTableMetadata_tableName' - The name of the table for which metadata is returned.
newGetTableMetadata ::
  -- | 'catalogName'
  Core.Text ->
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GetTableMetadata
newGetTableMetadata
  pCatalogName_
  pDatabaseName_
  pTableName_ =
    GetTableMetadata'
      { catalogName = pCatalogName_,
        databaseName = pDatabaseName_,
        tableName = pTableName_
      }

-- | The name of the data catalog that contains the database and table
-- metadata to return.
getTableMetadata_catalogName :: Lens.Lens' GetTableMetadata Core.Text
getTableMetadata_catalogName = Lens.lens (\GetTableMetadata' {catalogName} -> catalogName) (\s@GetTableMetadata' {} a -> s {catalogName = a} :: GetTableMetadata)

-- | The name of the database that contains the table metadata to return.
getTableMetadata_databaseName :: Lens.Lens' GetTableMetadata Core.Text
getTableMetadata_databaseName = Lens.lens (\GetTableMetadata' {databaseName} -> databaseName) (\s@GetTableMetadata' {} a -> s {databaseName = a} :: GetTableMetadata)

-- | The name of the table for which metadata is returned.
getTableMetadata_tableName :: Lens.Lens' GetTableMetadata Core.Text
getTableMetadata_tableName = Lens.lens (\GetTableMetadata' {tableName} -> tableName) (\s@GetTableMetadata' {} a -> s {tableName = a} :: GetTableMetadata)

instance Core.AWSRequest GetTableMetadata where
  type
    AWSResponse GetTableMetadata =
      GetTableMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableMetadataResponse'
            Core.<$> (x Core..?> "TableMetadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTableMetadata

instance Core.NFData GetTableMetadata

instance Core.ToHeaders GetTableMetadata where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonAthena.GetTableMetadata" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTableMetadata where
  toJSON GetTableMetadata' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CatalogName" Core..= catalogName),
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetTableMetadata where
  toPath = Core.const "/"

instance Core.ToQuery GetTableMetadata where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTableMetadataResponse' smart constructor.
data GetTableMetadataResponse = GetTableMetadataResponse'
  { -- | An object that contains table metadata.
    tableMetadata :: Core.Maybe TableMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTableMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableMetadata', 'getTableMetadataResponse_tableMetadata' - An object that contains table metadata.
--
-- 'httpStatus', 'getTableMetadataResponse_httpStatus' - The response's http status code.
newGetTableMetadataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTableMetadataResponse
newGetTableMetadataResponse pHttpStatus_ =
  GetTableMetadataResponse'
    { tableMetadata =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that contains table metadata.
getTableMetadataResponse_tableMetadata :: Lens.Lens' GetTableMetadataResponse (Core.Maybe TableMetadata)
getTableMetadataResponse_tableMetadata = Lens.lens (\GetTableMetadataResponse' {tableMetadata} -> tableMetadata) (\s@GetTableMetadataResponse' {} a -> s {tableMetadata = a} :: GetTableMetadataResponse)

-- | The response's http status code.
getTableMetadataResponse_httpStatus :: Lens.Lens' GetTableMetadataResponse Core.Int
getTableMetadataResponse_httpStatus = Lens.lens (\GetTableMetadataResponse' {httpStatus} -> httpStatus) (\s@GetTableMetadataResponse' {} a -> s {httpStatus = a} :: GetTableMetadataResponse)

instance Core.NFData GetTableMetadataResponse
