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
-- Module      : Network.AWS.Glue.GetTableVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified version of a table.
module Network.AWS.Glue.GetTableVersion
  ( -- * Creating a Request
    GetTableVersion (..),
    newGetTableVersion,

    -- * Request Lenses
    getTableVersion_catalogId,
    getTableVersion_versionId,
    getTableVersion_databaseName,
    getTableVersion_tableName,

    -- * Destructuring the Response
    GetTableVersionResponse (..),
    newGetTableVersionResponse,

    -- * Response Lenses
    getTableVersionResponse_tableVersion,
    getTableVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTableVersion' smart constructor.
data GetTableVersion = GetTableVersion'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The ID value of the table version to be retrieved. A @VersionID@ is a
    -- string representation of an integer. Each version is incremented by 1.
    versionId :: Core.Maybe Core.Text,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Core.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTableVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getTableVersion_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
--
-- 'versionId', 'getTableVersion_versionId' - The ID value of the table version to be retrieved. A @VersionID@ is a
-- string representation of an integer. Each version is incremented by 1.
--
-- 'databaseName', 'getTableVersion_databaseName' - The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableName', 'getTableVersion_tableName' - The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
newGetTableVersion ::
  -- | 'databaseName'
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  GetTableVersion
newGetTableVersion pDatabaseName_ pTableName_ =
  GetTableVersion'
    { catalogId = Core.Nothing,
      versionId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
getTableVersion_catalogId :: Lens.Lens' GetTableVersion (Core.Maybe Core.Text)
getTableVersion_catalogId = Lens.lens (\GetTableVersion' {catalogId} -> catalogId) (\s@GetTableVersion' {} a -> s {catalogId = a} :: GetTableVersion)

-- | The ID value of the table version to be retrieved. A @VersionID@ is a
-- string representation of an integer. Each version is incremented by 1.
getTableVersion_versionId :: Lens.Lens' GetTableVersion (Core.Maybe Core.Text)
getTableVersion_versionId = Lens.lens (\GetTableVersion' {versionId} -> versionId) (\s@GetTableVersion' {} a -> s {versionId = a} :: GetTableVersion)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
getTableVersion_databaseName :: Lens.Lens' GetTableVersion Core.Text
getTableVersion_databaseName = Lens.lens (\GetTableVersion' {databaseName} -> databaseName) (\s@GetTableVersion' {} a -> s {databaseName = a} :: GetTableVersion)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
getTableVersion_tableName :: Lens.Lens' GetTableVersion Core.Text
getTableVersion_tableName = Lens.lens (\GetTableVersion' {tableName} -> tableName) (\s@GetTableVersion' {} a -> s {tableName = a} :: GetTableVersion)

instance Core.AWSRequest GetTableVersion where
  type
    AWSResponse GetTableVersion =
      GetTableVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTableVersionResponse'
            Core.<$> (x Core..?> "TableVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTableVersion

instance Core.NFData GetTableVersion

instance Core.ToHeaders GetTableVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTableVersion" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTableVersion where
  toJSON GetTableVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            ("VersionId" Core..=) Core.<$> versionId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetTableVersion where
  toPath = Core.const "/"

instance Core.ToQuery GetTableVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTableVersionResponse' smart constructor.
data GetTableVersionResponse = GetTableVersionResponse'
  { -- | The requested table version.
    tableVersion :: Core.Maybe TableVersion,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTableVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableVersion', 'getTableVersionResponse_tableVersion' - The requested table version.
--
-- 'httpStatus', 'getTableVersionResponse_httpStatus' - The response's http status code.
newGetTableVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTableVersionResponse
newGetTableVersionResponse pHttpStatus_ =
  GetTableVersionResponse'
    { tableVersion =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested table version.
getTableVersionResponse_tableVersion :: Lens.Lens' GetTableVersionResponse (Core.Maybe TableVersion)
getTableVersionResponse_tableVersion = Lens.lens (\GetTableVersionResponse' {tableVersion} -> tableVersion) (\s@GetTableVersionResponse' {} a -> s {tableVersion = a} :: GetTableVersionResponse)

-- | The response's http status code.
getTableVersionResponse_httpStatus :: Lens.Lens' GetTableVersionResponse Core.Int
getTableVersionResponse_httpStatus = Lens.lens (\GetTableVersionResponse' {httpStatus} -> httpStatus) (\s@GetTableVersionResponse' {} a -> s {httpStatus = a} :: GetTableVersionResponse)

instance Core.NFData GetTableVersionResponse
