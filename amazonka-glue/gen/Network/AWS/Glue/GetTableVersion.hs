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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTableVersion' smart constructor.
data GetTableVersion = GetTableVersion'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The ID value of the table version to be retrieved. A @VersionID@ is a
    -- string representation of an integer. Each version is incremented by 1.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GetTableVersion
newGetTableVersion pDatabaseName_ pTableName_ =
  GetTableVersion'
    { catalogId = Prelude.Nothing,
      versionId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
getTableVersion_catalogId :: Lens.Lens' GetTableVersion (Prelude.Maybe Prelude.Text)
getTableVersion_catalogId = Lens.lens (\GetTableVersion' {catalogId} -> catalogId) (\s@GetTableVersion' {} a -> s {catalogId = a} :: GetTableVersion)

-- | The ID value of the table version to be retrieved. A @VersionID@ is a
-- string representation of an integer. Each version is incremented by 1.
getTableVersion_versionId :: Lens.Lens' GetTableVersion (Prelude.Maybe Prelude.Text)
getTableVersion_versionId = Lens.lens (\GetTableVersion' {versionId} -> versionId) (\s@GetTableVersion' {} a -> s {versionId = a} :: GetTableVersion)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
getTableVersion_databaseName :: Lens.Lens' GetTableVersion Prelude.Text
getTableVersion_databaseName = Lens.lens (\GetTableVersion' {databaseName} -> databaseName) (\s@GetTableVersion' {} a -> s {databaseName = a} :: GetTableVersion)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
getTableVersion_tableName :: Lens.Lens' GetTableVersion Prelude.Text
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
            Prelude.<$> (x Core..?> "TableVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTableVersion

instance Prelude.NFData GetTableVersion

instance Core.ToHeaders GetTableVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetTableVersion" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTableVersion where
  toJSON GetTableVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("VersionId" Core..=) Prelude.<$> versionId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )

instance Core.ToPath GetTableVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTableVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTableVersionResponse' smart constructor.
data GetTableVersionResponse = GetTableVersionResponse'
  { -- | The requested table version.
    tableVersion :: Prelude.Maybe TableVersion,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTableVersionResponse
newGetTableVersionResponse pHttpStatus_ =
  GetTableVersionResponse'
    { tableVersion =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The requested table version.
getTableVersionResponse_tableVersion :: Lens.Lens' GetTableVersionResponse (Prelude.Maybe TableVersion)
getTableVersionResponse_tableVersion = Lens.lens (\GetTableVersionResponse' {tableVersion} -> tableVersion) (\s@GetTableVersionResponse' {} a -> s {tableVersion = a} :: GetTableVersionResponse)

-- | The response's http status code.
getTableVersionResponse_httpStatus :: Lens.Lens' GetTableVersionResponse Prelude.Int
getTableVersionResponse_httpStatus = Lens.lens (\GetTableVersionResponse' {httpStatus} -> httpStatus) (\s@GetTableVersionResponse' {} a -> s {httpStatus = a} :: GetTableVersionResponse)

instance Prelude.NFData GetTableVersionResponse
