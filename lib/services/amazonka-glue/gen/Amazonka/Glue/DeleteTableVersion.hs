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
-- Module      : Amazonka.Glue.DeleteTableVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified version of a table.
module Amazonka.Glue.DeleteTableVersion
  ( -- * Creating a Request
    DeleteTableVersion (..),
    newDeleteTableVersion,

    -- * Request Lenses
    deleteTableVersion_catalogId,
    deleteTableVersion_databaseName,
    deleteTableVersion_tableName,
    deleteTableVersion_versionId,

    -- * Destructuring the Response
    DeleteTableVersionResponse (..),
    newDeleteTableVersionResponse,

    -- * Response Lenses
    deleteTableVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTableVersion' smart constructor.
data DeleteTableVersion = DeleteTableVersion'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Prelude.Text,
    -- | The ID of the table version to be deleted. A @VersionID@ is a string
    -- representation of an integer. Each version is incremented by 1.
    versionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTableVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'deleteTableVersion_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'deleteTableVersion_databaseName' - The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableName', 'deleteTableVersion_tableName' - The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
--
-- 'versionId', 'deleteTableVersion_versionId' - The ID of the table version to be deleted. A @VersionID@ is a string
-- representation of an integer. Each version is incremented by 1.
newDeleteTableVersion ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'versionId'
  Prelude.Text ->
  DeleteTableVersion
newDeleteTableVersion
  pDatabaseName_
  pTableName_
  pVersionId_ =
    DeleteTableVersion'
      { catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        versionId = pVersionId_
      }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
deleteTableVersion_catalogId :: Lens.Lens' DeleteTableVersion (Prelude.Maybe Prelude.Text)
deleteTableVersion_catalogId = Lens.lens (\DeleteTableVersion' {catalogId} -> catalogId) (\s@DeleteTableVersion' {} a -> s {catalogId = a} :: DeleteTableVersion)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
deleteTableVersion_databaseName :: Lens.Lens' DeleteTableVersion Prelude.Text
deleteTableVersion_databaseName = Lens.lens (\DeleteTableVersion' {databaseName} -> databaseName) (\s@DeleteTableVersion' {} a -> s {databaseName = a} :: DeleteTableVersion)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
deleteTableVersion_tableName :: Lens.Lens' DeleteTableVersion Prelude.Text
deleteTableVersion_tableName = Lens.lens (\DeleteTableVersion' {tableName} -> tableName) (\s@DeleteTableVersion' {} a -> s {tableName = a} :: DeleteTableVersion)

-- | The ID of the table version to be deleted. A @VersionID@ is a string
-- representation of an integer. Each version is incremented by 1.
deleteTableVersion_versionId :: Lens.Lens' DeleteTableVersion Prelude.Text
deleteTableVersion_versionId = Lens.lens (\DeleteTableVersion' {versionId} -> versionId) (\s@DeleteTableVersion' {} a -> s {versionId = a} :: DeleteTableVersion)

instance Core.AWSRequest DeleteTableVersion where
  type
    AWSResponse DeleteTableVersion =
      DeleteTableVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteTableVersionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTableVersion where
  hashWithSalt _salt DeleteTableVersion' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData DeleteTableVersion where
  rnf DeleteTableVersion' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf versionId

instance Data.ToHeaders DeleteTableVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.DeleteTableVersion" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteTableVersion where
  toJSON DeleteTableVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("VersionId" Data..= versionId)
          ]
      )

instance Data.ToPath DeleteTableVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteTableVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteTableVersionResponse' smart constructor.
data DeleteTableVersionResponse = DeleteTableVersionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTableVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteTableVersionResponse_httpStatus' - The response's http status code.
newDeleteTableVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteTableVersionResponse
newDeleteTableVersionResponse pHttpStatus_ =
  DeleteTableVersionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteTableVersionResponse_httpStatus :: Lens.Lens' DeleteTableVersionResponse Prelude.Int
deleteTableVersionResponse_httpStatus = Lens.lens (\DeleteTableVersionResponse' {httpStatus} -> httpStatus) (\s@DeleteTableVersionResponse' {} a -> s {httpStatus = a} :: DeleteTableVersionResponse)

instance Prelude.NFData DeleteTableVersionResponse where
  rnf DeleteTableVersionResponse' {..} =
    Prelude.rnf httpStatus
