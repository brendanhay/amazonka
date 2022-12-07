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
-- Module      : Amazonka.Glue.BatchDeleteTableVersion
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified batch of versions of a table.
module Amazonka.Glue.BatchDeleteTableVersion
  ( -- * Creating a Request
    BatchDeleteTableVersion (..),
    newBatchDeleteTableVersion,

    -- * Request Lenses
    batchDeleteTableVersion_catalogId,
    batchDeleteTableVersion_databaseName,
    batchDeleteTableVersion_tableName,
    batchDeleteTableVersion_versionIds,

    -- * Destructuring the Response
    BatchDeleteTableVersionResponse (..),
    newBatchDeleteTableVersionResponse,

    -- * Response Lenses
    batchDeleteTableVersionResponse_errors,
    batchDeleteTableVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Prelude.Text,
    -- | A list of the IDs of versions to be deleted. A @VersionId@ is a string
    -- representation of an integer. Each version is incremented by 1.
    versionIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteTableVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeleteTableVersion_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'batchDeleteTableVersion_databaseName' - The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableName', 'batchDeleteTableVersion_tableName' - The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
--
-- 'versionIds', 'batchDeleteTableVersion_versionIds' - A list of the IDs of versions to be deleted. A @VersionId@ is a string
-- representation of an integer. Each version is incremented by 1.
newBatchDeleteTableVersion ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  BatchDeleteTableVersion
newBatchDeleteTableVersion pDatabaseName_ pTableName_ =
  BatchDeleteTableVersion'
    { catalogId =
        Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      versionIds = Prelude.mempty
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the Amazon Web Services account ID is used by default.
batchDeleteTableVersion_catalogId :: Lens.Lens' BatchDeleteTableVersion (Prelude.Maybe Prelude.Text)
batchDeleteTableVersion_catalogId = Lens.lens (\BatchDeleteTableVersion' {catalogId} -> catalogId) (\s@BatchDeleteTableVersion' {} a -> s {catalogId = a} :: BatchDeleteTableVersion)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
batchDeleteTableVersion_databaseName :: Lens.Lens' BatchDeleteTableVersion Prelude.Text
batchDeleteTableVersion_databaseName = Lens.lens (\BatchDeleteTableVersion' {databaseName} -> databaseName) (\s@BatchDeleteTableVersion' {} a -> s {databaseName = a} :: BatchDeleteTableVersion)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
batchDeleteTableVersion_tableName :: Lens.Lens' BatchDeleteTableVersion Prelude.Text
batchDeleteTableVersion_tableName = Lens.lens (\BatchDeleteTableVersion' {tableName} -> tableName) (\s@BatchDeleteTableVersion' {} a -> s {tableName = a} :: BatchDeleteTableVersion)

-- | A list of the IDs of versions to be deleted. A @VersionId@ is a string
-- representation of an integer. Each version is incremented by 1.
batchDeleteTableVersion_versionIds :: Lens.Lens' BatchDeleteTableVersion [Prelude.Text]
batchDeleteTableVersion_versionIds = Lens.lens (\BatchDeleteTableVersion' {versionIds} -> versionIds) (\s@BatchDeleteTableVersion' {} a -> s {versionIds = a} :: BatchDeleteTableVersion) Prelude.. Lens.coerced

instance Core.AWSRequest BatchDeleteTableVersion where
  type
    AWSResponse BatchDeleteTableVersion =
      BatchDeleteTableVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableVersionResponse'
            Prelude.<$> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteTableVersion where
  hashWithSalt _salt BatchDeleteTableVersion' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` versionIds

instance Prelude.NFData BatchDeleteTableVersion where
  rnf BatchDeleteTableVersion' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf versionIds

instance Data.ToHeaders BatchDeleteTableVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.BatchDeleteTableVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchDeleteTableVersion where
  toJSON BatchDeleteTableVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Data..= databaseName),
            Prelude.Just ("TableName" Data..= tableName),
            Prelude.Just ("VersionIds" Data..= versionIds)
          ]
      )

instance Data.ToPath BatchDeleteTableVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchDeleteTableVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { -- | A list of errors encountered while trying to delete the specified table
    -- versions.
    errors :: Prelude.Maybe [TableVersionError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchDeleteTableVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errors', 'batchDeleteTableVersionResponse_errors' - A list of errors encountered while trying to delete the specified table
-- versions.
--
-- 'httpStatus', 'batchDeleteTableVersionResponse_httpStatus' - The response's http status code.
newBatchDeleteTableVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchDeleteTableVersionResponse
newBatchDeleteTableVersionResponse pHttpStatus_ =
  BatchDeleteTableVersionResponse'
    { errors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors encountered while trying to delete the specified table
-- versions.
batchDeleteTableVersionResponse_errors :: Lens.Lens' BatchDeleteTableVersionResponse (Prelude.Maybe [TableVersionError])
batchDeleteTableVersionResponse_errors = Lens.lens (\BatchDeleteTableVersionResponse' {errors} -> errors) (\s@BatchDeleteTableVersionResponse' {} a -> s {errors = a} :: BatchDeleteTableVersionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchDeleteTableVersionResponse_httpStatus :: Lens.Lens' BatchDeleteTableVersionResponse Prelude.Int
batchDeleteTableVersionResponse_httpStatus = Lens.lens (\BatchDeleteTableVersionResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteTableVersionResponse' {} a -> s {httpStatus = a} :: BatchDeleteTableVersionResponse)

instance
  Prelude.NFData
    BatchDeleteTableVersionResponse
  where
  rnf BatchDeleteTableVersionResponse' {..} =
    Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
