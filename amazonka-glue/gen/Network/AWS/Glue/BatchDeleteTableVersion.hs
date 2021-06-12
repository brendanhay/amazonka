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
-- Module      : Network.AWS.Glue.BatchDeleteTableVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a specified batch of versions of a table.
module Network.AWS.Glue.BatchDeleteTableVersion
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

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The database in the catalog in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Core.Text,
    -- | The name of the table. For Hive compatibility, this name is entirely
    -- lowercase.
    tableName :: Core.Text,
    -- | A list of the IDs of versions to be deleted. A @VersionId@ is a string
    -- representation of an integer. Each version is incremented by 1.
    versionIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'BatchDeleteTableVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'batchDeleteTableVersion_catalogId' - The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
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
  Core.Text ->
  -- | 'tableName'
  Core.Text ->
  BatchDeleteTableVersion
newBatchDeleteTableVersion pDatabaseName_ pTableName_ =
  BatchDeleteTableVersion'
    { catalogId = Core.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_,
      versionIds = Core.mempty
    }

-- | The ID of the Data Catalog where the tables reside. If none is provided,
-- the AWS account ID is used by default.
batchDeleteTableVersion_catalogId :: Lens.Lens' BatchDeleteTableVersion (Core.Maybe Core.Text)
batchDeleteTableVersion_catalogId = Lens.lens (\BatchDeleteTableVersion' {catalogId} -> catalogId) (\s@BatchDeleteTableVersion' {} a -> s {catalogId = a} :: BatchDeleteTableVersion)

-- | The database in the catalog in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
batchDeleteTableVersion_databaseName :: Lens.Lens' BatchDeleteTableVersion Core.Text
batchDeleteTableVersion_databaseName = Lens.lens (\BatchDeleteTableVersion' {databaseName} -> databaseName) (\s@BatchDeleteTableVersion' {} a -> s {databaseName = a} :: BatchDeleteTableVersion)

-- | The name of the table. For Hive compatibility, this name is entirely
-- lowercase.
batchDeleteTableVersion_tableName :: Lens.Lens' BatchDeleteTableVersion Core.Text
batchDeleteTableVersion_tableName = Lens.lens (\BatchDeleteTableVersion' {tableName} -> tableName) (\s@BatchDeleteTableVersion' {} a -> s {tableName = a} :: BatchDeleteTableVersion)

-- | A list of the IDs of versions to be deleted. A @VersionId@ is a string
-- representation of an integer. Each version is incremented by 1.
batchDeleteTableVersion_versionIds :: Lens.Lens' BatchDeleteTableVersion [Core.Text]
batchDeleteTableVersion_versionIds = Lens.lens (\BatchDeleteTableVersion' {versionIds} -> versionIds) (\s@BatchDeleteTableVersion' {} a -> s {versionIds = a} :: BatchDeleteTableVersion) Core.. Lens._Coerce

instance Core.AWSRequest BatchDeleteTableVersion where
  type
    AWSResponse BatchDeleteTableVersion =
      BatchDeleteTableVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableVersionResponse'
            Core.<$> (x Core..?> "Errors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable BatchDeleteTableVersion

instance Core.NFData BatchDeleteTableVersion

instance Core.ToHeaders BatchDeleteTableVersion where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSGlue.BatchDeleteTableVersion" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON BatchDeleteTableVersion where
  toJSON BatchDeleteTableVersion' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CatalogId" Core..=) Core.<$> catalogId,
            Core.Just ("DatabaseName" Core..= databaseName),
            Core.Just ("TableName" Core..= tableName),
            Core.Just ("VersionIds" Core..= versionIds)
          ]
      )

instance Core.ToPath BatchDeleteTableVersion where
  toPath = Core.const "/"

instance Core.ToQuery BatchDeleteTableVersion where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newBatchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { -- | A list of errors encountered while trying to delete the specified table
    -- versions.
    errors :: Core.Maybe [TableVersionError],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  BatchDeleteTableVersionResponse
newBatchDeleteTableVersionResponse pHttpStatus_ =
  BatchDeleteTableVersionResponse'
    { errors =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of errors encountered while trying to delete the specified table
-- versions.
batchDeleteTableVersionResponse_errors :: Lens.Lens' BatchDeleteTableVersionResponse (Core.Maybe [TableVersionError])
batchDeleteTableVersionResponse_errors = Lens.lens (\BatchDeleteTableVersionResponse' {errors} -> errors) (\s@BatchDeleteTableVersionResponse' {} a -> s {errors = a} :: BatchDeleteTableVersionResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
batchDeleteTableVersionResponse_httpStatus :: Lens.Lens' BatchDeleteTableVersionResponse Core.Int
batchDeleteTableVersionResponse_httpStatus = Lens.lens (\BatchDeleteTableVersionResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteTableVersionResponse' {} a -> s {httpStatus = a} :: BatchDeleteTableVersionResponse)

instance Core.NFData BatchDeleteTableVersionResponse
