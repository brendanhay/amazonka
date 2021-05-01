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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newBatchDeleteTableVersion' smart constructor.
data BatchDeleteTableVersion = BatchDeleteTableVersion'
  { -- | The ID of the Data Catalog where the tables reside. If none is provided,
    -- the AWS account ID is used by default.
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- the AWS account ID is used by default.
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
batchDeleteTableVersion_versionIds = Lens.lens (\BatchDeleteTableVersion' {versionIds} -> versionIds) (\s@BatchDeleteTableVersion' {} a -> s {versionIds = a} :: BatchDeleteTableVersion) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest BatchDeleteTableVersion where
  type
    Rs BatchDeleteTableVersion =
      BatchDeleteTableVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchDeleteTableVersionResponse'
            Prelude.<$> (x Prelude..?> "Errors" Prelude..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchDeleteTableVersion

instance Prelude.NFData BatchDeleteTableVersion

instance Prelude.ToHeaders BatchDeleteTableVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AWSGlue.BatchDeleteTableVersion" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON BatchDeleteTableVersion where
  toJSON BatchDeleteTableVersion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            Prelude.Just
              ("DatabaseName" Prelude..= databaseName),
            Prelude.Just ("TableName" Prelude..= tableName),
            Prelude.Just ("VersionIds" Prelude..= versionIds)
          ]
      )

instance Prelude.ToPath BatchDeleteTableVersion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery BatchDeleteTableVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchDeleteTableVersionResponse' smart constructor.
data BatchDeleteTableVersionResponse = BatchDeleteTableVersionResponse'
  { -- | A list of errors encountered while trying to delete the specified table
    -- versions.
    errors :: Prelude.Maybe [TableVersionError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
batchDeleteTableVersionResponse_errors = Lens.lens (\BatchDeleteTableVersionResponse' {errors} -> errors) (\s@BatchDeleteTableVersionResponse' {} a -> s {errors = a} :: BatchDeleteTableVersionResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
batchDeleteTableVersionResponse_httpStatus :: Lens.Lens' BatchDeleteTableVersionResponse Prelude.Int
batchDeleteTableVersionResponse_httpStatus = Lens.lens (\BatchDeleteTableVersionResponse' {httpStatus} -> httpStatus) (\s@BatchDeleteTableVersionResponse' {} a -> s {httpStatus = a} :: BatchDeleteTableVersionResponse)

instance
  Prelude.NFData
    BatchDeleteTableVersionResponse
