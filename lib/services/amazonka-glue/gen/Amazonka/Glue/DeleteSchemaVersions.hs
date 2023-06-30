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
-- Module      : Amazonka.Glue.DeleteSchemaVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove versions from the specified schema. A version number or range may
-- be supplied. If the compatibility mode forbids deleting of a version
-- that is necessary, such as BACKWARDS_FULL, an error is returned. Calling
-- the @GetSchemaVersions@ API after this call will list the status of the
-- deleted versions.
--
-- When the range of version numbers contain check pointed version, the API
-- will return a 409 conflict and will not proceed with the deletion. You
-- have to remove the checkpoint first using the @DeleteSchemaCheckpoint@
-- API before using this API.
--
-- You cannot use the @DeleteSchemaVersions@ API to delete the first schema
-- version in the schema set. The first schema version can only be deleted
-- by the @DeleteSchema@ API. This operation will also delete the attached
-- @SchemaVersionMetadata@ under the schema versions. Hard deletes will be
-- enforced on the database.
--
-- If the compatibility mode forbids deleting of a version that is
-- necessary, such as BACKWARDS_FULL, an error is returned.
module Amazonka.Glue.DeleteSchemaVersions
  ( -- * Creating a Request
    DeleteSchemaVersions (..),
    newDeleteSchemaVersions,

    -- * Request Lenses
    deleteSchemaVersions_schemaId,
    deleteSchemaVersions_versions,

    -- * Destructuring the Response
    DeleteSchemaVersionsResponse (..),
    newDeleteSchemaVersionsResponse,

    -- * Response Lenses
    deleteSchemaVersionsResponse_schemaVersionErrors,
    deleteSchemaVersionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteSchemaVersions' smart constructor.
data DeleteSchemaVersions = DeleteSchemaVersions'
  { -- | This is a wrapper structure that may contain the schema name and Amazon
    -- Resource Name (ARN).
    schemaId :: SchemaId,
    -- | A version range may be supplied which may be of the format:
    --
    -- -   a single version number, 5
    --
    -- -   a range, 5-8 : deletes versions 5, 6, 7, 8
    versions :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchemaVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaId', 'deleteSchemaVersions_schemaId' - This is a wrapper structure that may contain the schema name and Amazon
-- Resource Name (ARN).
--
-- 'versions', 'deleteSchemaVersions_versions' - A version range may be supplied which may be of the format:
--
-- -   a single version number, 5
--
-- -   a range, 5-8 : deletes versions 5, 6, 7, 8
newDeleteSchemaVersions ::
  -- | 'schemaId'
  SchemaId ->
  -- | 'versions'
  Prelude.Text ->
  DeleteSchemaVersions
newDeleteSchemaVersions pSchemaId_ pVersions_ =
  DeleteSchemaVersions'
    { schemaId = pSchemaId_,
      versions = pVersions_
    }

-- | This is a wrapper structure that may contain the schema name and Amazon
-- Resource Name (ARN).
deleteSchemaVersions_schemaId :: Lens.Lens' DeleteSchemaVersions SchemaId
deleteSchemaVersions_schemaId = Lens.lens (\DeleteSchemaVersions' {schemaId} -> schemaId) (\s@DeleteSchemaVersions' {} a -> s {schemaId = a} :: DeleteSchemaVersions)

-- | A version range may be supplied which may be of the format:
--
-- -   a single version number, 5
--
-- -   a range, 5-8 : deletes versions 5, 6, 7, 8
deleteSchemaVersions_versions :: Lens.Lens' DeleteSchemaVersions Prelude.Text
deleteSchemaVersions_versions = Lens.lens (\DeleteSchemaVersions' {versions} -> versions) (\s@DeleteSchemaVersions' {} a -> s {versions = a} :: DeleteSchemaVersions)

instance Core.AWSRequest DeleteSchemaVersions where
  type
    AWSResponse DeleteSchemaVersions =
      DeleteSchemaVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteSchemaVersionsResponse'
            Prelude.<$> ( x
                            Data..?> "SchemaVersionErrors"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSchemaVersions where
  hashWithSalt _salt DeleteSchemaVersions' {..} =
    _salt
      `Prelude.hashWithSalt` schemaId
      `Prelude.hashWithSalt` versions

instance Prelude.NFData DeleteSchemaVersions where
  rnf DeleteSchemaVersions' {..} =
    Prelude.rnf schemaId
      `Prelude.seq` Prelude.rnf versions

instance Data.ToHeaders DeleteSchemaVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.DeleteSchemaVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteSchemaVersions where
  toJSON DeleteSchemaVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SchemaId" Data..= schemaId),
            Prelude.Just ("Versions" Data..= versions)
          ]
      )

instance Data.ToPath DeleteSchemaVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteSchemaVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteSchemaVersionsResponse' smart constructor.
data DeleteSchemaVersionsResponse = DeleteSchemaVersionsResponse'
  { -- | A list of @SchemaVersionErrorItem@ objects, each containing an error and
    -- schema version.
    schemaVersionErrors :: Prelude.Maybe [SchemaVersionErrorItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSchemaVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaVersionErrors', 'deleteSchemaVersionsResponse_schemaVersionErrors' - A list of @SchemaVersionErrorItem@ objects, each containing an error and
-- schema version.
--
-- 'httpStatus', 'deleteSchemaVersionsResponse_httpStatus' - The response's http status code.
newDeleteSchemaVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSchemaVersionsResponse
newDeleteSchemaVersionsResponse pHttpStatus_ =
  DeleteSchemaVersionsResponse'
    { schemaVersionErrors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of @SchemaVersionErrorItem@ objects, each containing an error and
-- schema version.
deleteSchemaVersionsResponse_schemaVersionErrors :: Lens.Lens' DeleteSchemaVersionsResponse (Prelude.Maybe [SchemaVersionErrorItem])
deleteSchemaVersionsResponse_schemaVersionErrors = Lens.lens (\DeleteSchemaVersionsResponse' {schemaVersionErrors} -> schemaVersionErrors) (\s@DeleteSchemaVersionsResponse' {} a -> s {schemaVersionErrors = a} :: DeleteSchemaVersionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteSchemaVersionsResponse_httpStatus :: Lens.Lens' DeleteSchemaVersionsResponse Prelude.Int
deleteSchemaVersionsResponse_httpStatus = Lens.lens (\DeleteSchemaVersionsResponse' {httpStatus} -> httpStatus) (\s@DeleteSchemaVersionsResponse' {} a -> s {httpStatus = a} :: DeleteSchemaVersionsResponse)

instance Prelude.NFData DeleteSchemaVersionsResponse where
  rnf DeleteSchemaVersionsResponse' {..} =
    Prelude.rnf schemaVersionErrors
      `Prelude.seq` Prelude.rnf httpStatus
