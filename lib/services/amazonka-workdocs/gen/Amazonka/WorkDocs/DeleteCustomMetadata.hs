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
-- Module      : Amazonka.WorkDocs.DeleteCustomMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes custom metadata from the specified resource.
module Amazonka.WorkDocs.DeleteCustomMetadata
  ( -- * Creating a Request
    DeleteCustomMetadata (..),
    newDeleteCustomMetadata,

    -- * Request Lenses
    deleteCustomMetadata_authenticationToken,
    deleteCustomMetadata_deleteAll,
    deleteCustomMetadata_keys,
    deleteCustomMetadata_versionId,
    deleteCustomMetadata_resourceId,

    -- * Destructuring the Response
    DeleteCustomMetadataResponse (..),
    newDeleteCustomMetadataResponse,

    -- * Response Lenses
    deleteCustomMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkDocs.Types

-- | /See:/ 'newDeleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Flag to indicate removal of all custom metadata properties from the
    -- specified resource.
    deleteAll :: Prelude.Maybe Prelude.Bool,
    -- | List of properties to remove.
    keys :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the version, if the custom metadata is being deleted from a
    -- document version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the resource, either a document or folder.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationToken', 'deleteCustomMetadata_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'deleteAll', 'deleteCustomMetadata_deleteAll' - Flag to indicate removal of all custom metadata properties from the
-- specified resource.
--
-- 'keys', 'deleteCustomMetadata_keys' - List of properties to remove.
--
-- 'versionId', 'deleteCustomMetadata_versionId' - The ID of the version, if the custom metadata is being deleted from a
-- document version.
--
-- 'resourceId', 'deleteCustomMetadata_resourceId' - The ID of the resource, either a document or folder.
newDeleteCustomMetadata ::
  -- | 'resourceId'
  Prelude.Text ->
  DeleteCustomMetadata
newDeleteCustomMetadata pResourceId_ =
  DeleteCustomMetadata'
    { authenticationToken =
        Prelude.Nothing,
      deleteAll = Prelude.Nothing,
      keys = Prelude.Nothing,
      versionId = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteCustomMetadata_authenticationToken :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe Prelude.Text)
deleteCustomMetadata_authenticationToken = Lens.lens (\DeleteCustomMetadata' {authenticationToken} -> authenticationToken) (\s@DeleteCustomMetadata' {} a -> s {authenticationToken = a} :: DeleteCustomMetadata) Prelude.. Lens.mapping Data._Sensitive

-- | Flag to indicate removal of all custom metadata properties from the
-- specified resource.
deleteCustomMetadata_deleteAll :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe Prelude.Bool)
deleteCustomMetadata_deleteAll = Lens.lens (\DeleteCustomMetadata' {deleteAll} -> deleteAll) (\s@DeleteCustomMetadata' {} a -> s {deleteAll = a} :: DeleteCustomMetadata)

-- | List of properties to remove.
deleteCustomMetadata_keys :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe [Prelude.Text])
deleteCustomMetadata_keys = Lens.lens (\DeleteCustomMetadata' {keys} -> keys) (\s@DeleteCustomMetadata' {} a -> s {keys = a} :: DeleteCustomMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the version, if the custom metadata is being deleted from a
-- document version.
deleteCustomMetadata_versionId :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe Prelude.Text)
deleteCustomMetadata_versionId = Lens.lens (\DeleteCustomMetadata' {versionId} -> versionId) (\s@DeleteCustomMetadata' {} a -> s {versionId = a} :: DeleteCustomMetadata)

-- | The ID of the resource, either a document or folder.
deleteCustomMetadata_resourceId :: Lens.Lens' DeleteCustomMetadata Prelude.Text
deleteCustomMetadata_resourceId = Lens.lens (\DeleteCustomMetadata' {resourceId} -> resourceId) (\s@DeleteCustomMetadata' {} a -> s {resourceId = a} :: DeleteCustomMetadata)

instance Core.AWSRequest DeleteCustomMetadata where
  type
    AWSResponse DeleteCustomMetadata =
      DeleteCustomMetadataResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomMetadata where
  hashWithSalt _salt DeleteCustomMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` authenticationToken
      `Prelude.hashWithSalt` deleteAll
      `Prelude.hashWithSalt` keys
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DeleteCustomMetadata where
  rnf DeleteCustomMetadata' {..} =
    Prelude.rnf authenticationToken `Prelude.seq`
      Prelude.rnf deleteAll `Prelude.seq`
        Prelude.rnf keys `Prelude.seq`
          Prelude.rnf versionId `Prelude.seq`
            Prelude.rnf resourceId

instance Data.ToHeaders DeleteCustomMetadata where
  toHeaders DeleteCustomMetadata' {..} =
    Prelude.mconcat
      [ "Authentication" Data.=# authenticationToken,
        "Content-Type"
          Data.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Data.ToPath DeleteCustomMetadata where
  toPath DeleteCustomMetadata' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Data.toBS resourceId,
        "/customMetadata"
      ]

instance Data.ToQuery DeleteCustomMetadata where
  toQuery DeleteCustomMetadata' {..} =
    Prelude.mconcat
      [ "deleteAll" Data.=: deleteAll,
        "keys"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> keys),
        "versionId" Data.=: versionId
      ]

-- | /See:/ 'newDeleteCustomMetadataResponse' smart constructor.
data DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteCustomMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteCustomMetadataResponse_httpStatus' - The response's http status code.
newDeleteCustomMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteCustomMetadataResponse
newDeleteCustomMetadataResponse pHttpStatus_ =
  DeleteCustomMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCustomMetadataResponse_httpStatus :: Lens.Lens' DeleteCustomMetadataResponse Prelude.Int
deleteCustomMetadataResponse_httpStatus = Lens.lens (\DeleteCustomMetadataResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomMetadataResponse' {} a -> s {httpStatus = a} :: DeleteCustomMetadataResponse)

instance Prelude.NFData DeleteCustomMetadataResponse where
  rnf DeleteCustomMetadataResponse' {..} =
    Prelude.rnf httpStatus
