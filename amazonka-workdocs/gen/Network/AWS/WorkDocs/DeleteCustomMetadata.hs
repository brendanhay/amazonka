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
-- Module      : Network.AWS.WorkDocs.DeleteCustomMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes custom metadata from the specified resource.
module Network.AWS.WorkDocs.DeleteCustomMetadata
  ( -- * Creating a Request
    DeleteCustomMetadata (..),
    newDeleteCustomMetadata,

    -- * Request Lenses
    deleteCustomMetadata_versionId,
    deleteCustomMetadata_keys,
    deleteCustomMetadata_authenticationToken,
    deleteCustomMetadata_deleteAll,
    deleteCustomMetadata_resourceId,

    -- * Destructuring the Response
    DeleteCustomMetadataResponse (..),
    newDeleteCustomMetadataResponse,

    -- * Response Lenses
    deleteCustomMetadataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { -- | The ID of the version, if the custom metadata is being deleted from a
    -- document version.
    versionId :: Core.Maybe Core.Text,
    -- | List of properties to remove.
    keys :: Core.Maybe [Core.Text],
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | Flag to indicate removal of all custom metadata properties from the
    -- specified resource.
    deleteAll :: Core.Maybe Core.Bool,
    -- | The ID of the resource, either a document or folder.
    resourceId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCustomMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'deleteCustomMetadata_versionId' - The ID of the version, if the custom metadata is being deleted from a
-- document version.
--
-- 'keys', 'deleteCustomMetadata_keys' - List of properties to remove.
--
-- 'authenticationToken', 'deleteCustomMetadata_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'deleteAll', 'deleteCustomMetadata_deleteAll' - Flag to indicate removal of all custom metadata properties from the
-- specified resource.
--
-- 'resourceId', 'deleteCustomMetadata_resourceId' - The ID of the resource, either a document or folder.
newDeleteCustomMetadata ::
  -- | 'resourceId'
  Core.Text ->
  DeleteCustomMetadata
newDeleteCustomMetadata pResourceId_ =
  DeleteCustomMetadata'
    { versionId = Core.Nothing,
      keys = Core.Nothing,
      authenticationToken = Core.Nothing,
      deleteAll = Core.Nothing,
      resourceId = pResourceId_
    }

-- | The ID of the version, if the custom metadata is being deleted from a
-- document version.
deleteCustomMetadata_versionId :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Core.Text)
deleteCustomMetadata_versionId = Lens.lens (\DeleteCustomMetadata' {versionId} -> versionId) (\s@DeleteCustomMetadata' {} a -> s {versionId = a} :: DeleteCustomMetadata)

-- | List of properties to remove.
deleteCustomMetadata_keys :: Lens.Lens' DeleteCustomMetadata (Core.Maybe [Core.Text])
deleteCustomMetadata_keys = Lens.lens (\DeleteCustomMetadata' {keys} -> keys) (\s@DeleteCustomMetadata' {} a -> s {keys = a} :: DeleteCustomMetadata) Core.. Lens.mapping Lens._Coerce

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteCustomMetadata_authenticationToken :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Core.Text)
deleteCustomMetadata_authenticationToken = Lens.lens (\DeleteCustomMetadata' {authenticationToken} -> authenticationToken) (\s@DeleteCustomMetadata' {} a -> s {authenticationToken = a} :: DeleteCustomMetadata) Core.. Lens.mapping Core._Sensitive

-- | Flag to indicate removal of all custom metadata properties from the
-- specified resource.
deleteCustomMetadata_deleteAll :: Lens.Lens' DeleteCustomMetadata (Core.Maybe Core.Bool)
deleteCustomMetadata_deleteAll = Lens.lens (\DeleteCustomMetadata' {deleteAll} -> deleteAll) (\s@DeleteCustomMetadata' {} a -> s {deleteAll = a} :: DeleteCustomMetadata)

-- | The ID of the resource, either a document or folder.
deleteCustomMetadata_resourceId :: Lens.Lens' DeleteCustomMetadata Core.Text
deleteCustomMetadata_resourceId = Lens.lens (\DeleteCustomMetadata' {resourceId} -> resourceId) (\s@DeleteCustomMetadata' {} a -> s {resourceId = a} :: DeleteCustomMetadata)

instance Core.AWSRequest DeleteCustomMetadata where
  type
    AWSResponse DeleteCustomMetadata =
      DeleteCustomMetadataResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomMetadataResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCustomMetadata

instance Core.NFData DeleteCustomMetadata

instance Core.ToHeaders DeleteCustomMetadata where
  toHeaders DeleteCustomMetadata' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToPath DeleteCustomMetadata where
  toPath DeleteCustomMetadata' {..} =
    Core.mconcat
      [ "/api/v1/resources/",
        Core.toBS resourceId,
        "/customMetadata"
      ]

instance Core.ToQuery DeleteCustomMetadata where
  toQuery DeleteCustomMetadata' {..} =
    Core.mconcat
      [ "versionId" Core.=: versionId,
        "keys"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> keys),
        "deleteAll" Core.=: deleteAll
      ]

-- | /See:/ 'newDeleteCustomMetadataResponse' smart constructor.
data DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DeleteCustomMetadataResponse
newDeleteCustomMetadataResponse pHttpStatus_ =
  DeleteCustomMetadataResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteCustomMetadataResponse_httpStatus :: Lens.Lens' DeleteCustomMetadataResponse Core.Int
deleteCustomMetadataResponse_httpStatus = Lens.lens (\DeleteCustomMetadataResponse' {httpStatus} -> httpStatus) (\s@DeleteCustomMetadataResponse' {} a -> s {httpStatus = a} :: DeleteCustomMetadataResponse)

instance Core.NFData DeleteCustomMetadataResponse
