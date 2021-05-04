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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newDeleteCustomMetadata' smart constructor.
data DeleteCustomMetadata = DeleteCustomMetadata'
  { -- | The ID of the version, if the custom metadata is being deleted from a
    -- document version.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | List of properties to remove.
    keys :: Prelude.Maybe [Prelude.Text],
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | Flag to indicate removal of all custom metadata properties from the
    -- specified resource.
    deleteAll :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the resource, either a document or folder.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteCustomMetadata
newDeleteCustomMetadata pResourceId_ =
  DeleteCustomMetadata'
    { versionId = Prelude.Nothing,
      keys = Prelude.Nothing,
      authenticationToken = Prelude.Nothing,
      deleteAll = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The ID of the version, if the custom metadata is being deleted from a
-- document version.
deleteCustomMetadata_versionId :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe Prelude.Text)
deleteCustomMetadata_versionId = Lens.lens (\DeleteCustomMetadata' {versionId} -> versionId) (\s@DeleteCustomMetadata' {} a -> s {versionId = a} :: DeleteCustomMetadata)

-- | List of properties to remove.
deleteCustomMetadata_keys :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe [Prelude.Text])
deleteCustomMetadata_keys = Lens.lens (\DeleteCustomMetadata' {keys} -> keys) (\s@DeleteCustomMetadata' {} a -> s {keys = a} :: DeleteCustomMetadata) Prelude.. Lens.mapping Prelude._Coerce

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
deleteCustomMetadata_authenticationToken :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe Prelude.Text)
deleteCustomMetadata_authenticationToken = Lens.lens (\DeleteCustomMetadata' {authenticationToken} -> authenticationToken) (\s@DeleteCustomMetadata' {} a -> s {authenticationToken = a} :: DeleteCustomMetadata) Prelude.. Lens.mapping Prelude._Sensitive

-- | Flag to indicate removal of all custom metadata properties from the
-- specified resource.
deleteCustomMetadata_deleteAll :: Lens.Lens' DeleteCustomMetadata (Prelude.Maybe Prelude.Bool)
deleteCustomMetadata_deleteAll = Lens.lens (\DeleteCustomMetadata' {deleteAll} -> deleteAll) (\s@DeleteCustomMetadata' {} a -> s {deleteAll = a} :: DeleteCustomMetadata)

-- | The ID of the resource, either a document or folder.
deleteCustomMetadata_resourceId :: Lens.Lens' DeleteCustomMetadata Prelude.Text
deleteCustomMetadata_resourceId = Lens.lens (\DeleteCustomMetadata' {resourceId} -> resourceId) (\s@DeleteCustomMetadata' {} a -> s {resourceId = a} :: DeleteCustomMetadata)

instance Prelude.AWSRequest DeleteCustomMetadata where
  type
    Rs DeleteCustomMetadata =
      DeleteCustomMetadataResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteCustomMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteCustomMetadata

instance Prelude.NFData DeleteCustomMetadata

instance Prelude.ToHeaders DeleteCustomMetadata where
  toHeaders DeleteCustomMetadata' {..} =
    Prelude.mconcat
      [ "Authentication" Prelude.=# authenticationToken,
        "Content-Type"
          Prelude.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Prelude.ToPath DeleteCustomMetadata where
  toPath DeleteCustomMetadata' {..} =
    Prelude.mconcat
      [ "/api/v1/resources/",
        Prelude.toBS resourceId,
        "/customMetadata"
      ]

instance Prelude.ToQuery DeleteCustomMetadata where
  toQuery DeleteCustomMetadata' {..} =
    Prelude.mconcat
      [ "versionId" Prelude.=: versionId,
        "keys"
          Prelude.=: Prelude.toQuery
            (Prelude.toQueryList "member" Prelude.<$> keys),
        "deleteAll" Prelude.=: deleteAll
      ]

-- | /See:/ 'newDeleteCustomMetadataResponse' smart constructor.
data DeleteCustomMetadataResponse = DeleteCustomMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData DeleteCustomMetadataResponse
