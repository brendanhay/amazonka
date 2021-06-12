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
-- Module      : Network.AWS.WorkDocs.UpdateDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of a document. The user must have
-- access to both the document and its parent folder, if applicable.
module Network.AWS.WorkDocs.UpdateDocument
  ( -- * Creating a Request
    UpdateDocument (..),
    newUpdateDocument,

    -- * Request Lenses
    updateDocument_parentFolderId,
    updateDocument_name,
    updateDocument_authenticationToken,
    updateDocument_resourceState,
    updateDocument_documentId,

    -- * Destructuring the Response
    UpdateDocumentResponse (..),
    newUpdateDocumentResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | The ID of the parent folder.
    parentFolderId :: Core.Maybe Core.Text,
    -- | The name of the document.
    name :: Core.Maybe Core.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The resource state of the document. Only ACTIVE and RECYCLED are
    -- supported.
    resourceState :: Core.Maybe ResourceStateType,
    -- | The ID of the document.
    documentId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentFolderId', 'updateDocument_parentFolderId' - The ID of the parent folder.
--
-- 'name', 'updateDocument_name' - The name of the document.
--
-- 'authenticationToken', 'updateDocument_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'resourceState', 'updateDocument_resourceState' - The resource state of the document. Only ACTIVE and RECYCLED are
-- supported.
--
-- 'documentId', 'updateDocument_documentId' - The ID of the document.
newUpdateDocument ::
  -- | 'documentId'
  Core.Text ->
  UpdateDocument
newUpdateDocument pDocumentId_ =
  UpdateDocument'
    { parentFolderId = Core.Nothing,
      name = Core.Nothing,
      authenticationToken = Core.Nothing,
      resourceState = Core.Nothing,
      documentId = pDocumentId_
    }

-- | The ID of the parent folder.
updateDocument_parentFolderId :: Lens.Lens' UpdateDocument (Core.Maybe Core.Text)
updateDocument_parentFolderId = Lens.lens (\UpdateDocument' {parentFolderId} -> parentFolderId) (\s@UpdateDocument' {} a -> s {parentFolderId = a} :: UpdateDocument)

-- | The name of the document.
updateDocument_name :: Lens.Lens' UpdateDocument (Core.Maybe Core.Text)
updateDocument_name = Lens.lens (\UpdateDocument' {name} -> name) (\s@UpdateDocument' {} a -> s {name = a} :: UpdateDocument)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
updateDocument_authenticationToken :: Lens.Lens' UpdateDocument (Core.Maybe Core.Text)
updateDocument_authenticationToken = Lens.lens (\UpdateDocument' {authenticationToken} -> authenticationToken) (\s@UpdateDocument' {} a -> s {authenticationToken = a} :: UpdateDocument) Core.. Lens.mapping Core._Sensitive

-- | The resource state of the document. Only ACTIVE and RECYCLED are
-- supported.
updateDocument_resourceState :: Lens.Lens' UpdateDocument (Core.Maybe ResourceStateType)
updateDocument_resourceState = Lens.lens (\UpdateDocument' {resourceState} -> resourceState) (\s@UpdateDocument' {} a -> s {resourceState = a} :: UpdateDocument)

-- | The ID of the document.
updateDocument_documentId :: Lens.Lens' UpdateDocument Core.Text
updateDocument_documentId = Lens.lens (\UpdateDocument' {documentId} -> documentId) (\s@UpdateDocument' {} a -> s {documentId = a} :: UpdateDocument)

instance Core.AWSRequest UpdateDocument where
  type
    AWSResponse UpdateDocument =
      UpdateDocumentResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveNull UpdateDocumentResponse'

instance Core.Hashable UpdateDocument

instance Core.NFData UpdateDocument

instance Core.ToHeaders UpdateDocument where
  toHeaders UpdateDocument' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON UpdateDocument where
  toJSON UpdateDocument' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ParentFolderId" Core..=) Core.<$> parentFolderId,
            ("Name" Core..=) Core.<$> name,
            ("ResourceState" Core..=) Core.<$> resourceState
          ]
      )

instance Core.ToPath UpdateDocument where
  toPath UpdateDocument' {..} =
    Core.mconcat
      ["/api/v1/documents/", Core.toBS documentId]

instance Core.ToQuery UpdateDocument where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateDocumentResponse ::
  UpdateDocumentResponse
newUpdateDocumentResponse = UpdateDocumentResponse'

instance Core.NFData UpdateDocumentResponse
