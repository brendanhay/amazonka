{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of a document. The user must have access to both the document and its parent folder, if applicable.
module Network.AWS.WorkDocs.UpdateDocument
  ( -- * Creating a request
    UpdateDocument (..),
    mkUpdateDocument,

    -- ** Request lenses
    udParentFolderId,
    udDocumentId,
    udAuthenticationToken,
    udName,
    udResourceState,

    -- * Destructuring the response
    UpdateDocumentResponse (..),
    mkUpdateDocumentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | The ID of the parent folder.
    parentFolderId :: Lude.Maybe Lude.Text,
    -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The name of the document.
    name :: Lude.Maybe Lude.Text,
    -- | The resource state of the document. Only ACTIVE and RECYCLED are supported.
    resourceState :: Lude.Maybe ResourceStateType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocument' with the minimum fields required to make a request.
--
-- * 'parentFolderId' - The ID of the parent folder.
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'name' - The name of the document.
-- * 'resourceState' - The resource state of the document. Only ACTIVE and RECYCLED are supported.
mkUpdateDocument ::
  -- | 'documentId'
  Lude.Text ->
  UpdateDocument
mkUpdateDocument pDocumentId_ =
  UpdateDocument'
    { parentFolderId = Lude.Nothing,
      documentId = pDocumentId_,
      authenticationToken = Lude.Nothing,
      name = Lude.Nothing,
      resourceState = Lude.Nothing
    }

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udParentFolderId :: Lens.Lens' UpdateDocument (Lude.Maybe Lude.Text)
udParentFolderId = Lens.lens (parentFolderId :: UpdateDocument -> Lude.Maybe Lude.Text) (\s a -> s {parentFolderId = a} :: UpdateDocument)
{-# DEPRECATED udParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udDocumentId :: Lens.Lens' UpdateDocument Lude.Text
udDocumentId = Lens.lens (documentId :: UpdateDocument -> Lude.Text) (\s a -> s {documentId = a} :: UpdateDocument)
{-# DEPRECATED udDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udAuthenticationToken :: Lens.Lens' UpdateDocument (Lude.Maybe (Lude.Sensitive Lude.Text))
udAuthenticationToken = Lens.lens (authenticationToken :: UpdateDocument -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: UpdateDocument)
{-# DEPRECATED udAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udName :: Lens.Lens' UpdateDocument (Lude.Maybe Lude.Text)
udName = Lens.lens (name :: UpdateDocument -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateDocument)
{-# DEPRECATED udName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The resource state of the document. Only ACTIVE and RECYCLED are supported.
--
-- /Note:/ Consider using 'resourceState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udResourceState :: Lens.Lens' UpdateDocument (Lude.Maybe ResourceStateType)
udResourceState = Lens.lens (resourceState :: UpdateDocument -> Lude.Maybe ResourceStateType) (\s a -> s {resourceState = a} :: UpdateDocument)
{-# DEPRECATED udResourceState "Use generic-lens or generic-optics with 'resourceState' instead." #-}

instance Lude.AWSRequest UpdateDocument where
  type Rs UpdateDocument = UpdateDocumentResponse
  request = Req.patchJSON workDocsService
  response = Res.receiveNull UpdateDocumentResponse'

instance Lude.ToHeaders UpdateDocument where
  toHeaders UpdateDocument' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON UpdateDocument where
  toJSON UpdateDocument' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ParentFolderId" Lude..=) Lude.<$> parentFolderId,
            ("Name" Lude..=) Lude.<$> name,
            ("ResourceState" Lude..=) Lude.<$> resourceState
          ]
      )

instance Lude.ToPath UpdateDocument where
  toPath UpdateDocument' {..} =
    Lude.mconcat ["/api/v1/documents/", Lude.toBS documentId]

instance Lude.ToQuery UpdateDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentResponse' with the minimum fields required to make a request.
mkUpdateDocumentResponse ::
  UpdateDocumentResponse
mkUpdateDocumentResponse = UpdateDocumentResponse'
