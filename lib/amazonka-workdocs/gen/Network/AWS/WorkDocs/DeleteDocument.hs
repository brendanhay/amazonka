{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.DeleteDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently deletes the specified document and its associated metadata.
module Network.AWS.WorkDocs.DeleteDocument
  ( -- * Creating a request
    DeleteDocument (..),
    mkDeleteDocument,

    -- ** Request lenses
    ddDocumentId,
    ddAuthenticationToken,

    -- * Destructuring the response
    DeleteDocumentResponse (..),
    mkDeleteDocumentResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkDeleteDocument' smart constructor.
data DeleteDocument = DeleteDocument'
  { -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocument' with the minimum fields required to make a request.
--
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
mkDeleteDocument ::
  -- | 'documentId'
  Lude.Text ->
  DeleteDocument
mkDeleteDocument pDocumentId_ =
  DeleteDocument'
    { documentId = pDocumentId_,
      authenticationToken = Lude.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDocumentId :: Lens.Lens' DeleteDocument Lude.Text
ddDocumentId = Lens.lens (documentId :: DeleteDocument -> Lude.Text) (\s a -> s {documentId = a} :: DeleteDocument)
{-# DEPRECATED ddDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddAuthenticationToken :: Lens.Lens' DeleteDocument (Lude.Maybe (Lude.Sensitive Lude.Text))
ddAuthenticationToken = Lens.lens (authenticationToken :: DeleteDocument -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: DeleteDocument)
{-# DEPRECATED ddAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Lude.AWSRequest DeleteDocument where
  type Rs DeleteDocument = DeleteDocumentResponse
  request = Req.delete workDocsService
  response = Res.receiveNull DeleteDocumentResponse'

instance Lude.ToHeaders DeleteDocument where
  toHeaders DeleteDocument' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath DeleteDocument where
  toPath DeleteDocument' {..} =
    Lude.mconcat ["/api/v1/documents/", Lude.toBS documentId]

instance Lude.ToQuery DeleteDocument where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDocumentResponse' smart constructor.
data DeleteDocumentResponse = DeleteDocumentResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDocumentResponse' with the minimum fields required to make a request.
mkDeleteDocumentResponse ::
  DeleteDocumentResponse
mkDeleteDocumentResponse = DeleteDocumentResponse'
