{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetDocument
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves details of a document.
module Network.AWS.WorkDocs.GetDocument
  ( -- * Creating a request
    GetDocument (..),
    mkGetDocument,

    -- ** Request lenses
    gdDocumentId,
    gdAuthenticationToken,
    gdIncludeCustomMetadata,

    -- * Destructuring the response
    GetDocumentResponse (..),
    mkGetDocumentResponse,

    -- ** Response lenses
    gdrsCustomMetadata,
    gdrsMetadata,
    gdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Set this to @TRUE@ to include custom metadata in the response.
    includeCustomMetadata :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocument' with the minimum fields required to make a request.
--
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'includeCustomMetadata' - Set this to @TRUE@ to include custom metadata in the response.
mkGetDocument ::
  -- | 'documentId'
  Lude.Text ->
  GetDocument
mkGetDocument pDocumentId_ =
  GetDocument'
    { documentId = pDocumentId_,
      authenticationToken = Lude.Nothing,
      includeCustomMetadata = Lude.Nothing
    }

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdDocumentId :: Lens.Lens' GetDocument Lude.Text
gdDocumentId = Lens.lens (documentId :: GetDocument -> Lude.Text) (\s a -> s {documentId = a} :: GetDocument)
{-# DEPRECATED gdDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdAuthenticationToken :: Lens.Lens' GetDocument (Lude.Maybe (Lude.Sensitive Lude.Text))
gdAuthenticationToken = Lens.lens (authenticationToken :: GetDocument -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: GetDocument)
{-# DEPRECATED gdAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Set this to @TRUE@ to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdIncludeCustomMetadata :: Lens.Lens' GetDocument (Lude.Maybe Lude.Bool)
gdIncludeCustomMetadata = Lens.lens (includeCustomMetadata :: GetDocument -> Lude.Maybe Lude.Bool) (\s a -> s {includeCustomMetadata = a} :: GetDocument)
{-# DEPRECATED gdIncludeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead." #-}

instance Lude.AWSRequest GetDocument where
  type Rs GetDocument = GetDocumentResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Lude.<$> (x Lude..?> "CustomMetadata" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDocument where
  toHeaders GetDocument' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetDocument where
  toPath GetDocument' {..} =
    Lude.mconcat ["/api/v1/documents/", Lude.toBS documentId]

instance Lude.ToQuery GetDocument where
  toQuery GetDocument' {..} =
    Lude.mconcat
      ["includeCustomMetadata" Lude.=: includeCustomMetadata]

-- | /See:/ 'mkGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | The custom metadata on the document.
    customMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The metadata details of the document.
    metadata :: Lude.Maybe DocumentMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentResponse' with the minimum fields required to make a request.
--
-- * 'customMetadata' - The custom metadata on the document.
-- * 'metadata' - The metadata details of the document.
-- * 'responseStatus' - The response status code.
mkGetDocumentResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDocumentResponse
mkGetDocumentResponse pResponseStatus_ =
  GetDocumentResponse'
    { customMetadata = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The custom metadata on the document.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsCustomMetadata :: Lens.Lens' GetDocumentResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdrsCustomMetadata = Lens.lens (customMetadata :: GetDocumentResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {customMetadata = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | The metadata details of the document.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsMetadata :: Lens.Lens' GetDocumentResponse (Lude.Maybe DocumentMetadata)
gdrsMetadata = Lens.lens (metadata :: GetDocumentResponse -> Lude.Maybe DocumentMetadata) (\s a -> s {metadata = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdrsResponseStatus :: Lens.Lens' GetDocumentResponse Lude.Int
gdrsResponseStatus = Lens.lens (responseStatus :: GetDocumentResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDocumentResponse)
{-# DEPRECATED gdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
