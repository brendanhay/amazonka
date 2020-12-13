{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.GetDocumentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves version metadata for the specified document.
module Network.AWS.WorkDocs.GetDocumentVersion
  ( -- * Creating a request
    GetDocumentVersion (..),
    mkGetDocumentVersion,

    -- ** Request lenses
    gdvVersionId,
    gdvDocumentId,
    gdvAuthenticationToken,
    gdvIncludeCustomMetadata,
    gdvFields,

    -- * Destructuring the response
    GetDocumentVersionResponse (..),
    mkGetDocumentVersionResponse,

    -- ** Response lenses
    gdvrsCustomMetadata,
    gdvrsMetadata,
    gdvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkGetDocumentVersion' smart constructor.
data GetDocumentVersion = GetDocumentVersion'
  { -- | The version ID of the document.
    versionId :: Lude.Text,
    -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | Set this to TRUE to include custom metadata in the response.
    includeCustomMetadata :: Lude.Maybe Lude.Bool,
    -- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
    fields :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The version ID of the document.
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'includeCustomMetadata' - Set this to TRUE to include custom metadata in the response.
-- * 'fields' - A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
mkGetDocumentVersion ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'documentId'
  Lude.Text ->
  GetDocumentVersion
mkGetDocumentVersion pVersionId_ pDocumentId_ =
  GetDocumentVersion'
    { versionId = pVersionId_,
      documentId = pDocumentId_,
      authenticationToken = Lude.Nothing,
      includeCustomMetadata = Lude.Nothing,
      fields = Lude.Nothing
    }

-- | The version ID of the document.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvVersionId :: Lens.Lens' GetDocumentVersion Lude.Text
gdvVersionId = Lens.lens (versionId :: GetDocumentVersion -> Lude.Text) (\s a -> s {versionId = a} :: GetDocumentVersion)
{-# DEPRECATED gdvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvDocumentId :: Lens.Lens' GetDocumentVersion Lude.Text
gdvDocumentId = Lens.lens (documentId :: GetDocumentVersion -> Lude.Text) (\s a -> s {documentId = a} :: GetDocumentVersion)
{-# DEPRECATED gdvDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvAuthenticationToken :: Lens.Lens' GetDocumentVersion (Lude.Maybe (Lude.Sensitive Lude.Text))
gdvAuthenticationToken = Lens.lens (authenticationToken :: GetDocumentVersion -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: GetDocumentVersion)
{-# DEPRECATED gdvAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | Set this to TRUE to include custom metadata in the response.
--
-- /Note:/ Consider using 'includeCustomMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvIncludeCustomMetadata :: Lens.Lens' GetDocumentVersion (Lude.Maybe Lude.Bool)
gdvIncludeCustomMetadata = Lens.lens (includeCustomMetadata :: GetDocumentVersion -> Lude.Maybe Lude.Bool) (\s a -> s {includeCustomMetadata = a} :: GetDocumentVersion)
{-# DEPRECATED gdvIncludeCustomMetadata "Use generic-lens or generic-optics with 'includeCustomMetadata' instead." #-}

-- | A comma-separated list of values. Specify "SOURCE" to include a URL for the source document.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvFields :: Lens.Lens' GetDocumentVersion (Lude.Maybe Lude.Text)
gdvFields = Lens.lens (fields :: GetDocumentVersion -> Lude.Maybe Lude.Text) (\s a -> s {fields = a} :: GetDocumentVersion)
{-# DEPRECATED gdvFields "Use generic-lens or generic-optics with 'fields' instead." #-}

instance Lude.AWSRequest GetDocumentVersion where
  type Rs GetDocumentVersion = GetDocumentVersionResponse
  request = Req.get workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetDocumentVersionResponse'
            Lude.<$> (x Lude..?> "CustomMetadata" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Metadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetDocumentVersion where
  toHeaders GetDocumentVersion' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath GetDocumentVersion where
  toPath GetDocumentVersion' {..} =
    Lude.mconcat
      [ "/api/v1/documents/",
        Lude.toBS documentId,
        "/versions/",
        Lude.toBS versionId
      ]

instance Lude.ToQuery GetDocumentVersion where
  toQuery GetDocumentVersion' {..} =
    Lude.mconcat
      [ "includeCustomMetadata" Lude.=: includeCustomMetadata,
        "fields" Lude.=: fields
      ]

-- | /See:/ 'mkGetDocumentVersionResponse' smart constructor.
data GetDocumentVersionResponse = GetDocumentVersionResponse'
  { -- | The custom metadata on the document version.
    customMetadata :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The version metadata.
    metadata :: Lude.Maybe DocumentVersionMetadata,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetDocumentVersionResponse' with the minimum fields required to make a request.
--
-- * 'customMetadata' - The custom metadata on the document version.
-- * 'metadata' - The version metadata.
-- * 'responseStatus' - The response status code.
mkGetDocumentVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetDocumentVersionResponse
mkGetDocumentVersionResponse pResponseStatus_ =
  GetDocumentVersionResponse'
    { customMetadata = Lude.Nothing,
      metadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The custom metadata on the document version.
--
-- /Note:/ Consider using 'customMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsCustomMetadata :: Lens.Lens' GetDocumentVersionResponse (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
gdvrsCustomMetadata = Lens.lens (customMetadata :: GetDocumentVersionResponse -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {customMetadata = a} :: GetDocumentVersionResponse)
{-# DEPRECATED gdvrsCustomMetadata "Use generic-lens or generic-optics with 'customMetadata' instead." #-}

-- | The version metadata.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsMetadata :: Lens.Lens' GetDocumentVersionResponse (Lude.Maybe DocumentVersionMetadata)
gdvrsMetadata = Lens.lens (metadata :: GetDocumentVersionResponse -> Lude.Maybe DocumentVersionMetadata) (\s a -> s {metadata = a} :: GetDocumentVersionResponse)
{-# DEPRECATED gdvrsMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdvrsResponseStatus :: Lens.Lens' GetDocumentVersionResponse Lude.Int
gdvrsResponseStatus = Lens.lens (responseStatus :: GetDocumentVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetDocumentVersionResponse)
{-# DEPRECATED gdvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
