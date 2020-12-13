{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.AbortDocumentVersionUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Aborts the upload of the specified document version that was previously initiated by 'InitiateDocumentVersionUpload' . The client should make this call only when it no longer intends to upload the document version, or fails to do so.
module Network.AWS.WorkDocs.AbortDocumentVersionUpload
  ( -- * Creating a request
    AbortDocumentVersionUpload (..),
    mkAbortDocumentVersionUpload,

    -- ** Request lenses
    advuVersionId,
    advuDocumentId,
    advuAuthenticationToken,

    -- * Destructuring the response
    AbortDocumentVersionUploadResponse (..),
    mkAbortDocumentVersionUploadResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkAbortDocumentVersionUpload' smart constructor.
data AbortDocumentVersionUpload = AbortDocumentVersionUpload'
  { -- | The ID of the version.
    versionId :: Lude.Text,
    -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortDocumentVersionUpload' with the minimum fields required to make a request.
--
-- * 'versionId' - The ID of the version.
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
mkAbortDocumentVersionUpload ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'documentId'
  Lude.Text ->
  AbortDocumentVersionUpload
mkAbortDocumentVersionUpload pVersionId_ pDocumentId_ =
  AbortDocumentVersionUpload'
    { versionId = pVersionId_,
      documentId = pDocumentId_,
      authenticationToken = Lude.Nothing
    }

-- | The ID of the version.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuVersionId :: Lens.Lens' AbortDocumentVersionUpload Lude.Text
advuVersionId = Lens.lens (versionId :: AbortDocumentVersionUpload -> Lude.Text) (\s a -> s {versionId = a} :: AbortDocumentVersionUpload)
{-# DEPRECATED advuVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuDocumentId :: Lens.Lens' AbortDocumentVersionUpload Lude.Text
advuDocumentId = Lens.lens (documentId :: AbortDocumentVersionUpload -> Lude.Text) (\s a -> s {documentId = a} :: AbortDocumentVersionUpload)
{-# DEPRECATED advuDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
advuAuthenticationToken :: Lens.Lens' AbortDocumentVersionUpload (Lude.Maybe (Lude.Sensitive Lude.Text))
advuAuthenticationToken = Lens.lens (authenticationToken :: AbortDocumentVersionUpload -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: AbortDocumentVersionUpload)
{-# DEPRECATED advuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

instance Lude.AWSRequest AbortDocumentVersionUpload where
  type
    Rs AbortDocumentVersionUpload =
      AbortDocumentVersionUploadResponse
  request = Req.delete workDocsService
  response = Res.receiveNull AbortDocumentVersionUploadResponse'

instance Lude.ToHeaders AbortDocumentVersionUpload where
  toHeaders AbortDocumentVersionUpload' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToPath AbortDocumentVersionUpload where
  toPath AbortDocumentVersionUpload' {..} =
    Lude.mconcat
      [ "/api/v1/documents/",
        Lude.toBS documentId,
        "/versions/",
        Lude.toBS versionId
      ]

instance Lude.ToQuery AbortDocumentVersionUpload where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAbortDocumentVersionUploadResponse' smart constructor.
data AbortDocumentVersionUploadResponse = AbortDocumentVersionUploadResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AbortDocumentVersionUploadResponse' with the minimum fields required to make a request.
mkAbortDocumentVersionUploadResponse ::
  AbortDocumentVersionUploadResponse
mkAbortDocumentVersionUploadResponse =
  AbortDocumentVersionUploadResponse'
