{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.UpdateDocumentVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the document version to ACTIVE.
--
-- Amazon WorkDocs also sets its document container to ACTIVE. This is the last step in a document upload, after the client uploads the document to an S3-presigned URL returned by 'InitiateDocumentVersionUpload' .
module Network.AWS.WorkDocs.UpdateDocumentVersion
  ( -- * Creating a request
    UpdateDocumentVersion (..),
    mkUpdateDocumentVersion,

    -- ** Request lenses
    udvVersionId,
    udvDocumentId,
    udvAuthenticationToken,
    udvVersionStatus,

    -- * Destructuring the response
    UpdateDocumentVersionResponse (..),
    mkUpdateDocumentVersionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkUpdateDocumentVersion' smart constructor.
data UpdateDocumentVersion = UpdateDocumentVersion'
  { -- | The version ID of the document.
    versionId :: Lude.Text,
    -- | The ID of the document.
    documentId :: Lude.Text,
    -- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
    authenticationToken :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The status of the version.
    versionStatus :: Lude.Maybe DocumentVersionStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentVersion' with the minimum fields required to make a request.
--
-- * 'versionId' - The version ID of the document.
-- * 'documentId' - The ID of the document.
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'versionStatus' - The status of the version.
mkUpdateDocumentVersion ::
  -- | 'versionId'
  Lude.Text ->
  -- | 'documentId'
  Lude.Text ->
  UpdateDocumentVersion
mkUpdateDocumentVersion pVersionId_ pDocumentId_ =
  UpdateDocumentVersion'
    { versionId = pVersionId_,
      documentId = pDocumentId_,
      authenticationToken = Lude.Nothing,
      versionStatus = Lude.Nothing
    }

-- | The version ID of the document.
--
-- /Note:/ Consider using 'versionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvVersionId :: Lens.Lens' UpdateDocumentVersion Lude.Text
udvVersionId = Lens.lens (versionId :: UpdateDocumentVersion -> Lude.Text) (\s a -> s {versionId = a} :: UpdateDocumentVersion)
{-# DEPRECATED udvVersionId "Use generic-lens or generic-optics with 'versionId' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'documentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvDocumentId :: Lens.Lens' UpdateDocumentVersion Lude.Text
udvDocumentId = Lens.lens (documentId :: UpdateDocumentVersion -> Lude.Text) (\s a -> s {documentId = a} :: UpdateDocumentVersion)
{-# DEPRECATED udvDocumentId "Use generic-lens or generic-optics with 'documentId' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvAuthenticationToken :: Lens.Lens' UpdateDocumentVersion (Lude.Maybe (Lude.Sensitive Lude.Text))
udvAuthenticationToken = Lens.lens (authenticationToken :: UpdateDocumentVersion -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: UpdateDocumentVersion)
{-# DEPRECATED udvAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The status of the version.
--
-- /Note:/ Consider using 'versionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udvVersionStatus :: Lens.Lens' UpdateDocumentVersion (Lude.Maybe DocumentVersionStatus)
udvVersionStatus = Lens.lens (versionStatus :: UpdateDocumentVersion -> Lude.Maybe DocumentVersionStatus) (\s a -> s {versionStatus = a} :: UpdateDocumentVersion)
{-# DEPRECATED udvVersionStatus "Use generic-lens or generic-optics with 'versionStatus' instead." #-}

instance Lude.AWSRequest UpdateDocumentVersion where
  type Rs UpdateDocumentVersion = UpdateDocumentVersionResponse
  request = Req.patchJSON workDocsService
  response = Res.receiveNull UpdateDocumentVersionResponse'

instance Lude.ToHeaders UpdateDocumentVersion where
  toHeaders UpdateDocumentVersion' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON UpdateDocumentVersion where
  toJSON UpdateDocumentVersion' {..} =
    Lude.object
      (Lude.catMaybes [("VersionStatus" Lude..=) Lude.<$> versionStatus])

instance Lude.ToPath UpdateDocumentVersion where
  toPath UpdateDocumentVersion' {..} =
    Lude.mconcat
      [ "/api/v1/documents/",
        Lude.toBS documentId,
        "/versions/",
        Lude.toBS versionId
      ]

instance Lude.ToQuery UpdateDocumentVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDocumentVersionResponse' smart constructor.
data UpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDocumentVersionResponse' with the minimum fields required to make a request.
mkUpdateDocumentVersionResponse ::
  UpdateDocumentVersionResponse
mkUpdateDocumentVersionResponse = UpdateDocumentVersionResponse'
