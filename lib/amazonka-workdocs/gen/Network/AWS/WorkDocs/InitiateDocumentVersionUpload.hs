{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.InitiateDocumentVersionUpload
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document object and version object.
--
-- The client specifies the parent folder ID and name of the document to upload. The ID is optionally specified when creating a new version of an existing document. This is the first step to upload a document. Next, upload the document to the URL returned from the call, and then call 'UpdateDocumentVersion' .
-- To cancel the document upload, call 'AbortDocumentVersionUpload' .
module Network.AWS.WorkDocs.InitiateDocumentVersionUpload
  ( -- * Creating a request
    InitiateDocumentVersionUpload (..),
    mkInitiateDocumentVersionUpload,

    -- ** Request lenses
    idvuDocumentSizeInBytes,
    idvuContentCreatedTimestamp,
    idvuAuthenticationToken,
    idvuName,
    idvuId,
    idvuContentModifiedTimestamp,
    idvuContentType,
    idvuParentFolderId,

    -- * Destructuring the response
    InitiateDocumentVersionUploadResponse (..),
    mkInitiateDocumentVersionUploadResponse,

    -- ** Response lenses
    idvursMetadata,
    idvursUploadMetadata,
    idvursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.WorkDocs.Types

-- | /See:/ 'mkInitiateDocumentVersionUpload' smart constructor.
data InitiateDocumentVersionUpload = InitiateDocumentVersionUpload'
  { documentSizeInBytes ::
      Lude.Maybe Lude.Integer,
    contentCreatedTimestamp ::
      Lude.Maybe Lude.Timestamp,
    authenticationToken ::
      Lude.Maybe
        (Lude.Sensitive Lude.Text),
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    contentModifiedTimestamp ::
      Lude.Maybe Lude.Timestamp,
    contentType ::
      Lude.Maybe Lude.Text,
    parentFolderId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateDocumentVersionUpload' with the minimum fields required to make a request.
--
-- * 'authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
-- * 'contentCreatedTimestamp' - The timestamp when the content of the document was originally created.
-- * 'contentModifiedTimestamp' - The timestamp when the content of the document was modified.
-- * 'contentType' - The content type of the document.
-- * 'documentSizeInBytes' - The size of the document, in bytes.
-- * 'id' - The ID of the document.
-- * 'name' - The name of the document.
-- * 'parentFolderId' - The ID of the parent folder.
mkInitiateDocumentVersionUpload ::
  -- | 'parentFolderId'
  Lude.Text ->
  InitiateDocumentVersionUpload
mkInitiateDocumentVersionUpload pParentFolderId_ =
  InitiateDocumentVersionUpload'
    { documentSizeInBytes =
        Lude.Nothing,
      contentCreatedTimestamp = Lude.Nothing,
      authenticationToken = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      contentModifiedTimestamp = Lude.Nothing,
      contentType = Lude.Nothing,
      parentFolderId = pParentFolderId_
    }

-- | The size of the document, in bytes.
--
-- /Note:/ Consider using 'documentSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuDocumentSizeInBytes :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe Lude.Integer)
idvuDocumentSizeInBytes = Lens.lens (documentSizeInBytes :: InitiateDocumentVersionUpload -> Lude.Maybe Lude.Integer) (\s a -> s {documentSizeInBytes = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuDocumentSizeInBytes "Use generic-lens or generic-optics with 'documentSizeInBytes' instead." #-}

-- | The timestamp when the content of the document was originally created.
--
-- /Note:/ Consider using 'contentCreatedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuContentCreatedTimestamp :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe Lude.Timestamp)
idvuContentCreatedTimestamp = Lens.lens (contentCreatedTimestamp :: InitiateDocumentVersionUpload -> Lude.Maybe Lude.Timestamp) (\s a -> s {contentCreatedTimestamp = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuContentCreatedTimestamp "Use generic-lens or generic-optics with 'contentCreatedTimestamp' instead." #-}

-- | Amazon WorkDocs authentication token. Not required when using AWS administrator credentials to access the API.
--
-- /Note:/ Consider using 'authenticationToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuAuthenticationToken :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe (Lude.Sensitive Lude.Text))
idvuAuthenticationToken = Lens.lens (authenticationToken :: InitiateDocumentVersionUpload -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {authenticationToken = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuAuthenticationToken "Use generic-lens or generic-optics with 'authenticationToken' instead." #-}

-- | The name of the document.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuName :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe Lude.Text)
idvuName = Lens.lens (name :: InitiateDocumentVersionUpload -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the document.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuId :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe Lude.Text)
idvuId = Lens.lens (id :: InitiateDocumentVersionUpload -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The timestamp when the content of the document was modified.
--
-- /Note:/ Consider using 'contentModifiedTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuContentModifiedTimestamp :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe Lude.Timestamp)
idvuContentModifiedTimestamp = Lens.lens (contentModifiedTimestamp :: InitiateDocumentVersionUpload -> Lude.Maybe Lude.Timestamp) (\s a -> s {contentModifiedTimestamp = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuContentModifiedTimestamp "Use generic-lens or generic-optics with 'contentModifiedTimestamp' instead." #-}

-- | The content type of the document.
--
-- /Note:/ Consider using 'contentType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuContentType :: Lens.Lens' InitiateDocumentVersionUpload (Lude.Maybe Lude.Text)
idvuContentType = Lens.lens (contentType :: InitiateDocumentVersionUpload -> Lude.Maybe Lude.Text) (\s a -> s {contentType = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuContentType "Use generic-lens or generic-optics with 'contentType' instead." #-}

-- | The ID of the parent folder.
--
-- /Note:/ Consider using 'parentFolderId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvuParentFolderId :: Lens.Lens' InitiateDocumentVersionUpload Lude.Text
idvuParentFolderId = Lens.lens (parentFolderId :: InitiateDocumentVersionUpload -> Lude.Text) (\s a -> s {parentFolderId = a} :: InitiateDocumentVersionUpload)
{-# DEPRECATED idvuParentFolderId "Use generic-lens or generic-optics with 'parentFolderId' instead." #-}

instance Lude.AWSRequest InitiateDocumentVersionUpload where
  type
    Rs InitiateDocumentVersionUpload =
      InitiateDocumentVersionUploadResponse
  request = Req.postJSON workDocsService
  response =
    Res.receiveJSON
      ( \s h x ->
          InitiateDocumentVersionUploadResponse'
            Lude.<$> (x Lude..?> "Metadata")
            Lude.<*> (x Lude..?> "UploadMetadata")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders InitiateDocumentVersionUpload where
  toHeaders InitiateDocumentVersionUpload' {..} =
    Lude.mconcat
      [ "Authentication" Lude.=# authenticationToken,
        "Content-Type"
          Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
      ]

instance Lude.ToJSON InitiateDocumentVersionUpload where
  toJSON InitiateDocumentVersionUpload' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DocumentSizeInBytes" Lude..=) Lude.<$> documentSizeInBytes,
            ("ContentCreatedTimestamp" Lude..=)
              Lude.<$> contentCreatedTimestamp,
            ("Name" Lude..=) Lude.<$> name,
            ("Id" Lude..=) Lude.<$> id,
            ("ContentModifiedTimestamp" Lude..=)
              Lude.<$> contentModifiedTimestamp,
            ("ContentType" Lude..=) Lude.<$> contentType,
            Lude.Just ("ParentFolderId" Lude..= parentFolderId)
          ]
      )

instance Lude.ToPath InitiateDocumentVersionUpload where
  toPath = Lude.const "/api/v1/documents"

instance Lude.ToQuery InitiateDocumentVersionUpload where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkInitiateDocumentVersionUploadResponse' smart constructor.
data InitiateDocumentVersionUploadResponse = InitiateDocumentVersionUploadResponse'
  { metadata ::
      Lude.Maybe
        DocumentMetadata,
    uploadMetadata ::
      Lude.Maybe
        UploadMetadata,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InitiateDocumentVersionUploadResponse' with the minimum fields required to make a request.
--
-- * 'metadata' - The document metadata.
-- * 'responseStatus' - The response status code.
-- * 'uploadMetadata' - The upload metadata.
mkInitiateDocumentVersionUploadResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  InitiateDocumentVersionUploadResponse
mkInitiateDocumentVersionUploadResponse pResponseStatus_ =
  InitiateDocumentVersionUploadResponse'
    { metadata = Lude.Nothing,
      uploadMetadata = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The document metadata.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvursMetadata :: Lens.Lens' InitiateDocumentVersionUploadResponse (Lude.Maybe DocumentMetadata)
idvursMetadata = Lens.lens (metadata :: InitiateDocumentVersionUploadResponse -> Lude.Maybe DocumentMetadata) (\s a -> s {metadata = a} :: InitiateDocumentVersionUploadResponse)
{-# DEPRECATED idvursMetadata "Use generic-lens or generic-optics with 'metadata' instead." #-}

-- | The upload metadata.
--
-- /Note:/ Consider using 'uploadMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvursUploadMetadata :: Lens.Lens' InitiateDocumentVersionUploadResponse (Lude.Maybe UploadMetadata)
idvursUploadMetadata = Lens.lens (uploadMetadata :: InitiateDocumentVersionUploadResponse -> Lude.Maybe UploadMetadata) (\s a -> s {uploadMetadata = a} :: InitiateDocumentVersionUploadResponse)
{-# DEPRECATED idvursUploadMetadata "Use generic-lens or generic-optics with 'uploadMetadata' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
idvursResponseStatus :: Lens.Lens' InitiateDocumentVersionUploadResponse Lude.Int
idvursResponseStatus = Lens.lens (responseStatus :: InitiateDocumentVersionUploadResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: InitiateDocumentVersionUploadResponse)
{-# DEPRECATED idvursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
