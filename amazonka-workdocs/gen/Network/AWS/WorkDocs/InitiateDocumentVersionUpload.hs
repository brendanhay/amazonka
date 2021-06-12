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
-- Module      : Network.AWS.WorkDocs.InitiateDocumentVersionUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document object and version object.
--
-- The client specifies the parent folder ID and name of the document to
-- upload. The ID is optionally specified when creating a new version of an
-- existing document. This is the first step to upload a document. Next,
-- upload the document to the URL returned from the call, and then call
-- UpdateDocumentVersion.
--
-- To cancel the document upload, call AbortDocumentVersionUpload.
module Network.AWS.WorkDocs.InitiateDocumentVersionUpload
  ( -- * Creating a Request
    InitiateDocumentVersionUpload (..),
    newInitiateDocumentVersionUpload,

    -- * Request Lenses
    initiateDocumentVersionUpload_contentType,
    initiateDocumentVersionUpload_contentModifiedTimestamp,
    initiateDocumentVersionUpload_id,
    initiateDocumentVersionUpload_contentCreatedTimestamp,
    initiateDocumentVersionUpload_name,
    initiateDocumentVersionUpload_documentSizeInBytes,
    initiateDocumentVersionUpload_authenticationToken,
    initiateDocumentVersionUpload_parentFolderId,

    -- * Destructuring the Response
    InitiateDocumentVersionUploadResponse (..),
    newInitiateDocumentVersionUploadResponse,

    -- * Response Lenses
    initiateDocumentVersionUploadResponse_uploadMetadata,
    initiateDocumentVersionUploadResponse_metadata,
    initiateDocumentVersionUploadResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkDocs.Types

-- | /See:/ 'newInitiateDocumentVersionUpload' smart constructor.
data InitiateDocumentVersionUpload = InitiateDocumentVersionUpload'
  { -- | The content type of the document.
    contentType :: Core.Maybe Core.Text,
    -- | The timestamp when the content of the document was modified.
    contentModifiedTimestamp :: Core.Maybe Core.POSIX,
    -- | The ID of the document.
    id :: Core.Maybe Core.Text,
    -- | The timestamp when the content of the document was originally created.
    contentCreatedTimestamp :: Core.Maybe Core.POSIX,
    -- | The name of the document.
    name :: Core.Maybe Core.Text,
    -- | The size of the document, in bytes.
    documentSizeInBytes :: Core.Maybe Core.Integer,
    -- | Amazon WorkDocs authentication token. Not required when using AWS
    -- administrator credentials to access the API.
    authenticationToken :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The ID of the parent folder.
    parentFolderId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitiateDocumentVersionUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contentType', 'initiateDocumentVersionUpload_contentType' - The content type of the document.
--
-- 'contentModifiedTimestamp', 'initiateDocumentVersionUpload_contentModifiedTimestamp' - The timestamp when the content of the document was modified.
--
-- 'id', 'initiateDocumentVersionUpload_id' - The ID of the document.
--
-- 'contentCreatedTimestamp', 'initiateDocumentVersionUpload_contentCreatedTimestamp' - The timestamp when the content of the document was originally created.
--
-- 'name', 'initiateDocumentVersionUpload_name' - The name of the document.
--
-- 'documentSizeInBytes', 'initiateDocumentVersionUpload_documentSizeInBytes' - The size of the document, in bytes.
--
-- 'authenticationToken', 'initiateDocumentVersionUpload_authenticationToken' - Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
--
-- 'parentFolderId', 'initiateDocumentVersionUpload_parentFolderId' - The ID of the parent folder.
newInitiateDocumentVersionUpload ::
  -- | 'parentFolderId'
  Core.Text ->
  InitiateDocumentVersionUpload
newInitiateDocumentVersionUpload pParentFolderId_ =
  InitiateDocumentVersionUpload'
    { contentType =
        Core.Nothing,
      contentModifiedTimestamp = Core.Nothing,
      id = Core.Nothing,
      contentCreatedTimestamp = Core.Nothing,
      name = Core.Nothing,
      documentSizeInBytes = Core.Nothing,
      authenticationToken = Core.Nothing,
      parentFolderId = pParentFolderId_
    }

-- | The content type of the document.
initiateDocumentVersionUpload_contentType :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.Text)
initiateDocumentVersionUpload_contentType = Lens.lens (\InitiateDocumentVersionUpload' {contentType} -> contentType) (\s@InitiateDocumentVersionUpload' {} a -> s {contentType = a} :: InitiateDocumentVersionUpload)

-- | The timestamp when the content of the document was modified.
initiateDocumentVersionUpload_contentModifiedTimestamp :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.UTCTime)
initiateDocumentVersionUpload_contentModifiedTimestamp = Lens.lens (\InitiateDocumentVersionUpload' {contentModifiedTimestamp} -> contentModifiedTimestamp) (\s@InitiateDocumentVersionUpload' {} a -> s {contentModifiedTimestamp = a} :: InitiateDocumentVersionUpload) Core.. Lens.mapping Core._Time

-- | The ID of the document.
initiateDocumentVersionUpload_id :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.Text)
initiateDocumentVersionUpload_id = Lens.lens (\InitiateDocumentVersionUpload' {id} -> id) (\s@InitiateDocumentVersionUpload' {} a -> s {id = a} :: InitiateDocumentVersionUpload)

-- | The timestamp when the content of the document was originally created.
initiateDocumentVersionUpload_contentCreatedTimestamp :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.UTCTime)
initiateDocumentVersionUpload_contentCreatedTimestamp = Lens.lens (\InitiateDocumentVersionUpload' {contentCreatedTimestamp} -> contentCreatedTimestamp) (\s@InitiateDocumentVersionUpload' {} a -> s {contentCreatedTimestamp = a} :: InitiateDocumentVersionUpload) Core.. Lens.mapping Core._Time

-- | The name of the document.
initiateDocumentVersionUpload_name :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.Text)
initiateDocumentVersionUpload_name = Lens.lens (\InitiateDocumentVersionUpload' {name} -> name) (\s@InitiateDocumentVersionUpload' {} a -> s {name = a} :: InitiateDocumentVersionUpload)

-- | The size of the document, in bytes.
initiateDocumentVersionUpload_documentSizeInBytes :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.Integer)
initiateDocumentVersionUpload_documentSizeInBytes = Lens.lens (\InitiateDocumentVersionUpload' {documentSizeInBytes} -> documentSizeInBytes) (\s@InitiateDocumentVersionUpload' {} a -> s {documentSizeInBytes = a} :: InitiateDocumentVersionUpload)

-- | Amazon WorkDocs authentication token. Not required when using AWS
-- administrator credentials to access the API.
initiateDocumentVersionUpload_authenticationToken :: Lens.Lens' InitiateDocumentVersionUpload (Core.Maybe Core.Text)
initiateDocumentVersionUpload_authenticationToken = Lens.lens (\InitiateDocumentVersionUpload' {authenticationToken} -> authenticationToken) (\s@InitiateDocumentVersionUpload' {} a -> s {authenticationToken = a} :: InitiateDocumentVersionUpload) Core.. Lens.mapping Core._Sensitive

-- | The ID of the parent folder.
initiateDocumentVersionUpload_parentFolderId :: Lens.Lens' InitiateDocumentVersionUpload Core.Text
initiateDocumentVersionUpload_parentFolderId = Lens.lens (\InitiateDocumentVersionUpload' {parentFolderId} -> parentFolderId) (\s@InitiateDocumentVersionUpload' {} a -> s {parentFolderId = a} :: InitiateDocumentVersionUpload)

instance
  Core.AWSRequest
    InitiateDocumentVersionUpload
  where
  type
    AWSResponse InitiateDocumentVersionUpload =
      InitiateDocumentVersionUploadResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          InitiateDocumentVersionUploadResponse'
            Core.<$> (x Core..?> "UploadMetadata")
            Core.<*> (x Core..?> "Metadata")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable InitiateDocumentVersionUpload

instance Core.NFData InitiateDocumentVersionUpload

instance Core.ToHeaders InitiateDocumentVersionUpload where
  toHeaders InitiateDocumentVersionUpload' {..} =
    Core.mconcat
      [ "Authentication" Core.=# authenticationToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
      ]

instance Core.ToJSON InitiateDocumentVersionUpload where
  toJSON InitiateDocumentVersionUpload' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContentType" Core..=) Core.<$> contentType,
            ("ContentModifiedTimestamp" Core..=)
              Core.<$> contentModifiedTimestamp,
            ("Id" Core..=) Core.<$> id,
            ("ContentCreatedTimestamp" Core..=)
              Core.<$> contentCreatedTimestamp,
            ("Name" Core..=) Core.<$> name,
            ("DocumentSizeInBytes" Core..=)
              Core.<$> documentSizeInBytes,
            Core.Just ("ParentFolderId" Core..= parentFolderId)
          ]
      )

instance Core.ToPath InitiateDocumentVersionUpload where
  toPath = Core.const "/api/v1/documents"

instance Core.ToQuery InitiateDocumentVersionUpload where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newInitiateDocumentVersionUploadResponse' smart constructor.
data InitiateDocumentVersionUploadResponse = InitiateDocumentVersionUploadResponse'
  { -- | The upload metadata.
    uploadMetadata :: Core.Maybe UploadMetadata,
    -- | The document metadata.
    metadata :: Core.Maybe DocumentMetadata,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InitiateDocumentVersionUploadResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uploadMetadata', 'initiateDocumentVersionUploadResponse_uploadMetadata' - The upload metadata.
--
-- 'metadata', 'initiateDocumentVersionUploadResponse_metadata' - The document metadata.
--
-- 'httpStatus', 'initiateDocumentVersionUploadResponse_httpStatus' - The response's http status code.
newInitiateDocumentVersionUploadResponse ::
  -- | 'httpStatus'
  Core.Int ->
  InitiateDocumentVersionUploadResponse
newInitiateDocumentVersionUploadResponse pHttpStatus_ =
  InitiateDocumentVersionUploadResponse'
    { uploadMetadata =
        Core.Nothing,
      metadata = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The upload metadata.
initiateDocumentVersionUploadResponse_uploadMetadata :: Lens.Lens' InitiateDocumentVersionUploadResponse (Core.Maybe UploadMetadata)
initiateDocumentVersionUploadResponse_uploadMetadata = Lens.lens (\InitiateDocumentVersionUploadResponse' {uploadMetadata} -> uploadMetadata) (\s@InitiateDocumentVersionUploadResponse' {} a -> s {uploadMetadata = a} :: InitiateDocumentVersionUploadResponse)

-- | The document metadata.
initiateDocumentVersionUploadResponse_metadata :: Lens.Lens' InitiateDocumentVersionUploadResponse (Core.Maybe DocumentMetadata)
initiateDocumentVersionUploadResponse_metadata = Lens.lens (\InitiateDocumentVersionUploadResponse' {metadata} -> metadata) (\s@InitiateDocumentVersionUploadResponse' {} a -> s {metadata = a} :: InitiateDocumentVersionUploadResponse)

-- | The response's http status code.
initiateDocumentVersionUploadResponse_httpStatus :: Lens.Lens' InitiateDocumentVersionUploadResponse Core.Int
initiateDocumentVersionUploadResponse_httpStatus = Lens.lens (\InitiateDocumentVersionUploadResponse' {httpStatus} -> httpStatus) (\s@InitiateDocumentVersionUploadResponse' {} a -> s {httpStatus = a} :: InitiateDocumentVersionUploadResponse)

instance
  Core.NFData
    InitiateDocumentVersionUploadResponse
