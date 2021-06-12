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
-- Module      : Network.AWS.SSM.GetDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Systems Manager document.
module Network.AWS.SSM.GetDocument
  ( -- * Creating a Request
    GetDocument (..),
    newGetDocument,

    -- * Request Lenses
    getDocument_versionName,
    getDocument_documentFormat,
    getDocument_documentVersion,
    getDocument_name,

    -- * Destructuring the Response
    GetDocumentResponse (..),
    newGetDocumentResponse,

    -- * Response Lenses
    getDocumentResponse_documentType,
    getDocumentResponse_status,
    getDocumentResponse_requires,
    getDocumentResponse_attachmentsContent,
    getDocumentResponse_statusInformation,
    getDocumentResponse_versionName,
    getDocumentResponse_name,
    getDocumentResponse_documentFormat,
    getDocumentResponse_content,
    getDocumentResponse_reviewStatus,
    getDocumentResponse_documentVersion,
    getDocumentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document and can\'t be changed.
    versionName :: Core.Maybe Core.Text,
    -- | Returns the document in the specified format. The document format can be
    -- either JSON or YAML. JSON is the default format.
    documentFormat :: Core.Maybe DocumentFormat,
    -- | The document version for which you want information.
    documentVersion :: Core.Maybe Core.Text,
    -- | The name of the Systems Manager document.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionName', 'getDocument_versionName' - An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document and can\'t be changed.
--
-- 'documentFormat', 'getDocument_documentFormat' - Returns the document in the specified format. The document format can be
-- either JSON or YAML. JSON is the default format.
--
-- 'documentVersion', 'getDocument_documentVersion' - The document version for which you want information.
--
-- 'name', 'getDocument_name' - The name of the Systems Manager document.
newGetDocument ::
  -- | 'name'
  Core.Text ->
  GetDocument
newGetDocument pName_ =
  GetDocument'
    { versionName = Core.Nothing,
      documentFormat = Core.Nothing,
      documentVersion = Core.Nothing,
      name = pName_
    }

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document and can\'t be changed.
getDocument_versionName :: Lens.Lens' GetDocument (Core.Maybe Core.Text)
getDocument_versionName = Lens.lens (\GetDocument' {versionName} -> versionName) (\s@GetDocument' {} a -> s {versionName = a} :: GetDocument)

-- | Returns the document in the specified format. The document format can be
-- either JSON or YAML. JSON is the default format.
getDocument_documentFormat :: Lens.Lens' GetDocument (Core.Maybe DocumentFormat)
getDocument_documentFormat = Lens.lens (\GetDocument' {documentFormat} -> documentFormat) (\s@GetDocument' {} a -> s {documentFormat = a} :: GetDocument)

-- | The document version for which you want information.
getDocument_documentVersion :: Lens.Lens' GetDocument (Core.Maybe Core.Text)
getDocument_documentVersion = Lens.lens (\GetDocument' {documentVersion} -> documentVersion) (\s@GetDocument' {} a -> s {documentVersion = a} :: GetDocument)

-- | The name of the Systems Manager document.
getDocument_name :: Lens.Lens' GetDocument Core.Text
getDocument_name = Lens.lens (\GetDocument' {name} -> name) (\s@GetDocument' {} a -> s {name = a} :: GetDocument)

instance Core.AWSRequest GetDocument where
  type AWSResponse GetDocument = GetDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Core.<$> (x Core..?> "DocumentType")
            Core.<*> (x Core..?> "Status")
            Core.<*> (x Core..?> "Requires")
            Core.<*> ( x Core..?> "AttachmentsContent"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "StatusInformation")
            Core.<*> (x Core..?> "VersionName")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "DocumentFormat")
            Core.<*> (x Core..?> "Content")
            Core.<*> (x Core..?> "ReviewStatus")
            Core.<*> (x Core..?> "DocumentVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDocument

instance Core.NFData GetDocument

instance Core.ToHeaders GetDocument where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetDocument" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDocument where
  toJSON GetDocument' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VersionName" Core..=) Core.<$> versionName,
            ("DocumentFormat" Core..=) Core.<$> documentFormat,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetDocument where
  toPath = Core.const "/"

instance Core.ToQuery GetDocument where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | The document type.
    documentType :: Core.Maybe DocumentType,
    -- | The status of the Systems Manager document, such as @Creating@,
    -- @Active@, @Updating@, @Failed@, and @Deleting@.
    status :: Core.Maybe DocumentStatus,
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Core.Maybe (Core.NonEmpty DocumentRequires),
    -- | A description of the document attachments, including names, locations,
    -- sizes, and so on.
    attachmentsContent :: Core.Maybe [AttachmentContent],
    -- | A message returned by AWS Systems Manager that explains the @Status@
    -- value. For example, a @Failed@ status might be explained by the
    -- @StatusInformation@ message, \"The specified S3 bucket does not exist.
    -- Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Core.Maybe Core.Text,
    -- | The version of the artifact associated with the document. For example,
    -- \"Release 12, Update 6\". This value is unique across all versions of a
    -- document, and cannot be changed.
    versionName :: Core.Maybe Core.Text,
    -- | The name of the Systems Manager document.
    name :: Core.Maybe Core.Text,
    -- | The document format, either JSON or YAML.
    documentFormat :: Core.Maybe DocumentFormat,
    -- | The contents of the Systems Manager document.
    content :: Core.Maybe Core.Text,
    -- | The current review status of a new custom Systems Manager document (SSM
    -- document) created by a member of your organization, or of the latest
    -- version of an existing SSM document.
    --
    -- Only one version of an SSM document can be in the APPROVED state at a
    -- time. When a new version is approved, the status of the previous version
    -- changes to REJECTED.
    --
    -- Only one version of an SSM document can be in review, or PENDING, at a
    -- time.
    reviewStatus :: Core.Maybe ReviewStatus,
    -- | The document version.
    documentVersion :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentType', 'getDocumentResponse_documentType' - The document type.
--
-- 'status', 'getDocumentResponse_status' - The status of the Systems Manager document, such as @Creating@,
-- @Active@, @Updating@, @Failed@, and @Deleting@.
--
-- 'requires', 'getDocumentResponse_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'attachmentsContent', 'getDocumentResponse_attachmentsContent' - A description of the document attachments, including names, locations,
-- sizes, and so on.
--
-- 'statusInformation', 'getDocumentResponse_statusInformation' - A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
--
-- 'versionName', 'getDocumentResponse_versionName' - The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and cannot be changed.
--
-- 'name', 'getDocumentResponse_name' - The name of the Systems Manager document.
--
-- 'documentFormat', 'getDocumentResponse_documentFormat' - The document format, either JSON or YAML.
--
-- 'content', 'getDocumentResponse_content' - The contents of the Systems Manager document.
--
-- 'reviewStatus', 'getDocumentResponse_reviewStatus' - The current review status of a new custom Systems Manager document (SSM
-- document) created by a member of your organization, or of the latest
-- version of an existing SSM document.
--
-- Only one version of an SSM document can be in the APPROVED state at a
-- time. When a new version is approved, the status of the previous version
-- changes to REJECTED.
--
-- Only one version of an SSM document can be in review, or PENDING, at a
-- time.
--
-- 'documentVersion', 'getDocumentResponse_documentVersion' - The document version.
--
-- 'httpStatus', 'getDocumentResponse_httpStatus' - The response's http status code.
newGetDocumentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDocumentResponse
newGetDocumentResponse pHttpStatus_ =
  GetDocumentResponse'
    { documentType = Core.Nothing,
      status = Core.Nothing,
      requires = Core.Nothing,
      attachmentsContent = Core.Nothing,
      statusInformation = Core.Nothing,
      versionName = Core.Nothing,
      name = Core.Nothing,
      documentFormat = Core.Nothing,
      content = Core.Nothing,
      reviewStatus = Core.Nothing,
      documentVersion = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The document type.
getDocumentResponse_documentType :: Lens.Lens' GetDocumentResponse (Core.Maybe DocumentType)
getDocumentResponse_documentType = Lens.lens (\GetDocumentResponse' {documentType} -> documentType) (\s@GetDocumentResponse' {} a -> s {documentType = a} :: GetDocumentResponse)

-- | The status of the Systems Manager document, such as @Creating@,
-- @Active@, @Updating@, @Failed@, and @Deleting@.
getDocumentResponse_status :: Lens.Lens' GetDocumentResponse (Core.Maybe DocumentStatus)
getDocumentResponse_status = Lens.lens (\GetDocumentResponse' {status} -> status) (\s@GetDocumentResponse' {} a -> s {status = a} :: GetDocumentResponse)

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
getDocumentResponse_requires :: Lens.Lens' GetDocumentResponse (Core.Maybe (Core.NonEmpty DocumentRequires))
getDocumentResponse_requires = Lens.lens (\GetDocumentResponse' {requires} -> requires) (\s@GetDocumentResponse' {} a -> s {requires = a} :: GetDocumentResponse) Core.. Lens.mapping Lens._Coerce

-- | A description of the document attachments, including names, locations,
-- sizes, and so on.
getDocumentResponse_attachmentsContent :: Lens.Lens' GetDocumentResponse (Core.Maybe [AttachmentContent])
getDocumentResponse_attachmentsContent = Lens.lens (\GetDocumentResponse' {attachmentsContent} -> attachmentsContent) (\s@GetDocumentResponse' {} a -> s {attachmentsContent = a} :: GetDocumentResponse) Core.. Lens.mapping Lens._Coerce

-- | A message returned by AWS Systems Manager that explains the @Status@
-- value. For example, a @Failed@ status might be explained by the
-- @StatusInformation@ message, \"The specified S3 bucket does not exist.
-- Verify that the URL of the S3 bucket is correct.\"
getDocumentResponse_statusInformation :: Lens.Lens' GetDocumentResponse (Core.Maybe Core.Text)
getDocumentResponse_statusInformation = Lens.lens (\GetDocumentResponse' {statusInformation} -> statusInformation) (\s@GetDocumentResponse' {} a -> s {statusInformation = a} :: GetDocumentResponse)

-- | The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and cannot be changed.
getDocumentResponse_versionName :: Lens.Lens' GetDocumentResponse (Core.Maybe Core.Text)
getDocumentResponse_versionName = Lens.lens (\GetDocumentResponse' {versionName} -> versionName) (\s@GetDocumentResponse' {} a -> s {versionName = a} :: GetDocumentResponse)

-- | The name of the Systems Manager document.
getDocumentResponse_name :: Lens.Lens' GetDocumentResponse (Core.Maybe Core.Text)
getDocumentResponse_name = Lens.lens (\GetDocumentResponse' {name} -> name) (\s@GetDocumentResponse' {} a -> s {name = a} :: GetDocumentResponse)

-- | The document format, either JSON or YAML.
getDocumentResponse_documentFormat :: Lens.Lens' GetDocumentResponse (Core.Maybe DocumentFormat)
getDocumentResponse_documentFormat = Lens.lens (\GetDocumentResponse' {documentFormat} -> documentFormat) (\s@GetDocumentResponse' {} a -> s {documentFormat = a} :: GetDocumentResponse)

-- | The contents of the Systems Manager document.
getDocumentResponse_content :: Lens.Lens' GetDocumentResponse (Core.Maybe Core.Text)
getDocumentResponse_content = Lens.lens (\GetDocumentResponse' {content} -> content) (\s@GetDocumentResponse' {} a -> s {content = a} :: GetDocumentResponse)

-- | The current review status of a new custom Systems Manager document (SSM
-- document) created by a member of your organization, or of the latest
-- version of an existing SSM document.
--
-- Only one version of an SSM document can be in the APPROVED state at a
-- time. When a new version is approved, the status of the previous version
-- changes to REJECTED.
--
-- Only one version of an SSM document can be in review, or PENDING, at a
-- time.
getDocumentResponse_reviewStatus :: Lens.Lens' GetDocumentResponse (Core.Maybe ReviewStatus)
getDocumentResponse_reviewStatus = Lens.lens (\GetDocumentResponse' {reviewStatus} -> reviewStatus) (\s@GetDocumentResponse' {} a -> s {reviewStatus = a} :: GetDocumentResponse)

-- | The document version.
getDocumentResponse_documentVersion :: Lens.Lens' GetDocumentResponse (Core.Maybe Core.Text)
getDocumentResponse_documentVersion = Lens.lens (\GetDocumentResponse' {documentVersion} -> documentVersion) (\s@GetDocumentResponse' {} a -> s {documentVersion = a} :: GetDocumentResponse)

-- | The response's http status code.
getDocumentResponse_httpStatus :: Lens.Lens' GetDocumentResponse Core.Int
getDocumentResponse_httpStatus = Lens.lens (\GetDocumentResponse' {httpStatus} -> httpStatus) (\s@GetDocumentResponse' {} a -> s {httpStatus = a} :: GetDocumentResponse)

instance Core.NFData GetDocumentResponse
