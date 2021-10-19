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
-- Gets the contents of the specified Amazon Web Services Systems Manager
-- document (SSM document).
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
    getDocumentResponse_status,
    getDocumentResponse_documentType,
    getDocumentResponse_versionName,
    getDocumentResponse_attachmentsContent,
    getDocumentResponse_reviewStatus,
    getDocumentResponse_content,
    getDocumentResponse_createdDate,
    getDocumentResponse_documentFormat,
    getDocumentResponse_name,
    getDocumentResponse_documentVersion,
    getDocumentResponse_displayName,
    getDocumentResponse_statusInformation,
    getDocumentResponse_requires,
    getDocumentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetDocument' smart constructor.
data GetDocument = GetDocument'
  { -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | Returns the document in the specified format. The document format can be
    -- either JSON or YAML. JSON is the default format.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The document version for which you want information.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM document.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'name', 'getDocument_name' - The name of the SSM document.
newGetDocument ::
  -- | 'name'
  Prelude.Text ->
  GetDocument
newGetDocument pName_ =
  GetDocument'
    { versionName = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      name = pName_
    }

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document and can\'t be changed.
getDocument_versionName :: Lens.Lens' GetDocument (Prelude.Maybe Prelude.Text)
getDocument_versionName = Lens.lens (\GetDocument' {versionName} -> versionName) (\s@GetDocument' {} a -> s {versionName = a} :: GetDocument)

-- | Returns the document in the specified format. The document format can be
-- either JSON or YAML. JSON is the default format.
getDocument_documentFormat :: Lens.Lens' GetDocument (Prelude.Maybe DocumentFormat)
getDocument_documentFormat = Lens.lens (\GetDocument' {documentFormat} -> documentFormat) (\s@GetDocument' {} a -> s {documentFormat = a} :: GetDocument)

-- | The document version for which you want information.
getDocument_documentVersion :: Lens.Lens' GetDocument (Prelude.Maybe Prelude.Text)
getDocument_documentVersion = Lens.lens (\GetDocument' {documentVersion} -> documentVersion) (\s@GetDocument' {} a -> s {documentVersion = a} :: GetDocument)

-- | The name of the SSM document.
getDocument_name :: Lens.Lens' GetDocument Prelude.Text
getDocument_name = Lens.lens (\GetDocument' {name} -> name) (\s@GetDocument' {} a -> s {name = a} :: GetDocument)

instance Core.AWSRequest GetDocument where
  type AWSResponse GetDocument = GetDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "DocumentType")
            Prelude.<*> (x Core..?> "VersionName")
            Prelude.<*> ( x Core..?> "AttachmentsContent"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "ReviewStatus")
            Prelude.<*> (x Core..?> "Content")
            Prelude.<*> (x Core..?> "CreatedDate")
            Prelude.<*> (x Core..?> "DocumentFormat")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "DocumentVersion")
            Prelude.<*> (x Core..?> "DisplayName")
            Prelude.<*> (x Core..?> "StatusInformation")
            Prelude.<*> (x Core..?> "Requires")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocument

instance Prelude.NFData GetDocument

instance Core.ToHeaders GetDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetDocument" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDocument where
  toJSON GetDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VersionName" Core..=) Prelude.<$> versionName,
            ("DocumentFormat" Core..=)
              Prelude.<$> documentFormat,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetDocument where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | The status of the SSM document, such as @Creating@, @Active@,
    -- @Updating@, @Failed@, and @Deleting@.
    status :: Prelude.Maybe DocumentStatus,
    -- | The document type.
    documentType :: Prelude.Maybe DocumentType,
    -- | The version of the artifact associated with the document. For example,
    -- \"Release 12, Update 6\". This value is unique across all versions of a
    -- document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | A description of the document attachments, including names, locations,
    -- sizes, and so on.
    attachmentsContent :: Prelude.Maybe [AttachmentContent],
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
    reviewStatus :: Prelude.Maybe ReviewStatus,
    -- | The contents of the SSM document.
    content :: Prelude.Maybe Prelude.Text,
    -- | The date the SSM document was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the SSM document. This value can differ for each
    -- version of the document. If you want to update this value, see
    -- UpdateDocument.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | A message returned by Amazon Web Services Systems Manager that explains
    -- the @Status@ value. For example, a @Failed@ status might be explained by
    -- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
    -- exist. Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Prelude.Maybe Prelude.Text,
    -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getDocumentResponse_status' - The status of the SSM document, such as @Creating@, @Active@,
-- @Updating@, @Failed@, and @Deleting@.
--
-- 'documentType', 'getDocumentResponse_documentType' - The document type.
--
-- 'versionName', 'getDocumentResponse_versionName' - The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and can\'t be changed.
--
-- 'attachmentsContent', 'getDocumentResponse_attachmentsContent' - A description of the document attachments, including names, locations,
-- sizes, and so on.
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
-- 'content', 'getDocumentResponse_content' - The contents of the SSM document.
--
-- 'createdDate', 'getDocumentResponse_createdDate' - The date the SSM document was created.
--
-- 'documentFormat', 'getDocumentResponse_documentFormat' - The document format, either JSON or YAML.
--
-- 'name', 'getDocumentResponse_name' - The name of the SSM document.
--
-- 'documentVersion', 'getDocumentResponse_documentVersion' - The document version.
--
-- 'displayName', 'getDocumentResponse_displayName' - The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
--
-- 'statusInformation', 'getDocumentResponse_statusInformation' - A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
--
-- 'requires', 'getDocumentResponse_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'httpStatus', 'getDocumentResponse_httpStatus' - The response's http status code.
newGetDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentResponse
newGetDocumentResponse pHttpStatus_ =
  GetDocumentResponse'
    { status = Prelude.Nothing,
      documentType = Prelude.Nothing,
      versionName = Prelude.Nothing,
      attachmentsContent = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      content = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      name = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      displayName = Prelude.Nothing,
      statusInformation = Prelude.Nothing,
      requires = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the SSM document, such as @Creating@, @Active@,
-- @Updating@, @Failed@, and @Deleting@.
getDocumentResponse_status :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentStatus)
getDocumentResponse_status = Lens.lens (\GetDocumentResponse' {status} -> status) (\s@GetDocumentResponse' {} a -> s {status = a} :: GetDocumentResponse)

-- | The document type.
getDocumentResponse_documentType :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentType)
getDocumentResponse_documentType = Lens.lens (\GetDocumentResponse' {documentType} -> documentType) (\s@GetDocumentResponse' {} a -> s {documentType = a} :: GetDocumentResponse)

-- | The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and can\'t be changed.
getDocumentResponse_versionName :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_versionName = Lens.lens (\GetDocumentResponse' {versionName} -> versionName) (\s@GetDocumentResponse' {} a -> s {versionName = a} :: GetDocumentResponse)

-- | A description of the document attachments, including names, locations,
-- sizes, and so on.
getDocumentResponse_attachmentsContent :: Lens.Lens' GetDocumentResponse (Prelude.Maybe [AttachmentContent])
getDocumentResponse_attachmentsContent = Lens.lens (\GetDocumentResponse' {attachmentsContent} -> attachmentsContent) (\s@GetDocumentResponse' {} a -> s {attachmentsContent = a} :: GetDocumentResponse) Prelude.. Lens.mapping Lens.coerced

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
getDocumentResponse_reviewStatus :: Lens.Lens' GetDocumentResponse (Prelude.Maybe ReviewStatus)
getDocumentResponse_reviewStatus = Lens.lens (\GetDocumentResponse' {reviewStatus} -> reviewStatus) (\s@GetDocumentResponse' {} a -> s {reviewStatus = a} :: GetDocumentResponse)

-- | The contents of the SSM document.
getDocumentResponse_content :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_content = Lens.lens (\GetDocumentResponse' {content} -> content) (\s@GetDocumentResponse' {} a -> s {content = a} :: GetDocumentResponse)

-- | The date the SSM document was created.
getDocumentResponse_createdDate :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.UTCTime)
getDocumentResponse_createdDate = Lens.lens (\GetDocumentResponse' {createdDate} -> createdDate) (\s@GetDocumentResponse' {} a -> s {createdDate = a} :: GetDocumentResponse) Prelude.. Lens.mapping Core._Time

-- | The document format, either JSON or YAML.
getDocumentResponse_documentFormat :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentFormat)
getDocumentResponse_documentFormat = Lens.lens (\GetDocumentResponse' {documentFormat} -> documentFormat) (\s@GetDocumentResponse' {} a -> s {documentFormat = a} :: GetDocumentResponse)

-- | The name of the SSM document.
getDocumentResponse_name :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_name = Lens.lens (\GetDocumentResponse' {name} -> name) (\s@GetDocumentResponse' {} a -> s {name = a} :: GetDocumentResponse)

-- | The document version.
getDocumentResponse_documentVersion :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_documentVersion = Lens.lens (\GetDocumentResponse' {documentVersion} -> documentVersion) (\s@GetDocumentResponse' {} a -> s {documentVersion = a} :: GetDocumentResponse)

-- | The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
getDocumentResponse_displayName :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_displayName = Lens.lens (\GetDocumentResponse' {displayName} -> displayName) (\s@GetDocumentResponse' {} a -> s {displayName = a} :: GetDocumentResponse)

-- | A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
getDocumentResponse_statusInformation :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_statusInformation = Lens.lens (\GetDocumentResponse' {statusInformation} -> statusInformation) (\s@GetDocumentResponse' {} a -> s {statusInformation = a} :: GetDocumentResponse)

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
getDocumentResponse_requires :: Lens.Lens' GetDocumentResponse (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
getDocumentResponse_requires = Lens.lens (\GetDocumentResponse' {requires} -> requires) (\s@GetDocumentResponse' {} a -> s {requires = a} :: GetDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDocumentResponse_httpStatus :: Lens.Lens' GetDocumentResponse Prelude.Int
getDocumentResponse_httpStatus = Lens.lens (\GetDocumentResponse' {httpStatus} -> httpStatus) (\s@GetDocumentResponse' {} a -> s {httpStatus = a} :: GetDocumentResponse)

instance Prelude.NFData GetDocumentResponse
