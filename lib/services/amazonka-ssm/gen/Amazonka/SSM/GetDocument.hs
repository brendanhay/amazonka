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
-- Module      : Amazonka.SSM.GetDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the contents of the specified Amazon Web Services Systems Manager
-- document (SSM document).
module Amazonka.SSM.GetDocument
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
    getDocumentResponse_requires,
    getDocumentResponse_documentType,
    getDocumentResponse_name,
    getDocumentResponse_displayName,
    getDocumentResponse_status,
    getDocumentResponse_versionName,
    getDocumentResponse_statusInformation,
    getDocumentResponse_createdDate,
    getDocumentResponse_content,
    getDocumentResponse_attachmentsContent,
    getDocumentResponse_reviewStatus,
    getDocumentResponse_documentFormat,
    getDocumentResponse_documentVersion,
    getDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDocumentResponse'
            Prelude.<$> (x Data..?> "Requires")
            Prelude.<*> (x Data..?> "DocumentType")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "DisplayName")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "VersionName")
            Prelude.<*> (x Data..?> "StatusInformation")
            Prelude.<*> (x Data..?> "CreatedDate")
            Prelude.<*> (x Data..?> "Content")
            Prelude.<*> ( x Data..?> "AttachmentsContent"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "ReviewStatus")
            Prelude.<*> (x Data..?> "DocumentFormat")
            Prelude.<*> (x Data..?> "DocumentVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDocument where
  hashWithSalt _salt GetDocument' {..} =
    _salt `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` documentFormat
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetDocument where
  rnf GetDocument' {..} =
    Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.GetDocument" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDocument where
  toJSON GetDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("VersionName" Data..=) Prelude.<$> versionName,
            ("DocumentFormat" Data..=)
              Prelude.<$> documentFormat,
            ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath GetDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDocumentResponse' smart constructor.
data GetDocumentResponse = GetDocumentResponse'
  { -- | A list of SSM documents required by a document. For example, an
    -- @ApplicationConfiguration@ document requires an
    -- @ApplicationConfigurationSchema@ document.
    requires :: Prelude.Maybe (Prelude.NonEmpty DocumentRequires),
    -- | The document type.
    documentType :: Prelude.Maybe DocumentType,
    -- | The name of the SSM document.
    name :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the SSM document. This value can differ for each
    -- version of the document. If you want to update this value, see
    -- UpdateDocument.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The status of the SSM document, such as @Creating@, @Active@,
    -- @Updating@, @Failed@, and @Deleting@.
    status :: Prelude.Maybe DocumentStatus,
    -- | The version of the artifact associated with the document. For example,
    -- \"Release 12, Update 6\". This value is unique across all versions of a
    -- document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | A message returned by Amazon Web Services Systems Manager that explains
    -- the @Status@ value. For example, a @Failed@ status might be explained by
    -- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
    -- exist. Verify that the URL of the S3 bucket is correct.\"
    statusInformation :: Prelude.Maybe Prelude.Text,
    -- | The date the SSM document was created.
    createdDate :: Prelude.Maybe Data.POSIX,
    -- | The contents of the SSM document.
    content :: Prelude.Maybe Prelude.Text,
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
    -- | The document format, either JSON or YAML.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The document version.
    documentVersion :: Prelude.Maybe Prelude.Text,
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
-- 'requires', 'getDocumentResponse_requires' - A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
--
-- 'documentType', 'getDocumentResponse_documentType' - The document type.
--
-- 'name', 'getDocumentResponse_name' - The name of the SSM document.
--
-- 'displayName', 'getDocumentResponse_displayName' - The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
--
-- 'status', 'getDocumentResponse_status' - The status of the SSM document, such as @Creating@, @Active@,
-- @Updating@, @Failed@, and @Deleting@.
--
-- 'versionName', 'getDocumentResponse_versionName' - The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and can\'t be changed.
--
-- 'statusInformation', 'getDocumentResponse_statusInformation' - A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
--
-- 'createdDate', 'getDocumentResponse_createdDate' - The date the SSM document was created.
--
-- 'content', 'getDocumentResponse_content' - The contents of the SSM document.
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
-- 'documentFormat', 'getDocumentResponse_documentFormat' - The document format, either JSON or YAML.
--
-- 'documentVersion', 'getDocumentResponse_documentVersion' - The document version.
--
-- 'httpStatus', 'getDocumentResponse_httpStatus' - The response's http status code.
newGetDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDocumentResponse
newGetDocumentResponse pHttpStatus_ =
  GetDocumentResponse'
    { requires = Prelude.Nothing,
      documentType = Prelude.Nothing,
      name = Prelude.Nothing,
      displayName = Prelude.Nothing,
      status = Prelude.Nothing,
      versionName = Prelude.Nothing,
      statusInformation = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      content = Prelude.Nothing,
      attachmentsContent = Prelude.Nothing,
      reviewStatus = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of SSM documents required by a document. For example, an
-- @ApplicationConfiguration@ document requires an
-- @ApplicationConfigurationSchema@ document.
getDocumentResponse_requires :: Lens.Lens' GetDocumentResponse (Prelude.Maybe (Prelude.NonEmpty DocumentRequires))
getDocumentResponse_requires = Lens.lens (\GetDocumentResponse' {requires} -> requires) (\s@GetDocumentResponse' {} a -> s {requires = a} :: GetDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The document type.
getDocumentResponse_documentType :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentType)
getDocumentResponse_documentType = Lens.lens (\GetDocumentResponse' {documentType} -> documentType) (\s@GetDocumentResponse' {} a -> s {documentType = a} :: GetDocumentResponse)

-- | The name of the SSM document.
getDocumentResponse_name :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_name = Lens.lens (\GetDocumentResponse' {name} -> name) (\s@GetDocumentResponse' {} a -> s {name = a} :: GetDocumentResponse)

-- | The friendly name of the SSM document. This value can differ for each
-- version of the document. If you want to update this value, see
-- UpdateDocument.
getDocumentResponse_displayName :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_displayName = Lens.lens (\GetDocumentResponse' {displayName} -> displayName) (\s@GetDocumentResponse' {} a -> s {displayName = a} :: GetDocumentResponse)

-- | The status of the SSM document, such as @Creating@, @Active@,
-- @Updating@, @Failed@, and @Deleting@.
getDocumentResponse_status :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentStatus)
getDocumentResponse_status = Lens.lens (\GetDocumentResponse' {status} -> status) (\s@GetDocumentResponse' {} a -> s {status = a} :: GetDocumentResponse)

-- | The version of the artifact associated with the document. For example,
-- \"Release 12, Update 6\". This value is unique across all versions of a
-- document, and can\'t be changed.
getDocumentResponse_versionName :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_versionName = Lens.lens (\GetDocumentResponse' {versionName} -> versionName) (\s@GetDocumentResponse' {} a -> s {versionName = a} :: GetDocumentResponse)

-- | A message returned by Amazon Web Services Systems Manager that explains
-- the @Status@ value. For example, a @Failed@ status might be explained by
-- the @StatusInformation@ message, \"The specified S3 bucket doesn\'t
-- exist. Verify that the URL of the S3 bucket is correct.\"
getDocumentResponse_statusInformation :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_statusInformation = Lens.lens (\GetDocumentResponse' {statusInformation} -> statusInformation) (\s@GetDocumentResponse' {} a -> s {statusInformation = a} :: GetDocumentResponse)

-- | The date the SSM document was created.
getDocumentResponse_createdDate :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.UTCTime)
getDocumentResponse_createdDate = Lens.lens (\GetDocumentResponse' {createdDate} -> createdDate) (\s@GetDocumentResponse' {} a -> s {createdDate = a} :: GetDocumentResponse) Prelude.. Lens.mapping Data._Time

-- | The contents of the SSM document.
getDocumentResponse_content :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_content = Lens.lens (\GetDocumentResponse' {content} -> content) (\s@GetDocumentResponse' {} a -> s {content = a} :: GetDocumentResponse)

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

-- | The document format, either JSON or YAML.
getDocumentResponse_documentFormat :: Lens.Lens' GetDocumentResponse (Prelude.Maybe DocumentFormat)
getDocumentResponse_documentFormat = Lens.lens (\GetDocumentResponse' {documentFormat} -> documentFormat) (\s@GetDocumentResponse' {} a -> s {documentFormat = a} :: GetDocumentResponse)

-- | The document version.
getDocumentResponse_documentVersion :: Lens.Lens' GetDocumentResponse (Prelude.Maybe Prelude.Text)
getDocumentResponse_documentVersion = Lens.lens (\GetDocumentResponse' {documentVersion} -> documentVersion) (\s@GetDocumentResponse' {} a -> s {documentVersion = a} :: GetDocumentResponse)

-- | The response's http status code.
getDocumentResponse_httpStatus :: Lens.Lens' GetDocumentResponse Prelude.Int
getDocumentResponse_httpStatus = Lens.lens (\GetDocumentResponse' {httpStatus} -> httpStatus) (\s@GetDocumentResponse' {} a -> s {httpStatus = a} :: GetDocumentResponse)

instance Prelude.NFData GetDocumentResponse where
  rnf GetDocumentResponse' {..} =
    Prelude.rnf requires
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf statusInformation
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf attachmentsContent
      `Prelude.seq` Prelude.rnf reviewStatus
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf httpStatus
