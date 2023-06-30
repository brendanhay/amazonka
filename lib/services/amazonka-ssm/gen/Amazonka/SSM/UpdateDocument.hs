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
-- Module      : Amazonka.SSM.UpdateDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more values for an SSM document.
module Amazonka.SSM.UpdateDocument
  ( -- * Creating a Request
    UpdateDocument (..),
    newUpdateDocument,

    -- * Request Lenses
    updateDocument_attachments,
    updateDocument_displayName,
    updateDocument_documentFormat,
    updateDocument_documentVersion,
    updateDocument_targetType,
    updateDocument_versionName,
    updateDocument_content,
    updateDocument_name,

    -- * Destructuring the Response
    UpdateDocumentResponse (..),
    newUpdateDocumentResponse,

    -- * Response Lenses
    updateDocumentResponse_documentDescription,
    updateDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | A list of key-value pairs that describe attachments to a version of a
    -- document.
    attachments :: Prelude.Maybe [AttachmentsSource],
    -- | The friendly name of the SSM document that you want to update. This
    -- value can differ for each version of the document. If you don\'t specify
    -- a value for this parameter in your request, the existing value is
    -- applied to the new document version.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Specify the document format for the new document version. Systems
    -- Manager supports JSON and YAML documents. JSON is the default format.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | The version of the document that you want to update. Currently, Systems
    -- Manager supports updating only the latest version of the document. You
    -- can specify the version number of the latest version or use the
    -- @$LATEST@ variable.
    --
    -- If you change a document version for a State Manager association,
    -- Systems Manager immediately runs the association unless you previously
    -- specifed the @apply-only-at-cron-interval@ parameter.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | Specify a new target type for the document.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | An optional field specifying the version of the artifact you are
    -- updating with the document. For example, \"Release 12, Update 6\". This
    -- value is unique across all versions of a document, and can\'t be
    -- changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | A valid JSON or YAML string.
    content :: Prelude.Text,
    -- | The name of the SSM document that you want to update.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachments', 'updateDocument_attachments' - A list of key-value pairs that describe attachments to a version of a
-- document.
--
-- 'displayName', 'updateDocument_displayName' - The friendly name of the SSM document that you want to update. This
-- value can differ for each version of the document. If you don\'t specify
-- a value for this parameter in your request, the existing value is
-- applied to the new document version.
--
-- 'documentFormat', 'updateDocument_documentFormat' - Specify the document format for the new document version. Systems
-- Manager supports JSON and YAML documents. JSON is the default format.
--
-- 'documentVersion', 'updateDocument_documentVersion' - The version of the document that you want to update. Currently, Systems
-- Manager supports updating only the latest version of the document. You
-- can specify the version number of the latest version or use the
-- @$LATEST@ variable.
--
-- If you change a document version for a State Manager association,
-- Systems Manager immediately runs the association unless you previously
-- specifed the @apply-only-at-cron-interval@ parameter.
--
-- 'targetType', 'updateDocument_targetType' - Specify a new target type for the document.
--
-- 'versionName', 'updateDocument_versionName' - An optional field specifying the version of the artifact you are
-- updating with the document. For example, \"Release 12, Update 6\". This
-- value is unique across all versions of a document, and can\'t be
-- changed.
--
-- 'content', 'updateDocument_content' - A valid JSON or YAML string.
--
-- 'name', 'updateDocument_name' - The name of the SSM document that you want to update.
newUpdateDocument ::
  -- | 'content'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateDocument
newUpdateDocument pContent_ pName_ =
  UpdateDocument'
    { attachments = Prelude.Nothing,
      displayName = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      targetType = Prelude.Nothing,
      versionName = Prelude.Nothing,
      content = pContent_,
      name = pName_
    }

-- | A list of key-value pairs that describe attachments to a version of a
-- document.
updateDocument_attachments :: Lens.Lens' UpdateDocument (Prelude.Maybe [AttachmentsSource])
updateDocument_attachments = Lens.lens (\UpdateDocument' {attachments} -> attachments) (\s@UpdateDocument' {} a -> s {attachments = a} :: UpdateDocument) Prelude.. Lens.mapping Lens.coerced

-- | The friendly name of the SSM document that you want to update. This
-- value can differ for each version of the document. If you don\'t specify
-- a value for this parameter in your request, the existing value is
-- applied to the new document version.
updateDocument_displayName :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_displayName = Lens.lens (\UpdateDocument' {displayName} -> displayName) (\s@UpdateDocument' {} a -> s {displayName = a} :: UpdateDocument)

-- | Specify the document format for the new document version. Systems
-- Manager supports JSON and YAML documents. JSON is the default format.
updateDocument_documentFormat :: Lens.Lens' UpdateDocument (Prelude.Maybe DocumentFormat)
updateDocument_documentFormat = Lens.lens (\UpdateDocument' {documentFormat} -> documentFormat) (\s@UpdateDocument' {} a -> s {documentFormat = a} :: UpdateDocument)

-- | The version of the document that you want to update. Currently, Systems
-- Manager supports updating only the latest version of the document. You
-- can specify the version number of the latest version or use the
-- @$LATEST@ variable.
--
-- If you change a document version for a State Manager association,
-- Systems Manager immediately runs the association unless you previously
-- specifed the @apply-only-at-cron-interval@ parameter.
updateDocument_documentVersion :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_documentVersion = Lens.lens (\UpdateDocument' {documentVersion} -> documentVersion) (\s@UpdateDocument' {} a -> s {documentVersion = a} :: UpdateDocument)

-- | Specify a new target type for the document.
updateDocument_targetType :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_targetType = Lens.lens (\UpdateDocument' {targetType} -> targetType) (\s@UpdateDocument' {} a -> s {targetType = a} :: UpdateDocument)

-- | An optional field specifying the version of the artifact you are
-- updating with the document. For example, \"Release 12, Update 6\". This
-- value is unique across all versions of a document, and can\'t be
-- changed.
updateDocument_versionName :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_versionName = Lens.lens (\UpdateDocument' {versionName} -> versionName) (\s@UpdateDocument' {} a -> s {versionName = a} :: UpdateDocument)

-- | A valid JSON or YAML string.
updateDocument_content :: Lens.Lens' UpdateDocument Prelude.Text
updateDocument_content = Lens.lens (\UpdateDocument' {content} -> content) (\s@UpdateDocument' {} a -> s {content = a} :: UpdateDocument)

-- | The name of the SSM document that you want to update.
updateDocument_name :: Lens.Lens' UpdateDocument Prelude.Text
updateDocument_name = Lens.lens (\UpdateDocument' {name} -> name) (\s@UpdateDocument' {} a -> s {name = a} :: UpdateDocument)

instance Core.AWSRequest UpdateDocument where
  type
    AWSResponse UpdateDocument =
      UpdateDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDocumentResponse'
            Prelude.<$> (x Data..?> "DocumentDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDocument where
  hashWithSalt _salt UpdateDocument' {..} =
    _salt
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` documentFormat
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` content
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateDocument where
  rnf UpdateDocument' {..} =
    Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf documentFormat
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf content
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.UpdateDocument" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDocument where
  toJSON UpdateDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attachments" Data..=) Prelude.<$> attachments,
            ("DisplayName" Data..=) Prelude.<$> displayName,
            ("DocumentFormat" Data..=)
              Prelude.<$> documentFormat,
            ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("TargetType" Data..=) Prelude.<$> targetType,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just ("Content" Data..= content),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDocumentResponse' smart constructor.
data UpdateDocumentResponse = UpdateDocumentResponse'
  { -- | A description of the document that was updated.
    documentDescription :: Prelude.Maybe DocumentDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentDescription', 'updateDocumentResponse_documentDescription' - A description of the document that was updated.
--
-- 'httpStatus', 'updateDocumentResponse_httpStatus' - The response's http status code.
newUpdateDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDocumentResponse
newUpdateDocumentResponse pHttpStatus_ =
  UpdateDocumentResponse'
    { documentDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the document that was updated.
updateDocumentResponse_documentDescription :: Lens.Lens' UpdateDocumentResponse (Prelude.Maybe DocumentDescription)
updateDocumentResponse_documentDescription = Lens.lens (\UpdateDocumentResponse' {documentDescription} -> documentDescription) (\s@UpdateDocumentResponse' {} a -> s {documentDescription = a} :: UpdateDocumentResponse)

-- | The response's http status code.
updateDocumentResponse_httpStatus :: Lens.Lens' UpdateDocumentResponse Prelude.Int
updateDocumentResponse_httpStatus = Lens.lens (\UpdateDocumentResponse' {httpStatus} -> httpStatus) (\s@UpdateDocumentResponse' {} a -> s {httpStatus = a} :: UpdateDocumentResponse)

instance Prelude.NFData UpdateDocumentResponse where
  rnf UpdateDocumentResponse' {..} =
    Prelude.rnf documentDescription
      `Prelude.seq` Prelude.rnf httpStatus
