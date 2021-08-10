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
-- Module      : Network.AWS.SSM.UpdateDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates one or more values for an SSM document.
module Network.AWS.SSM.UpdateDocument
  ( -- * Creating a Request
    UpdateDocument (..),
    newUpdateDocument,

    -- * Request Lenses
    updateDocument_targetType,
    updateDocument_versionName,
    updateDocument_documentFormat,
    updateDocument_documentVersion,
    updateDocument_attachments,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newUpdateDocument' smart constructor.
data UpdateDocument = UpdateDocument'
  { -- | Specify a new target type for the document.
    targetType :: Prelude.Maybe Prelude.Text,
    -- | An optional field specifying the version of the artifact you are
    -- updating with the document. For example, \"Release 12, Update 6\". This
    -- value is unique across all versions of a document, and cannot be
    -- changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | Specify the document format for the new document version. Systems
    -- Manager supports JSON and YAML documents. JSON is the default format.
    documentFormat :: Prelude.Maybe DocumentFormat,
    -- | (Required) The latest version of the document that you want to update.
    -- The latest document version can be specified using the $LATEST variable
    -- or by the version number. Updating a previous version of a document is
    -- not supported.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of key and value pairs that describe attachments to a version of
    -- a document.
    attachments :: Prelude.Maybe [AttachmentsSource],
    -- | A valid JSON or YAML string.
    content :: Prelude.Text,
    -- | The name of the document that you want to update.
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
-- 'targetType', 'updateDocument_targetType' - Specify a new target type for the document.
--
-- 'versionName', 'updateDocument_versionName' - An optional field specifying the version of the artifact you are
-- updating with the document. For example, \"Release 12, Update 6\". This
-- value is unique across all versions of a document, and cannot be
-- changed.
--
-- 'documentFormat', 'updateDocument_documentFormat' - Specify the document format for the new document version. Systems
-- Manager supports JSON and YAML documents. JSON is the default format.
--
-- 'documentVersion', 'updateDocument_documentVersion' - (Required) The latest version of the document that you want to update.
-- The latest document version can be specified using the $LATEST variable
-- or by the version number. Updating a previous version of a document is
-- not supported.
--
-- 'attachments', 'updateDocument_attachments' - A list of key and value pairs that describe attachments to a version of
-- a document.
--
-- 'content', 'updateDocument_content' - A valid JSON or YAML string.
--
-- 'name', 'updateDocument_name' - The name of the document that you want to update.
newUpdateDocument ::
  -- | 'content'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateDocument
newUpdateDocument pContent_ pName_ =
  UpdateDocument'
    { targetType = Prelude.Nothing,
      versionName = Prelude.Nothing,
      documentFormat = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      attachments = Prelude.Nothing,
      content = pContent_,
      name = pName_
    }

-- | Specify a new target type for the document.
updateDocument_targetType :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_targetType = Lens.lens (\UpdateDocument' {targetType} -> targetType) (\s@UpdateDocument' {} a -> s {targetType = a} :: UpdateDocument)

-- | An optional field specifying the version of the artifact you are
-- updating with the document. For example, \"Release 12, Update 6\". This
-- value is unique across all versions of a document, and cannot be
-- changed.
updateDocument_versionName :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_versionName = Lens.lens (\UpdateDocument' {versionName} -> versionName) (\s@UpdateDocument' {} a -> s {versionName = a} :: UpdateDocument)

-- | Specify the document format for the new document version. Systems
-- Manager supports JSON and YAML documents. JSON is the default format.
updateDocument_documentFormat :: Lens.Lens' UpdateDocument (Prelude.Maybe DocumentFormat)
updateDocument_documentFormat = Lens.lens (\UpdateDocument' {documentFormat} -> documentFormat) (\s@UpdateDocument' {} a -> s {documentFormat = a} :: UpdateDocument)

-- | (Required) The latest version of the document that you want to update.
-- The latest document version can be specified using the $LATEST variable
-- or by the version number. Updating a previous version of a document is
-- not supported.
updateDocument_documentVersion :: Lens.Lens' UpdateDocument (Prelude.Maybe Prelude.Text)
updateDocument_documentVersion = Lens.lens (\UpdateDocument' {documentVersion} -> documentVersion) (\s@UpdateDocument' {} a -> s {documentVersion = a} :: UpdateDocument)

-- | A list of key and value pairs that describe attachments to a version of
-- a document.
updateDocument_attachments :: Lens.Lens' UpdateDocument (Prelude.Maybe [AttachmentsSource])
updateDocument_attachments = Lens.lens (\UpdateDocument' {attachments} -> attachments) (\s@UpdateDocument' {} a -> s {attachments = a} :: UpdateDocument) Prelude.. Lens.mapping Lens._Coerce

-- | A valid JSON or YAML string.
updateDocument_content :: Lens.Lens' UpdateDocument Prelude.Text
updateDocument_content = Lens.lens (\UpdateDocument' {content} -> content) (\s@UpdateDocument' {} a -> s {content = a} :: UpdateDocument)

-- | The name of the document that you want to update.
updateDocument_name :: Lens.Lens' UpdateDocument Prelude.Text
updateDocument_name = Lens.lens (\UpdateDocument' {name} -> name) (\s@UpdateDocument' {} a -> s {name = a} :: UpdateDocument)

instance Core.AWSRequest UpdateDocument where
  type
    AWSResponse UpdateDocument =
      UpdateDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDocumentResponse'
            Prelude.<$> (x Core..?> "DocumentDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDocument

instance Prelude.NFData UpdateDocument

instance Core.ToHeaders UpdateDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.UpdateDocument" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDocument where
  toJSON UpdateDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TargetType" Core..=) Prelude.<$> targetType,
            ("VersionName" Core..=) Prelude.<$> versionName,
            ("DocumentFormat" Core..=)
              Prelude.<$> documentFormat,
            ("DocumentVersion" Core..=)
              Prelude.<$> documentVersion,
            ("Attachments" Core..=) Prelude.<$> attachments,
            Prelude.Just ("Content" Core..= content),
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath UpdateDocument where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateDocument where
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

instance Prelude.NFData UpdateDocumentResponse
