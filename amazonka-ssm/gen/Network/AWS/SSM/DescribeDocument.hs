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
-- Module      : Network.AWS.SSM.DescribeDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Systems Manager document.
module Network.AWS.SSM.DescribeDocument
  ( -- * Creating a Request
    DescribeDocument (..),
    newDescribeDocument,

    -- * Request Lenses
    describeDocument_versionName,
    describeDocument_documentVersion,
    describeDocument_name,

    -- * Destructuring the Response
    DescribeDocumentResponse (..),
    newDescribeDocumentResponse,

    -- * Response Lenses
    describeDocumentResponse_document,
    describeDocumentResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newDescribeDocument' smart constructor.
data DescribeDocument = DescribeDocument'
  { -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document, and cannot be changed.
    versionName :: Core.Maybe Core.Text,
    -- | The document version for which you want information. Can be a specific
    -- version or the default version.
    documentVersion :: Core.Maybe Core.Text,
    -- | The name of the Systems Manager document.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionName', 'describeDocument_versionName' - An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and cannot be changed.
--
-- 'documentVersion', 'describeDocument_documentVersion' - The document version for which you want information. Can be a specific
-- version or the default version.
--
-- 'name', 'describeDocument_name' - The name of the Systems Manager document.
newDescribeDocument ::
  -- | 'name'
  Core.Text ->
  DescribeDocument
newDescribeDocument pName_ =
  DescribeDocument'
    { versionName = Core.Nothing,
      documentVersion = Core.Nothing,
      name = pName_
    }

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and cannot be changed.
describeDocument_versionName :: Lens.Lens' DescribeDocument (Core.Maybe Core.Text)
describeDocument_versionName = Lens.lens (\DescribeDocument' {versionName} -> versionName) (\s@DescribeDocument' {} a -> s {versionName = a} :: DescribeDocument)

-- | The document version for which you want information. Can be a specific
-- version or the default version.
describeDocument_documentVersion :: Lens.Lens' DescribeDocument (Core.Maybe Core.Text)
describeDocument_documentVersion = Lens.lens (\DescribeDocument' {documentVersion} -> documentVersion) (\s@DescribeDocument' {} a -> s {documentVersion = a} :: DescribeDocument)

-- | The name of the Systems Manager document.
describeDocument_name :: Lens.Lens' DescribeDocument Core.Text
describeDocument_name = Lens.lens (\DescribeDocument' {name} -> name) (\s@DescribeDocument' {} a -> s {name = a} :: DescribeDocument)

instance Core.AWSRequest DescribeDocument where
  type
    AWSResponse DescribeDocument =
      DescribeDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentResponse'
            Core.<$> (x Core..?> "Document")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDocument

instance Core.NFData DescribeDocument

instance Core.ToHeaders DescribeDocument where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.DescribeDocument" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDocument where
  toJSON DescribeDocument' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VersionName" Core..=) Core.<$> versionName,
            ("DocumentVersion" Core..=) Core.<$> documentVersion,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath DescribeDocument where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDocument where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDocumentResponse' smart constructor.
data DescribeDocumentResponse = DescribeDocumentResponse'
  { -- | Information about the Systems Manager document.
    document :: Core.Maybe DocumentDescription,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'describeDocumentResponse_document' - Information about the Systems Manager document.
--
-- 'httpStatus', 'describeDocumentResponse_httpStatus' - The response's http status code.
newDescribeDocumentResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDocumentResponse
newDescribeDocumentResponse pHttpStatus_ =
  DescribeDocumentResponse'
    { document = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Systems Manager document.
describeDocumentResponse_document :: Lens.Lens' DescribeDocumentResponse (Core.Maybe DocumentDescription)
describeDocumentResponse_document = Lens.lens (\DescribeDocumentResponse' {document} -> document) (\s@DescribeDocumentResponse' {} a -> s {document = a} :: DescribeDocumentResponse)

-- | The response's http status code.
describeDocumentResponse_httpStatus :: Lens.Lens' DescribeDocumentResponse Core.Int
describeDocumentResponse_httpStatus = Lens.lens (\DescribeDocumentResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentResponse' {} a -> s {httpStatus = a} :: DescribeDocumentResponse)

instance Core.NFData DescribeDocumentResponse
