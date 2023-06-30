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
-- Module      : Amazonka.SSM.DescribeDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified Amazon Web Services Systems Manager document
-- (SSM document).
module Amazonka.SSM.DescribeDocument
  ( -- * Creating a Request
    DescribeDocument (..),
    newDescribeDocument,

    -- * Request Lenses
    describeDocument_documentVersion,
    describeDocument_versionName,
    describeDocument_name,

    -- * Destructuring the Response
    DescribeDocumentResponse (..),
    newDescribeDocumentResponse,

    -- * Response Lenses
    describeDocumentResponse_document,
    describeDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newDescribeDocument' smart constructor.
data DescribeDocument = DescribeDocument'
  { -- | The document version for which you want information. Can be a specific
    -- version or the default version.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | An optional field specifying the version of the artifact associated with
    -- the document. For example, \"Release 12, Update 6\". This value is
    -- unique across all versions of a document, and can\'t be changed.
    versionName :: Prelude.Maybe Prelude.Text,
    -- | The name of the SSM document.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentVersion', 'describeDocument_documentVersion' - The document version for which you want information. Can be a specific
-- version or the default version.
--
-- 'versionName', 'describeDocument_versionName' - An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and can\'t be changed.
--
-- 'name', 'describeDocument_name' - The name of the SSM document.
newDescribeDocument ::
  -- | 'name'
  Prelude.Text ->
  DescribeDocument
newDescribeDocument pName_ =
  DescribeDocument'
    { documentVersion =
        Prelude.Nothing,
      versionName = Prelude.Nothing,
      name = pName_
    }

-- | The document version for which you want information. Can be a specific
-- version or the default version.
describeDocument_documentVersion :: Lens.Lens' DescribeDocument (Prelude.Maybe Prelude.Text)
describeDocument_documentVersion = Lens.lens (\DescribeDocument' {documentVersion} -> documentVersion) (\s@DescribeDocument' {} a -> s {documentVersion = a} :: DescribeDocument)

-- | An optional field specifying the version of the artifact associated with
-- the document. For example, \"Release 12, Update 6\". This value is
-- unique across all versions of a document, and can\'t be changed.
describeDocument_versionName :: Lens.Lens' DescribeDocument (Prelude.Maybe Prelude.Text)
describeDocument_versionName = Lens.lens (\DescribeDocument' {versionName} -> versionName) (\s@DescribeDocument' {} a -> s {versionName = a} :: DescribeDocument)

-- | The name of the SSM document.
describeDocument_name :: Lens.Lens' DescribeDocument Prelude.Text
describeDocument_name = Lens.lens (\DescribeDocument' {name} -> name) (\s@DescribeDocument' {} a -> s {name = a} :: DescribeDocument)

instance Core.AWSRequest DescribeDocument where
  type
    AWSResponse DescribeDocument =
      DescribeDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDocumentResponse'
            Prelude.<$> (x Data..?> "Document")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDocument where
  hashWithSalt _salt DescribeDocument' {..} =
    _salt
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` versionName
      `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeDocument where
  rnf DescribeDocument' {..} =
    Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf versionName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders DescribeDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.DescribeDocument" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDocument where
  toJSON DescribeDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentVersion" Data..=)
              Prelude.<$> documentVersion,
            ("VersionName" Data..=) Prelude.<$> versionName,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath DescribeDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDocumentResponse' smart constructor.
data DescribeDocumentResponse = DescribeDocumentResponse'
  { -- | Information about the SSM document.
    document :: Prelude.Maybe DocumentDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'describeDocumentResponse_document' - Information about the SSM document.
--
-- 'httpStatus', 'describeDocumentResponse_httpStatus' - The response's http status code.
newDescribeDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDocumentResponse
newDescribeDocumentResponse pHttpStatus_ =
  DescribeDocumentResponse'
    { document =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the SSM document.
describeDocumentResponse_document :: Lens.Lens' DescribeDocumentResponse (Prelude.Maybe DocumentDescription)
describeDocumentResponse_document = Lens.lens (\DescribeDocumentResponse' {document} -> document) (\s@DescribeDocumentResponse' {} a -> s {document = a} :: DescribeDocumentResponse)

-- | The response's http status code.
describeDocumentResponse_httpStatus :: Lens.Lens' DescribeDocumentResponse Prelude.Int
describeDocumentResponse_httpStatus = Lens.lens (\DescribeDocumentResponse' {httpStatus} -> httpStatus) (\s@DescribeDocumentResponse' {} a -> s {httpStatus = a} :: DescribeDocumentResponse)

instance Prelude.NFData DescribeDocumentResponse where
  rnf DescribeDocumentResponse' {..} =
    Prelude.rnf document
      `Prelude.seq` Prelude.rnf httpStatus
