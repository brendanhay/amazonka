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
-- Module      : Amazonka.Textract.DetectDocumentText
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects text in the input document. Amazon Textract can detect lines of
-- text and the words that make up a line of text. The input document must
-- be in one of the following image formats: JPEG, PNG, PDF, or TIFF.
-- @DetectDocumentText@ returns the detected text in an array of Block
-- objects.
--
-- Each document page has as an associated @Block@ of type PAGE. Each PAGE
-- @Block@ object is the parent of LINE @Block@ objects that represent the
-- lines of detected text on a page. A LINE @Block@ object is a parent for
-- each word that makes up the line. Words are represented by @Block@
-- objects of type WORD.
--
-- @DetectDocumentText@ is a synchronous operation. To analyze documents
-- asynchronously, use StartDocumentTextDetection.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-detecting.html Document Text Detection>.
module Amazonka.Textract.DetectDocumentText
  ( -- * Creating a Request
    DetectDocumentText (..),
    newDetectDocumentText,

    -- * Request Lenses
    detectDocumentText_document,

    -- * Destructuring the Response
    DetectDocumentTextResponse (..),
    newDetectDocumentTextResponse,

    -- * Response Lenses
    detectDocumentTextResponse_blocks,
    detectDocumentTextResponse_detectDocumentTextModelVersion,
    detectDocumentTextResponse_documentMetadata,
    detectDocumentTextResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newDetectDocumentText' smart constructor.
data DetectDocumentText = DetectDocumentText'
  { -- | The input document as base64-encoded bytes or an Amazon S3 object. If
    -- you use the AWS CLI to call Amazon Textract operations, you can\'t pass
    -- image bytes. The document must be an image in JPEG or PNG format.
    --
    -- If you\'re using an AWS SDK to call Amazon Textract, you might not need
    -- to base64-encode image bytes that are passed using the @Bytes@ field.
    document :: Document
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectDocumentText' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'detectDocumentText_document' - The input document as base64-encoded bytes or an Amazon S3 object. If
-- you use the AWS CLI to call Amazon Textract operations, you can\'t pass
-- image bytes. The document must be an image in JPEG or PNG format.
--
-- If you\'re using an AWS SDK to call Amazon Textract, you might not need
-- to base64-encode image bytes that are passed using the @Bytes@ field.
newDetectDocumentText ::
  -- | 'document'
  Document ->
  DetectDocumentText
newDetectDocumentText pDocument_ =
  DetectDocumentText' {document = pDocument_}

-- | The input document as base64-encoded bytes or an Amazon S3 object. If
-- you use the AWS CLI to call Amazon Textract operations, you can\'t pass
-- image bytes. The document must be an image in JPEG or PNG format.
--
-- If you\'re using an AWS SDK to call Amazon Textract, you might not need
-- to base64-encode image bytes that are passed using the @Bytes@ field.
detectDocumentText_document :: Lens.Lens' DetectDocumentText Document
detectDocumentText_document = Lens.lens (\DetectDocumentText' {document} -> document) (\s@DetectDocumentText' {} a -> s {document = a} :: DetectDocumentText)

instance Core.AWSRequest DetectDocumentText where
  type
    AWSResponse DetectDocumentText =
      DetectDocumentTextResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectDocumentTextResponse'
            Prelude.<$> (x Data..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "DetectDocumentTextModelVersion")
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectDocumentText where
  hashWithSalt _salt DetectDocumentText' {..} =
    _salt `Prelude.hashWithSalt` document

instance Prelude.NFData DetectDocumentText where
  rnf DetectDocumentText' {..} = Prelude.rnf document

instance Data.ToHeaders DetectDocumentText where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Textract.DetectDocumentText" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectDocumentText where
  toJSON DetectDocumentText' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Document" Data..= document)]
      )

instance Data.ToPath DetectDocumentText where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectDocumentText where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectDocumentTextResponse' smart constructor.
data DetectDocumentTextResponse = DetectDocumentTextResponse'
  { -- | An array of @Block@ objects that contain the text that\'s detected in
    -- the document.
    blocks :: Prelude.Maybe [Block],
    detectDocumentTextModelVersion :: Prelude.Maybe Prelude.Text,
    -- | Metadata about the document. It contains the number of pages that are
    -- detected in the document.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectDocumentTextResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blocks', 'detectDocumentTextResponse_blocks' - An array of @Block@ objects that contain the text that\'s detected in
-- the document.
--
-- 'detectDocumentTextModelVersion', 'detectDocumentTextResponse_detectDocumentTextModelVersion' -
--
-- 'documentMetadata', 'detectDocumentTextResponse_documentMetadata' - Metadata about the document. It contains the number of pages that are
-- detected in the document.
--
-- 'httpStatus', 'detectDocumentTextResponse_httpStatus' - The response's http status code.
newDetectDocumentTextResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectDocumentTextResponse
newDetectDocumentTextResponse pHttpStatus_ =
  DetectDocumentTextResponse'
    { blocks =
        Prelude.Nothing,
      detectDocumentTextModelVersion =
        Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @Block@ objects that contain the text that\'s detected in
-- the document.
detectDocumentTextResponse_blocks :: Lens.Lens' DetectDocumentTextResponse (Prelude.Maybe [Block])
detectDocumentTextResponse_blocks = Lens.lens (\DetectDocumentTextResponse' {blocks} -> blocks) (\s@DetectDocumentTextResponse' {} a -> s {blocks = a} :: DetectDocumentTextResponse) Prelude.. Lens.mapping Lens.coerced

detectDocumentTextResponse_detectDocumentTextModelVersion :: Lens.Lens' DetectDocumentTextResponse (Prelude.Maybe Prelude.Text)
detectDocumentTextResponse_detectDocumentTextModelVersion = Lens.lens (\DetectDocumentTextResponse' {detectDocumentTextModelVersion} -> detectDocumentTextModelVersion) (\s@DetectDocumentTextResponse' {} a -> s {detectDocumentTextModelVersion = a} :: DetectDocumentTextResponse)

-- | Metadata about the document. It contains the number of pages that are
-- detected in the document.
detectDocumentTextResponse_documentMetadata :: Lens.Lens' DetectDocumentTextResponse (Prelude.Maybe DocumentMetadata)
detectDocumentTextResponse_documentMetadata = Lens.lens (\DetectDocumentTextResponse' {documentMetadata} -> documentMetadata) (\s@DetectDocumentTextResponse' {} a -> s {documentMetadata = a} :: DetectDocumentTextResponse)

-- | The response's http status code.
detectDocumentTextResponse_httpStatus :: Lens.Lens' DetectDocumentTextResponse Prelude.Int
detectDocumentTextResponse_httpStatus = Lens.lens (\DetectDocumentTextResponse' {httpStatus} -> httpStatus) (\s@DetectDocumentTextResponse' {} a -> s {httpStatus = a} :: DetectDocumentTextResponse)

instance Prelude.NFData DetectDocumentTextResponse where
  rnf DetectDocumentTextResponse' {..} =
    Prelude.rnf blocks `Prelude.seq`
      Prelude.rnf detectDocumentTextModelVersion `Prelude.seq`
        Prelude.rnf documentMetadata `Prelude.seq`
          Prelude.rnf httpStatus
