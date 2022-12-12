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
-- Module      : Amazonka.Comprehend.ClassifyDocument
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new document classification request to analyze a single
-- document in real-time, using a previously created and trained custom
-- model and an endpoint.
--
-- You can input plain text or you can upload a single-page input document
-- (text, PDF, Word, or image).
--
-- If the system detects errors while processing a page in the input
-- document, the API response includes an entry in @Errors@ that describes
-- the errors.
--
-- If the system detects a document-level error in your input document, the
-- API returns an @InvalidRequestException@ error response. For details
-- about this exception, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/idp-inputs-sync-err.html Errors in semi-structured documents>
-- in the Comprehend Developer Guide.
module Amazonka.Comprehend.ClassifyDocument
  ( -- * Creating a Request
    ClassifyDocument (..),
    newClassifyDocument,

    -- * Request Lenses
    classifyDocument_bytes,
    classifyDocument_documentReaderConfig,
    classifyDocument_text,
    classifyDocument_endpointArn,

    -- * Destructuring the Response
    ClassifyDocumentResponse (..),
    newClassifyDocumentResponse,

    -- * Response Lenses
    classifyDocumentResponse_classes,
    classifyDocumentResponse_documentMetadata,
    classifyDocumentResponse_documentType,
    classifyDocumentResponse_errors,
    classifyDocumentResponse_labels,
    classifyDocumentResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newClassifyDocument' smart constructor.
data ClassifyDocument = ClassifyDocument'
  { -- | Use the @Bytes@ parameter to input a text, PDF, Word or image file. You
    -- can also use the @Bytes@ parameter to input an Amazon Textract
    -- @DetectDocumentText@ or @AnalyzeDocument@ output file.
    --
    -- Provide the input document as a sequence of base64-encoded bytes. If
    -- your code uses an Amazon Web Services SDK to classify documents, the SDK
    -- may encode the document file bytes for you.
    --
    -- The maximum length of this field depends on the input document type. For
    -- details, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/idp-inputs-sync.html Inputs for real-time custom analysis>
    -- in the Comprehend Developer Guide.
    --
    -- If you use the @Bytes@ parameter, do not use the @Text@ parameter.
    bytes :: Prelude.Maybe Data.Base64,
    -- | Provides configuration parameters to override the default actions for
    -- extracting text from PDF documents and image files.
    documentReaderConfig :: Prelude.Maybe DocumentReaderConfig,
    -- | The document text to be analyzed. If you enter text using this
    -- parameter, do not use the @Bytes@ parameter.
    text :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Number (ARN) of the endpoint. For information about
    -- endpoints, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassifyDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytes', 'classifyDocument_bytes' - Use the @Bytes@ parameter to input a text, PDF, Word or image file. You
-- can also use the @Bytes@ parameter to input an Amazon Textract
-- @DetectDocumentText@ or @AnalyzeDocument@ output file.
--
-- Provide the input document as a sequence of base64-encoded bytes. If
-- your code uses an Amazon Web Services SDK to classify documents, the SDK
-- may encode the document file bytes for you.
--
-- The maximum length of this field depends on the input document type. For
-- details, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/idp-inputs-sync.html Inputs for real-time custom analysis>
-- in the Comprehend Developer Guide.
--
-- If you use the @Bytes@ parameter, do not use the @Text@ parameter.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'documentReaderConfig', 'classifyDocument_documentReaderConfig' - Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
--
-- 'text', 'classifyDocument_text' - The document text to be analyzed. If you enter text using this
-- parameter, do not use the @Bytes@ parameter.
--
-- 'endpointArn', 'classifyDocument_endpointArn' - The Amazon Resource Number (ARN) of the endpoint. For information about
-- endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
newClassifyDocument ::
  -- | 'endpointArn'
  Prelude.Text ->
  ClassifyDocument
newClassifyDocument pEndpointArn_ =
  ClassifyDocument'
    { bytes = Prelude.Nothing,
      documentReaderConfig = Prelude.Nothing,
      text = Prelude.Nothing,
      endpointArn = pEndpointArn_
    }

-- | Use the @Bytes@ parameter to input a text, PDF, Word or image file. You
-- can also use the @Bytes@ parameter to input an Amazon Textract
-- @DetectDocumentText@ or @AnalyzeDocument@ output file.
--
-- Provide the input document as a sequence of base64-encoded bytes. If
-- your code uses an Amazon Web Services SDK to classify documents, the SDK
-- may encode the document file bytes for you.
--
-- The maximum length of this field depends on the input document type. For
-- details, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/idp-inputs-sync.html Inputs for real-time custom analysis>
-- in the Comprehend Developer Guide.
--
-- If you use the @Bytes@ parameter, do not use the @Text@ parameter.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
classifyDocument_bytes :: Lens.Lens' ClassifyDocument (Prelude.Maybe Prelude.ByteString)
classifyDocument_bytes = Lens.lens (\ClassifyDocument' {bytes} -> bytes) (\s@ClassifyDocument' {} a -> s {bytes = a} :: ClassifyDocument) Prelude.. Lens.mapping Data._Base64

-- | Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
classifyDocument_documentReaderConfig :: Lens.Lens' ClassifyDocument (Prelude.Maybe DocumentReaderConfig)
classifyDocument_documentReaderConfig = Lens.lens (\ClassifyDocument' {documentReaderConfig} -> documentReaderConfig) (\s@ClassifyDocument' {} a -> s {documentReaderConfig = a} :: ClassifyDocument)

-- | The document text to be analyzed. If you enter text using this
-- parameter, do not use the @Bytes@ parameter.
classifyDocument_text :: Lens.Lens' ClassifyDocument (Prelude.Maybe Prelude.Text)
classifyDocument_text = Lens.lens (\ClassifyDocument' {text} -> text) (\s@ClassifyDocument' {} a -> s {text = a} :: ClassifyDocument) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Number (ARN) of the endpoint. For information about
-- endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
classifyDocument_endpointArn :: Lens.Lens' ClassifyDocument Prelude.Text
classifyDocument_endpointArn = Lens.lens (\ClassifyDocument' {endpointArn} -> endpointArn) (\s@ClassifyDocument' {} a -> s {endpointArn = a} :: ClassifyDocument)

instance Core.AWSRequest ClassifyDocument where
  type
    AWSResponse ClassifyDocument =
      ClassifyDocumentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ClassifyDocumentResponse'
            Prelude.<$> (x Data..?> "Classes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> (x Data..?> "DocumentType" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Labels" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ClassifyDocument where
  hashWithSalt _salt ClassifyDocument' {..} =
    _salt `Prelude.hashWithSalt` bytes
      `Prelude.hashWithSalt` documentReaderConfig
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData ClassifyDocument where
  rnf ClassifyDocument' {..} =
    Prelude.rnf bytes
      `Prelude.seq` Prelude.rnf documentReaderConfig
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf endpointArn

instance Data.ToHeaders ClassifyDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ClassifyDocument" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ClassifyDocument where
  toJSON ClassifyDocument' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bytes" Data..=) Prelude.<$> bytes,
            ("DocumentReaderConfig" Data..=)
              Prelude.<$> documentReaderConfig,
            ("Text" Data..=) Prelude.<$> text,
            Prelude.Just ("EndpointArn" Data..= endpointArn)
          ]
      )

instance Data.ToPath ClassifyDocument where
  toPath = Prelude.const "/"

instance Data.ToQuery ClassifyDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newClassifyDocumentResponse' smart constructor.
data ClassifyDocumentResponse = ClassifyDocumentResponse'
  { -- | The classes used by the document being analyzed. These are used for
    -- multi-class trained models. Individual classes are mutually exclusive
    -- and each document is expected to have only a single class assigned to
    -- it. For example, an animal can be a dog or a cat, but not both at the
    -- same time.
    classes :: Prelude.Maybe [DocumentClass],
    -- | Extraction information about the document. This field is present in the
    -- response only if your request includes the @Byte@ parameter.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The document type for each page in the input document. This field is
    -- present in the response only if your request includes the @Byte@
    -- parameter.
    documentType :: Prelude.Maybe [DocumentTypeListItem],
    -- | Page-level errors that the system detected while processing the input
    -- document. The field is empty if the system encountered no errors.
    errors :: Prelude.Maybe [ErrorsListItem],
    -- | The labels used the document being analyzed. These are used for
    -- multi-label trained models. Individual labels represent different
    -- categories that are related in some manner and are not mutually
    -- exclusive. For example, a movie can be just an action movie, or it can
    -- be an action movie, a science fiction movie, and a comedy, all at the
    -- same time.
    labels :: Prelude.Maybe [DocumentLabel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClassifyDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'classes', 'classifyDocumentResponse_classes' - The classes used by the document being analyzed. These are used for
-- multi-class trained models. Individual classes are mutually exclusive
-- and each document is expected to have only a single class assigned to
-- it. For example, an animal can be a dog or a cat, but not both at the
-- same time.
--
-- 'documentMetadata', 'classifyDocumentResponse_documentMetadata' - Extraction information about the document. This field is present in the
-- response only if your request includes the @Byte@ parameter.
--
-- 'documentType', 'classifyDocumentResponse_documentType' - The document type for each page in the input document. This field is
-- present in the response only if your request includes the @Byte@
-- parameter.
--
-- 'errors', 'classifyDocumentResponse_errors' - Page-level errors that the system detected while processing the input
-- document. The field is empty if the system encountered no errors.
--
-- 'labels', 'classifyDocumentResponse_labels' - The labels used the document being analyzed. These are used for
-- multi-label trained models. Individual labels represent different
-- categories that are related in some manner and are not mutually
-- exclusive. For example, a movie can be just an action movie, or it can
-- be an action movie, a science fiction movie, and a comedy, all at the
-- same time.
--
-- 'httpStatus', 'classifyDocumentResponse_httpStatus' - The response's http status code.
newClassifyDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ClassifyDocumentResponse
newClassifyDocumentResponse pHttpStatus_ =
  ClassifyDocumentResponse'
    { classes =
        Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      documentType = Prelude.Nothing,
      errors = Prelude.Nothing,
      labels = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The classes used by the document being analyzed. These are used for
-- multi-class trained models. Individual classes are mutually exclusive
-- and each document is expected to have only a single class assigned to
-- it. For example, an animal can be a dog or a cat, but not both at the
-- same time.
classifyDocumentResponse_classes :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [DocumentClass])
classifyDocumentResponse_classes = Lens.lens (\ClassifyDocumentResponse' {classes} -> classes) (\s@ClassifyDocumentResponse' {} a -> s {classes = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | Extraction information about the document. This field is present in the
-- response only if your request includes the @Byte@ parameter.
classifyDocumentResponse_documentMetadata :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe DocumentMetadata)
classifyDocumentResponse_documentMetadata = Lens.lens (\ClassifyDocumentResponse' {documentMetadata} -> documentMetadata) (\s@ClassifyDocumentResponse' {} a -> s {documentMetadata = a} :: ClassifyDocumentResponse)

-- | The document type for each page in the input document. This field is
-- present in the response only if your request includes the @Byte@
-- parameter.
classifyDocumentResponse_documentType :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [DocumentTypeListItem])
classifyDocumentResponse_documentType = Lens.lens (\ClassifyDocumentResponse' {documentType} -> documentType) (\s@ClassifyDocumentResponse' {} a -> s {documentType = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | Page-level errors that the system detected while processing the input
-- document. The field is empty if the system encountered no errors.
classifyDocumentResponse_errors :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [ErrorsListItem])
classifyDocumentResponse_errors = Lens.lens (\ClassifyDocumentResponse' {errors} -> errors) (\s@ClassifyDocumentResponse' {} a -> s {errors = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The labels used the document being analyzed. These are used for
-- multi-label trained models. Individual labels represent different
-- categories that are related in some manner and are not mutually
-- exclusive. For example, a movie can be just an action movie, or it can
-- be an action movie, a science fiction movie, and a comedy, all at the
-- same time.
classifyDocumentResponse_labels :: Lens.Lens' ClassifyDocumentResponse (Prelude.Maybe [DocumentLabel])
classifyDocumentResponse_labels = Lens.lens (\ClassifyDocumentResponse' {labels} -> labels) (\s@ClassifyDocumentResponse' {} a -> s {labels = a} :: ClassifyDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
classifyDocumentResponse_httpStatus :: Lens.Lens' ClassifyDocumentResponse Prelude.Int
classifyDocumentResponse_httpStatus = Lens.lens (\ClassifyDocumentResponse' {httpStatus} -> httpStatus) (\s@ClassifyDocumentResponse' {} a -> s {httpStatus = a} :: ClassifyDocumentResponse)

instance Prelude.NFData ClassifyDocumentResponse where
  rnf ClassifyDocumentResponse' {..} =
    Prelude.rnf classes
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf httpStatus
