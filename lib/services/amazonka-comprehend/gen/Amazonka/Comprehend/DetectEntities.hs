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
-- Module      : Amazonka.Comprehend.DetectEntities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detects named entities in input text when you use the pre-trained model.
-- Detects custom entities if you have a custom entity recognition model.
--
-- When detecting named entities using the pre-trained model, use plain
-- text as the input. For more information about named entities, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-entities.html Entities>
-- in the Comprehend Developer Guide.
--
-- When you use a custom entity recognition model, you can input plain text
-- or you can upload a single-page input document (text, PDF, Word, or
-- image).
--
-- If the system detects errors while processing a page in the input
-- document, the API response includes an entry in @Errors@ for each error.
--
-- If the system detects a document-level error in your input document, the
-- API returns an @InvalidRequestException@ error response. For details
-- about this exception, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/idp-inputs-sync-err.html Errors in semi-structured documents>
-- in the Comprehend Developer Guide.
module Amazonka.Comprehend.DetectEntities
  ( -- * Creating a Request
    DetectEntities (..),
    newDetectEntities,

    -- * Request Lenses
    detectEntities_bytes,
    detectEntities_documentReaderConfig,
    detectEntities_endpointArn,
    detectEntities_languageCode,
    detectEntities_text,

    -- * Destructuring the Response
    DetectEntitiesResponse (..),
    newDetectEntitiesResponse,

    -- * Response Lenses
    detectEntitiesResponse_blocks,
    detectEntitiesResponse_documentMetadata,
    detectEntitiesResponse_documentType,
    detectEntitiesResponse_entities,
    detectEntitiesResponse_errors,
    detectEntitiesResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetectEntities' smart constructor.
data DetectEntities = DetectEntities'
  { -- | This field applies only when you use a custom entity recognition model
    -- that was trained with PDF annotations. For other cases, enter your text
    -- input in the @Text@ field.
    --
    -- Use the @Bytes@ parameter to input a text, PDF, Word or image file.
    -- Using a plain-text file in the @Bytes@ parameter is equivelent to using
    -- the @Text@ parameter (the @Entities@ field in the response is
    -- identical).
    --
    -- You can also use the @Bytes@ parameter to input an Amazon Textract
    -- @DetectDocumentText@ or @AnalyzeDocument@ output file.
    --
    -- Provide the input document as a sequence of base64-encoded bytes. If
    -- your code uses an Amazon Web Services SDK to detect entities, the SDK
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
    -- | The Amazon Resource Name of an endpoint that is associated with a custom
    -- entity recognition model. Provide an endpoint if you want to detect
    -- entities by using your own custom model instead of the default model
    -- that is used by Amazon Comprehend.
    --
    -- If you specify an endpoint, Amazon Comprehend uses the language of your
    -- custom model, and it ignores any language code that you provide in your
    -- request.
    --
    -- For information about endpoints, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
    endpointArn :: Prelude.Maybe Prelude.Text,
    -- | The language of the input documents. You can specify any of the primary
    -- languages supported by Amazon Comprehend. If your request includes the
    -- endpoint for a custom entity recognition model, Amazon Comprehend uses
    -- the language of your custom model, and it ignores any language code that
    -- you specify here.
    --
    -- All input documents must be in the same language.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | A UTF-8 text string. The maximum string size is 100 KB. If you enter
    -- text using this parameter, do not use the @Bytes@ parameter.
    text :: Prelude.Maybe (Data.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectEntities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bytes', 'detectEntities_bytes' - This field applies only when you use a custom entity recognition model
-- that was trained with PDF annotations. For other cases, enter your text
-- input in the @Text@ field.
--
-- Use the @Bytes@ parameter to input a text, PDF, Word or image file.
-- Using a plain-text file in the @Bytes@ parameter is equivelent to using
-- the @Text@ parameter (the @Entities@ field in the response is
-- identical).
--
-- You can also use the @Bytes@ parameter to input an Amazon Textract
-- @DetectDocumentText@ or @AnalyzeDocument@ output file.
--
-- Provide the input document as a sequence of base64-encoded bytes. If
-- your code uses an Amazon Web Services SDK to detect entities, the SDK
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
-- 'documentReaderConfig', 'detectEntities_documentReaderConfig' - Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
--
-- 'endpointArn', 'detectEntities_endpointArn' - The Amazon Resource Name of an endpoint that is associated with a custom
-- entity recognition model. Provide an endpoint if you want to detect
-- entities by using your own custom model instead of the default model
-- that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your
-- custom model, and it ignores any language code that you provide in your
-- request.
--
-- For information about endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
--
-- 'languageCode', 'detectEntities_languageCode' - The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. If your request includes the
-- endpoint for a custom entity recognition model, Amazon Comprehend uses
-- the language of your custom model, and it ignores any language code that
-- you specify here.
--
-- All input documents must be in the same language.
--
-- 'text', 'detectEntities_text' - A UTF-8 text string. The maximum string size is 100 KB. If you enter
-- text using this parameter, do not use the @Bytes@ parameter.
newDetectEntities ::
  DetectEntities
newDetectEntities =
  DetectEntities'
    { bytes = Prelude.Nothing,
      documentReaderConfig = Prelude.Nothing,
      endpointArn = Prelude.Nothing,
      languageCode = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | This field applies only when you use a custom entity recognition model
-- that was trained with PDF annotations. For other cases, enter your text
-- input in the @Text@ field.
--
-- Use the @Bytes@ parameter to input a text, PDF, Word or image file.
-- Using a plain-text file in the @Bytes@ parameter is equivelent to using
-- the @Text@ parameter (the @Entities@ field in the response is
-- identical).
--
-- You can also use the @Bytes@ parameter to input an Amazon Textract
-- @DetectDocumentText@ or @AnalyzeDocument@ output file.
--
-- Provide the input document as a sequence of base64-encoded bytes. If
-- your code uses an Amazon Web Services SDK to detect entities, the SDK
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
detectEntities_bytes :: Lens.Lens' DetectEntities (Prelude.Maybe Prelude.ByteString)
detectEntities_bytes = Lens.lens (\DetectEntities' {bytes} -> bytes) (\s@DetectEntities' {} a -> s {bytes = a} :: DetectEntities) Prelude.. Lens.mapping Data._Base64

-- | Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
detectEntities_documentReaderConfig :: Lens.Lens' DetectEntities (Prelude.Maybe DocumentReaderConfig)
detectEntities_documentReaderConfig = Lens.lens (\DetectEntities' {documentReaderConfig} -> documentReaderConfig) (\s@DetectEntities' {} a -> s {documentReaderConfig = a} :: DetectEntities)

-- | The Amazon Resource Name of an endpoint that is associated with a custom
-- entity recognition model. Provide an endpoint if you want to detect
-- entities by using your own custom model instead of the default model
-- that is used by Amazon Comprehend.
--
-- If you specify an endpoint, Amazon Comprehend uses the language of your
-- custom model, and it ignores any language code that you provide in your
-- request.
--
-- For information about endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
detectEntities_endpointArn :: Lens.Lens' DetectEntities (Prelude.Maybe Prelude.Text)
detectEntities_endpointArn = Lens.lens (\DetectEntities' {endpointArn} -> endpointArn) (\s@DetectEntities' {} a -> s {endpointArn = a} :: DetectEntities)

-- | The language of the input documents. You can specify any of the primary
-- languages supported by Amazon Comprehend. If your request includes the
-- endpoint for a custom entity recognition model, Amazon Comprehend uses
-- the language of your custom model, and it ignores any language code that
-- you specify here.
--
-- All input documents must be in the same language.
detectEntities_languageCode :: Lens.Lens' DetectEntities (Prelude.Maybe LanguageCode)
detectEntities_languageCode = Lens.lens (\DetectEntities' {languageCode} -> languageCode) (\s@DetectEntities' {} a -> s {languageCode = a} :: DetectEntities)

-- | A UTF-8 text string. The maximum string size is 100 KB. If you enter
-- text using this parameter, do not use the @Bytes@ parameter.
detectEntities_text :: Lens.Lens' DetectEntities (Prelude.Maybe Prelude.Text)
detectEntities_text = Lens.lens (\DetectEntities' {text} -> text) (\s@DetectEntities' {} a -> s {text = a} :: DetectEntities) Prelude.. Lens.mapping Data._Sensitive

instance Core.AWSRequest DetectEntities where
  type
    AWSResponse DetectEntities =
      DetectEntitiesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetectEntitiesResponse'
            Prelude.<$> (x Data..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "DocumentMetadata")
            Prelude.<*> (x Data..?> "DocumentType" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Entities" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetectEntities where
  hashWithSalt _salt DetectEntities' {..} =
    _salt
      `Prelude.hashWithSalt` bytes
      `Prelude.hashWithSalt` documentReaderConfig
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` languageCode
      `Prelude.hashWithSalt` text

instance Prelude.NFData DetectEntities where
  rnf DetectEntities' {..} =
    Prelude.rnf bytes
      `Prelude.seq` Prelude.rnf documentReaderConfig
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf text

instance Data.ToHeaders DetectEntities where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.DetectEntities" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DetectEntities where
  toJSON DetectEntities' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Bytes" Data..=) Prelude.<$> bytes,
            ("DocumentReaderConfig" Data..=)
              Prelude.<$> documentReaderConfig,
            ("EndpointArn" Data..=) Prelude.<$> endpointArn,
            ("LanguageCode" Data..=) Prelude.<$> languageCode,
            ("Text" Data..=) Prelude.<$> text
          ]
      )

instance Data.ToPath DetectEntities where
  toPath = Prelude.const "/"

instance Data.ToQuery DetectEntities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetectEntitiesResponse' smart constructor.
data DetectEntitiesResponse = DetectEntitiesResponse'
  { -- | Information about each block of text in the input document. Blocks are
    -- nested. A page block contains a block for each line of text, which
    -- contains a block for each word.
    --
    -- The @Block@ content for a Word input document does not include a
    -- @Geometry@ field.
    --
    -- The @Block@ field is not present in the response for plain-text inputs.
    blocks :: Prelude.Maybe [Block],
    -- | Information about the document, discovered during text extraction. This
    -- field is present in the response only if your request used the @Byte@
    -- parameter.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The document type for each page in the input document. This field is
    -- present in the response only if your request used the @Byte@ parameter.
    documentType :: Prelude.Maybe [DocumentTypeListItem],
    -- | A collection of entities identified in the input text. For each entity,
    -- the response provides the entity text, entity type, where the entity
    -- text begins and ends, and the level of confidence that Amazon Comprehend
    -- has in the detection.
    --
    -- If your request uses a custom entity recognition model, Amazon
    -- Comprehend detects the entities that the model is trained to recognize.
    -- Otherwise, it detects the default entity types. For a list of default
    -- entity types, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/how-entities.html Entities>
    -- in the Comprehend Developer Guide.
    entities :: Prelude.Maybe [Entity],
    -- | Page-level errors that the system detected while processing the input
    -- document. The field is empty if the system encountered no errors.
    errors :: Prelude.Maybe [ErrorsListItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectEntitiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blocks', 'detectEntitiesResponse_blocks' - Information about each block of text in the input document. Blocks are
-- nested. A page block contains a block for each line of text, which
-- contains a block for each word.
--
-- The @Block@ content for a Word input document does not include a
-- @Geometry@ field.
--
-- The @Block@ field is not present in the response for plain-text inputs.
--
-- 'documentMetadata', 'detectEntitiesResponse_documentMetadata' - Information about the document, discovered during text extraction. This
-- field is present in the response only if your request used the @Byte@
-- parameter.
--
-- 'documentType', 'detectEntitiesResponse_documentType' - The document type for each page in the input document. This field is
-- present in the response only if your request used the @Byte@ parameter.
--
-- 'entities', 'detectEntitiesResponse_entities' - A collection of entities identified in the input text. For each entity,
-- the response provides the entity text, entity type, where the entity
-- text begins and ends, and the level of confidence that Amazon Comprehend
-- has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon
-- Comprehend detects the entities that the model is trained to recognize.
-- Otherwise, it detects the default entity types. For a list of default
-- entity types, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-entities.html Entities>
-- in the Comprehend Developer Guide.
--
-- 'errors', 'detectEntitiesResponse_errors' - Page-level errors that the system detected while processing the input
-- document. The field is empty if the system encountered no errors.
--
-- 'httpStatus', 'detectEntitiesResponse_httpStatus' - The response's http status code.
newDetectEntitiesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetectEntitiesResponse
newDetectEntitiesResponse pHttpStatus_ =
  DetectEntitiesResponse'
    { blocks = Prelude.Nothing,
      documentMetadata = Prelude.Nothing,
      documentType = Prelude.Nothing,
      entities = Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about each block of text in the input document. Blocks are
-- nested. A page block contains a block for each line of text, which
-- contains a block for each word.
--
-- The @Block@ content for a Word input document does not include a
-- @Geometry@ field.
--
-- The @Block@ field is not present in the response for plain-text inputs.
detectEntitiesResponse_blocks :: Lens.Lens' DetectEntitiesResponse (Prelude.Maybe [Block])
detectEntitiesResponse_blocks = Lens.lens (\DetectEntitiesResponse' {blocks} -> blocks) (\s@DetectEntitiesResponse' {} a -> s {blocks = a} :: DetectEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the document, discovered during text extraction. This
-- field is present in the response only if your request used the @Byte@
-- parameter.
detectEntitiesResponse_documentMetadata :: Lens.Lens' DetectEntitiesResponse (Prelude.Maybe DocumentMetadata)
detectEntitiesResponse_documentMetadata = Lens.lens (\DetectEntitiesResponse' {documentMetadata} -> documentMetadata) (\s@DetectEntitiesResponse' {} a -> s {documentMetadata = a} :: DetectEntitiesResponse)

-- | The document type for each page in the input document. This field is
-- present in the response only if your request used the @Byte@ parameter.
detectEntitiesResponse_documentType :: Lens.Lens' DetectEntitiesResponse (Prelude.Maybe [DocumentTypeListItem])
detectEntitiesResponse_documentType = Lens.lens (\DetectEntitiesResponse' {documentType} -> documentType) (\s@DetectEntitiesResponse' {} a -> s {documentType = a} :: DetectEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | A collection of entities identified in the input text. For each entity,
-- the response provides the entity text, entity type, where the entity
-- text begins and ends, and the level of confidence that Amazon Comprehend
-- has in the detection.
--
-- If your request uses a custom entity recognition model, Amazon
-- Comprehend detects the entities that the model is trained to recognize.
-- Otherwise, it detects the default entity types. For a list of default
-- entity types, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/how-entities.html Entities>
-- in the Comprehend Developer Guide.
detectEntitiesResponse_entities :: Lens.Lens' DetectEntitiesResponse (Prelude.Maybe [Entity])
detectEntitiesResponse_entities = Lens.lens (\DetectEntitiesResponse' {entities} -> entities) (\s@DetectEntitiesResponse' {} a -> s {entities = a} :: DetectEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Page-level errors that the system detected while processing the input
-- document. The field is empty if the system encountered no errors.
detectEntitiesResponse_errors :: Lens.Lens' DetectEntitiesResponse (Prelude.Maybe [ErrorsListItem])
detectEntitiesResponse_errors = Lens.lens (\DetectEntitiesResponse' {errors} -> errors) (\s@DetectEntitiesResponse' {} a -> s {errors = a} :: DetectEntitiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
detectEntitiesResponse_httpStatus :: Lens.Lens' DetectEntitiesResponse Prelude.Int
detectEntitiesResponse_httpStatus = Lens.lens (\DetectEntitiesResponse' {httpStatus} -> httpStatus) (\s@DetectEntitiesResponse' {} a -> s {httpStatus = a} :: DetectEntitiesResponse)

instance Prelude.NFData DetectEntitiesResponse where
  rnf DetectEntitiesResponse' {..} =
    Prelude.rnf blocks
      `Prelude.seq` Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf entities
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf httpStatus
