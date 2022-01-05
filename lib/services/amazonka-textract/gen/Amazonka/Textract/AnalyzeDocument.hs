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
-- Module      : Amazonka.Textract.AnalyzeDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Analyzes an input document for relationships between detected items.
--
-- The types of information returned are as follows:
--
-- -   Form data (key-value pairs). The related information is returned in
--     two Block objects, each of type @KEY_VALUE_SET@: a KEY @Block@
--     object and a VALUE @Block@ object. For example, /Name: Ana Silva
--     Carolina/ contains a key and value. /Name:/ is the key. /Ana Silva
--     Carolina/ is the value.
--
-- -   Table and table cell data. A TABLE @Block@ object contains
--     information about a detected table. A CELL @Block@ object is
--     returned for each cell in a table.
--
-- -   Lines and words of text. A LINE @Block@ object contains one or more
--     WORD @Block@ objects. All lines and words that are detected in the
--     document are returned (including text that doesn\'t have a
--     relationship with the value of @FeatureTypes@).
--
-- Selection elements such as check boxes and option buttons (radio
-- buttons) can be detected in form data and in tables. A SELECTION_ELEMENT
-- @Block@ object contains information about a selection element, including
-- the selection status.
--
-- You can choose which type of analysis to perform by specifying the
-- @FeatureTypes@ list.
--
-- The output is returned in a list of @Block@ objects.
--
-- @AnalyzeDocument@ is a synchronous operation. To analyze documents
-- asynchronously, use StartDocumentAnalysis.
--
-- For more information, see
-- <https://docs.aws.amazon.com/textract/latest/dg/how-it-works-analyzing.html Document Text Analysis>.
module Amazonka.Textract.AnalyzeDocument
  ( -- * Creating a Request
    AnalyzeDocument (..),
    newAnalyzeDocument,

    -- * Request Lenses
    analyzeDocument_humanLoopConfig,
    analyzeDocument_document,
    analyzeDocument_featureTypes,

    -- * Destructuring the Response
    AnalyzeDocumentResponse (..),
    newAnalyzeDocumentResponse,

    -- * Response Lenses
    analyzeDocumentResponse_documentMetadata,
    analyzeDocumentResponse_blocks,
    analyzeDocumentResponse_humanLoopActivationOutput,
    analyzeDocumentResponse_analyzeDocumentModelVersion,
    analyzeDocumentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Textract.Types

-- | /See:/ 'newAnalyzeDocument' smart constructor.
data AnalyzeDocument = AnalyzeDocument'
  { -- | Sets the configuration for the human in the loop workflow for analyzing
    -- documents.
    humanLoopConfig :: Prelude.Maybe HumanLoopConfig,
    -- | The input document as base64-encoded bytes or an Amazon S3 object. If
    -- you use the AWS CLI to call Amazon Textract operations, you can\'t pass
    -- image bytes. The document must be an image in JPEG or PNG format.
    --
    -- If you\'re using an AWS SDK to call Amazon Textract, you might not need
    -- to base64-encode image bytes that are passed using the @Bytes@ field.
    document :: Document,
    -- | A list of the types of analysis to perform. Add TABLES to the list to
    -- return information about the tables that are detected in the input
    -- document. Add FORMS to return detected form data. To perform both types
    -- of analysis, add TABLES and FORMS to @FeatureTypes@. All lines and words
    -- detected in the document are included in the response (including text
    -- that isn\'t related to the value of @FeatureTypes@).
    featureTypes :: [FeatureType]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'humanLoopConfig', 'analyzeDocument_humanLoopConfig' - Sets the configuration for the human in the loop workflow for analyzing
-- documents.
--
-- 'document', 'analyzeDocument_document' - The input document as base64-encoded bytes or an Amazon S3 object. If
-- you use the AWS CLI to call Amazon Textract operations, you can\'t pass
-- image bytes. The document must be an image in JPEG or PNG format.
--
-- If you\'re using an AWS SDK to call Amazon Textract, you might not need
-- to base64-encode image bytes that are passed using the @Bytes@ field.
--
-- 'featureTypes', 'analyzeDocument_featureTypes' - A list of the types of analysis to perform. Add TABLES to the list to
-- return information about the tables that are detected in the input
-- document. Add FORMS to return detected form data. To perform both types
-- of analysis, add TABLES and FORMS to @FeatureTypes@. All lines and words
-- detected in the document are included in the response (including text
-- that isn\'t related to the value of @FeatureTypes@).
newAnalyzeDocument ::
  -- | 'document'
  Document ->
  AnalyzeDocument
newAnalyzeDocument pDocument_ =
  AnalyzeDocument'
    { humanLoopConfig = Prelude.Nothing,
      document = pDocument_,
      featureTypes = Prelude.mempty
    }

-- | Sets the configuration for the human in the loop workflow for analyzing
-- documents.
analyzeDocument_humanLoopConfig :: Lens.Lens' AnalyzeDocument (Prelude.Maybe HumanLoopConfig)
analyzeDocument_humanLoopConfig = Lens.lens (\AnalyzeDocument' {humanLoopConfig} -> humanLoopConfig) (\s@AnalyzeDocument' {} a -> s {humanLoopConfig = a} :: AnalyzeDocument)

-- | The input document as base64-encoded bytes or an Amazon S3 object. If
-- you use the AWS CLI to call Amazon Textract operations, you can\'t pass
-- image bytes. The document must be an image in JPEG or PNG format.
--
-- If you\'re using an AWS SDK to call Amazon Textract, you might not need
-- to base64-encode image bytes that are passed using the @Bytes@ field.
analyzeDocument_document :: Lens.Lens' AnalyzeDocument Document
analyzeDocument_document = Lens.lens (\AnalyzeDocument' {document} -> document) (\s@AnalyzeDocument' {} a -> s {document = a} :: AnalyzeDocument)

-- | A list of the types of analysis to perform. Add TABLES to the list to
-- return information about the tables that are detected in the input
-- document. Add FORMS to return detected form data. To perform both types
-- of analysis, add TABLES and FORMS to @FeatureTypes@. All lines and words
-- detected in the document are included in the response (including text
-- that isn\'t related to the value of @FeatureTypes@).
analyzeDocument_featureTypes :: Lens.Lens' AnalyzeDocument [FeatureType]
analyzeDocument_featureTypes = Lens.lens (\AnalyzeDocument' {featureTypes} -> featureTypes) (\s@AnalyzeDocument' {} a -> s {featureTypes = a} :: AnalyzeDocument) Prelude.. Lens.coerced

instance Core.AWSRequest AnalyzeDocument where
  type
    AWSResponse AnalyzeDocument =
      AnalyzeDocumentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AnalyzeDocumentResponse'
            Prelude.<$> (x Core..?> "DocumentMetadata")
            Prelude.<*> (x Core..?> "Blocks" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "HumanLoopActivationOutput")
            Prelude.<*> (x Core..?> "AnalyzeDocumentModelVersion")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AnalyzeDocument where
  hashWithSalt _salt AnalyzeDocument' {..} =
    _salt `Prelude.hashWithSalt` humanLoopConfig
      `Prelude.hashWithSalt` document
      `Prelude.hashWithSalt` featureTypes

instance Prelude.NFData AnalyzeDocument where
  rnf AnalyzeDocument' {..} =
    Prelude.rnf humanLoopConfig
      `Prelude.seq` Prelude.rnf document
      `Prelude.seq` Prelude.rnf featureTypes

instance Core.ToHeaders AnalyzeDocument where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("Textract.AnalyzeDocument" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON AnalyzeDocument where
  toJSON AnalyzeDocument' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HumanLoopConfig" Core..=)
              Prelude.<$> humanLoopConfig,
            Prelude.Just ("Document" Core..= document),
            Prelude.Just ("FeatureTypes" Core..= featureTypes)
          ]
      )

instance Core.ToPath AnalyzeDocument where
  toPath = Prelude.const "/"

instance Core.ToQuery AnalyzeDocument where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAnalyzeDocumentResponse' smart constructor.
data AnalyzeDocumentResponse = AnalyzeDocumentResponse'
  { -- | Metadata about the analyzed document. An example is the number of pages.
    documentMetadata :: Prelude.Maybe DocumentMetadata,
    -- | The items that are detected and analyzed by @AnalyzeDocument@.
    blocks :: Prelude.Maybe [Block],
    -- | Shows the results of the human in the loop evaluation.
    humanLoopActivationOutput :: Prelude.Maybe HumanLoopActivationOutput,
    -- | The version of the model used to analyze the document.
    analyzeDocumentModelVersion :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalyzeDocumentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentMetadata', 'analyzeDocumentResponse_documentMetadata' - Metadata about the analyzed document. An example is the number of pages.
--
-- 'blocks', 'analyzeDocumentResponse_blocks' - The items that are detected and analyzed by @AnalyzeDocument@.
--
-- 'humanLoopActivationOutput', 'analyzeDocumentResponse_humanLoopActivationOutput' - Shows the results of the human in the loop evaluation.
--
-- 'analyzeDocumentModelVersion', 'analyzeDocumentResponse_analyzeDocumentModelVersion' - The version of the model used to analyze the document.
--
-- 'httpStatus', 'analyzeDocumentResponse_httpStatus' - The response's http status code.
newAnalyzeDocumentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AnalyzeDocumentResponse
newAnalyzeDocumentResponse pHttpStatus_ =
  AnalyzeDocumentResponse'
    { documentMetadata =
        Prelude.Nothing,
      blocks = Prelude.Nothing,
      humanLoopActivationOutput = Prelude.Nothing,
      analyzeDocumentModelVersion = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Metadata about the analyzed document. An example is the number of pages.
analyzeDocumentResponse_documentMetadata :: Lens.Lens' AnalyzeDocumentResponse (Prelude.Maybe DocumentMetadata)
analyzeDocumentResponse_documentMetadata = Lens.lens (\AnalyzeDocumentResponse' {documentMetadata} -> documentMetadata) (\s@AnalyzeDocumentResponse' {} a -> s {documentMetadata = a} :: AnalyzeDocumentResponse)

-- | The items that are detected and analyzed by @AnalyzeDocument@.
analyzeDocumentResponse_blocks :: Lens.Lens' AnalyzeDocumentResponse (Prelude.Maybe [Block])
analyzeDocumentResponse_blocks = Lens.lens (\AnalyzeDocumentResponse' {blocks} -> blocks) (\s@AnalyzeDocumentResponse' {} a -> s {blocks = a} :: AnalyzeDocumentResponse) Prelude.. Lens.mapping Lens.coerced

-- | Shows the results of the human in the loop evaluation.
analyzeDocumentResponse_humanLoopActivationOutput :: Lens.Lens' AnalyzeDocumentResponse (Prelude.Maybe HumanLoopActivationOutput)
analyzeDocumentResponse_humanLoopActivationOutput = Lens.lens (\AnalyzeDocumentResponse' {humanLoopActivationOutput} -> humanLoopActivationOutput) (\s@AnalyzeDocumentResponse' {} a -> s {humanLoopActivationOutput = a} :: AnalyzeDocumentResponse)

-- | The version of the model used to analyze the document.
analyzeDocumentResponse_analyzeDocumentModelVersion :: Lens.Lens' AnalyzeDocumentResponse (Prelude.Maybe Prelude.Text)
analyzeDocumentResponse_analyzeDocumentModelVersion = Lens.lens (\AnalyzeDocumentResponse' {analyzeDocumentModelVersion} -> analyzeDocumentModelVersion) (\s@AnalyzeDocumentResponse' {} a -> s {analyzeDocumentModelVersion = a} :: AnalyzeDocumentResponse)

-- | The response's http status code.
analyzeDocumentResponse_httpStatus :: Lens.Lens' AnalyzeDocumentResponse Prelude.Int
analyzeDocumentResponse_httpStatus = Lens.lens (\AnalyzeDocumentResponse' {httpStatus} -> httpStatus) (\s@AnalyzeDocumentResponse' {} a -> s {httpStatus = a} :: AnalyzeDocumentResponse)

instance Prelude.NFData AnalyzeDocumentResponse where
  rnf AnalyzeDocumentResponse' {..} =
    Prelude.rnf documentMetadata
      `Prelude.seq` Prelude.rnf blocks
      `Prelude.seq` Prelude.rnf humanLoopActivationOutput
      `Prelude.seq` Prelude.rnf analyzeDocumentModelVersion
      `Prelude.seq` Prelude.rnf httpStatus
