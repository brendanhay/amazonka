{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Comprehend.Types.DocumentReaderConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentReaderConfig where

import Amazonka.Comprehend.Types.DocumentReadAction
import Amazonka.Comprehend.Types.DocumentReadFeatureTypes
import Amazonka.Comprehend.Types.DocumentReadMode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration parameters to override the default actions for
-- extracting text from PDF documents and image files.
--
-- By default, Amazon Comprehend performs the following actions to extract
-- text from files, based on the input file type:
--
-- -   __Word files__ - Amazon Comprehend parser extracts the text.
--
-- -   __Digital PDF files__ - Amazon Comprehend parser extracts the text.
--
-- -   __Image files and scanned PDF files__ - Amazon Comprehend uses the
--     Amazon Textract @DetectDocumentText@ API to extract the text.
--
-- @DocumentReaderConfig@ does not apply to plain text files or Word files.
--
-- For image files and PDF documents, you can override these default
-- actions using the fields listed below. For more information, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/detecting-cer.html#detecting-cer-pdf Setting text extraction options>.
--
-- /See:/ 'newDocumentReaderConfig' smart constructor.
data DocumentReaderConfig = DocumentReaderConfig'
  { -- | Determines the text extraction actions for PDF files. Enter one of the
    -- following values:
    --
    -- -   @SERVICE_DEFAULT@ - use the Amazon Comprehend service defaults for
    --     PDF files.
    --
    -- -   @FORCE_DOCUMENT_READ_ACTION@ - Amazon Comprehend uses the Textract
    --     API specified by DocumentReadAction for all PDF files, including
    --     digital PDF files.
    documentReadMode :: Prelude.Maybe DocumentReadMode,
    -- | Specifies the type of Amazon Textract features to apply. If you chose
    -- @TEXTRACT_ANALYZE_DOCUMENT@ as the read action, you must specify one or
    -- both of the following values:
    --
    -- -   @TABLES@ - Returns information about any tables that are detected in
    --     the input document.
    --
    -- -   @FORMS@ - Returns information and the data from any forms that are
    --     detected in the input document.
    featureTypes :: Prelude.Maybe (Prelude.NonEmpty DocumentReadFeatureTypes),
    -- | This field defines the Amazon Textract API operation that Amazon
    -- Comprehend uses to extract text from PDF files and image files. Enter
    -- one of the following values:
    --
    -- -   @TEXTRACT_DETECT_DOCUMENT_TEXT@ - The Amazon Comprehend service uses
    --     the @DetectDocumentText@ API operation.
    --
    -- -   @TEXTRACT_ANALYZE_DOCUMENT@ - The Amazon Comprehend service uses the
    --     @AnalyzeDocument@ API operation.
    documentReadAction :: DocumentReadAction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentReaderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentReadMode', 'documentReaderConfig_documentReadMode' - Determines the text extraction actions for PDF files. Enter one of the
-- following values:
--
-- -   @SERVICE_DEFAULT@ - use the Amazon Comprehend service defaults for
--     PDF files.
--
-- -   @FORCE_DOCUMENT_READ_ACTION@ - Amazon Comprehend uses the Textract
--     API specified by DocumentReadAction for all PDF files, including
--     digital PDF files.
--
-- 'featureTypes', 'documentReaderConfig_featureTypes' - Specifies the type of Amazon Textract features to apply. If you chose
-- @TEXTRACT_ANALYZE_DOCUMENT@ as the read action, you must specify one or
-- both of the following values:
--
-- -   @TABLES@ - Returns information about any tables that are detected in
--     the input document.
--
-- -   @FORMS@ - Returns information and the data from any forms that are
--     detected in the input document.
--
-- 'documentReadAction', 'documentReaderConfig_documentReadAction' - This field defines the Amazon Textract API operation that Amazon
-- Comprehend uses to extract text from PDF files and image files. Enter
-- one of the following values:
--
-- -   @TEXTRACT_DETECT_DOCUMENT_TEXT@ - The Amazon Comprehend service uses
--     the @DetectDocumentText@ API operation.
--
-- -   @TEXTRACT_ANALYZE_DOCUMENT@ - The Amazon Comprehend service uses the
--     @AnalyzeDocument@ API operation.
newDocumentReaderConfig ::
  -- | 'documentReadAction'
  DocumentReadAction ->
  DocumentReaderConfig
newDocumentReaderConfig pDocumentReadAction_ =
  DocumentReaderConfig'
    { documentReadMode =
        Prelude.Nothing,
      featureTypes = Prelude.Nothing,
      documentReadAction = pDocumentReadAction_
    }

-- | Determines the text extraction actions for PDF files. Enter one of the
-- following values:
--
-- -   @SERVICE_DEFAULT@ - use the Amazon Comprehend service defaults for
--     PDF files.
--
-- -   @FORCE_DOCUMENT_READ_ACTION@ - Amazon Comprehend uses the Textract
--     API specified by DocumentReadAction for all PDF files, including
--     digital PDF files.
documentReaderConfig_documentReadMode :: Lens.Lens' DocumentReaderConfig (Prelude.Maybe DocumentReadMode)
documentReaderConfig_documentReadMode = Lens.lens (\DocumentReaderConfig' {documentReadMode} -> documentReadMode) (\s@DocumentReaderConfig' {} a -> s {documentReadMode = a} :: DocumentReaderConfig)

-- | Specifies the type of Amazon Textract features to apply. If you chose
-- @TEXTRACT_ANALYZE_DOCUMENT@ as the read action, you must specify one or
-- both of the following values:
--
-- -   @TABLES@ - Returns information about any tables that are detected in
--     the input document.
--
-- -   @FORMS@ - Returns information and the data from any forms that are
--     detected in the input document.
documentReaderConfig_featureTypes :: Lens.Lens' DocumentReaderConfig (Prelude.Maybe (Prelude.NonEmpty DocumentReadFeatureTypes))
documentReaderConfig_featureTypes = Lens.lens (\DocumentReaderConfig' {featureTypes} -> featureTypes) (\s@DocumentReaderConfig' {} a -> s {featureTypes = a} :: DocumentReaderConfig) Prelude.. Lens.mapping Lens.coerced

-- | This field defines the Amazon Textract API operation that Amazon
-- Comprehend uses to extract text from PDF files and image files. Enter
-- one of the following values:
--
-- -   @TEXTRACT_DETECT_DOCUMENT_TEXT@ - The Amazon Comprehend service uses
--     the @DetectDocumentText@ API operation.
--
-- -   @TEXTRACT_ANALYZE_DOCUMENT@ - The Amazon Comprehend service uses the
--     @AnalyzeDocument@ API operation.
documentReaderConfig_documentReadAction :: Lens.Lens' DocumentReaderConfig DocumentReadAction
documentReaderConfig_documentReadAction = Lens.lens (\DocumentReaderConfig' {documentReadAction} -> documentReadAction) (\s@DocumentReaderConfig' {} a -> s {documentReadAction = a} :: DocumentReaderConfig)

instance Data.FromJSON DocumentReaderConfig where
  parseJSON =
    Data.withObject
      "DocumentReaderConfig"
      ( \x ->
          DocumentReaderConfig'
            Prelude.<$> (x Data..:? "DocumentReadMode")
            Prelude.<*> (x Data..:? "FeatureTypes")
            Prelude.<*> (x Data..: "DocumentReadAction")
      )

instance Prelude.Hashable DocumentReaderConfig where
  hashWithSalt _salt DocumentReaderConfig' {..} =
    _salt `Prelude.hashWithSalt` documentReadMode
      `Prelude.hashWithSalt` featureTypes
      `Prelude.hashWithSalt` documentReadAction

instance Prelude.NFData DocumentReaderConfig where
  rnf DocumentReaderConfig' {..} =
    Prelude.rnf documentReadMode
      `Prelude.seq` Prelude.rnf featureTypes
      `Prelude.seq` Prelude.rnf documentReadAction

instance Data.ToJSON DocumentReaderConfig where
  toJSON DocumentReaderConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentReadMode" Data..=)
              Prelude.<$> documentReadMode,
            ("FeatureTypes" Data..=) Prelude.<$> featureTypes,
            Prelude.Just
              ("DocumentReadAction" Data..= documentReadAction)
          ]
      )
