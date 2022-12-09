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
-- Module      : Amazonka.Comprehend.Types.AugmentedManifestsListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.AugmentedManifestsListItem where

import Amazonka.Comprehend.Types.AugmentedManifestsDocumentTypeFormat
import Amazonka.Comprehend.Types.Split
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An augmented manifest file that provides training data for your custom
-- model. An augmented manifest file is a labeled dataset that is produced
-- by Amazon SageMaker Ground Truth.
--
-- /See:/ 'newAugmentedManifestsListItem' smart constructor.
data AugmentedManifestsListItem = AugmentedManifestsListItem'
  { -- | The S3 prefix to the annotation files that are referred in the augmented
    -- manifest file.
    annotationDataS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The type of augmented manifest. PlainTextDocument or
    -- SemiStructuredDocument. If you don\'t specify, the default is
    -- PlainTextDocument.
    --
    -- -   @PLAIN_TEXT_DOCUMENT@ A document type that represents any unicode
    --     text that is encoded in UTF-8.
    --
    -- -   @SEMI_STRUCTURED_DOCUMENT@ A document type with positional and
    --     structural context, like a PDF. For training with Amazon Comprehend,
    --     only PDFs are supported. For inference, Amazon Comprehend support
    --     PDFs, DOCX and TXT.
    documentType :: Prelude.Maybe AugmentedManifestsDocumentTypeFormat,
    -- | The S3 prefix to the source files (PDFs) that are referred to in the
    -- augmented manifest file.
    sourceDocumentsS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The purpose of the data you\'ve provided in the augmented manifest. You
    -- can either train or test this data. If you don\'t specify, the default
    -- is train.
    --
    -- TRAIN - all of the documents in the manifest will be used for training.
    -- If no test documents are provided, Amazon Comprehend will automatically
    -- reserve a portion of the training documents for testing.
    --
    -- TEST - all of the documents in the manifest will be used for testing.
    split :: Prelude.Maybe Split,
    -- | The Amazon S3 location of the augmented manifest file.
    s3Uri :: Prelude.Text,
    -- | The JSON attribute that contains the annotations for your training
    -- documents. The number of attribute names that you specify depends on
    -- whether your augmented manifest file is the output of a single labeling
    -- job or a chained labeling job.
    --
    -- If your file is the output of a single labeling job, specify the
    -- LabelAttributeName key that was used when the job was created in Ground
    -- Truth.
    --
    -- If your file is the output of a chained labeling job, specify the
    -- LabelAttributeName key for one or more jobs in the chain. Each
    -- LabelAttributeName key provides the annotations from an individual job.
    attributeNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AugmentedManifestsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationDataS3Uri', 'augmentedManifestsListItem_annotationDataS3Uri' - The S3 prefix to the annotation files that are referred in the augmented
-- manifest file.
--
-- 'documentType', 'augmentedManifestsListItem_documentType' - The type of augmented manifest. PlainTextDocument or
-- SemiStructuredDocument. If you don\'t specify, the default is
-- PlainTextDocument.
--
-- -   @PLAIN_TEXT_DOCUMENT@ A document type that represents any unicode
--     text that is encoded in UTF-8.
--
-- -   @SEMI_STRUCTURED_DOCUMENT@ A document type with positional and
--     structural context, like a PDF. For training with Amazon Comprehend,
--     only PDFs are supported. For inference, Amazon Comprehend support
--     PDFs, DOCX and TXT.
--
-- 'sourceDocumentsS3Uri', 'augmentedManifestsListItem_sourceDocumentsS3Uri' - The S3 prefix to the source files (PDFs) that are referred to in the
-- augmented manifest file.
--
-- 'split', 'augmentedManifestsListItem_split' - The purpose of the data you\'ve provided in the augmented manifest. You
-- can either train or test this data. If you don\'t specify, the default
-- is train.
--
-- TRAIN - all of the documents in the manifest will be used for training.
-- If no test documents are provided, Amazon Comprehend will automatically
-- reserve a portion of the training documents for testing.
--
-- TEST - all of the documents in the manifest will be used for testing.
--
-- 's3Uri', 'augmentedManifestsListItem_s3Uri' - The Amazon S3 location of the augmented manifest file.
--
-- 'attributeNames', 'augmentedManifestsListItem_attributeNames' - The JSON attribute that contains the annotations for your training
-- documents. The number of attribute names that you specify depends on
-- whether your augmented manifest file is the output of a single labeling
-- job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the
-- LabelAttributeName key that was used when the job was created in Ground
-- Truth.
--
-- If your file is the output of a chained labeling job, specify the
-- LabelAttributeName key for one or more jobs in the chain. Each
-- LabelAttributeName key provides the annotations from an individual job.
newAugmentedManifestsListItem ::
  -- | 's3Uri'
  Prelude.Text ->
  AugmentedManifestsListItem
newAugmentedManifestsListItem pS3Uri_ =
  AugmentedManifestsListItem'
    { annotationDataS3Uri =
        Prelude.Nothing,
      documentType = Prelude.Nothing,
      sourceDocumentsS3Uri = Prelude.Nothing,
      split = Prelude.Nothing,
      s3Uri = pS3Uri_,
      attributeNames = Prelude.mempty
    }

-- | The S3 prefix to the annotation files that are referred in the augmented
-- manifest file.
augmentedManifestsListItem_annotationDataS3Uri :: Lens.Lens' AugmentedManifestsListItem (Prelude.Maybe Prelude.Text)
augmentedManifestsListItem_annotationDataS3Uri = Lens.lens (\AugmentedManifestsListItem' {annotationDataS3Uri} -> annotationDataS3Uri) (\s@AugmentedManifestsListItem' {} a -> s {annotationDataS3Uri = a} :: AugmentedManifestsListItem)

-- | The type of augmented manifest. PlainTextDocument or
-- SemiStructuredDocument. If you don\'t specify, the default is
-- PlainTextDocument.
--
-- -   @PLAIN_TEXT_DOCUMENT@ A document type that represents any unicode
--     text that is encoded in UTF-8.
--
-- -   @SEMI_STRUCTURED_DOCUMENT@ A document type with positional and
--     structural context, like a PDF. For training with Amazon Comprehend,
--     only PDFs are supported. For inference, Amazon Comprehend support
--     PDFs, DOCX and TXT.
augmentedManifestsListItem_documentType :: Lens.Lens' AugmentedManifestsListItem (Prelude.Maybe AugmentedManifestsDocumentTypeFormat)
augmentedManifestsListItem_documentType = Lens.lens (\AugmentedManifestsListItem' {documentType} -> documentType) (\s@AugmentedManifestsListItem' {} a -> s {documentType = a} :: AugmentedManifestsListItem)

-- | The S3 prefix to the source files (PDFs) that are referred to in the
-- augmented manifest file.
augmentedManifestsListItem_sourceDocumentsS3Uri :: Lens.Lens' AugmentedManifestsListItem (Prelude.Maybe Prelude.Text)
augmentedManifestsListItem_sourceDocumentsS3Uri = Lens.lens (\AugmentedManifestsListItem' {sourceDocumentsS3Uri} -> sourceDocumentsS3Uri) (\s@AugmentedManifestsListItem' {} a -> s {sourceDocumentsS3Uri = a} :: AugmentedManifestsListItem)

-- | The purpose of the data you\'ve provided in the augmented manifest. You
-- can either train or test this data. If you don\'t specify, the default
-- is train.
--
-- TRAIN - all of the documents in the manifest will be used for training.
-- If no test documents are provided, Amazon Comprehend will automatically
-- reserve a portion of the training documents for testing.
--
-- TEST - all of the documents in the manifest will be used for testing.
augmentedManifestsListItem_split :: Lens.Lens' AugmentedManifestsListItem (Prelude.Maybe Split)
augmentedManifestsListItem_split = Lens.lens (\AugmentedManifestsListItem' {split} -> split) (\s@AugmentedManifestsListItem' {} a -> s {split = a} :: AugmentedManifestsListItem)

-- | The Amazon S3 location of the augmented manifest file.
augmentedManifestsListItem_s3Uri :: Lens.Lens' AugmentedManifestsListItem Prelude.Text
augmentedManifestsListItem_s3Uri = Lens.lens (\AugmentedManifestsListItem' {s3Uri} -> s3Uri) (\s@AugmentedManifestsListItem' {} a -> s {s3Uri = a} :: AugmentedManifestsListItem)

-- | The JSON attribute that contains the annotations for your training
-- documents. The number of attribute names that you specify depends on
-- whether your augmented manifest file is the output of a single labeling
-- job or a chained labeling job.
--
-- If your file is the output of a single labeling job, specify the
-- LabelAttributeName key that was used when the job was created in Ground
-- Truth.
--
-- If your file is the output of a chained labeling job, specify the
-- LabelAttributeName key for one or more jobs in the chain. Each
-- LabelAttributeName key provides the annotations from an individual job.
augmentedManifestsListItem_attributeNames :: Lens.Lens' AugmentedManifestsListItem [Prelude.Text]
augmentedManifestsListItem_attributeNames = Lens.lens (\AugmentedManifestsListItem' {attributeNames} -> attributeNames) (\s@AugmentedManifestsListItem' {} a -> s {attributeNames = a} :: AugmentedManifestsListItem) Prelude.. Lens.coerced

instance Data.FromJSON AugmentedManifestsListItem where
  parseJSON =
    Data.withObject
      "AugmentedManifestsListItem"
      ( \x ->
          AugmentedManifestsListItem'
            Prelude.<$> (x Data..:? "AnnotationDataS3Uri")
            Prelude.<*> (x Data..:? "DocumentType")
            Prelude.<*> (x Data..:? "SourceDocumentsS3Uri")
            Prelude.<*> (x Data..:? "Split")
            Prelude.<*> (x Data..: "S3Uri")
            Prelude.<*> ( x Data..:? "AttributeNames"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AugmentedManifestsListItem where
  hashWithSalt _salt AugmentedManifestsListItem' {..} =
    _salt `Prelude.hashWithSalt` annotationDataS3Uri
      `Prelude.hashWithSalt` documentType
      `Prelude.hashWithSalt` sourceDocumentsS3Uri
      `Prelude.hashWithSalt` split
      `Prelude.hashWithSalt` s3Uri
      `Prelude.hashWithSalt` attributeNames

instance Prelude.NFData AugmentedManifestsListItem where
  rnf AugmentedManifestsListItem' {..} =
    Prelude.rnf annotationDataS3Uri
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf sourceDocumentsS3Uri
      `Prelude.seq` Prelude.rnf split
      `Prelude.seq` Prelude.rnf s3Uri
      `Prelude.seq` Prelude.rnf attributeNames

instance Data.ToJSON AugmentedManifestsListItem where
  toJSON AugmentedManifestsListItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnnotationDataS3Uri" Data..=)
              Prelude.<$> annotationDataS3Uri,
            ("DocumentType" Data..=) Prelude.<$> documentType,
            ("SourceDocumentsS3Uri" Data..=)
              Prelude.<$> sourceDocumentsS3Uri,
            ("Split" Data..=) Prelude.<$> split,
            Prelude.Just ("S3Uri" Data..= s3Uri),
            Prelude.Just
              ("AttributeNames" Data..= attributeNames)
          ]
      )
