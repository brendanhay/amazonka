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
-- Module      : Amazonka.Comprehend.Types.DatasetAugmentedManifestsListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetAugmentedManifestsListItem where

import Amazonka.Comprehend.Types.AugmentedManifestsDocumentTypeFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An augmented manifest file that provides training data for your custom
-- model. An augmented manifest file is a labeled dataset that is produced
-- by Amazon SageMaker Ground Truth.
--
-- /See:/ 'newDatasetAugmentedManifestsListItem' smart constructor.
data DatasetAugmentedManifestsListItem = DatasetAugmentedManifestsListItem'
  { -- | The S3 prefix to the annotation files that are referred in the augmented
    -- manifest file.
    annotationDataS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The type of augmented manifest. If you don\'t specify, the default is
    -- PlainTextDocument.
    --
    -- @PLAIN_TEXT_DOCUMENT@ A document type that represents any unicode text
    -- that is encoded in UTF-8.
    documentType :: Prelude.Maybe AugmentedManifestsDocumentTypeFormat,
    -- | The S3 prefix to the source files (PDFs) that are referred to in the
    -- augmented manifest file.
    sourceDocumentsS3Uri :: Prelude.Maybe Prelude.Text,
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
    attributeNames :: [Prelude.Text],
    -- | The Amazon S3 location of the augmented manifest file.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetAugmentedManifestsListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotationDataS3Uri', 'datasetAugmentedManifestsListItem_annotationDataS3Uri' - The S3 prefix to the annotation files that are referred in the augmented
-- manifest file.
--
-- 'documentType', 'datasetAugmentedManifestsListItem_documentType' - The type of augmented manifest. If you don\'t specify, the default is
-- PlainTextDocument.
--
-- @PLAIN_TEXT_DOCUMENT@ A document type that represents any unicode text
-- that is encoded in UTF-8.
--
-- 'sourceDocumentsS3Uri', 'datasetAugmentedManifestsListItem_sourceDocumentsS3Uri' - The S3 prefix to the source files (PDFs) that are referred to in the
-- augmented manifest file.
--
-- 'attributeNames', 'datasetAugmentedManifestsListItem_attributeNames' - The JSON attribute that contains the annotations for your training
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
--
-- 's3Uri', 'datasetAugmentedManifestsListItem_s3Uri' - The Amazon S3 location of the augmented manifest file.
newDatasetAugmentedManifestsListItem ::
  -- | 's3Uri'
  Prelude.Text ->
  DatasetAugmentedManifestsListItem
newDatasetAugmentedManifestsListItem pS3Uri_ =
  DatasetAugmentedManifestsListItem'
    { annotationDataS3Uri =
        Prelude.Nothing,
      documentType = Prelude.Nothing,
      sourceDocumentsS3Uri = Prelude.Nothing,
      attributeNames = Prelude.mempty,
      s3Uri = pS3Uri_
    }

-- | The S3 prefix to the annotation files that are referred in the augmented
-- manifest file.
datasetAugmentedManifestsListItem_annotationDataS3Uri :: Lens.Lens' DatasetAugmentedManifestsListItem (Prelude.Maybe Prelude.Text)
datasetAugmentedManifestsListItem_annotationDataS3Uri = Lens.lens (\DatasetAugmentedManifestsListItem' {annotationDataS3Uri} -> annotationDataS3Uri) (\s@DatasetAugmentedManifestsListItem' {} a -> s {annotationDataS3Uri = a} :: DatasetAugmentedManifestsListItem)

-- | The type of augmented manifest. If you don\'t specify, the default is
-- PlainTextDocument.
--
-- @PLAIN_TEXT_DOCUMENT@ A document type that represents any unicode text
-- that is encoded in UTF-8.
datasetAugmentedManifestsListItem_documentType :: Lens.Lens' DatasetAugmentedManifestsListItem (Prelude.Maybe AugmentedManifestsDocumentTypeFormat)
datasetAugmentedManifestsListItem_documentType = Lens.lens (\DatasetAugmentedManifestsListItem' {documentType} -> documentType) (\s@DatasetAugmentedManifestsListItem' {} a -> s {documentType = a} :: DatasetAugmentedManifestsListItem)

-- | The S3 prefix to the source files (PDFs) that are referred to in the
-- augmented manifest file.
datasetAugmentedManifestsListItem_sourceDocumentsS3Uri :: Lens.Lens' DatasetAugmentedManifestsListItem (Prelude.Maybe Prelude.Text)
datasetAugmentedManifestsListItem_sourceDocumentsS3Uri = Lens.lens (\DatasetAugmentedManifestsListItem' {sourceDocumentsS3Uri} -> sourceDocumentsS3Uri) (\s@DatasetAugmentedManifestsListItem' {} a -> s {sourceDocumentsS3Uri = a} :: DatasetAugmentedManifestsListItem)

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
datasetAugmentedManifestsListItem_attributeNames :: Lens.Lens' DatasetAugmentedManifestsListItem [Prelude.Text]
datasetAugmentedManifestsListItem_attributeNames = Lens.lens (\DatasetAugmentedManifestsListItem' {attributeNames} -> attributeNames) (\s@DatasetAugmentedManifestsListItem' {} a -> s {attributeNames = a} :: DatasetAugmentedManifestsListItem) Prelude.. Lens.coerced

-- | The Amazon S3 location of the augmented manifest file.
datasetAugmentedManifestsListItem_s3Uri :: Lens.Lens' DatasetAugmentedManifestsListItem Prelude.Text
datasetAugmentedManifestsListItem_s3Uri = Lens.lens (\DatasetAugmentedManifestsListItem' {s3Uri} -> s3Uri) (\s@DatasetAugmentedManifestsListItem' {} a -> s {s3Uri = a} :: DatasetAugmentedManifestsListItem)

instance
  Prelude.Hashable
    DatasetAugmentedManifestsListItem
  where
  hashWithSalt
    _salt
    DatasetAugmentedManifestsListItem' {..} =
      _salt
        `Prelude.hashWithSalt` annotationDataS3Uri
        `Prelude.hashWithSalt` documentType
        `Prelude.hashWithSalt` sourceDocumentsS3Uri
        `Prelude.hashWithSalt` attributeNames
        `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    DatasetAugmentedManifestsListItem
  where
  rnf DatasetAugmentedManifestsListItem' {..} =
    Prelude.rnf annotationDataS3Uri
      `Prelude.seq` Prelude.rnf documentType
      `Prelude.seq` Prelude.rnf sourceDocumentsS3Uri
      `Prelude.seq` Prelude.rnf attributeNames
      `Prelude.seq` Prelude.rnf s3Uri

instance
  Data.ToJSON
    DatasetAugmentedManifestsListItem
  where
  toJSON DatasetAugmentedManifestsListItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AnnotationDataS3Uri" Data..=)
              Prelude.<$> annotationDataS3Uri,
            ("DocumentType" Data..=) Prelude.<$> documentType,
            ("SourceDocumentsS3Uri" Data..=)
              Prelude.<$> sourceDocumentsS3Uri,
            Prelude.Just
              ("AttributeNames" Data..= attributeNames),
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
