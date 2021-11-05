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
-- Module      : Amazonka.Comprehend.Types.DocumentClassifierInputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DocumentClassifierInputDataConfig where

import Amazonka.Comprehend.Types.AugmentedManifestsListItem
import Amazonka.Comprehend.Types.DocumentClassifierDataFormat
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The input properties for training a document classifier.
--
-- For more information on how the input file is formatted, see
-- how-document-classification-training-data.
--
-- /See:/ 'newDocumentClassifierInputDataConfig' smart constructor.
data DocumentClassifierInputDataConfig = DocumentClassifierInputDataConfig'
  { -- | A list of augmented manifest files that provide training data for your
    -- custom model. An augmented manifest file is a labeled dataset that is
    -- produced by Amazon SageMaker Ground Truth.
    --
    -- This parameter is required if you set @DataFormat@ to
    -- @AUGMENTED_MANIFEST@.
    augmentedManifests :: Prelude.Maybe [AugmentedManifestsListItem],
    -- | The format of your training data:
    --
    -- -   @COMPREHEND_CSV@: A two-column CSV file, where labels are provided
    --     in the first column, and documents are provided in the second. If
    --     you use this value, you must provide the @S3Uri@ parameter in your
    --     request.
    --
    -- -   @AUGMENTED_MANIFEST@: A labeled dataset that is produced by Amazon
    --     SageMaker Ground Truth. This file is in JSON lines format. Each line
    --     is a complete JSON object that contains a training document and its
    --     associated labels.
    --
    --     If you use this value, you must provide the @AugmentedManifests@
    --     parameter in your request.
    --
    -- If you don\'t specify a value, Amazon Comprehend uses @COMPREHEND_CSV@
    -- as the default.
    dataFormat :: Prelude.Maybe DocumentClassifierDataFormat,
    -- | Indicates the delimiter used to separate each label for training a
    -- multi-label classifier. The default delimiter between labels is a pipe
    -- (|). You can use a different character as a delimiter (if it\'s an
    -- allowed character) by specifying it under Delimiter for labels. If the
    -- training documents use a delimiter other than the default or the
    -- delimiter you specify, the labels on that line will be combined to make
    -- a single unique label, such as LABELLABELLABEL.
    labelDelimiter :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI for the input data. The Amazon S3 bucket must be in
    -- the same AWS Region as the API endpoint that you are calling. The URI
    -- can point to a single input file or it can provide the prefix for a
    -- collection of input files.
    testS3Uri :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI for the input data. The S3 bucket must be in the same
    -- region as the API endpoint that you are calling. The URI can point to a
    -- single input file or it can provide the prefix for a collection of input
    -- files.
    --
    -- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
    -- prefix is a single file, Amazon Comprehend uses that file as input. If
    -- more than one file begins with the prefix, Amazon Comprehend uses all of
    -- them as input.
    --
    -- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
    s3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DocumentClassifierInputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'augmentedManifests', 'documentClassifierInputDataConfig_augmentedManifests' - A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to
-- @AUGMENTED_MANIFEST@.
--
-- 'dataFormat', 'documentClassifierInputDataConfig_dataFormat' - The format of your training data:
--
-- -   @COMPREHEND_CSV@: A two-column CSV file, where labels are provided
--     in the first column, and documents are provided in the second. If
--     you use this value, you must provide the @S3Uri@ parameter in your
--     request.
--
-- -   @AUGMENTED_MANIFEST@: A labeled dataset that is produced by Amazon
--     SageMaker Ground Truth. This file is in JSON lines format. Each line
--     is a complete JSON object that contains a training document and its
--     associated labels.
--
--     If you use this value, you must provide the @AugmentedManifests@
--     parameter in your request.
--
-- If you don\'t specify a value, Amazon Comprehend uses @COMPREHEND_CSV@
-- as the default.
--
-- 'labelDelimiter', 'documentClassifierInputDataConfig_labelDelimiter' - Indicates the delimiter used to separate each label for training a
-- multi-label classifier. The default delimiter between labels is a pipe
-- (|). You can use a different character as a delimiter (if it\'s an
-- allowed character) by specifying it under Delimiter for labels. If the
-- training documents use a delimiter other than the default or the
-- delimiter you specify, the labels on that line will be combined to make
-- a single unique label, such as LABELLABELLABEL.
--
-- 'testS3Uri', 'documentClassifierInputDataConfig_testS3Uri' - The Amazon S3 URI for the input data. The Amazon S3 bucket must be in
-- the same AWS Region as the API endpoint that you are calling. The URI
-- can point to a single input file or it can provide the prefix for a
-- collection of input files.
--
-- 's3Uri', 'documentClassifierInputDataConfig_s3Uri' - The Amazon S3 URI for the input data. The S3 bucket must be in the same
-- region as the API endpoint that you are calling. The URI can point to a
-- single input file or it can provide the prefix for a collection of input
-- files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
newDocumentClassifierInputDataConfig ::
  DocumentClassifierInputDataConfig
newDocumentClassifierInputDataConfig =
  DocumentClassifierInputDataConfig'
    { augmentedManifests =
        Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      labelDelimiter = Prelude.Nothing,
      testS3Uri = Prelude.Nothing,
      s3Uri = Prelude.Nothing
    }

-- | A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to
-- @AUGMENTED_MANIFEST@.
documentClassifierInputDataConfig_augmentedManifests :: Lens.Lens' DocumentClassifierInputDataConfig (Prelude.Maybe [AugmentedManifestsListItem])
documentClassifierInputDataConfig_augmentedManifests = Lens.lens (\DocumentClassifierInputDataConfig' {augmentedManifests} -> augmentedManifests) (\s@DocumentClassifierInputDataConfig' {} a -> s {augmentedManifests = a} :: DocumentClassifierInputDataConfig) Prelude.. Lens.mapping Lens.coerced

-- | The format of your training data:
--
-- -   @COMPREHEND_CSV@: A two-column CSV file, where labels are provided
--     in the first column, and documents are provided in the second. If
--     you use this value, you must provide the @S3Uri@ parameter in your
--     request.
--
-- -   @AUGMENTED_MANIFEST@: A labeled dataset that is produced by Amazon
--     SageMaker Ground Truth. This file is in JSON lines format. Each line
--     is a complete JSON object that contains a training document and its
--     associated labels.
--
--     If you use this value, you must provide the @AugmentedManifests@
--     parameter in your request.
--
-- If you don\'t specify a value, Amazon Comprehend uses @COMPREHEND_CSV@
-- as the default.
documentClassifierInputDataConfig_dataFormat :: Lens.Lens' DocumentClassifierInputDataConfig (Prelude.Maybe DocumentClassifierDataFormat)
documentClassifierInputDataConfig_dataFormat = Lens.lens (\DocumentClassifierInputDataConfig' {dataFormat} -> dataFormat) (\s@DocumentClassifierInputDataConfig' {} a -> s {dataFormat = a} :: DocumentClassifierInputDataConfig)

-- | Indicates the delimiter used to separate each label for training a
-- multi-label classifier. The default delimiter between labels is a pipe
-- (|). You can use a different character as a delimiter (if it\'s an
-- allowed character) by specifying it under Delimiter for labels. If the
-- training documents use a delimiter other than the default or the
-- delimiter you specify, the labels on that line will be combined to make
-- a single unique label, such as LABELLABELLABEL.
documentClassifierInputDataConfig_labelDelimiter :: Lens.Lens' DocumentClassifierInputDataConfig (Prelude.Maybe Prelude.Text)
documentClassifierInputDataConfig_labelDelimiter = Lens.lens (\DocumentClassifierInputDataConfig' {labelDelimiter} -> labelDelimiter) (\s@DocumentClassifierInputDataConfig' {} a -> s {labelDelimiter = a} :: DocumentClassifierInputDataConfig)

-- | The Amazon S3 URI for the input data. The Amazon S3 bucket must be in
-- the same AWS Region as the API endpoint that you are calling. The URI
-- can point to a single input file or it can provide the prefix for a
-- collection of input files.
documentClassifierInputDataConfig_testS3Uri :: Lens.Lens' DocumentClassifierInputDataConfig (Prelude.Maybe Prelude.Text)
documentClassifierInputDataConfig_testS3Uri = Lens.lens (\DocumentClassifierInputDataConfig' {testS3Uri} -> testS3Uri) (\s@DocumentClassifierInputDataConfig' {} a -> s {testS3Uri = a} :: DocumentClassifierInputDataConfig)

-- | The Amazon S3 URI for the input data. The S3 bucket must be in the same
-- region as the API endpoint that you are calling. The URI can point to a
-- single input file or it can provide the prefix for a collection of input
-- files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
documentClassifierInputDataConfig_s3Uri :: Lens.Lens' DocumentClassifierInputDataConfig (Prelude.Maybe Prelude.Text)
documentClassifierInputDataConfig_s3Uri = Lens.lens (\DocumentClassifierInputDataConfig' {s3Uri} -> s3Uri) (\s@DocumentClassifierInputDataConfig' {} a -> s {s3Uri = a} :: DocumentClassifierInputDataConfig)

instance
  Core.FromJSON
    DocumentClassifierInputDataConfig
  where
  parseJSON =
    Core.withObject
      "DocumentClassifierInputDataConfig"
      ( \x ->
          DocumentClassifierInputDataConfig'
            Prelude.<$> ( x Core..:? "AugmentedManifests"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DataFormat")
            Prelude.<*> (x Core..:? "LabelDelimiter")
            Prelude.<*> (x Core..:? "TestS3Uri")
            Prelude.<*> (x Core..:? "S3Uri")
      )

instance
  Prelude.Hashable
    DocumentClassifierInputDataConfig

instance
  Prelude.NFData
    DocumentClassifierInputDataConfig

instance
  Core.ToJSON
    DocumentClassifierInputDataConfig
  where
  toJSON DocumentClassifierInputDataConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AugmentedManifests" Core..=)
              Prelude.<$> augmentedManifests,
            ("DataFormat" Core..=) Prelude.<$> dataFormat,
            ("LabelDelimiter" Core..=)
              Prelude.<$> labelDelimiter,
            ("TestS3Uri" Core..=) Prelude.<$> testS3Uri,
            ("S3Uri" Core..=) Prelude.<$> s3Uri
          ]
      )
