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
-- Module      : Amazonka.Comprehend.Types.DatasetDocumentClassifierInputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetDocumentClassifierInputDataConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the dataset input data configuration for a document classifier
-- model.
--
-- For more information on how the input file is formatted, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/prep-classifier-data.html Preparing training data>
-- in the Comprehend Developer Guide.
--
-- /See:/ 'newDatasetDocumentClassifierInputDataConfig' smart constructor.
data DatasetDocumentClassifierInputDataConfig = DatasetDocumentClassifierInputDataConfig'
  { -- | Indicates the delimiter used to separate each label for training a
    -- multi-label classifier. The default delimiter between labels is a pipe
    -- (|). You can use a different character as a delimiter (if it\'s an
    -- allowed character) by specifying it under Delimiter for labels. If the
    -- training documents use a delimiter other than the default or the
    -- delimiter you specify, the labels on that line will be combined to make
    -- a single unique label, such as LABELLABELLABEL.
    labelDelimiter :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 URI for the input data. The S3 bucket must be in the same
    -- Region as the API endpoint that you are calling. The URI can point to a
    -- single input file or it can provide the prefix for a collection of input
    -- files.
    --
    -- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
    -- prefix is a single file, Amazon Comprehend uses that file as input. If
    -- more than one file begins with the prefix, Amazon Comprehend uses all of
    -- them as input.
    --
    -- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetDocumentClassifierInputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelDelimiter', 'datasetDocumentClassifierInputDataConfig_labelDelimiter' - Indicates the delimiter used to separate each label for training a
-- multi-label classifier. The default delimiter between labels is a pipe
-- (|). You can use a different character as a delimiter (if it\'s an
-- allowed character) by specifying it under Delimiter for labels. If the
-- training documents use a delimiter other than the default or the
-- delimiter you specify, the labels on that line will be combined to make
-- a single unique label, such as LABELLABELLABEL.
--
-- 's3Uri', 'datasetDocumentClassifierInputDataConfig_s3Uri' - The Amazon S3 URI for the input data. The S3 bucket must be in the same
-- Region as the API endpoint that you are calling. The URI can point to a
-- single input file or it can provide the prefix for a collection of input
-- files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
newDatasetDocumentClassifierInputDataConfig ::
  -- | 's3Uri'
  Prelude.Text ->
  DatasetDocumentClassifierInputDataConfig
newDatasetDocumentClassifierInputDataConfig pS3Uri_ =
  DatasetDocumentClassifierInputDataConfig'
    { labelDelimiter =
        Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | Indicates the delimiter used to separate each label for training a
-- multi-label classifier. The default delimiter between labels is a pipe
-- (|). You can use a different character as a delimiter (if it\'s an
-- allowed character) by specifying it under Delimiter for labels. If the
-- training documents use a delimiter other than the default or the
-- delimiter you specify, the labels on that line will be combined to make
-- a single unique label, such as LABELLABELLABEL.
datasetDocumentClassifierInputDataConfig_labelDelimiter :: Lens.Lens' DatasetDocumentClassifierInputDataConfig (Prelude.Maybe Prelude.Text)
datasetDocumentClassifierInputDataConfig_labelDelimiter = Lens.lens (\DatasetDocumentClassifierInputDataConfig' {labelDelimiter} -> labelDelimiter) (\s@DatasetDocumentClassifierInputDataConfig' {} a -> s {labelDelimiter = a} :: DatasetDocumentClassifierInputDataConfig)

-- | The Amazon S3 URI for the input data. The S3 bucket must be in the same
-- Region as the API endpoint that you are calling. The URI can point to a
-- single input file or it can provide the prefix for a collection of input
-- files.
--
-- For example, if you use the URI @S3:\/\/bucketName\/prefix@, if the
-- prefix is a single file, Amazon Comprehend uses that file as input. If
-- more than one file begins with the prefix, Amazon Comprehend uses all of
-- them as input.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
datasetDocumentClassifierInputDataConfig_s3Uri :: Lens.Lens' DatasetDocumentClassifierInputDataConfig Prelude.Text
datasetDocumentClassifierInputDataConfig_s3Uri = Lens.lens (\DatasetDocumentClassifierInputDataConfig' {s3Uri} -> s3Uri) (\s@DatasetDocumentClassifierInputDataConfig' {} a -> s {s3Uri = a} :: DatasetDocumentClassifierInputDataConfig)

instance
  Prelude.Hashable
    DatasetDocumentClassifierInputDataConfig
  where
  hashWithSalt
    _salt
    DatasetDocumentClassifierInputDataConfig' {..} =
      _salt
        `Prelude.hashWithSalt` labelDelimiter
        `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    DatasetDocumentClassifierInputDataConfig
  where
  rnf DatasetDocumentClassifierInputDataConfig' {..} =
    Prelude.rnf labelDelimiter
      `Prelude.seq` Prelude.rnf s3Uri

instance
  Data.ToJSON
    DatasetDocumentClassifierInputDataConfig
  where
  toJSON DatasetDocumentClassifierInputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelDelimiter" Data..=)
              Prelude.<$> labelDelimiter,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
