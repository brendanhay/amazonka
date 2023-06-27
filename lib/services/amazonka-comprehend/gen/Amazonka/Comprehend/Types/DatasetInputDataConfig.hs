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
-- Module      : Amazonka.Comprehend.Types.DatasetInputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetInputDataConfig where

import Amazonka.Comprehend.Types.DatasetAugmentedManifestsListItem
import Amazonka.Comprehend.Types.DatasetDataFormat
import Amazonka.Comprehend.Types.DatasetDocumentClassifierInputDataConfig
import Amazonka.Comprehend.Types.DatasetEntityRecognizerInputDataConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the format and location of the input data for the dataset.
--
-- /See:/ 'newDatasetInputDataConfig' smart constructor.
data DatasetInputDataConfig = DatasetInputDataConfig'
  { -- | A list of augmented manifest files that provide training data for your
    -- custom model. An augmented manifest file is a labeled dataset that is
    -- produced by Amazon SageMaker Ground Truth.
    augmentedManifests :: Prelude.Maybe [DatasetAugmentedManifestsListItem],
    -- | @COMPREHEND_CSV@: The data format is a two-column CSV file, where the
    -- first column contains labels and the second column contains documents.
    --
    -- @AUGMENTED_MANIFEST@: The data format
    dataFormat :: Prelude.Maybe DatasetDataFormat,
    -- | The input properties for training a document classifier model.
    --
    -- For more information on how the input file is formatted, see
    -- <https://docs.aws.amazon.com/comprehend/latest/dg/prep-classifier-data.html Preparing training data>
    -- in the Comprehend Developer Guide.
    documentClassifierInputDataConfig :: Prelude.Maybe DatasetDocumentClassifierInputDataConfig,
    -- | The input properties for training an entity recognizer model.
    entityRecognizerInputDataConfig :: Prelude.Maybe DatasetEntityRecognizerInputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetInputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'augmentedManifests', 'datasetInputDataConfig_augmentedManifests' - A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
--
-- 'dataFormat', 'datasetInputDataConfig_dataFormat' - @COMPREHEND_CSV@: The data format is a two-column CSV file, where the
-- first column contains labels and the second column contains documents.
--
-- @AUGMENTED_MANIFEST@: The data format
--
-- 'documentClassifierInputDataConfig', 'datasetInputDataConfig_documentClassifierInputDataConfig' - The input properties for training a document classifier model.
--
-- For more information on how the input file is formatted, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/prep-classifier-data.html Preparing training data>
-- in the Comprehend Developer Guide.
--
-- 'entityRecognizerInputDataConfig', 'datasetInputDataConfig_entityRecognizerInputDataConfig' - The input properties for training an entity recognizer model.
newDatasetInputDataConfig ::
  DatasetInputDataConfig
newDatasetInputDataConfig =
  DatasetInputDataConfig'
    { augmentedManifests =
        Prelude.Nothing,
      dataFormat = Prelude.Nothing,
      documentClassifierInputDataConfig = Prelude.Nothing,
      entityRecognizerInputDataConfig = Prelude.Nothing
    }

-- | A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
datasetInputDataConfig_augmentedManifests :: Lens.Lens' DatasetInputDataConfig (Prelude.Maybe [DatasetAugmentedManifestsListItem])
datasetInputDataConfig_augmentedManifests = Lens.lens (\DatasetInputDataConfig' {augmentedManifests} -> augmentedManifests) (\s@DatasetInputDataConfig' {} a -> s {augmentedManifests = a} :: DatasetInputDataConfig) Prelude.. Lens.mapping Lens.coerced

-- | @COMPREHEND_CSV@: The data format is a two-column CSV file, where the
-- first column contains labels and the second column contains documents.
--
-- @AUGMENTED_MANIFEST@: The data format
datasetInputDataConfig_dataFormat :: Lens.Lens' DatasetInputDataConfig (Prelude.Maybe DatasetDataFormat)
datasetInputDataConfig_dataFormat = Lens.lens (\DatasetInputDataConfig' {dataFormat} -> dataFormat) (\s@DatasetInputDataConfig' {} a -> s {dataFormat = a} :: DatasetInputDataConfig)

-- | The input properties for training a document classifier model.
--
-- For more information on how the input file is formatted, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/prep-classifier-data.html Preparing training data>
-- in the Comprehend Developer Guide.
datasetInputDataConfig_documentClassifierInputDataConfig :: Lens.Lens' DatasetInputDataConfig (Prelude.Maybe DatasetDocumentClassifierInputDataConfig)
datasetInputDataConfig_documentClassifierInputDataConfig = Lens.lens (\DatasetInputDataConfig' {documentClassifierInputDataConfig} -> documentClassifierInputDataConfig) (\s@DatasetInputDataConfig' {} a -> s {documentClassifierInputDataConfig = a} :: DatasetInputDataConfig)

-- | The input properties for training an entity recognizer model.
datasetInputDataConfig_entityRecognizerInputDataConfig :: Lens.Lens' DatasetInputDataConfig (Prelude.Maybe DatasetEntityRecognizerInputDataConfig)
datasetInputDataConfig_entityRecognizerInputDataConfig = Lens.lens (\DatasetInputDataConfig' {entityRecognizerInputDataConfig} -> entityRecognizerInputDataConfig) (\s@DatasetInputDataConfig' {} a -> s {entityRecognizerInputDataConfig = a} :: DatasetInputDataConfig)

instance Prelude.Hashable DatasetInputDataConfig where
  hashWithSalt _salt DatasetInputDataConfig' {..} =
    _salt
      `Prelude.hashWithSalt` augmentedManifests
      `Prelude.hashWithSalt` dataFormat
      `Prelude.hashWithSalt` documentClassifierInputDataConfig
      `Prelude.hashWithSalt` entityRecognizerInputDataConfig

instance Prelude.NFData DatasetInputDataConfig where
  rnf DatasetInputDataConfig' {..} =
    Prelude.rnf augmentedManifests
      `Prelude.seq` Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf documentClassifierInputDataConfig
      `Prelude.seq` Prelude.rnf entityRecognizerInputDataConfig

instance Data.ToJSON DatasetInputDataConfig where
  toJSON DatasetInputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AugmentedManifests" Data..=)
              Prelude.<$> augmentedManifests,
            ("DataFormat" Data..=) Prelude.<$> dataFormat,
            ("DocumentClassifierInputDataConfig" Data..=)
              Prelude.<$> documentClassifierInputDataConfig,
            ("EntityRecognizerInputDataConfig" Data..=)
              Prelude.<$> entityRecognizerInputDataConfig
          ]
      )
