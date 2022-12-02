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
-- Module      : Amazonka.SageMaker.Types.AutoMLCandidateGenerationConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLCandidateGenerationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Stores the config information for how a candidate is generated
-- (optional).
--
-- /See:/ 'newAutoMLCandidateGenerationConfig' smart constructor.
data AutoMLCandidateGenerationConfig = AutoMLCandidateGenerationConfig'
  { -- | A URL to the Amazon S3 data source containing selected features from the
    -- input data source to run an Autopilot job. You can input
    -- @FeatureAttributeNames@ (optional) in JSON format as shown below:
    --
    -- @{ \"FeatureAttributeNames\":[\"col1\", \"col2\", ...] }@.
    --
    -- You can also specify the data type of the feature (optional) in the
    -- format shown below:
    --
    -- @{ \"FeatureDataTypes\":{\"col1\":\"numeric\", \"col2\":\"categorical\" ... } }@
    --
    -- These column keys may not include the target column.
    --
    -- In ensembling mode, Autopilot will only support the following data
    -- types: @numeric@, @categorical@, @text@ and @datetime@. In HPO mode,
    -- Autopilot can support @numeric@, @categorical@, @text@, @datetime@ and
    -- @sequence@.
    --
    -- If only @FeatureDataTypes@ is provided, the column keys (@col1@,
    -- @col2@,..) should be a subset of the column names in the input data.
    --
    -- If both @FeatureDataTypes@ and @FeatureAttributeNames@ are provided,
    -- then the column keys should be a subset of the column names provided in
    -- @FeatureAttributeNames@.
    --
    -- The key name @FeatureAttributeNames@ is fixed. The values listed in
    -- @[\"col1\", \"col2\", ...]@ is case sensitive and should be a list of
    -- strings containing unique values that are a subset of the column names
    -- in the input data. The list of columns provided must not include the
    -- target column.
    featureSpecificationS3Uri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoMLCandidateGenerationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureSpecificationS3Uri', 'autoMLCandidateGenerationConfig_featureSpecificationS3Uri' - A URL to the Amazon S3 data source containing selected features from the
-- input data source to run an Autopilot job. You can input
-- @FeatureAttributeNames@ (optional) in JSON format as shown below:
--
-- @{ \"FeatureAttributeNames\":[\"col1\", \"col2\", ...] }@.
--
-- You can also specify the data type of the feature (optional) in the
-- format shown below:
--
-- @{ \"FeatureDataTypes\":{\"col1\":\"numeric\", \"col2\":\"categorical\" ... } }@
--
-- These column keys may not include the target column.
--
-- In ensembling mode, Autopilot will only support the following data
-- types: @numeric@, @categorical@, @text@ and @datetime@. In HPO mode,
-- Autopilot can support @numeric@, @categorical@, @text@, @datetime@ and
-- @sequence@.
--
-- If only @FeatureDataTypes@ is provided, the column keys (@col1@,
-- @col2@,..) should be a subset of the column names in the input data.
--
-- If both @FeatureDataTypes@ and @FeatureAttributeNames@ are provided,
-- then the column keys should be a subset of the column names provided in
-- @FeatureAttributeNames@.
--
-- The key name @FeatureAttributeNames@ is fixed. The values listed in
-- @[\"col1\", \"col2\", ...]@ is case sensitive and should be a list of
-- strings containing unique values that are a subset of the column names
-- in the input data. The list of columns provided must not include the
-- target column.
newAutoMLCandidateGenerationConfig ::
  AutoMLCandidateGenerationConfig
newAutoMLCandidateGenerationConfig =
  AutoMLCandidateGenerationConfig'
    { featureSpecificationS3Uri =
        Prelude.Nothing
    }

-- | A URL to the Amazon S3 data source containing selected features from the
-- input data source to run an Autopilot job. You can input
-- @FeatureAttributeNames@ (optional) in JSON format as shown below:
--
-- @{ \"FeatureAttributeNames\":[\"col1\", \"col2\", ...] }@.
--
-- You can also specify the data type of the feature (optional) in the
-- format shown below:
--
-- @{ \"FeatureDataTypes\":{\"col1\":\"numeric\", \"col2\":\"categorical\" ... } }@
--
-- These column keys may not include the target column.
--
-- In ensembling mode, Autopilot will only support the following data
-- types: @numeric@, @categorical@, @text@ and @datetime@. In HPO mode,
-- Autopilot can support @numeric@, @categorical@, @text@, @datetime@ and
-- @sequence@.
--
-- If only @FeatureDataTypes@ is provided, the column keys (@col1@,
-- @col2@,..) should be a subset of the column names in the input data.
--
-- If both @FeatureDataTypes@ and @FeatureAttributeNames@ are provided,
-- then the column keys should be a subset of the column names provided in
-- @FeatureAttributeNames@.
--
-- The key name @FeatureAttributeNames@ is fixed. The values listed in
-- @[\"col1\", \"col2\", ...]@ is case sensitive and should be a list of
-- strings containing unique values that are a subset of the column names
-- in the input data. The list of columns provided must not include the
-- target column.
autoMLCandidateGenerationConfig_featureSpecificationS3Uri :: Lens.Lens' AutoMLCandidateGenerationConfig (Prelude.Maybe Prelude.Text)
autoMLCandidateGenerationConfig_featureSpecificationS3Uri = Lens.lens (\AutoMLCandidateGenerationConfig' {featureSpecificationS3Uri} -> featureSpecificationS3Uri) (\s@AutoMLCandidateGenerationConfig' {} a -> s {featureSpecificationS3Uri = a} :: AutoMLCandidateGenerationConfig)

instance
  Data.FromJSON
    AutoMLCandidateGenerationConfig
  where
  parseJSON =
    Data.withObject
      "AutoMLCandidateGenerationConfig"
      ( \x ->
          AutoMLCandidateGenerationConfig'
            Prelude.<$> (x Data..:? "FeatureSpecificationS3Uri")
      )

instance
  Prelude.Hashable
    AutoMLCandidateGenerationConfig
  where
  hashWithSalt
    _salt
    AutoMLCandidateGenerationConfig' {..} =
      _salt
        `Prelude.hashWithSalt` featureSpecificationS3Uri

instance
  Prelude.NFData
    AutoMLCandidateGenerationConfig
  where
  rnf AutoMLCandidateGenerationConfig' {..} =
    Prelude.rnf featureSpecificationS3Uri

instance Data.ToJSON AutoMLCandidateGenerationConfig where
  toJSON AutoMLCandidateGenerationConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FeatureSpecificationS3Uri" Data..=)
              Prelude.<$> featureSpecificationS3Uri
          ]
      )
