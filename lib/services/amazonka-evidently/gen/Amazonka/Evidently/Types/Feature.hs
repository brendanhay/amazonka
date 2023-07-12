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
-- Module      : Amazonka.Evidently.Types.Feature
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Evidently.Types.Feature where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types.EvaluationRule
import Amazonka.Evidently.Types.FeatureEvaluationStrategy
import Amazonka.Evidently.Types.FeatureStatus
import Amazonka.Evidently.Types.Variation
import Amazonka.Evidently.Types.VariationValueType
import qualified Amazonka.Prelude as Prelude

-- | This structure contains information about one Evidently feature in your
-- account.
--
-- /See:/ 'newFeature' smart constructor.
data Feature = Feature'
  { -- | The name of the variation that is used as the default variation. The
    -- default variation is served to users who are not allocated to any
    -- ongoing launches or experiments of this feature.
    --
    -- This variation must also be listed in the @variations@ structure.
    --
    -- If you omit @defaultVariation@, the first variation listed in the
    -- @variations@ structure is used as the default variation.
    defaultVariation :: Prelude.Maybe Prelude.Text,
    -- | The description of the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | A set of key-value pairs that specify users who should always be served
    -- a specific variation of a feature. Each key specifies a user using their
    -- user ID, account ID, or some other identifier. The value specifies the
    -- name of the variation that the user is to be served.
    --
    -- For the override to be successful, the value of the key must match the
    -- @entityId@ used in the
    -- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
    -- operation.
    entityOverrides :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An array of structures that define the evaluation rules for the feature.
    evaluationRules :: Prelude.Maybe [EvaluationRule],
    -- | The name or ARN of the project that contains the feature.
    project :: Prelude.Maybe Prelude.Text,
    -- | The list of tag keys and values associated with this feature.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ARN of the feature.
    arn :: Prelude.Text,
    -- | The date and time that the feature is created.
    createdTime :: Data.POSIX,
    -- | If this value is @ALL_RULES@, the traffic allocation specified by any
    -- ongoing launches or experiments is being used. If this is
    -- @DEFAULT_VARIATION@, the default variation is being served to all users.
    evaluationStrategy :: FeatureEvaluationStrategy,
    -- | The date and time that the feature was most recently updated.
    lastUpdatedTime :: Data.POSIX,
    -- | The name of the feature.
    name :: Prelude.Text,
    -- | The current state of the feature.
    status :: FeatureStatus,
    -- | Defines the type of value used to define the different feature
    -- variations. For more information, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-variationtypes.html Variation types>
    valueType :: VariationValueType,
    -- | An array of structures that contain the configuration of the feature\'s
    -- different variations.
    variations :: [Variation]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Feature' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultVariation', 'feature_defaultVariation' - The name of the variation that is used as the default variation. The
-- default variation is served to users who are not allocated to any
-- ongoing launches or experiments of this feature.
--
-- This variation must also be listed in the @variations@ structure.
--
-- If you omit @defaultVariation@, the first variation listed in the
-- @variations@ structure is used as the default variation.
--
-- 'description', 'feature_description' - The description of the feature.
--
-- 'entityOverrides', 'feature_entityOverrides' - A set of key-value pairs that specify users who should always be served
-- a specific variation of a feature. Each key specifies a user using their
-- user ID, account ID, or some other identifier. The value specifies the
-- name of the variation that the user is to be served.
--
-- For the override to be successful, the value of the key must match the
-- @entityId@ used in the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation.
--
-- 'evaluationRules', 'feature_evaluationRules' - An array of structures that define the evaluation rules for the feature.
--
-- 'project', 'feature_project' - The name or ARN of the project that contains the feature.
--
-- 'tags', 'feature_tags' - The list of tag keys and values associated with this feature.
--
-- 'arn', 'feature_arn' - The ARN of the feature.
--
-- 'createdTime', 'feature_createdTime' - The date and time that the feature is created.
--
-- 'evaluationStrategy', 'feature_evaluationStrategy' - If this value is @ALL_RULES@, the traffic allocation specified by any
-- ongoing launches or experiments is being used. If this is
-- @DEFAULT_VARIATION@, the default variation is being served to all users.
--
-- 'lastUpdatedTime', 'feature_lastUpdatedTime' - The date and time that the feature was most recently updated.
--
-- 'name', 'feature_name' - The name of the feature.
--
-- 'status', 'feature_status' - The current state of the feature.
--
-- 'valueType', 'feature_valueType' - Defines the type of value used to define the different feature
-- variations. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-variationtypes.html Variation types>
--
-- 'variations', 'feature_variations' - An array of structures that contain the configuration of the feature\'s
-- different variations.
newFeature ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  -- | 'evaluationStrategy'
  FeatureEvaluationStrategy ->
  -- | 'lastUpdatedTime'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  FeatureStatus ->
  -- | 'valueType'
  VariationValueType ->
  Feature
newFeature
  pArn_
  pCreatedTime_
  pEvaluationStrategy_
  pLastUpdatedTime_
  pName_
  pStatus_
  pValueType_ =
    Feature'
      { defaultVariation = Prelude.Nothing,
        description = Prelude.Nothing,
        entityOverrides = Prelude.Nothing,
        evaluationRules = Prelude.Nothing,
        project = Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        createdTime = Data._Time Lens.# pCreatedTime_,
        evaluationStrategy = pEvaluationStrategy_,
        lastUpdatedTime =
          Data._Time Lens.# pLastUpdatedTime_,
        name = pName_,
        status = pStatus_,
        valueType = pValueType_,
        variations = Prelude.mempty
      }

-- | The name of the variation that is used as the default variation. The
-- default variation is served to users who are not allocated to any
-- ongoing launches or experiments of this feature.
--
-- This variation must also be listed in the @variations@ structure.
--
-- If you omit @defaultVariation@, the first variation listed in the
-- @variations@ structure is used as the default variation.
feature_defaultVariation :: Lens.Lens' Feature (Prelude.Maybe Prelude.Text)
feature_defaultVariation = Lens.lens (\Feature' {defaultVariation} -> defaultVariation) (\s@Feature' {} a -> s {defaultVariation = a} :: Feature)

-- | The description of the feature.
feature_description :: Lens.Lens' Feature (Prelude.Maybe Prelude.Text)
feature_description = Lens.lens (\Feature' {description} -> description) (\s@Feature' {} a -> s {description = a} :: Feature)

-- | A set of key-value pairs that specify users who should always be served
-- a specific variation of a feature. Each key specifies a user using their
-- user ID, account ID, or some other identifier. The value specifies the
-- name of the variation that the user is to be served.
--
-- For the override to be successful, the value of the key must match the
-- @entityId@ used in the
-- <https://docs.aws.amazon.com/cloudwatchevidently/latest/APIReference/API_EvaluateFeature.html EvaluateFeature>
-- operation.
feature_entityOverrides :: Lens.Lens' Feature (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
feature_entityOverrides = Lens.lens (\Feature' {entityOverrides} -> entityOverrides) (\s@Feature' {} a -> s {entityOverrides = a} :: Feature) Prelude.. Lens.mapping Lens.coerced

-- | An array of structures that define the evaluation rules for the feature.
feature_evaluationRules :: Lens.Lens' Feature (Prelude.Maybe [EvaluationRule])
feature_evaluationRules = Lens.lens (\Feature' {evaluationRules} -> evaluationRules) (\s@Feature' {} a -> s {evaluationRules = a} :: Feature) Prelude.. Lens.mapping Lens.coerced

-- | The name or ARN of the project that contains the feature.
feature_project :: Lens.Lens' Feature (Prelude.Maybe Prelude.Text)
feature_project = Lens.lens (\Feature' {project} -> project) (\s@Feature' {} a -> s {project = a} :: Feature)

-- | The list of tag keys and values associated with this feature.
feature_tags :: Lens.Lens' Feature (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
feature_tags = Lens.lens (\Feature' {tags} -> tags) (\s@Feature' {} a -> s {tags = a} :: Feature) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the feature.
feature_arn :: Lens.Lens' Feature Prelude.Text
feature_arn = Lens.lens (\Feature' {arn} -> arn) (\s@Feature' {} a -> s {arn = a} :: Feature)

-- | The date and time that the feature is created.
feature_createdTime :: Lens.Lens' Feature Prelude.UTCTime
feature_createdTime = Lens.lens (\Feature' {createdTime} -> createdTime) (\s@Feature' {} a -> s {createdTime = a} :: Feature) Prelude.. Data._Time

-- | If this value is @ALL_RULES@, the traffic allocation specified by any
-- ongoing launches or experiments is being used. If this is
-- @DEFAULT_VARIATION@, the default variation is being served to all users.
feature_evaluationStrategy :: Lens.Lens' Feature FeatureEvaluationStrategy
feature_evaluationStrategy = Lens.lens (\Feature' {evaluationStrategy} -> evaluationStrategy) (\s@Feature' {} a -> s {evaluationStrategy = a} :: Feature)

-- | The date and time that the feature was most recently updated.
feature_lastUpdatedTime :: Lens.Lens' Feature Prelude.UTCTime
feature_lastUpdatedTime = Lens.lens (\Feature' {lastUpdatedTime} -> lastUpdatedTime) (\s@Feature' {} a -> s {lastUpdatedTime = a} :: Feature) Prelude.. Data._Time

-- | The name of the feature.
feature_name :: Lens.Lens' Feature Prelude.Text
feature_name = Lens.lens (\Feature' {name} -> name) (\s@Feature' {} a -> s {name = a} :: Feature)

-- | The current state of the feature.
feature_status :: Lens.Lens' Feature FeatureStatus
feature_status = Lens.lens (\Feature' {status} -> status) (\s@Feature' {} a -> s {status = a} :: Feature)

-- | Defines the type of value used to define the different feature
-- variations. For more information, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch-Evidently-variationtypes.html Variation types>
feature_valueType :: Lens.Lens' Feature VariationValueType
feature_valueType = Lens.lens (\Feature' {valueType} -> valueType) (\s@Feature' {} a -> s {valueType = a} :: Feature)

-- | An array of structures that contain the configuration of the feature\'s
-- different variations.
feature_variations :: Lens.Lens' Feature [Variation]
feature_variations = Lens.lens (\Feature' {variations} -> variations) (\s@Feature' {} a -> s {variations = a} :: Feature) Prelude.. Lens.coerced

instance Data.FromJSON Feature where
  parseJSON =
    Data.withObject
      "Feature"
      ( \x ->
          Feature'
            Prelude.<$> (x Data..:? "defaultVariation")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> ( x
                            Data..:? "entityOverrides"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "evaluationRules"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "project")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "createdTime")
            Prelude.<*> (x Data..: "evaluationStrategy")
            Prelude.<*> (x Data..: "lastUpdatedTime")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "valueType")
            Prelude.<*> (x Data..:? "variations" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Feature where
  hashWithSalt _salt Feature' {..} =
    _salt
      `Prelude.hashWithSalt` defaultVariation
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` entityOverrides
      `Prelude.hashWithSalt` evaluationRules
      `Prelude.hashWithSalt` project
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` evaluationStrategy
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` valueType
      `Prelude.hashWithSalt` variations

instance Prelude.NFData Feature where
  rnf Feature' {..} =
    Prelude.rnf defaultVariation
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf entityOverrides
      `Prelude.seq` Prelude.rnf evaluationRules
      `Prelude.seq` Prelude.rnf project
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf evaluationStrategy
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf valueType
      `Prelude.seq` Prelude.rnf variations
