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
-- Module      : Amazonka.SageMaker.Types.TabularJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TabularJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLMode
import Amazonka.SageMaker.Types.CandidateGenerationConfig
import Amazonka.SageMaker.Types.ProblemType

-- | The collection of settings used by an AutoML job V2 for the @TABULAR@
-- problem type.
--
-- /See:/ 'newTabularJobConfig' smart constructor.
data TabularJobConfig = TabularJobConfig'
  { -- | The configuration information of how model candidates are generated.
    candidateGenerationConfig :: Prelude.Maybe CandidateGenerationConfig,
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria,
    -- | A URL to the Amazon S3 data source containing selected features from the
    -- input data source to run an Autopilot job V2. You can input
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
    -- In ensembling mode, Autopilot only supports the following data types:
    -- @numeric@, @categorical@, @text@, and @datetime@. In HPO mode, Autopilot
    -- can support @numeric@, @categorical@, @text@, @datetime@, and
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
    -- @[\"col1\", \"col2\", ...]@ are case sensitive and should be a list of
    -- strings containing unique values that are a subset of the column names
    -- in the input data. The list of columns provided must not include the
    -- target column.
    featureSpecificationS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Generates possible candidates without training the models. A model
    -- candidate is a combination of data preprocessors, algorithms, and
    -- algorithm parameter settings.
    generateCandidateDefinitionsOnly :: Prelude.Maybe Prelude.Bool,
    -- | The method that Autopilot uses to train the data. You can either specify
    -- the mode manually or let Autopilot choose for you based on the dataset
    -- size by selecting @AUTO@. In @AUTO@ mode, Autopilot chooses @ENSEMBLING@
    -- for datasets smaller than 100 MB, and @HYPERPARAMETER_TUNING@ for larger
    -- ones.
    --
    -- The @ENSEMBLING@ mode uses a multi-stack ensemble model to predict
    -- classification and regression tasks directly from your dataset. This
    -- machine learning mode combines several base models to produce an optimal
    -- predictive model. It then uses a stacking ensemble method to combine
    -- predictions from contributing members. A multi-stack ensemble model can
    -- provide better performance over a single model by combining the
    -- predictive capabilities of multiple models. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Autopilot algorithm support>
    -- for a list of algorithms supported by @ENSEMBLING@ mode.
    --
    -- The @HYPERPARAMETER_TUNING@ (HPO) mode uses the best hyperparameters to
    -- train the best version of a model. HPO automatically selects an
    -- algorithm for the type of problem you want to solve. Then HPO finds the
    -- best hyperparameters according to your objective metric. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Autopilot algorithm support>
    -- for a list of algorithms supported by @HYPERPARAMETER_TUNING@ mode.
    mode :: Prelude.Maybe AutoMLMode,
    -- | The type of supervised learning problem available for the model
    -- candidates of the AutoML job V2. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-problem-types Amazon SageMaker Autopilot problem types>.
    --
    -- You must either specify the type of supervised learning problem in
    -- @ProblemType@ and provide the
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJobV2.html#sagemaker-CreateAutoMLJobV2-request-AutoMLJobObjective AutoMLJobObjective>
    -- metric, or none at all.
    problemType :: Prelude.Maybe ProblemType,
    -- | If specified, this column name indicates which column of the dataset
    -- should be treated as sample weights for use by the objective metric
    -- during the training, evaluation, and the selection of the best model.
    -- This column is not considered as a predictive feature. For more
    -- information on Autopilot metrics, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html Metrics and validation>.
    --
    -- Sample weights should be numeric, non-negative, with larger values
    -- indicating which rows are more important than others. Data points that
    -- have invalid or no weight value are excluded.
    --
    -- Support for sample weights is available in
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLAlgorithmConfig.html Ensembling>
    -- mode only.
    sampleWeightAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The name of the target variable in supervised learning, usually
    -- represented by \'y\'.
    targetAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TabularJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'candidateGenerationConfig', 'tabularJobConfig_candidateGenerationConfig' - The configuration information of how model candidates are generated.
--
-- 'completionCriteria', 'tabularJobConfig_completionCriteria' - Undocumented member.
--
-- 'featureSpecificationS3Uri', 'tabularJobConfig_featureSpecificationS3Uri' - A URL to the Amazon S3 data source containing selected features from the
-- input data source to run an Autopilot job V2. You can input
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
-- In ensembling mode, Autopilot only supports the following data types:
-- @numeric@, @categorical@, @text@, and @datetime@. In HPO mode, Autopilot
-- can support @numeric@, @categorical@, @text@, @datetime@, and
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
-- @[\"col1\", \"col2\", ...]@ are case sensitive and should be a list of
-- strings containing unique values that are a subset of the column names
-- in the input data. The list of columns provided must not include the
-- target column.
--
-- 'generateCandidateDefinitionsOnly', 'tabularJobConfig_generateCandidateDefinitionsOnly' - Generates possible candidates without training the models. A model
-- candidate is a combination of data preprocessors, algorithms, and
-- algorithm parameter settings.
--
-- 'mode', 'tabularJobConfig_mode' - The method that Autopilot uses to train the data. You can either specify
-- the mode manually or let Autopilot choose for you based on the dataset
-- size by selecting @AUTO@. In @AUTO@ mode, Autopilot chooses @ENSEMBLING@
-- for datasets smaller than 100 MB, and @HYPERPARAMETER_TUNING@ for larger
-- ones.
--
-- The @ENSEMBLING@ mode uses a multi-stack ensemble model to predict
-- classification and regression tasks directly from your dataset. This
-- machine learning mode combines several base models to produce an optimal
-- predictive model. It then uses a stacking ensemble method to combine
-- predictions from contributing members. A multi-stack ensemble model can
-- provide better performance over a single model by combining the
-- predictive capabilities of multiple models. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Autopilot algorithm support>
-- for a list of algorithms supported by @ENSEMBLING@ mode.
--
-- The @HYPERPARAMETER_TUNING@ (HPO) mode uses the best hyperparameters to
-- train the best version of a model. HPO automatically selects an
-- algorithm for the type of problem you want to solve. Then HPO finds the
-- best hyperparameters according to your objective metric. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Autopilot algorithm support>
-- for a list of algorithms supported by @HYPERPARAMETER_TUNING@ mode.
--
-- 'problemType', 'tabularJobConfig_problemType' - The type of supervised learning problem available for the model
-- candidates of the AutoML job V2. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-problem-types Amazon SageMaker Autopilot problem types>.
--
-- You must either specify the type of supervised learning problem in
-- @ProblemType@ and provide the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJobV2.html#sagemaker-CreateAutoMLJobV2-request-AutoMLJobObjective AutoMLJobObjective>
-- metric, or none at all.
--
-- 'sampleWeightAttributeName', 'tabularJobConfig_sampleWeightAttributeName' - If specified, this column name indicates which column of the dataset
-- should be treated as sample weights for use by the objective metric
-- during the training, evaluation, and the selection of the best model.
-- This column is not considered as a predictive feature. For more
-- information on Autopilot metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html Metrics and validation>.
--
-- Sample weights should be numeric, non-negative, with larger values
-- indicating which rows are more important than others. Data points that
-- have invalid or no weight value are excluded.
--
-- Support for sample weights is available in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLAlgorithmConfig.html Ensembling>
-- mode only.
--
-- 'targetAttributeName', 'tabularJobConfig_targetAttributeName' - The name of the target variable in supervised learning, usually
-- represented by \'y\'.
newTabularJobConfig ::
  -- | 'targetAttributeName'
  Prelude.Text ->
  TabularJobConfig
newTabularJobConfig pTargetAttributeName_ =
  TabularJobConfig'
    { candidateGenerationConfig =
        Prelude.Nothing,
      completionCriteria = Prelude.Nothing,
      featureSpecificationS3Uri = Prelude.Nothing,
      generateCandidateDefinitionsOnly = Prelude.Nothing,
      mode = Prelude.Nothing,
      problemType = Prelude.Nothing,
      sampleWeightAttributeName = Prelude.Nothing,
      targetAttributeName = pTargetAttributeName_
    }

-- | The configuration information of how model candidates are generated.
tabularJobConfig_candidateGenerationConfig :: Lens.Lens' TabularJobConfig (Prelude.Maybe CandidateGenerationConfig)
tabularJobConfig_candidateGenerationConfig = Lens.lens (\TabularJobConfig' {candidateGenerationConfig} -> candidateGenerationConfig) (\s@TabularJobConfig' {} a -> s {candidateGenerationConfig = a} :: TabularJobConfig)

-- | Undocumented member.
tabularJobConfig_completionCriteria :: Lens.Lens' TabularJobConfig (Prelude.Maybe AutoMLJobCompletionCriteria)
tabularJobConfig_completionCriteria = Lens.lens (\TabularJobConfig' {completionCriteria} -> completionCriteria) (\s@TabularJobConfig' {} a -> s {completionCriteria = a} :: TabularJobConfig)

-- | A URL to the Amazon S3 data source containing selected features from the
-- input data source to run an Autopilot job V2. You can input
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
-- In ensembling mode, Autopilot only supports the following data types:
-- @numeric@, @categorical@, @text@, and @datetime@. In HPO mode, Autopilot
-- can support @numeric@, @categorical@, @text@, @datetime@, and
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
-- @[\"col1\", \"col2\", ...]@ are case sensitive and should be a list of
-- strings containing unique values that are a subset of the column names
-- in the input data. The list of columns provided must not include the
-- target column.
tabularJobConfig_featureSpecificationS3Uri :: Lens.Lens' TabularJobConfig (Prelude.Maybe Prelude.Text)
tabularJobConfig_featureSpecificationS3Uri = Lens.lens (\TabularJobConfig' {featureSpecificationS3Uri} -> featureSpecificationS3Uri) (\s@TabularJobConfig' {} a -> s {featureSpecificationS3Uri = a} :: TabularJobConfig)

-- | Generates possible candidates without training the models. A model
-- candidate is a combination of data preprocessors, algorithms, and
-- algorithm parameter settings.
tabularJobConfig_generateCandidateDefinitionsOnly :: Lens.Lens' TabularJobConfig (Prelude.Maybe Prelude.Bool)
tabularJobConfig_generateCandidateDefinitionsOnly = Lens.lens (\TabularJobConfig' {generateCandidateDefinitionsOnly} -> generateCandidateDefinitionsOnly) (\s@TabularJobConfig' {} a -> s {generateCandidateDefinitionsOnly = a} :: TabularJobConfig)

-- | The method that Autopilot uses to train the data. You can either specify
-- the mode manually or let Autopilot choose for you based on the dataset
-- size by selecting @AUTO@. In @AUTO@ mode, Autopilot chooses @ENSEMBLING@
-- for datasets smaller than 100 MB, and @HYPERPARAMETER_TUNING@ for larger
-- ones.
--
-- The @ENSEMBLING@ mode uses a multi-stack ensemble model to predict
-- classification and regression tasks directly from your dataset. This
-- machine learning mode combines several base models to produce an optimal
-- predictive model. It then uses a stacking ensemble method to combine
-- predictions from contributing members. A multi-stack ensemble model can
-- provide better performance over a single model by combining the
-- predictive capabilities of multiple models. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Autopilot algorithm support>
-- for a list of algorithms supported by @ENSEMBLING@ mode.
--
-- The @HYPERPARAMETER_TUNING@ (HPO) mode uses the best hyperparameters to
-- train the best version of a model. HPO automatically selects an
-- algorithm for the type of problem you want to solve. Then HPO finds the
-- best hyperparameters according to your objective metric. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-model-support-validation.html#autopilot-algorithm-support Autopilot algorithm support>
-- for a list of algorithms supported by @HYPERPARAMETER_TUNING@ mode.
tabularJobConfig_mode :: Lens.Lens' TabularJobConfig (Prelude.Maybe AutoMLMode)
tabularJobConfig_mode = Lens.lens (\TabularJobConfig' {mode} -> mode) (\s@TabularJobConfig' {} a -> s {mode = a} :: TabularJobConfig)

-- | The type of supervised learning problem available for the model
-- candidates of the AutoML job V2. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-problem-types Amazon SageMaker Autopilot problem types>.
--
-- You must either specify the type of supervised learning problem in
-- @ProblemType@ and provide the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateAutoMLJobV2.html#sagemaker-CreateAutoMLJobV2-request-AutoMLJobObjective AutoMLJobObjective>
-- metric, or none at all.
tabularJobConfig_problemType :: Lens.Lens' TabularJobConfig (Prelude.Maybe ProblemType)
tabularJobConfig_problemType = Lens.lens (\TabularJobConfig' {problemType} -> problemType) (\s@TabularJobConfig' {} a -> s {problemType = a} :: TabularJobConfig)

-- | If specified, this column name indicates which column of the dataset
-- should be treated as sample weights for use by the objective metric
-- during the training, evaluation, and the selection of the best model.
-- This column is not considered as a predictive feature. For more
-- information on Autopilot metrics, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-metrics-validation.html Metrics and validation>.
--
-- Sample weights should be numeric, non-negative, with larger values
-- indicating which rows are more important than others. Data points that
-- have invalid or no weight value are excluded.
--
-- Support for sample weights is available in
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_AutoMLAlgorithmConfig.html Ensembling>
-- mode only.
tabularJobConfig_sampleWeightAttributeName :: Lens.Lens' TabularJobConfig (Prelude.Maybe Prelude.Text)
tabularJobConfig_sampleWeightAttributeName = Lens.lens (\TabularJobConfig' {sampleWeightAttributeName} -> sampleWeightAttributeName) (\s@TabularJobConfig' {} a -> s {sampleWeightAttributeName = a} :: TabularJobConfig)

-- | The name of the target variable in supervised learning, usually
-- represented by \'y\'.
tabularJobConfig_targetAttributeName :: Lens.Lens' TabularJobConfig Prelude.Text
tabularJobConfig_targetAttributeName = Lens.lens (\TabularJobConfig' {targetAttributeName} -> targetAttributeName) (\s@TabularJobConfig' {} a -> s {targetAttributeName = a} :: TabularJobConfig)

instance Data.FromJSON TabularJobConfig where
  parseJSON =
    Data.withObject
      "TabularJobConfig"
      ( \x ->
          TabularJobConfig'
            Prelude.<$> (x Data..:? "CandidateGenerationConfig")
            Prelude.<*> (x Data..:? "CompletionCriteria")
            Prelude.<*> (x Data..:? "FeatureSpecificationS3Uri")
            Prelude.<*> (x Data..:? "GenerateCandidateDefinitionsOnly")
            Prelude.<*> (x Data..:? "Mode")
            Prelude.<*> (x Data..:? "ProblemType")
            Prelude.<*> (x Data..:? "SampleWeightAttributeName")
            Prelude.<*> (x Data..: "TargetAttributeName")
      )

instance Prelude.Hashable TabularJobConfig where
  hashWithSalt _salt TabularJobConfig' {..} =
    _salt
      `Prelude.hashWithSalt` candidateGenerationConfig
      `Prelude.hashWithSalt` completionCriteria
      `Prelude.hashWithSalt` featureSpecificationS3Uri
      `Prelude.hashWithSalt` generateCandidateDefinitionsOnly
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` problemType
      `Prelude.hashWithSalt` sampleWeightAttributeName
      `Prelude.hashWithSalt` targetAttributeName

instance Prelude.NFData TabularJobConfig where
  rnf TabularJobConfig' {..} =
    Prelude.rnf candidateGenerationConfig
      `Prelude.seq` Prelude.rnf completionCriteria
      `Prelude.seq` Prelude.rnf featureSpecificationS3Uri
      `Prelude.seq` Prelude.rnf generateCandidateDefinitionsOnly
      `Prelude.seq` Prelude.rnf mode
      `Prelude.seq` Prelude.rnf problemType
      `Prelude.seq` Prelude.rnf sampleWeightAttributeName
      `Prelude.seq` Prelude.rnf targetAttributeName

instance Data.ToJSON TabularJobConfig where
  toJSON TabularJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CandidateGenerationConfig" Data..=)
              Prelude.<$> candidateGenerationConfig,
            ("CompletionCriteria" Data..=)
              Prelude.<$> completionCriteria,
            ("FeatureSpecificationS3Uri" Data..=)
              Prelude.<$> featureSpecificationS3Uri,
            ("GenerateCandidateDefinitionsOnly" Data..=)
              Prelude.<$> generateCandidateDefinitionsOnly,
            ("Mode" Data..=) Prelude.<$> mode,
            ("ProblemType" Data..=) Prelude.<$> problemType,
            ("SampleWeightAttributeName" Data..=)
              Prelude.<$> sampleWeightAttributeName,
            Prelude.Just
              ("TargetAttributeName" Data..= targetAttributeName)
          ]
      )
