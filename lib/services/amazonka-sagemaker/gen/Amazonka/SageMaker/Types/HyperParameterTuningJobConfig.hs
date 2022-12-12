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
-- Module      : Amazonka.SageMaker.Types.HyperParameterTuningJobConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterTuningJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyType
import Amazonka.SageMaker.Types.ParameterRanges
import Amazonka.SageMaker.Types.ResourceLimits
import Amazonka.SageMaker.Types.TrainingJobEarlyStoppingType
import Amazonka.SageMaker.Types.TuningJobCompletionCriteria

-- | Configures a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTuningJobConfig' smart constructor.
data HyperParameterTuningJobConfig = HyperParameterTuningJobConfig'
  { -- | The HyperParameterTuningJobObjective object that specifies the objective
    -- metric for this tuning job.
    hyperParameterTuningJobObjective :: Prelude.Maybe HyperParameterTuningJobObjective,
    -- | The ParameterRanges object that specifies the ranges of hyperparameters
    -- that this tuning job searches.
    parameterRanges :: Prelude.Maybe ParameterRanges,
    -- | The configuration for the @Hyperband@ optimization strategy. This
    -- parameter should be provided only if @Hyperband@ is selected as the
    -- strategy for @HyperParameterTuningJobConfig@.
    strategyConfig :: Prelude.Maybe HyperParameterTuningJobStrategyConfig,
    -- | Specifies whether to use early stopping for training jobs launched by
    -- the hyperparameter tuning job. Because the @Hyperband@ strategy has its
    -- own advanced internal early stopping mechanism,
    -- @TrainingJobEarlyStoppingType@ must be @OFF@ to use @Hyperband@. This
    -- parameter can take on one of the following values (the default value is
    -- @OFF@):
    --
    -- [OFF]
    --     Training jobs launched by the hyperparameter tuning job do not use
    --     early stopping.
    --
    -- [AUTO]
    --     SageMaker stops training jobs launched by the hyperparameter tuning
    --     job when they are unlikely to perform better than previously
    --     completed training jobs. For more information, see
    --     <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early>.
    trainingJobEarlyStoppingType :: Prelude.Maybe TrainingJobEarlyStoppingType,
    -- | The tuning job\'s completion criteria.
    tuningJobCompletionCriteria :: Prelude.Maybe TuningJobCompletionCriteria,
    -- | Specifies how hyperparameter tuning chooses the combinations of
    -- hyperparameter values to use for the training job it launches. For
    -- information about search strategies, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
    strategy :: HyperParameterTuningJobStrategyType,
    -- | The ResourceLimits object that specifies the maximum number of training
    -- jobs and parallel training jobs for this tuning job.
    resourceLimits :: ResourceLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterTuningJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperParameterTuningJobObjective', 'hyperParameterTuningJobConfig_hyperParameterTuningJobObjective' - The HyperParameterTuningJobObjective object that specifies the objective
-- metric for this tuning job.
--
-- 'parameterRanges', 'hyperParameterTuningJobConfig_parameterRanges' - The ParameterRanges object that specifies the ranges of hyperparameters
-- that this tuning job searches.
--
-- 'strategyConfig', 'hyperParameterTuningJobConfig_strategyConfig' - The configuration for the @Hyperband@ optimization strategy. This
-- parameter should be provided only if @Hyperband@ is selected as the
-- strategy for @HyperParameterTuningJobConfig@.
--
-- 'trainingJobEarlyStoppingType', 'hyperParameterTuningJobConfig_trainingJobEarlyStoppingType' - Specifies whether to use early stopping for training jobs launched by
-- the hyperparameter tuning job. Because the @Hyperband@ strategy has its
-- own advanced internal early stopping mechanism,
-- @TrainingJobEarlyStoppingType@ must be @OFF@ to use @Hyperband@. This
-- parameter can take on one of the following values (the default value is
-- @OFF@):
--
-- [OFF]
--     Training jobs launched by the hyperparameter tuning job do not use
--     early stopping.
--
-- [AUTO]
--     SageMaker stops training jobs launched by the hyperparameter tuning
--     job when they are unlikely to perform better than previously
--     completed training jobs. For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early>.
--
-- 'tuningJobCompletionCriteria', 'hyperParameterTuningJobConfig_tuningJobCompletionCriteria' - The tuning job\'s completion criteria.
--
-- 'strategy', 'hyperParameterTuningJobConfig_strategy' - Specifies how hyperparameter tuning chooses the combinations of
-- hyperparameter values to use for the training job it launches. For
-- information about search strategies, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
--
-- 'resourceLimits', 'hyperParameterTuningJobConfig_resourceLimits' - The ResourceLimits object that specifies the maximum number of training
-- jobs and parallel training jobs for this tuning job.
newHyperParameterTuningJobConfig ::
  -- | 'strategy'
  HyperParameterTuningJobStrategyType ->
  -- | 'resourceLimits'
  ResourceLimits ->
  HyperParameterTuningJobConfig
newHyperParameterTuningJobConfig
  pStrategy_
  pResourceLimits_ =
    HyperParameterTuningJobConfig'
      { hyperParameterTuningJobObjective =
          Prelude.Nothing,
        parameterRanges = Prelude.Nothing,
        strategyConfig = Prelude.Nothing,
        trainingJobEarlyStoppingType =
          Prelude.Nothing,
        tuningJobCompletionCriteria =
          Prelude.Nothing,
        strategy = pStrategy_,
        resourceLimits = pResourceLimits_
      }

-- | The HyperParameterTuningJobObjective object that specifies the objective
-- metric for this tuning job.
hyperParameterTuningJobConfig_hyperParameterTuningJobObjective :: Lens.Lens' HyperParameterTuningJobConfig (Prelude.Maybe HyperParameterTuningJobObjective)
hyperParameterTuningJobConfig_hyperParameterTuningJobObjective = Lens.lens (\HyperParameterTuningJobConfig' {hyperParameterTuningJobObjective} -> hyperParameterTuningJobObjective) (\s@HyperParameterTuningJobConfig' {} a -> s {hyperParameterTuningJobObjective = a} :: HyperParameterTuningJobConfig)

-- | The ParameterRanges object that specifies the ranges of hyperparameters
-- that this tuning job searches.
hyperParameterTuningJobConfig_parameterRanges :: Lens.Lens' HyperParameterTuningJobConfig (Prelude.Maybe ParameterRanges)
hyperParameterTuningJobConfig_parameterRanges = Lens.lens (\HyperParameterTuningJobConfig' {parameterRanges} -> parameterRanges) (\s@HyperParameterTuningJobConfig' {} a -> s {parameterRanges = a} :: HyperParameterTuningJobConfig)

-- | The configuration for the @Hyperband@ optimization strategy. This
-- parameter should be provided only if @Hyperband@ is selected as the
-- strategy for @HyperParameterTuningJobConfig@.
hyperParameterTuningJobConfig_strategyConfig :: Lens.Lens' HyperParameterTuningJobConfig (Prelude.Maybe HyperParameterTuningJobStrategyConfig)
hyperParameterTuningJobConfig_strategyConfig = Lens.lens (\HyperParameterTuningJobConfig' {strategyConfig} -> strategyConfig) (\s@HyperParameterTuningJobConfig' {} a -> s {strategyConfig = a} :: HyperParameterTuningJobConfig)

-- | Specifies whether to use early stopping for training jobs launched by
-- the hyperparameter tuning job. Because the @Hyperband@ strategy has its
-- own advanced internal early stopping mechanism,
-- @TrainingJobEarlyStoppingType@ must be @OFF@ to use @Hyperband@. This
-- parameter can take on one of the following values (the default value is
-- @OFF@):
--
-- [OFF]
--     Training jobs launched by the hyperparameter tuning job do not use
--     early stopping.
--
-- [AUTO]
--     SageMaker stops training jobs launched by the hyperparameter tuning
--     job when they are unlikely to perform better than previously
--     completed training jobs. For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early>.
hyperParameterTuningJobConfig_trainingJobEarlyStoppingType :: Lens.Lens' HyperParameterTuningJobConfig (Prelude.Maybe TrainingJobEarlyStoppingType)
hyperParameterTuningJobConfig_trainingJobEarlyStoppingType = Lens.lens (\HyperParameterTuningJobConfig' {trainingJobEarlyStoppingType} -> trainingJobEarlyStoppingType) (\s@HyperParameterTuningJobConfig' {} a -> s {trainingJobEarlyStoppingType = a} :: HyperParameterTuningJobConfig)

-- | The tuning job\'s completion criteria.
hyperParameterTuningJobConfig_tuningJobCompletionCriteria :: Lens.Lens' HyperParameterTuningJobConfig (Prelude.Maybe TuningJobCompletionCriteria)
hyperParameterTuningJobConfig_tuningJobCompletionCriteria = Lens.lens (\HyperParameterTuningJobConfig' {tuningJobCompletionCriteria} -> tuningJobCompletionCriteria) (\s@HyperParameterTuningJobConfig' {} a -> s {tuningJobCompletionCriteria = a} :: HyperParameterTuningJobConfig)

-- | Specifies how hyperparameter tuning chooses the combinations of
-- hyperparameter values to use for the training job it launches. For
-- information about search strategies, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
hyperParameterTuningJobConfig_strategy :: Lens.Lens' HyperParameterTuningJobConfig HyperParameterTuningJobStrategyType
hyperParameterTuningJobConfig_strategy = Lens.lens (\HyperParameterTuningJobConfig' {strategy} -> strategy) (\s@HyperParameterTuningJobConfig' {} a -> s {strategy = a} :: HyperParameterTuningJobConfig)

-- | The ResourceLimits object that specifies the maximum number of training
-- jobs and parallel training jobs for this tuning job.
hyperParameterTuningJobConfig_resourceLimits :: Lens.Lens' HyperParameterTuningJobConfig ResourceLimits
hyperParameterTuningJobConfig_resourceLimits = Lens.lens (\HyperParameterTuningJobConfig' {resourceLimits} -> resourceLimits) (\s@HyperParameterTuningJobConfig' {} a -> s {resourceLimits = a} :: HyperParameterTuningJobConfig)

instance Data.FromJSON HyperParameterTuningJobConfig where
  parseJSON =
    Data.withObject
      "HyperParameterTuningJobConfig"
      ( \x ->
          HyperParameterTuningJobConfig'
            Prelude.<$> (x Data..:? "HyperParameterTuningJobObjective")
            Prelude.<*> (x Data..:? "ParameterRanges")
            Prelude.<*> (x Data..:? "StrategyConfig")
            Prelude.<*> (x Data..:? "TrainingJobEarlyStoppingType")
            Prelude.<*> (x Data..:? "TuningJobCompletionCriteria")
            Prelude.<*> (x Data..: "Strategy")
            Prelude.<*> (x Data..: "ResourceLimits")
      )

instance
  Prelude.Hashable
    HyperParameterTuningJobConfig
  where
  hashWithSalt _salt HyperParameterTuningJobConfig' {..} =
    _salt
      `Prelude.hashWithSalt` hyperParameterTuningJobObjective
      `Prelude.hashWithSalt` parameterRanges
      `Prelude.hashWithSalt` strategyConfig
      `Prelude.hashWithSalt` trainingJobEarlyStoppingType
      `Prelude.hashWithSalt` tuningJobCompletionCriteria
      `Prelude.hashWithSalt` strategy
      `Prelude.hashWithSalt` resourceLimits

instance Prelude.NFData HyperParameterTuningJobConfig where
  rnf HyperParameterTuningJobConfig' {..} =
    Prelude.rnf hyperParameterTuningJobObjective
      `Prelude.seq` Prelude.rnf parameterRanges
      `Prelude.seq` Prelude.rnf strategyConfig
      `Prelude.seq` Prelude.rnf trainingJobEarlyStoppingType
      `Prelude.seq` Prelude.rnf tuningJobCompletionCriteria
      `Prelude.seq` Prelude.rnf strategy
      `Prelude.seq` Prelude.rnf resourceLimits

instance Data.ToJSON HyperParameterTuningJobConfig where
  toJSON HyperParameterTuningJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HyperParameterTuningJobObjective" Data..=)
              Prelude.<$> hyperParameterTuningJobObjective,
            ("ParameterRanges" Data..=)
              Prelude.<$> parameterRanges,
            ("StrategyConfig" Data..=)
              Prelude.<$> strategyConfig,
            ("TrainingJobEarlyStoppingType" Data..=)
              Prelude.<$> trainingJobEarlyStoppingType,
            ("TuningJobCompletionCriteria" Data..=)
              Prelude.<$> tuningJobCompletionCriteria,
            Prelude.Just ("Strategy" Data..= strategy),
            Prelude.Just
              ("ResourceLimits" Data..= resourceLimits)
          ]
      )
