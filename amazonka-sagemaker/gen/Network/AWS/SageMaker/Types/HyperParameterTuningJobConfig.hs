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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
import Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType
import Network.AWS.SageMaker.Types.TuningJobCompletionCriteria

-- | Configures a hyperparameter tuning job.
--
-- /See:/ 'newHyperParameterTuningJobConfig' smart constructor.
data HyperParameterTuningJobConfig = HyperParameterTuningJobConfig'
  { -- | The HyperParameterTuningJobObjective object that specifies the objective
    -- metric for this tuning job.
    hyperParameterTuningJobObjective :: Core.Maybe HyperParameterTuningJobObjective,
    -- | The ParameterRanges object that specifies the ranges of hyperparameters
    -- that this tuning job searches.
    parameterRanges :: Core.Maybe ParameterRanges,
    -- | The tuning job\'s completion criteria.
    tuningJobCompletionCriteria :: Core.Maybe TuningJobCompletionCriteria,
    -- | Specifies whether to use early stopping for training jobs launched by
    -- the hyperparameter tuning job. This can be one of the following values
    -- (the default value is @OFF@):
    --
    -- [OFF]
    --     Training jobs launched by the hyperparameter tuning job do not use
    --     early stopping.
    --
    -- [AUTO]
    --     Amazon SageMaker stops training jobs launched by the hyperparameter
    --     tuning job when they are unlikely to perform better than previously
    --     completed training jobs. For more information, see
    --     <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early>.
    trainingJobEarlyStoppingType :: Core.Maybe TrainingJobEarlyStoppingType,
    -- | Specifies how hyperparameter tuning chooses the combinations of
    -- hyperparameter values to use for the training job it launches. To use
    -- the Bayesian search strategy, set this to @Bayesian@. To randomly
    -- search, set it to @Random@. For information about search strategies, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
    strategy :: HyperParameterTuningJobStrategyType,
    -- | The ResourceLimits object that specifies the maximum number of training
    -- jobs and parallel training jobs for this tuning job.
    resourceLimits :: ResourceLimits
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'tuningJobCompletionCriteria', 'hyperParameterTuningJobConfig_tuningJobCompletionCriteria' - The tuning job\'s completion criteria.
--
-- 'trainingJobEarlyStoppingType', 'hyperParameterTuningJobConfig_trainingJobEarlyStoppingType' - Specifies whether to use early stopping for training jobs launched by
-- the hyperparameter tuning job. This can be one of the following values
-- (the default value is @OFF@):
--
-- [OFF]
--     Training jobs launched by the hyperparameter tuning job do not use
--     early stopping.
--
-- [AUTO]
--     Amazon SageMaker stops training jobs launched by the hyperparameter
--     tuning job when they are unlikely to perform better than previously
--     completed training jobs. For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early>.
--
-- 'strategy', 'hyperParameterTuningJobConfig_strategy' - Specifies how hyperparameter tuning chooses the combinations of
-- hyperparameter values to use for the training job it launches. To use
-- the Bayesian search strategy, set this to @Bayesian@. To randomly
-- search, set it to @Random@. For information about search strategies, see
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
          Core.Nothing,
        parameterRanges = Core.Nothing,
        tuningJobCompletionCriteria = Core.Nothing,
        trainingJobEarlyStoppingType = Core.Nothing,
        strategy = pStrategy_,
        resourceLimits = pResourceLimits_
      }

-- | The HyperParameterTuningJobObjective object that specifies the objective
-- metric for this tuning job.
hyperParameterTuningJobConfig_hyperParameterTuningJobObjective :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe HyperParameterTuningJobObjective)
hyperParameterTuningJobConfig_hyperParameterTuningJobObjective = Lens.lens (\HyperParameterTuningJobConfig' {hyperParameterTuningJobObjective} -> hyperParameterTuningJobObjective) (\s@HyperParameterTuningJobConfig' {} a -> s {hyperParameterTuningJobObjective = a} :: HyperParameterTuningJobConfig)

-- | The ParameterRanges object that specifies the ranges of hyperparameters
-- that this tuning job searches.
hyperParameterTuningJobConfig_parameterRanges :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe ParameterRanges)
hyperParameterTuningJobConfig_parameterRanges = Lens.lens (\HyperParameterTuningJobConfig' {parameterRanges} -> parameterRanges) (\s@HyperParameterTuningJobConfig' {} a -> s {parameterRanges = a} :: HyperParameterTuningJobConfig)

-- | The tuning job\'s completion criteria.
hyperParameterTuningJobConfig_tuningJobCompletionCriteria :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe TuningJobCompletionCriteria)
hyperParameterTuningJobConfig_tuningJobCompletionCriteria = Lens.lens (\HyperParameterTuningJobConfig' {tuningJobCompletionCriteria} -> tuningJobCompletionCriteria) (\s@HyperParameterTuningJobConfig' {} a -> s {tuningJobCompletionCriteria = a} :: HyperParameterTuningJobConfig)

-- | Specifies whether to use early stopping for training jobs launched by
-- the hyperparameter tuning job. This can be one of the following values
-- (the default value is @OFF@):
--
-- [OFF]
--     Training jobs launched by the hyperparameter tuning job do not use
--     early stopping.
--
-- [AUTO]
--     Amazon SageMaker stops training jobs launched by the hyperparameter
--     tuning job when they are unlikely to perform better than previously
--     completed training jobs. For more information, see
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early>.
hyperParameterTuningJobConfig_trainingJobEarlyStoppingType :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe TrainingJobEarlyStoppingType)
hyperParameterTuningJobConfig_trainingJobEarlyStoppingType = Lens.lens (\HyperParameterTuningJobConfig' {trainingJobEarlyStoppingType} -> trainingJobEarlyStoppingType) (\s@HyperParameterTuningJobConfig' {} a -> s {trainingJobEarlyStoppingType = a} :: HyperParameterTuningJobConfig)

-- | Specifies how hyperparameter tuning chooses the combinations of
-- hyperparameter values to use for the training job it launches. To use
-- the Bayesian search strategy, set this to @Bayesian@. To randomly
-- search, set it to @Random@. For information about search strategies, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works>.
hyperParameterTuningJobConfig_strategy :: Lens.Lens' HyperParameterTuningJobConfig HyperParameterTuningJobStrategyType
hyperParameterTuningJobConfig_strategy = Lens.lens (\HyperParameterTuningJobConfig' {strategy} -> strategy) (\s@HyperParameterTuningJobConfig' {} a -> s {strategy = a} :: HyperParameterTuningJobConfig)

-- | The ResourceLimits object that specifies the maximum number of training
-- jobs and parallel training jobs for this tuning job.
hyperParameterTuningJobConfig_resourceLimits :: Lens.Lens' HyperParameterTuningJobConfig ResourceLimits
hyperParameterTuningJobConfig_resourceLimits = Lens.lens (\HyperParameterTuningJobConfig' {resourceLimits} -> resourceLimits) (\s@HyperParameterTuningJobConfig' {} a -> s {resourceLimits = a} :: HyperParameterTuningJobConfig)

instance Core.FromJSON HyperParameterTuningJobConfig where
  parseJSON =
    Core.withObject
      "HyperParameterTuningJobConfig"
      ( \x ->
          HyperParameterTuningJobConfig'
            Core.<$> (x Core..:? "HyperParameterTuningJobObjective")
            Core.<*> (x Core..:? "ParameterRanges")
            Core.<*> (x Core..:? "TuningJobCompletionCriteria")
            Core.<*> (x Core..:? "TrainingJobEarlyStoppingType")
            Core.<*> (x Core..: "Strategy")
            Core.<*> (x Core..: "ResourceLimits")
      )

instance Core.Hashable HyperParameterTuningJobConfig

instance Core.NFData HyperParameterTuningJobConfig

instance Core.ToJSON HyperParameterTuningJobConfig where
  toJSON HyperParameterTuningJobConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HyperParameterTuningJobObjective" Core..=)
              Core.<$> hyperParameterTuningJobObjective,
            ("ParameterRanges" Core..=) Core.<$> parameterRanges,
            ("TuningJobCompletionCriteria" Core..=)
              Core.<$> tuningJobCompletionCriteria,
            ("TrainingJobEarlyStoppingType" Core..=)
              Core.<$> trainingJobEarlyStoppingType,
            Core.Just ("Strategy" Core..= strategy),
            Core.Just ("ResourceLimits" Core..= resourceLimits)
          ]
      )
