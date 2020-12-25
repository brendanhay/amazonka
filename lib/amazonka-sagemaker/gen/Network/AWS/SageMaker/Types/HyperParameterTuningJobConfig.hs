{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
  ( HyperParameterTuningJobConfig (..),

    -- * Smart constructor
    mkHyperParameterTuningJobConfig,

    -- * Lenses
    hptjcStrategy,
    hptjcResourceLimits,
    hptjcHyperParameterTuningJobObjective,
    hptjcParameterRanges,
    hptjcTrainingJobEarlyStoppingType,
    hptjcTuningJobCompletionCriteria,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterTuningJobStrategyType as Types
import qualified Network.AWS.SageMaker.Types.ParameterRanges as Types
import qualified Network.AWS.SageMaker.Types.ResourceLimits as Types
import qualified Network.AWS.SageMaker.Types.TrainingJobEarlyStoppingType as Types
import qualified Network.AWS.SageMaker.Types.TuningJobCompletionCriteria as Types

-- | Configures a hyperparameter tuning job.
--
-- /See:/ 'mkHyperParameterTuningJobConfig' smart constructor.
data HyperParameterTuningJobConfig = HyperParameterTuningJobConfig'
  { -- | Specifies how hyperparameter tuning chooses the combinations of hyperparameter values to use for the training job it launches. To use the Bayesian search strategy, set this to @Bayesian@ . To randomly search, set it to @Random@ . For information about search strategies, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
    strategy :: Types.HyperParameterTuningJobStrategyType,
    -- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs for this tuning job.
    resourceLimits :: Types.ResourceLimits,
    -- | The 'HyperParameterTuningJobObjective' object that specifies the objective metric for this tuning job.
    hyperParameterTuningJobObjective :: Core.Maybe Types.HyperParameterTuningJobObjective,
    -- | The 'ParameterRanges' object that specifies the ranges of hyperparameters that this tuning job searches.
    parameterRanges :: Core.Maybe Types.ParameterRanges,
    -- | Specifies whether to use early stopping for training jobs launched by the hyperparameter tuning job. This can be one of the following values (the default value is @OFF@ ):
    --
    --
    --     * OFF
    --
    --     * Training jobs launched by the hyperparameter tuning job do not use early stopping.
    --
    --
    --     * AUTO
    --
    --     * Amazon SageMaker stops training jobs launched by the hyperparameter tuning job when they are unlikely to perform better than previously completed training jobs. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early> .
    trainingJobEarlyStoppingType :: Core.Maybe Types.TrainingJobEarlyStoppingType,
    -- | The tuning job's completion criteria.
    tuningJobCompletionCriteria :: Core.Maybe Types.TuningJobCompletionCriteria
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterTuningJobConfig' value with any optional fields omitted.
mkHyperParameterTuningJobConfig ::
  -- | 'strategy'
  Types.HyperParameterTuningJobStrategyType ->
  -- | 'resourceLimits'
  Types.ResourceLimits ->
  HyperParameterTuningJobConfig
mkHyperParameterTuningJobConfig strategy resourceLimits =
  HyperParameterTuningJobConfig'
    { strategy,
      resourceLimits,
      hyperParameterTuningJobObjective = Core.Nothing,
      parameterRanges = Core.Nothing,
      trainingJobEarlyStoppingType = Core.Nothing,
      tuningJobCompletionCriteria = Core.Nothing
    }

-- | Specifies how hyperparameter tuning chooses the combinations of hyperparameter values to use for the training job it launches. To use the Bayesian search strategy, set this to @Bayesian@ . To randomly search, set it to @Random@ . For information about search strategies, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-how-it-works.html How Hyperparameter Tuning Works> .
--
-- /Note:/ Consider using 'strategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjcStrategy :: Lens.Lens' HyperParameterTuningJobConfig Types.HyperParameterTuningJobStrategyType
hptjcStrategy = Lens.field @"strategy"
{-# DEPRECATED hptjcStrategy "Use generic-lens or generic-optics with 'strategy' instead." #-}

-- | The 'ResourceLimits' object that specifies the maximum number of training jobs and parallel training jobs for this tuning job.
--
-- /Note:/ Consider using 'resourceLimits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjcResourceLimits :: Lens.Lens' HyperParameterTuningJobConfig Types.ResourceLimits
hptjcResourceLimits = Lens.field @"resourceLimits"
{-# DEPRECATED hptjcResourceLimits "Use generic-lens or generic-optics with 'resourceLimits' instead." #-}

-- | The 'HyperParameterTuningJobObjective' object that specifies the objective metric for this tuning job.
--
-- /Note:/ Consider using 'hyperParameterTuningJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjcHyperParameterTuningJobObjective :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe Types.HyperParameterTuningJobObjective)
hptjcHyperParameterTuningJobObjective = Lens.field @"hyperParameterTuningJobObjective"
{-# DEPRECATED hptjcHyperParameterTuningJobObjective "Use generic-lens or generic-optics with 'hyperParameterTuningJobObjective' instead." #-}

-- | The 'ParameterRanges' object that specifies the ranges of hyperparameters that this tuning job searches.
--
-- /Note:/ Consider using 'parameterRanges' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjcParameterRanges :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe Types.ParameterRanges)
hptjcParameterRanges = Lens.field @"parameterRanges"
{-# DEPRECATED hptjcParameterRanges "Use generic-lens or generic-optics with 'parameterRanges' instead." #-}

-- | Specifies whether to use early stopping for training jobs launched by the hyperparameter tuning job. This can be one of the following values (the default value is @OFF@ ):
--
--
--     * OFF
--
--     * Training jobs launched by the hyperparameter tuning job do not use early stopping.
--
--
--     * AUTO
--
--     * Amazon SageMaker stops training jobs launched by the hyperparameter tuning job when they are unlikely to perform better than previously completed training jobs. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/automatic-model-tuning-early-stopping.html Stop Training Jobs Early> .
--
--
--
-- /Note:/ Consider using 'trainingJobEarlyStoppingType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjcTrainingJobEarlyStoppingType :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe Types.TrainingJobEarlyStoppingType)
hptjcTrainingJobEarlyStoppingType = Lens.field @"trainingJobEarlyStoppingType"
{-# DEPRECATED hptjcTrainingJobEarlyStoppingType "Use generic-lens or generic-optics with 'trainingJobEarlyStoppingType' instead." #-}

-- | The tuning job's completion criteria.
--
-- /Note:/ Consider using 'tuningJobCompletionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hptjcTuningJobCompletionCriteria :: Lens.Lens' HyperParameterTuningJobConfig (Core.Maybe Types.TuningJobCompletionCriteria)
hptjcTuningJobCompletionCriteria = Lens.field @"tuningJobCompletionCriteria"
{-# DEPRECATED hptjcTuningJobCompletionCriteria "Use generic-lens or generic-optics with 'tuningJobCompletionCriteria' instead." #-}

instance Core.FromJSON HyperParameterTuningJobConfig where
  toJSON HyperParameterTuningJobConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Strategy" Core..= strategy),
            Core.Just ("ResourceLimits" Core..= resourceLimits),
            ("HyperParameterTuningJobObjective" Core..=)
              Core.<$> hyperParameterTuningJobObjective,
            ("ParameterRanges" Core..=) Core.<$> parameterRanges,
            ("TrainingJobEarlyStoppingType" Core..=)
              Core.<$> trainingJobEarlyStoppingType,
            ("TuningJobCompletionCriteria" Core..=)
              Core.<$> tuningJobCompletionCriteria
          ]
      )

instance Core.FromJSON HyperParameterTuningJobConfig where
  parseJSON =
    Core.withObject "HyperParameterTuningJobConfig" Core.$
      \x ->
        HyperParameterTuningJobConfig'
          Core.<$> (x Core..: "Strategy")
          Core.<*> (x Core..: "ResourceLimits")
          Core.<*> (x Core..:? "HyperParameterTuningJobObjective")
          Core.<*> (x Core..:? "ParameterRanges")
          Core.<*> (x Core..:? "TrainingJobEarlyStoppingType")
          Core.<*> (x Core..:? "TuningJobCompletionCriteria")
