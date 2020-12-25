{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrainingJobDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobDefinition
  ( TrainingJobDefinition (..),

    -- * Smart constructor
    mkTrainingJobDefinition,

    -- * Lenses
    tjdTrainingInputMode,
    tjdInputDataConfig,
    tjdOutputDataConfig,
    tjdResourceConfig,
    tjdStoppingCondition,
    tjdHyperParameters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Channel as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterKey as Types
import qualified Network.AWS.SageMaker.Types.HyperParameterValue as Types
import qualified Network.AWS.SageMaker.Types.OutputDataConfig as Types
import qualified Network.AWS.SageMaker.Types.ResourceConfig as Types
import qualified Network.AWS.SageMaker.Types.StoppingCondition as Types
import qualified Network.AWS.SageMaker.Types.TrainingInputMode as Types

-- | Defines the input needed to run a training job using the algorithm.
--
-- /See:/ 'mkTrainingJobDefinition' smart constructor.
data TrainingJobDefinition = TrainingJobDefinition'
  { -- | The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
    --
    -- If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
    trainingInputMode :: Types.TrainingInputMode,
    -- | An array of @Channel@ objects, each of which specifies an input source.
    inputDataConfig :: Core.NonEmpty Types.Channel,
    -- | the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
    outputDataConfig :: Types.OutputDataConfig,
    -- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
    resourceConfig :: Types.ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts.
    stoppingCondition :: Types.StoppingCondition,
    -- | The hyperparameters used for the training job.
    hyperParameters :: Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrainingJobDefinition' value with any optional fields omitted.
mkTrainingJobDefinition ::
  -- | 'trainingInputMode'
  Types.TrainingInputMode ->
  -- | 'inputDataConfig'
  Core.NonEmpty Types.Channel ->
  -- | 'outputDataConfig'
  Types.OutputDataConfig ->
  -- | 'resourceConfig'
  Types.ResourceConfig ->
  -- | 'stoppingCondition'
  Types.StoppingCondition ->
  TrainingJobDefinition
mkTrainingJobDefinition
  trainingInputMode
  inputDataConfig
  outputDataConfig
  resourceConfig
  stoppingCondition =
    TrainingJobDefinition'
      { trainingInputMode,
        inputDataConfig,
        outputDataConfig,
        resourceConfig,
        stoppingCondition,
        hyperParameters = Core.Nothing
      }

-- | The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
--
-- /Note:/ Consider using 'trainingInputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTrainingInputMode :: Lens.Lens' TrainingJobDefinition Types.TrainingInputMode
tjdTrainingInputMode = Lens.field @"trainingInputMode"
{-# DEPRECATED tjdTrainingInputMode "Use generic-lens or generic-optics with 'trainingInputMode' instead." #-}

-- | An array of @Channel@ objects, each of which specifies an input source.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdInputDataConfig :: Lens.Lens' TrainingJobDefinition (Core.NonEmpty Types.Channel)
tjdInputDataConfig = Lens.field @"inputDataConfig"
{-# DEPRECATED tjdInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdOutputDataConfig :: Lens.Lens' TrainingJobDefinition Types.OutputDataConfig
tjdOutputDataConfig = Lens.field @"outputDataConfig"
{-# DEPRECATED tjdOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdResourceConfig :: Lens.Lens' TrainingJobDefinition Types.ResourceConfig
tjdResourceConfig = Lens.field @"resourceConfig"
{-# DEPRECATED tjdResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdStoppingCondition :: Lens.Lens' TrainingJobDefinition Types.StoppingCondition
tjdStoppingCondition = Lens.field @"stoppingCondition"
{-# DEPRECATED tjdStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

-- | The hyperparameters used for the training job.
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdHyperParameters :: Lens.Lens' TrainingJobDefinition (Core.Maybe (Core.HashMap Types.HyperParameterKey Types.HyperParameterValue))
tjdHyperParameters = Lens.field @"hyperParameters"
{-# DEPRECATED tjdHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

instance Core.FromJSON TrainingJobDefinition where
  toJSON TrainingJobDefinition {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TrainingInputMode" Core..= trainingInputMode),
            Core.Just ("InputDataConfig" Core..= inputDataConfig),
            Core.Just ("OutputDataConfig" Core..= outputDataConfig),
            Core.Just ("ResourceConfig" Core..= resourceConfig),
            Core.Just ("StoppingCondition" Core..= stoppingCondition),
            ("HyperParameters" Core..=) Core.<$> hyperParameters
          ]
      )

instance Core.FromJSON TrainingJobDefinition where
  parseJSON =
    Core.withObject "TrainingJobDefinition" Core.$
      \x ->
        TrainingJobDefinition'
          Core.<$> (x Core..: "TrainingInputMode")
          Core.<*> (x Core..: "InputDataConfig")
          Core.<*> (x Core..: "OutputDataConfig")
          Core.<*> (x Core..: "ResourceConfig")
          Core.<*> (x Core..: "StoppingCondition")
          Core.<*> (x Core..:? "HyperParameters")
