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
    tjdHyperParameters,
    tjdTrainingInputMode,
    tjdInputDataConfig,
    tjdOutputDataConfig,
    tjdResourceConfig,
    tjdStoppingCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Defines the input needed to run a training job using the algorithm.
--
-- /See:/ 'mkTrainingJobDefinition' smart constructor.
data TrainingJobDefinition = TrainingJobDefinition'
  { hyperParameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    trainingInputMode :: TrainingInputMode,
    inputDataConfig :: Lude.NonEmpty Channel,
    outputDataConfig :: OutputDataConfig,
    resourceConfig :: ResourceConfig,
    stoppingCondition :: StoppingCondition
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrainingJobDefinition' with the minimum fields required to make a request.
--
-- * 'hyperParameters' - The hyperparameters used for the training job.
-- * 'inputDataConfig' - An array of @Channel@ objects, each of which specifies an input source.
-- * 'outputDataConfig' - the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
-- * 'resourceConfig' - The resources, including the ML compute instances and ML storage volumes, to use for model training.
-- * 'stoppingCondition' - Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts.
-- * 'trainingInputMode' - The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
mkTrainingJobDefinition ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  -- | 'inputDataConfig'
  Lude.NonEmpty Channel ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  TrainingJobDefinition
mkTrainingJobDefinition
  pTrainingInputMode_
  pInputDataConfig_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    TrainingJobDefinition'
      { hyperParameters = Lude.Nothing,
        trainingInputMode = pTrainingInputMode_,
        inputDataConfig = pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | The hyperparameters used for the training job.
--
-- /Note:/ Consider using 'hyperParameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdHyperParameters :: Lens.Lens' TrainingJobDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
tjdHyperParameters = Lens.lens (hyperParameters :: TrainingJobDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {hyperParameters = a} :: TrainingJobDefinition)
{-# DEPRECATED tjdHyperParameters "Use generic-lens or generic-optics with 'hyperParameters' instead." #-}

-- | The input mode used by the algorithm for the training job. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.
--
-- /Note:/ Consider using 'trainingInputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdTrainingInputMode :: Lens.Lens' TrainingJobDefinition TrainingInputMode
tjdTrainingInputMode = Lens.lens (trainingInputMode :: TrainingJobDefinition -> TrainingInputMode) (\s a -> s {trainingInputMode = a} :: TrainingJobDefinition)
{-# DEPRECATED tjdTrainingInputMode "Use generic-lens or generic-optics with 'trainingInputMode' instead." #-}

-- | An array of @Channel@ objects, each of which specifies an input source.
--
-- /Note:/ Consider using 'inputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdInputDataConfig :: Lens.Lens' TrainingJobDefinition (Lude.NonEmpty Channel)
tjdInputDataConfig = Lens.lens (inputDataConfig :: TrainingJobDefinition -> Lude.NonEmpty Channel) (\s a -> s {inputDataConfig = a} :: TrainingJobDefinition)
{-# DEPRECATED tjdInputDataConfig "Use generic-lens or generic-optics with 'inputDataConfig' instead." #-}

-- | the path to the S3 bucket where you want to store model artifacts. Amazon SageMaker creates subfolders for the artifacts.
--
-- /Note:/ Consider using 'outputDataConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdOutputDataConfig :: Lens.Lens' TrainingJobDefinition OutputDataConfig
tjdOutputDataConfig = Lens.lens (outputDataConfig :: TrainingJobDefinition -> OutputDataConfig) (\s a -> s {outputDataConfig = a} :: TrainingJobDefinition)
{-# DEPRECATED tjdOutputDataConfig "Use generic-lens or generic-optics with 'outputDataConfig' instead." #-}

-- | The resources, including the ML compute instances and ML storage volumes, to use for model training.
--
-- /Note:/ Consider using 'resourceConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdResourceConfig :: Lens.Lens' TrainingJobDefinition ResourceConfig
tjdResourceConfig = Lens.lens (resourceConfig :: TrainingJobDefinition -> ResourceConfig) (\s a -> s {resourceConfig = a} :: TrainingJobDefinition)
{-# DEPRECATED tjdResourceConfig "Use generic-lens or generic-optics with 'resourceConfig' instead." #-}

-- | Specifies a limit to how long a model training job can run. When the job reaches the time limit, Amazon SageMaker ends the training job. Use this API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal, which delays job termination for 120 seconds. Algorithms can use this 120-second window to save the model artifacts.
--
-- /Note:/ Consider using 'stoppingCondition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tjdStoppingCondition :: Lens.Lens' TrainingJobDefinition StoppingCondition
tjdStoppingCondition = Lens.lens (stoppingCondition :: TrainingJobDefinition -> StoppingCondition) (\s a -> s {stoppingCondition = a} :: TrainingJobDefinition)
{-# DEPRECATED tjdStoppingCondition "Use generic-lens or generic-optics with 'stoppingCondition' instead." #-}

instance Lude.FromJSON TrainingJobDefinition where
  parseJSON =
    Lude.withObject
      "TrainingJobDefinition"
      ( \x ->
          TrainingJobDefinition'
            Lude.<$> (x Lude..:? "HyperParameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "TrainingInputMode")
            Lude.<*> (x Lude..: "InputDataConfig")
            Lude.<*> (x Lude..: "OutputDataConfig")
            Lude.<*> (x Lude..: "ResourceConfig")
            Lude.<*> (x Lude..: "StoppingCondition")
      )

instance Lude.ToJSON TrainingJobDefinition where
  toJSON TrainingJobDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HyperParameters" Lude..=) Lude.<$> hyperParameters,
            Lude.Just ("TrainingInputMode" Lude..= trainingInputMode),
            Lude.Just ("InputDataConfig" Lude..= inputDataConfig),
            Lude.Just ("OutputDataConfig" Lude..= outputDataConfig),
            Lude.Just ("ResourceConfig" Lude..= resourceConfig),
            Lude.Just ("StoppingCondition" Lude..= stoppingCondition)
          ]
      )
