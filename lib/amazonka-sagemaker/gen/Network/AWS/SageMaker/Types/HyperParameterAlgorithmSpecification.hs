-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
  ( HyperParameterAlgorithmSpecification (..),

    -- * Smart constructor
    mkHyperParameterAlgorithmSpecification,

    -- * Lenses
    hpasAlgorithmName,
    hpasTrainingImage,
    hpasMetricDefinitions,
    hpasTrainingInputMode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Specifies which training algorithm to use for training jobs that a hyperparameter tuning job launches and the metrics to monitor.
--
-- /See:/ 'mkHyperParameterAlgorithmSpecification' smart constructor.
data HyperParameterAlgorithmSpecification = HyperParameterAlgorithmSpecification'
  { algorithmName ::
      Lude.Maybe
        Lude.Text,
    trainingImage ::
      Lude.Maybe
        Lude.Text,
    metricDefinitions ::
      Lude.Maybe
        [MetricDefinition],
    trainingInputMode ::
      TrainingInputMode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HyperParameterAlgorithmSpecification' with the minimum fields required to make a request.
--
-- * 'algorithmName' - The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
-- * 'metricDefinitions' - An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
-- * 'trainingImage' - The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
-- * 'trainingInputMode' - The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container.
--
-- If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information.
--
-- For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
mkHyperParameterAlgorithmSpecification ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  HyperParameterAlgorithmSpecification
mkHyperParameterAlgorithmSpecification pTrainingInputMode_ =
  HyperParameterAlgorithmSpecification'
    { algorithmName =
        Lude.Nothing,
      trainingImage = Lude.Nothing,
      metricDefinitions = Lude.Nothing,
      trainingInputMode = pTrainingInputMode_
    }

-- | The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasAlgorithmName :: Lens.Lens' HyperParameterAlgorithmSpecification (Lude.Maybe Lude.Text)
hpasAlgorithmName = Lens.lens (algorithmName :: HyperParameterAlgorithmSpecification -> Lude.Maybe Lude.Text) (\s a -> s {algorithmName = a} :: HyperParameterAlgorithmSpecification)
{-# DEPRECATED hpasAlgorithmName "Use generic-lens or generic-optics with 'algorithmName' instead." #-}

-- | The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'trainingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasTrainingImage :: Lens.Lens' HyperParameterAlgorithmSpecification (Lude.Maybe Lude.Text)
hpasTrainingImage = Lens.lens (trainingImage :: HyperParameterAlgorithmSpecification -> Lude.Maybe Lude.Text) (\s a -> s {trainingImage = a} :: HyperParameterAlgorithmSpecification)
{-# DEPRECATED hpasTrainingImage "Use generic-lens or generic-optics with 'trainingImage' instead." #-}

-- | An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
--
-- /Note:/ Consider using 'metricDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasMetricDefinitions :: Lens.Lens' HyperParameterAlgorithmSpecification (Lude.Maybe [MetricDefinition])
hpasMetricDefinitions = Lens.lens (metricDefinitions :: HyperParameterAlgorithmSpecification -> Lude.Maybe [MetricDefinition]) (\s a -> s {metricDefinitions = a} :: HyperParameterAlgorithmSpecification)
{-# DEPRECATED hpasMetricDefinitions "Use generic-lens or generic-optics with 'metricDefinitions' instead." #-}

-- | The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container.
--
-- If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information.
--
-- For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
--
-- /Note:/ Consider using 'trainingInputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasTrainingInputMode :: Lens.Lens' HyperParameterAlgorithmSpecification TrainingInputMode
hpasTrainingInputMode = Lens.lens (trainingInputMode :: HyperParameterAlgorithmSpecification -> TrainingInputMode) (\s a -> s {trainingInputMode = a} :: HyperParameterAlgorithmSpecification)
{-# DEPRECATED hpasTrainingInputMode "Use generic-lens or generic-optics with 'trainingInputMode' instead." #-}

instance Lude.FromJSON HyperParameterAlgorithmSpecification where
  parseJSON =
    Lude.withObject
      "HyperParameterAlgorithmSpecification"
      ( \x ->
          HyperParameterAlgorithmSpecification'
            Lude.<$> (x Lude..:? "AlgorithmName")
            Lude.<*> (x Lude..:? "TrainingImage")
            Lude.<*> (x Lude..:? "MetricDefinitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "TrainingInputMode")
      )

instance Lude.ToJSON HyperParameterAlgorithmSpecification where
  toJSON HyperParameterAlgorithmSpecification' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AlgorithmName" Lude..=) Lude.<$> algorithmName,
            ("TrainingImage" Lude..=) Lude.<$> trainingImage,
            ("MetricDefinitions" Lude..=) Lude.<$> metricDefinitions,
            Lude.Just ("TrainingInputMode" Lude..= trainingInputMode)
          ]
      )
