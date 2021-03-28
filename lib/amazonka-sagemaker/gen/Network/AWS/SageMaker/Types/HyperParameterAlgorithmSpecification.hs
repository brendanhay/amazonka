{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
  ( HyperParameterAlgorithmSpecification (..)
  -- * Smart constructor
  , mkHyperParameterAlgorithmSpecification
  -- * Lenses
  , hpasTrainingInputMode
  , hpasAlgorithmName
  , hpasMetricDefinitions
  , hpasTrainingImage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.ArnOrName as Types
import qualified Network.AWS.SageMaker.Types.MetricDefinition as Types
import qualified Network.AWS.SageMaker.Types.TrainingImage as Types
import qualified Network.AWS.SageMaker.Types.TrainingInputMode as Types

-- | Specifies which training algorithm to use for training jobs that a hyperparameter tuning job launches and the metrics to monitor.
--
-- /See:/ 'mkHyperParameterAlgorithmSpecification' smart constructor.
data HyperParameterAlgorithmSpecification = HyperParameterAlgorithmSpecification'
  { trainingInputMode :: Types.TrainingInputMode
    -- ^ The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container. 
--
-- If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information.
--
-- For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . 
  , algorithmName :: Core.Maybe Types.ArnOrName
    -- ^ The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
  , metricDefinitions :: Core.Maybe [Types.MetricDefinition]
    -- ^ An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
  , trainingImage :: Core.Maybe Types.TrainingImage
    -- ^ The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HyperParameterAlgorithmSpecification' value with any optional fields omitted.
mkHyperParameterAlgorithmSpecification
    :: Types.TrainingInputMode -- ^ 'trainingInputMode'
    -> HyperParameterAlgorithmSpecification
mkHyperParameterAlgorithmSpecification trainingInputMode
  = HyperParameterAlgorithmSpecification'{trainingInputMode,
                                          algorithmName = Core.Nothing,
                                          metricDefinitions = Core.Nothing,
                                          trainingImage = Core.Nothing}

-- | The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container. 
--
-- If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information.
--
-- For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . 
--
-- /Note:/ Consider using 'trainingInputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasTrainingInputMode :: Lens.Lens' HyperParameterAlgorithmSpecification Types.TrainingInputMode
hpasTrainingInputMode = Lens.field @"trainingInputMode"
{-# INLINEABLE hpasTrainingInputMode #-}
{-# DEPRECATED trainingInputMode "Use generic-lens or generic-optics with 'trainingInputMode' instead"  #-}

-- | The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasAlgorithmName :: Lens.Lens' HyperParameterAlgorithmSpecification (Core.Maybe Types.ArnOrName)
hpasAlgorithmName = Lens.field @"algorithmName"
{-# INLINEABLE hpasAlgorithmName #-}
{-# DEPRECATED algorithmName "Use generic-lens or generic-optics with 'algorithmName' instead"  #-}

-- | An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
--
-- /Note:/ Consider using 'metricDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasMetricDefinitions :: Lens.Lens' HyperParameterAlgorithmSpecification (Core.Maybe [Types.MetricDefinition])
hpasMetricDefinitions = Lens.field @"metricDefinitions"
{-# INLINEABLE hpasMetricDefinitions #-}
{-# DEPRECATED metricDefinitions "Use generic-lens or generic-optics with 'metricDefinitions' instead"  #-}

-- | The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'trainingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hpasTrainingImage :: Lens.Lens' HyperParameterAlgorithmSpecification (Core.Maybe Types.TrainingImage)
hpasTrainingImage = Lens.field @"trainingImage"
{-# INLINEABLE hpasTrainingImage #-}
{-# DEPRECATED trainingImage "Use generic-lens or generic-optics with 'trainingImage' instead"  #-}

instance Core.FromJSON HyperParameterAlgorithmSpecification where
        toJSON HyperParameterAlgorithmSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrainingInputMode" Core..= trainingInputMode),
                  ("AlgorithmName" Core..=) Core.<$> algorithmName,
                  ("MetricDefinitions" Core..=) Core.<$> metricDefinitions,
                  ("TrainingImage" Core..=) Core.<$> trainingImage])

instance Core.FromJSON HyperParameterAlgorithmSpecification where
        parseJSON
          = Core.withObject "HyperParameterAlgorithmSpecification" Core.$
              \ x ->
                HyperParameterAlgorithmSpecification' Core.<$>
                  (x Core..: "TrainingInputMode") Core.<*> x Core..:? "AlgorithmName"
                    Core.<*> x Core..:? "MetricDefinitions"
                    Core.<*> x Core..:? "TrainingImage"
