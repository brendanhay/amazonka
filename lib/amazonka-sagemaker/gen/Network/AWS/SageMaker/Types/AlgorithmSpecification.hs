{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.AlgorithmSpecification
  ( AlgorithmSpecification (..)
  -- * Smart constructor
  , mkAlgorithmSpecification
  -- * Lenses
  , asTrainingInputMode
  , asAlgorithmName
  , asEnableSageMakerMetricsTimeSeries
  , asMetricDefinitions
  , asTrainingImage
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AlgorithmImage as Types
import qualified Network.AWS.SageMaker.Types.ArnOrName as Types
import qualified Network.AWS.SageMaker.Types.MetricDefinition as Types
import qualified Network.AWS.SageMaker.Types.TrainingInputMode as Types

-- | Specifies the training algorithm to use in a 'CreateTrainingJob' request.
--
-- For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about using your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> . 
--
-- /See:/ 'mkAlgorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { trainingInputMode :: Types.TrainingInputMode
    -- ^ The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container. 
--
-- In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any. 
-- For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training. 
  , algorithmName :: Core.Maybe Types.ArnOrName
    -- ^ The name of the algorithm resource to use for the training job. This must be an algorithm resource that you created or subscribe to on AWS Marketplace. If you specify a value for this parameter, you can't specify a value for @TrainingImage@ .
  , enableSageMakerMetricsTimeSeries :: Core.Maybe Core.Bool
    -- ^ To generate and save time-series metrics during training, set to @true@ . The default is @false@ and time-series metrics aren't generated except in the following cases:
--
--
--     * You use one of the Amazon SageMaker built-in algorithms
--
--
--     * You use one of the following <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images> :
--
--     * Tensorflow (version >= 1.15)
--
--
--     * MXNet (version >= 1.6)
--
--
--     * PyTorch (version >= 1.3)
--
--
--
--
--     * You specify at least one 'MetricDefinition' 
--
--
  , metricDefinitions :: Core.Maybe [Types.MetricDefinition]
    -- ^ A list of metric definition objects. Each object specifies the metric name and regular expressions used to parse algorithm logs. Amazon SageMaker publishes each metric to Amazon CloudWatch.
  , trainingImage :: Core.Maybe Types.AlgorithmImage
    -- ^ The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AlgorithmSpecification' value with any optional fields omitted.
mkAlgorithmSpecification
    :: Types.TrainingInputMode -- ^ 'trainingInputMode'
    -> AlgorithmSpecification
mkAlgorithmSpecification trainingInputMode
  = AlgorithmSpecification'{trainingInputMode,
                            algorithmName = Core.Nothing,
                            enableSageMakerMetricsTimeSeries = Core.Nothing,
                            metricDefinitions = Core.Nothing, trainingImage = Core.Nothing}

-- | The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container. 
--
-- In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any. 
-- For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training. 
--
-- /Note:/ Consider using 'trainingInputMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTrainingInputMode :: Lens.Lens' AlgorithmSpecification Types.TrainingInputMode
asTrainingInputMode = Lens.field @"trainingInputMode"
{-# INLINEABLE asTrainingInputMode #-}
{-# DEPRECATED trainingInputMode "Use generic-lens or generic-optics with 'trainingInputMode' instead"  #-}

-- | The name of the algorithm resource to use for the training job. This must be an algorithm resource that you created or subscribe to on AWS Marketplace. If you specify a value for this parameter, you can't specify a value for @TrainingImage@ .
--
-- /Note:/ Consider using 'algorithmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAlgorithmName :: Lens.Lens' AlgorithmSpecification (Core.Maybe Types.ArnOrName)
asAlgorithmName = Lens.field @"algorithmName"
{-# INLINEABLE asAlgorithmName #-}
{-# DEPRECATED algorithmName "Use generic-lens or generic-optics with 'algorithmName' instead"  #-}

-- | To generate and save time-series metrics during training, set to @true@ . The default is @false@ and time-series metrics aren't generated except in the following cases:
--
--
--     * You use one of the Amazon SageMaker built-in algorithms
--
--
--     * You use one of the following <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images> :
--
--     * Tensorflow (version >= 1.15)
--
--
--     * MXNet (version >= 1.6)
--
--
--     * PyTorch (version >= 1.3)
--
--
--
--
--     * You specify at least one 'MetricDefinition' 
--
--
--
-- /Note:/ Consider using 'enableSageMakerMetricsTimeSeries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asEnableSageMakerMetricsTimeSeries :: Lens.Lens' AlgorithmSpecification (Core.Maybe Core.Bool)
asEnableSageMakerMetricsTimeSeries = Lens.field @"enableSageMakerMetricsTimeSeries"
{-# INLINEABLE asEnableSageMakerMetricsTimeSeries #-}
{-# DEPRECATED enableSageMakerMetricsTimeSeries "Use generic-lens or generic-optics with 'enableSageMakerMetricsTimeSeries' instead"  #-}

-- | A list of metric definition objects. Each object specifies the metric name and regular expressions used to parse algorithm logs. Amazon SageMaker publishes each metric to Amazon CloudWatch.
--
-- /Note:/ Consider using 'metricDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asMetricDefinitions :: Lens.Lens' AlgorithmSpecification (Core.Maybe [Types.MetricDefinition])
asMetricDefinitions = Lens.field @"metricDefinitions"
{-# INLINEABLE asMetricDefinitions #-}
{-# DEPRECATED metricDefinitions "Use generic-lens or generic-optics with 'metricDefinitions' instead"  #-}

-- | The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- /Note:/ Consider using 'trainingImage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTrainingImage :: Lens.Lens' AlgorithmSpecification (Core.Maybe Types.AlgorithmImage)
asTrainingImage = Lens.field @"trainingImage"
{-# INLINEABLE asTrainingImage #-}
{-# DEPRECATED trainingImage "Use generic-lens or generic-optics with 'trainingImage' instead"  #-}

instance Core.FromJSON AlgorithmSpecification where
        toJSON AlgorithmSpecification{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TrainingInputMode" Core..= trainingInputMode),
                  ("AlgorithmName" Core..=) Core.<$> algorithmName,
                  ("EnableSageMakerMetricsTimeSeries" Core..=) Core.<$>
                    enableSageMakerMetricsTimeSeries,
                  ("MetricDefinitions" Core..=) Core.<$> metricDefinitions,
                  ("TrainingImage" Core..=) Core.<$> trainingImage])

instance Core.FromJSON AlgorithmSpecification where
        parseJSON
          = Core.withObject "AlgorithmSpecification" Core.$
              \ x ->
                AlgorithmSpecification' Core.<$>
                  (x Core..: "TrainingInputMode") Core.<*> x Core..:? "AlgorithmName"
                    Core.<*> x Core..:? "EnableSageMakerMetricsTimeSeries"
                    Core.<*> x Core..:? "MetricDefinitions"
                    Core.<*> x Core..:? "TrainingImage"
