{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Specifies the training algorithm to use in a 'CreateTrainingJob' request.
--
--
-- For more information about algorithms provided by Amazon SageMaker, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . For information about using your own algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
--
-- /See:/ 'algorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { _asEnableSageMakerMetricsTimeSeries ::
      !(Maybe Bool),
    _asAlgorithmName :: !(Maybe Text),
    _asTrainingImage :: !(Maybe Text),
    _asMetricDefinitions ::
      !(Maybe [MetricDefinition]),
    _asTrainingInputMode :: !TrainingInputMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asEnableSageMakerMetricsTimeSeries' - To generate and save time-series metrics during training, set to @true@ . The default is @false@ and time-series metrics aren't generated except in the following cases:     * You use one of the Amazon SageMaker built-in algorithms     * You use one of the following <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images> :     * Tensorflow (version >= 1.15)     * MXNet (version >= 1.6)     * PyTorch (version >= 1.3)     * You specify at least one 'MetricDefinition'
--
-- * 'asAlgorithmName' - The name of the algorithm resource to use for the training job. This must be an algorithm resource that you created or subscribe to on AWS Marketplace. If you specify a value for this parameter, you can't specify a value for @TrainingImage@ .
--
-- * 'asTrainingImage' - The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- * 'asMetricDefinitions' - A list of metric definition objects. Each object specifies the metric name and regular expressions used to parse algorithm logs. Amazon SageMaker publishes each metric to Amazon CloudWatch.
--
-- * 'asTrainingInputMode' - The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.  In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any.  For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training.
algorithmSpecification ::
  -- | 'asTrainingInputMode'
  TrainingInputMode ->
  AlgorithmSpecification
algorithmSpecification pTrainingInputMode_ =
  AlgorithmSpecification'
    { _asEnableSageMakerMetricsTimeSeries =
        Nothing,
      _asAlgorithmName = Nothing,
      _asTrainingImage = Nothing,
      _asMetricDefinitions = Nothing,
      _asTrainingInputMode = pTrainingInputMode_
    }

-- | To generate and save time-series metrics during training, set to @true@ . The default is @false@ and time-series metrics aren't generated except in the following cases:     * You use one of the Amazon SageMaker built-in algorithms     * You use one of the following <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images> :     * Tensorflow (version >= 1.15)     * MXNet (version >= 1.6)     * PyTorch (version >= 1.3)     * You specify at least one 'MetricDefinition'
asEnableSageMakerMetricsTimeSeries :: Lens' AlgorithmSpecification (Maybe Bool)
asEnableSageMakerMetricsTimeSeries = lens _asEnableSageMakerMetricsTimeSeries (\s a -> s {_asEnableSageMakerMetricsTimeSeries = a})

-- | The name of the algorithm resource to use for the training job. This must be an algorithm resource that you created or subscribe to on AWS Marketplace. If you specify a value for this parameter, you can't specify a value for @TrainingImage@ .
asAlgorithmName :: Lens' AlgorithmSpecification (Maybe Text)
asAlgorithmName = lens _asAlgorithmName (\s a -> s {_asAlgorithmName = a})

-- | The registry path of the Docker image that contains the training algorithm. For information about docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
asTrainingImage :: Lens' AlgorithmSpecification (Maybe Text)
asTrainingImage = lens _asTrainingImage (\s a -> s {_asTrainingImage = a})

-- | A list of metric definition objects. Each object specifies the metric name and regular expressions used to parse algorithm logs. Amazon SageMaker publishes each metric to Amazon CloudWatch.
asMetricDefinitions :: Lens' AlgorithmSpecification [MetricDefinition]
asMetricDefinitions = lens _asMetricDefinitions (\s a -> s {_asMetricDefinitions = a}) . _Default . _Coerce

-- | The input mode that the algorithm supports. For the input modes that Amazon SageMaker algorithms support, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> . If an algorithm supports the @File@ input mode, Amazon SageMaker downloads the training data from S3 to the provisioned ML storage Volume, and mounts the directory to docker volume for training container. If an algorithm supports the @Pipe@ input mode, Amazon SageMaker streams data directly from S3 to the container.  In File mode, make sure you provision ML storage volume with sufficient capacity to accommodate the data download from S3. In addition to the training data, the ML storage volume also stores the output model. The algorithm container use ML storage volume to also store intermediate information, if any.  For distributed algorithms using File mode, training data is distributed uniformly, and your training duration is predictable if the input data objects size is approximately same. Amazon SageMaker does not split the files any further for model training. If the object sizes are skewed, training won't be optimal as the data distribution is also skewed where one host in a training cluster is overloaded, thus becoming bottleneck in training.
asTrainingInputMode :: Lens' AlgorithmSpecification TrainingInputMode
asTrainingInputMode = lens _asTrainingInputMode (\s a -> s {_asTrainingInputMode = a})

instance FromJSON AlgorithmSpecification where
  parseJSON =
    withObject
      "AlgorithmSpecification"
      ( \x ->
          AlgorithmSpecification'
            <$> (x .:? "EnableSageMakerMetricsTimeSeries")
            <*> (x .:? "AlgorithmName")
            <*> (x .:? "TrainingImage")
            <*> (x .:? "MetricDefinitions" .!= mempty)
            <*> (x .: "TrainingInputMode")
      )

instance Hashable AlgorithmSpecification

instance NFData AlgorithmSpecification

instance ToJSON AlgorithmSpecification where
  toJSON AlgorithmSpecification' {..} =
    object
      ( catMaybes
          [ ("EnableSageMakerMetricsTimeSeries" .=)
              <$> _asEnableSageMakerMetricsTimeSeries,
            ("AlgorithmName" .=) <$> _asAlgorithmName,
            ("TrainingImage" .=) <$> _asTrainingImage,
            ("MetricDefinitions" .=) <$> _asMetricDefinitions,
            Just ("TrainingInputMode" .= _asTrainingInputMode)
          ]
      )
