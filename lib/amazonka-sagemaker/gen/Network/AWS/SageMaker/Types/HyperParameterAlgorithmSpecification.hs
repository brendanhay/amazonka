{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Specifies which training algorithm to use for training jobs that a hyperparameter tuning job launches and the metrics to monitor.
--
--
--
-- /See:/ 'hyperParameterAlgorithmSpecification' smart constructor.
data HyperParameterAlgorithmSpecification = HyperParameterAlgorithmSpecification'
  { _hpasAlgorithmName ::
      !(Maybe Text),
    _hpasTrainingImage ::
      !(Maybe Text),
    _hpasMetricDefinitions ::
      !( Maybe
           [MetricDefinition]
       ),
    _hpasTrainingInputMode ::
      !TrainingInputMode
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HyperParameterAlgorithmSpecification' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hpasAlgorithmName' - The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
--
-- * 'hpasTrainingImage' - The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
--
-- * 'hpasMetricDefinitions' - An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
--
-- * 'hpasTrainingInputMode' - The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container.  If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information. For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
hyperParameterAlgorithmSpecification ::
  -- | 'hpasTrainingInputMode'
  TrainingInputMode ->
  HyperParameterAlgorithmSpecification
hyperParameterAlgorithmSpecification pTrainingInputMode_ =
  HyperParameterAlgorithmSpecification'
    { _hpasAlgorithmName =
        Nothing,
      _hpasTrainingImage = Nothing,
      _hpasMetricDefinitions = Nothing,
      _hpasTrainingInputMode = pTrainingInputMode_
    }

-- | The name of the resource algorithm to use for the hyperparameter tuning job. If you specify a value for this parameter, do not specify a value for @TrainingImage@ .
hpasAlgorithmName :: Lens' HyperParameterAlgorithmSpecification (Maybe Text)
hpasAlgorithmName = lens _hpasAlgorithmName (\s a -> s {_hpasAlgorithmName = a})

-- | The registry path of the Docker image that contains the training algorithm. For information about Docker registry paths for built-in algorithms, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters> . Amazon SageMaker supports both @registry/repository[:tag]@ and @registry/repository[@digest]@ image path formats. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker> .
hpasTrainingImage :: Lens' HyperParameterAlgorithmSpecification (Maybe Text)
hpasTrainingImage = lens _hpasTrainingImage (\s a -> s {_hpasTrainingImage = a})

-- | An array of 'MetricDefinition' objects that specify the metrics that the algorithm emits.
hpasMetricDefinitions :: Lens' HyperParameterAlgorithmSpecification [MetricDefinition]
hpasMetricDefinitions = lens _hpasMetricDefinitions (\s a -> s {_hpasMetricDefinitions = a}) . _Default . _Coerce

-- | The input mode that the algorithm supports: File or Pipe. In File input mode, Amazon SageMaker downloads the training data from Amazon S3 to the storage volume that is attached to the training instance and mounts the directory to the Docker volume for the training container. In Pipe input mode, Amazon SageMaker streams data directly from Amazon S3 to the container.  If you specify File mode, make sure that you provision the storage volume that is attached to the training instance with enough capacity to accommodate the training data downloaded from Amazon S3, the model artifacts, and intermediate information. For more information about input modes, see <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms> .
hpasTrainingInputMode :: Lens' HyperParameterAlgorithmSpecification TrainingInputMode
hpasTrainingInputMode = lens _hpasTrainingInputMode (\s a -> s {_hpasTrainingInputMode = a})

instance FromJSON HyperParameterAlgorithmSpecification where
  parseJSON =
    withObject
      "HyperParameterAlgorithmSpecification"
      ( \x ->
          HyperParameterAlgorithmSpecification'
            <$> (x .:? "AlgorithmName")
            <*> (x .:? "TrainingImage")
            <*> (x .:? "MetricDefinitions" .!= mempty)
            <*> (x .: "TrainingInputMode")
      )

instance Hashable HyperParameterAlgorithmSpecification

instance NFData HyperParameterAlgorithmSpecification

instance ToJSON HyperParameterAlgorithmSpecification where
  toJSON HyperParameterAlgorithmSpecification' {..} =
    object
      ( catMaybes
          [ ("AlgorithmName" .=) <$> _hpasAlgorithmName,
            ("TrainingImage" .=) <$> _hpasTrainingImage,
            ("MetricDefinitions" .=) <$> _hpasMetricDefinitions,
            Just ("TrainingInputMode" .= _hpasTrainingInputMode)
          ]
      )
