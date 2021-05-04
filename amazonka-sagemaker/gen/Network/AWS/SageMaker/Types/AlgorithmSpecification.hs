{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.AlgorithmSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.AlgorithmSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Specifies the training algorithm to use in a CreateTrainingJob request.
--
-- For more information about algorithms provided by Amazon SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- For information about using your own algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- /See:/ 'newAlgorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { -- | The registry path of the Docker image that contains the training
    -- algorithm. For information about docker registry paths for built-in
    -- algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
    -- Amazon SageMaker supports both @registry\/repository[:tag]@ and
    -- @registry\/repository[\@digest]@ image path formats. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    trainingImage :: Prelude.Maybe Prelude.Text,
    -- | To generate and save time-series metrics during training, set to @true@.
    -- The default is @false@ and time-series metrics aren\'t generated except
    -- in the following cases:
    --
    -- -   You use one of the Amazon SageMaker built-in algorithms
    --
    -- -   You use one of the following
    --     <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images>:
    --
    --     -   Tensorflow (version >= 1.15)
    --
    --     -   MXNet (version >= 1.6)
    --
    --     -   PyTorch (version >= 1.3)
    --
    -- -   You specify at least one MetricDefinition
    enableSageMakerMetricsTimeSeries :: Prelude.Maybe Prelude.Bool,
    -- | A list of metric definition objects. Each object specifies the metric
    -- name and regular expressions used to parse algorithm logs. Amazon
    -- SageMaker publishes each metric to Amazon CloudWatch.
    metricDefinitions :: Prelude.Maybe [MetricDefinition],
    -- | The name of the algorithm resource to use for the training job. This
    -- must be an algorithm resource that you created or subscribe to on AWS
    -- Marketplace. If you specify a value for this parameter, you can\'t
    -- specify a value for @TrainingImage@.
    algorithmName :: Prelude.Maybe Prelude.Text,
    -- | The input mode that the algorithm supports. For the input modes that
    -- Amazon SageMaker algorithms support, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    -- If an algorithm supports the @File@ input mode, Amazon SageMaker
    -- downloads the training data from S3 to the provisioned ML storage
    -- Volume, and mounts the directory to docker volume for training
    -- container. If an algorithm supports the @Pipe@ input mode, Amazon
    -- SageMaker streams data directly from S3 to the container.
    --
    -- In File mode, make sure you provision ML storage volume with sufficient
    -- capacity to accommodate the data download from S3. In addition to the
    -- training data, the ML storage volume also stores the output model. The
    -- algorithm container use ML storage volume to also store intermediate
    -- information, if any.
    --
    -- For distributed algorithms using File mode, training data is distributed
    -- uniformly, and your training duration is predictable if the input data
    -- objects size is approximately same. Amazon SageMaker does not split the
    -- files any further for model training. If the object sizes are skewed,
    -- training won\'t be optimal as the data distribution is also skewed where
    -- one host in a training cluster is overloaded, thus becoming bottleneck
    -- in training.
    trainingInputMode :: TrainingInputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingImage', 'algorithmSpecification_trainingImage' - The registry path of the Docker image that contains the training
-- algorithm. For information about docker registry paths for built-in
-- algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
-- Amazon SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- 'enableSageMakerMetricsTimeSeries', 'algorithmSpecification_enableSageMakerMetricsTimeSeries' - To generate and save time-series metrics during training, set to @true@.
-- The default is @false@ and time-series metrics aren\'t generated except
-- in the following cases:
--
-- -   You use one of the Amazon SageMaker built-in algorithms
--
-- -   You use one of the following
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images>:
--
--     -   Tensorflow (version >= 1.15)
--
--     -   MXNet (version >= 1.6)
--
--     -   PyTorch (version >= 1.3)
--
-- -   You specify at least one MetricDefinition
--
-- 'metricDefinitions', 'algorithmSpecification_metricDefinitions' - A list of metric definition objects. Each object specifies the metric
-- name and regular expressions used to parse algorithm logs. Amazon
-- SageMaker publishes each metric to Amazon CloudWatch.
--
-- 'algorithmName', 'algorithmSpecification_algorithmName' - The name of the algorithm resource to use for the training job. This
-- must be an algorithm resource that you created or subscribe to on AWS
-- Marketplace. If you specify a value for this parameter, you can\'t
-- specify a value for @TrainingImage@.
--
-- 'trainingInputMode', 'algorithmSpecification_trainingInputMode' - The input mode that the algorithm supports. For the input modes that
-- Amazon SageMaker algorithms support, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- If an algorithm supports the @File@ input mode, Amazon SageMaker
-- downloads the training data from S3 to the provisioned ML storage
-- Volume, and mounts the directory to docker volume for training
-- container. If an algorithm supports the @Pipe@ input mode, Amazon
-- SageMaker streams data directly from S3 to the container.
--
-- In File mode, make sure you provision ML storage volume with sufficient
-- capacity to accommodate the data download from S3. In addition to the
-- training data, the ML storage volume also stores the output model. The
-- algorithm container use ML storage volume to also store intermediate
-- information, if any.
--
-- For distributed algorithms using File mode, training data is distributed
-- uniformly, and your training duration is predictable if the input data
-- objects size is approximately same. Amazon SageMaker does not split the
-- files any further for model training. If the object sizes are skewed,
-- training won\'t be optimal as the data distribution is also skewed where
-- one host in a training cluster is overloaded, thus becoming bottleneck
-- in training.
newAlgorithmSpecification ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  AlgorithmSpecification
newAlgorithmSpecification pTrainingInputMode_ =
  AlgorithmSpecification'
    { trainingImage =
        Prelude.Nothing,
      enableSageMakerMetricsTimeSeries = Prelude.Nothing,
      metricDefinitions = Prelude.Nothing,
      algorithmName = Prelude.Nothing,
      trainingInputMode = pTrainingInputMode_
    }

-- | The registry path of the Docker image that contains the training
-- algorithm. For information about docker registry paths for built-in
-- algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
-- Amazon SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
algorithmSpecification_trainingImage :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe Prelude.Text)
algorithmSpecification_trainingImage = Lens.lens (\AlgorithmSpecification' {trainingImage} -> trainingImage) (\s@AlgorithmSpecification' {} a -> s {trainingImage = a} :: AlgorithmSpecification)

-- | To generate and save time-series metrics during training, set to @true@.
-- The default is @false@ and time-series metrics aren\'t generated except
-- in the following cases:
--
-- -   You use one of the Amazon SageMaker built-in algorithms
--
-- -   You use one of the following
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt Amazon SageMaker Docker Images>:
--
--     -   Tensorflow (version >= 1.15)
--
--     -   MXNet (version >= 1.6)
--
--     -   PyTorch (version >= 1.3)
--
-- -   You specify at least one MetricDefinition
algorithmSpecification_enableSageMakerMetricsTimeSeries :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe Prelude.Bool)
algorithmSpecification_enableSageMakerMetricsTimeSeries = Lens.lens (\AlgorithmSpecification' {enableSageMakerMetricsTimeSeries} -> enableSageMakerMetricsTimeSeries) (\s@AlgorithmSpecification' {} a -> s {enableSageMakerMetricsTimeSeries = a} :: AlgorithmSpecification)

-- | A list of metric definition objects. Each object specifies the metric
-- name and regular expressions used to parse algorithm logs. Amazon
-- SageMaker publishes each metric to Amazon CloudWatch.
algorithmSpecification_metricDefinitions :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe [MetricDefinition])
algorithmSpecification_metricDefinitions = Lens.lens (\AlgorithmSpecification' {metricDefinitions} -> metricDefinitions) (\s@AlgorithmSpecification' {} a -> s {metricDefinitions = a} :: AlgorithmSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the algorithm resource to use for the training job. This
-- must be an algorithm resource that you created or subscribe to on AWS
-- Marketplace. If you specify a value for this parameter, you can\'t
-- specify a value for @TrainingImage@.
algorithmSpecification_algorithmName :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe Prelude.Text)
algorithmSpecification_algorithmName = Lens.lens (\AlgorithmSpecification' {algorithmName} -> algorithmName) (\s@AlgorithmSpecification' {} a -> s {algorithmName = a} :: AlgorithmSpecification)

-- | The input mode that the algorithm supports. For the input modes that
-- Amazon SageMaker algorithms support, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- If an algorithm supports the @File@ input mode, Amazon SageMaker
-- downloads the training data from S3 to the provisioned ML storage
-- Volume, and mounts the directory to docker volume for training
-- container. If an algorithm supports the @Pipe@ input mode, Amazon
-- SageMaker streams data directly from S3 to the container.
--
-- In File mode, make sure you provision ML storage volume with sufficient
-- capacity to accommodate the data download from S3. In addition to the
-- training data, the ML storage volume also stores the output model. The
-- algorithm container use ML storage volume to also store intermediate
-- information, if any.
--
-- For distributed algorithms using File mode, training data is distributed
-- uniformly, and your training duration is predictable if the input data
-- objects size is approximately same. Amazon SageMaker does not split the
-- files any further for model training. If the object sizes are skewed,
-- training won\'t be optimal as the data distribution is also skewed where
-- one host in a training cluster is overloaded, thus becoming bottleneck
-- in training.
algorithmSpecification_trainingInputMode :: Lens.Lens' AlgorithmSpecification TrainingInputMode
algorithmSpecification_trainingInputMode = Lens.lens (\AlgorithmSpecification' {trainingInputMode} -> trainingInputMode) (\s@AlgorithmSpecification' {} a -> s {trainingInputMode = a} :: AlgorithmSpecification)

instance Prelude.FromJSON AlgorithmSpecification where
  parseJSON =
    Prelude.withObject
      "AlgorithmSpecification"
      ( \x ->
          AlgorithmSpecification'
            Prelude.<$> (x Prelude..:? "TrainingImage")
            Prelude.<*> (x Prelude..:? "EnableSageMakerMetricsTimeSeries")
            Prelude.<*> ( x Prelude..:? "MetricDefinitions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AlgorithmName")
            Prelude.<*> (x Prelude..: "TrainingInputMode")
      )

instance Prelude.Hashable AlgorithmSpecification

instance Prelude.NFData AlgorithmSpecification

instance Prelude.ToJSON AlgorithmSpecification where
  toJSON AlgorithmSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TrainingImage" Prelude..=)
              Prelude.<$> trainingImage,
            ("EnableSageMakerMetricsTimeSeries" Prelude..=)
              Prelude.<$> enableSageMakerMetricsTimeSeries,
            ("MetricDefinitions" Prelude..=)
              Prelude.<$> metricDefinitions,
            ("AlgorithmName" Prelude..=)
              Prelude.<$> algorithmName,
            Prelude.Just
              ("TrainingInputMode" Prelude..= trainingInputMode)
          ]
      )
