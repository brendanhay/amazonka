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
-- Module      : Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Specifies which training algorithm to use for training jobs that a
-- hyperparameter tuning job launches and the metrics to monitor.
--
-- /See:/ 'newHyperParameterAlgorithmSpecification' smart constructor.
data HyperParameterAlgorithmSpecification = HyperParameterAlgorithmSpecification'
  { -- | The registry path of the Docker image that contains the training
    -- algorithm. For information about Docker registry paths for built-in
    -- algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
    -- Amazon SageMaker supports both @registry\/repository[:tag]@ and
    -- @registry\/repository[\@digest]@ image path formats. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    trainingImage :: Prelude.Maybe Prelude.Text,
    -- | An array of MetricDefinition objects that specify the metrics that the
    -- algorithm emits.
    metricDefinitions :: Prelude.Maybe [MetricDefinition],
    -- | The name of the resource algorithm to use for the hyperparameter tuning
    -- job. If you specify a value for this parameter, do not specify a value
    -- for @TrainingImage@.
    algorithmName :: Prelude.Maybe Prelude.Text,
    -- | The input mode that the algorithm supports: File or Pipe. In File input
    -- mode, Amazon SageMaker downloads the training data from Amazon S3 to the
    -- storage volume that is attached to the training instance and mounts the
    -- directory to the Docker volume for the training container. In Pipe input
    -- mode, Amazon SageMaker streams data directly from Amazon S3 to the
    -- container.
    --
    -- If you specify File mode, make sure that you provision the storage
    -- volume that is attached to the training instance with enough capacity to
    -- accommodate the training data downloaded from Amazon S3, the model
    -- artifacts, and intermediate information.
    --
    -- For more information about input modes, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    trainingInputMode :: TrainingInputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterAlgorithmSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingImage', 'hyperParameterAlgorithmSpecification_trainingImage' - The registry path of the Docker image that contains the training
-- algorithm. For information about Docker registry paths for built-in
-- algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
-- Amazon SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- 'metricDefinitions', 'hyperParameterAlgorithmSpecification_metricDefinitions' - An array of MetricDefinition objects that specify the metrics that the
-- algorithm emits.
--
-- 'algorithmName', 'hyperParameterAlgorithmSpecification_algorithmName' - The name of the resource algorithm to use for the hyperparameter tuning
-- job. If you specify a value for this parameter, do not specify a value
-- for @TrainingImage@.
--
-- 'trainingInputMode', 'hyperParameterAlgorithmSpecification_trainingInputMode' - The input mode that the algorithm supports: File or Pipe. In File input
-- mode, Amazon SageMaker downloads the training data from Amazon S3 to the
-- storage volume that is attached to the training instance and mounts the
-- directory to the Docker volume for the training container. In Pipe input
-- mode, Amazon SageMaker streams data directly from Amazon S3 to the
-- container.
--
-- If you specify File mode, make sure that you provision the storage
-- volume that is attached to the training instance with enough capacity to
-- accommodate the training data downloaded from Amazon S3, the model
-- artifacts, and intermediate information.
--
-- For more information about input modes, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
newHyperParameterAlgorithmSpecification ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  HyperParameterAlgorithmSpecification
newHyperParameterAlgorithmSpecification
  pTrainingInputMode_ =
    HyperParameterAlgorithmSpecification'
      { trainingImage =
          Prelude.Nothing,
        metricDefinitions = Prelude.Nothing,
        algorithmName = Prelude.Nothing,
        trainingInputMode =
          pTrainingInputMode_
      }

-- | The registry path of the Docker image that contains the training
-- algorithm. For information about Docker registry paths for built-in
-- algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
-- Amazon SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
hyperParameterAlgorithmSpecification_trainingImage :: Lens.Lens' HyperParameterAlgorithmSpecification (Prelude.Maybe Prelude.Text)
hyperParameterAlgorithmSpecification_trainingImage = Lens.lens (\HyperParameterAlgorithmSpecification' {trainingImage} -> trainingImage) (\s@HyperParameterAlgorithmSpecification' {} a -> s {trainingImage = a} :: HyperParameterAlgorithmSpecification)

-- | An array of MetricDefinition objects that specify the metrics that the
-- algorithm emits.
hyperParameterAlgorithmSpecification_metricDefinitions :: Lens.Lens' HyperParameterAlgorithmSpecification (Prelude.Maybe [MetricDefinition])
hyperParameterAlgorithmSpecification_metricDefinitions = Lens.lens (\HyperParameterAlgorithmSpecification' {metricDefinitions} -> metricDefinitions) (\s@HyperParameterAlgorithmSpecification' {} a -> s {metricDefinitions = a} :: HyperParameterAlgorithmSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the resource algorithm to use for the hyperparameter tuning
-- job. If you specify a value for this parameter, do not specify a value
-- for @TrainingImage@.
hyperParameterAlgorithmSpecification_algorithmName :: Lens.Lens' HyperParameterAlgorithmSpecification (Prelude.Maybe Prelude.Text)
hyperParameterAlgorithmSpecification_algorithmName = Lens.lens (\HyperParameterAlgorithmSpecification' {algorithmName} -> algorithmName) (\s@HyperParameterAlgorithmSpecification' {} a -> s {algorithmName = a} :: HyperParameterAlgorithmSpecification)

-- | The input mode that the algorithm supports: File or Pipe. In File input
-- mode, Amazon SageMaker downloads the training data from Amazon S3 to the
-- storage volume that is attached to the training instance and mounts the
-- directory to the Docker volume for the training container. In Pipe input
-- mode, Amazon SageMaker streams data directly from Amazon S3 to the
-- container.
--
-- If you specify File mode, make sure that you provision the storage
-- volume that is attached to the training instance with enough capacity to
-- accommodate the training data downloaded from Amazon S3, the model
-- artifacts, and intermediate information.
--
-- For more information about input modes, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
hyperParameterAlgorithmSpecification_trainingInputMode :: Lens.Lens' HyperParameterAlgorithmSpecification TrainingInputMode
hyperParameterAlgorithmSpecification_trainingInputMode = Lens.lens (\HyperParameterAlgorithmSpecification' {trainingInputMode} -> trainingInputMode) (\s@HyperParameterAlgorithmSpecification' {} a -> s {trainingInputMode = a} :: HyperParameterAlgorithmSpecification)

instance
  Prelude.FromJSON
    HyperParameterAlgorithmSpecification
  where
  parseJSON =
    Prelude.withObject
      "HyperParameterAlgorithmSpecification"
      ( \x ->
          HyperParameterAlgorithmSpecification'
            Prelude.<$> (x Prelude..:? "TrainingImage")
            Prelude.<*> ( x Prelude..:? "MetricDefinitions"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "AlgorithmName")
            Prelude.<*> (x Prelude..: "TrainingInputMode")
      )

instance
  Prelude.Hashable
    HyperParameterAlgorithmSpecification

instance
  Prelude.NFData
    HyperParameterAlgorithmSpecification

instance
  Prelude.ToJSON
    HyperParameterAlgorithmSpecification
  where
  toJSON HyperParameterAlgorithmSpecification' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TrainingImage" Prelude..=)
              Prelude.<$> trainingImage,
            ("MetricDefinitions" Prelude..=)
              Prelude.<$> metricDefinitions,
            ("AlgorithmName" Prelude..=)
              Prelude.<$> algorithmName,
            Prelude.Just
              ("TrainingInputMode" Prelude..= trainingInputMode)
          ]
      )
