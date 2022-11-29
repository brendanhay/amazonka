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
-- Module      : Amazonka.SageMaker.Types.HyperParameterAlgorithmSpecification
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HyperParameterAlgorithmSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricDefinition
import Amazonka.SageMaker.Types.TrainingInputMode

-- | Specifies which training algorithm to use for training jobs that a
-- hyperparameter tuning job launches and the metrics to monitor.
--
-- /See:/ 'newHyperParameterAlgorithmSpecification' smart constructor.
data HyperParameterAlgorithmSpecification = HyperParameterAlgorithmSpecification'
  { -- | The name of the resource algorithm to use for the hyperparameter tuning
    -- job. If you specify a value for this parameter, do not specify a value
    -- for @TrainingImage@.
    algorithmName :: Prelude.Maybe Prelude.Text,
    -- | An array of MetricDefinition objects that specify the metrics that the
    -- algorithm emits.
    metricDefinitions :: Prelude.Maybe [MetricDefinition],
    -- | The registry path of the Docker image that contains the training
    -- algorithm. For information about Docker registry paths for built-in
    -- algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
    -- SageMaker supports both @registry\/repository[:tag]@ and
    -- @registry\/repository[\@digest]@ image path formats. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    trainingImage :: Prelude.Maybe Prelude.Text,
    trainingInputMode :: TrainingInputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HyperParameterAlgorithmSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'hyperParameterAlgorithmSpecification_algorithmName' - The name of the resource algorithm to use for the hyperparameter tuning
-- job. If you specify a value for this parameter, do not specify a value
-- for @TrainingImage@.
--
-- 'metricDefinitions', 'hyperParameterAlgorithmSpecification_metricDefinitions' - An array of MetricDefinition objects that specify the metrics that the
-- algorithm emits.
--
-- 'trainingImage', 'hyperParameterAlgorithmSpecification_trainingImage' - The registry path of the Docker image that contains the training
-- algorithm. For information about Docker registry paths for built-in
-- algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
-- SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- 'trainingInputMode', 'hyperParameterAlgorithmSpecification_trainingInputMode' - Undocumented member.
newHyperParameterAlgorithmSpecification ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  HyperParameterAlgorithmSpecification
newHyperParameterAlgorithmSpecification
  pTrainingInputMode_ =
    HyperParameterAlgorithmSpecification'
      { algorithmName =
          Prelude.Nothing,
        metricDefinitions = Prelude.Nothing,
        trainingImage = Prelude.Nothing,
        trainingInputMode =
          pTrainingInputMode_
      }

-- | The name of the resource algorithm to use for the hyperparameter tuning
-- job. If you specify a value for this parameter, do not specify a value
-- for @TrainingImage@.
hyperParameterAlgorithmSpecification_algorithmName :: Lens.Lens' HyperParameterAlgorithmSpecification (Prelude.Maybe Prelude.Text)
hyperParameterAlgorithmSpecification_algorithmName = Lens.lens (\HyperParameterAlgorithmSpecification' {algorithmName} -> algorithmName) (\s@HyperParameterAlgorithmSpecification' {} a -> s {algorithmName = a} :: HyperParameterAlgorithmSpecification)

-- | An array of MetricDefinition objects that specify the metrics that the
-- algorithm emits.
hyperParameterAlgorithmSpecification_metricDefinitions :: Lens.Lens' HyperParameterAlgorithmSpecification (Prelude.Maybe [MetricDefinition])
hyperParameterAlgorithmSpecification_metricDefinitions = Lens.lens (\HyperParameterAlgorithmSpecification' {metricDefinitions} -> metricDefinitions) (\s@HyperParameterAlgorithmSpecification' {} a -> s {metricDefinitions = a} :: HyperParameterAlgorithmSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The registry path of the Docker image that contains the training
-- algorithm. For information about Docker registry paths for built-in
-- algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Algorithms Provided by Amazon SageMaker: Common Parameters>.
-- SageMaker supports both @registry\/repository[:tag]@ and
-- @registry\/repository[\@digest]@ image path formats. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
hyperParameterAlgorithmSpecification_trainingImage :: Lens.Lens' HyperParameterAlgorithmSpecification (Prelude.Maybe Prelude.Text)
hyperParameterAlgorithmSpecification_trainingImage = Lens.lens (\HyperParameterAlgorithmSpecification' {trainingImage} -> trainingImage) (\s@HyperParameterAlgorithmSpecification' {} a -> s {trainingImage = a} :: HyperParameterAlgorithmSpecification)

-- | Undocumented member.
hyperParameterAlgorithmSpecification_trainingInputMode :: Lens.Lens' HyperParameterAlgorithmSpecification TrainingInputMode
hyperParameterAlgorithmSpecification_trainingInputMode = Lens.lens (\HyperParameterAlgorithmSpecification' {trainingInputMode} -> trainingInputMode) (\s@HyperParameterAlgorithmSpecification' {} a -> s {trainingInputMode = a} :: HyperParameterAlgorithmSpecification)

instance
  Core.FromJSON
    HyperParameterAlgorithmSpecification
  where
  parseJSON =
    Core.withObject
      "HyperParameterAlgorithmSpecification"
      ( \x ->
          HyperParameterAlgorithmSpecification'
            Prelude.<$> (x Core..:? "AlgorithmName")
            Prelude.<*> ( x Core..:? "MetricDefinitions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TrainingImage")
            Prelude.<*> (x Core..: "TrainingInputMode")
      )

instance
  Prelude.Hashable
    HyperParameterAlgorithmSpecification
  where
  hashWithSalt
    _salt
    HyperParameterAlgorithmSpecification' {..} =
      _salt `Prelude.hashWithSalt` algorithmName
        `Prelude.hashWithSalt` metricDefinitions
        `Prelude.hashWithSalt` trainingImage
        `Prelude.hashWithSalt` trainingInputMode

instance
  Prelude.NFData
    HyperParameterAlgorithmSpecification
  where
  rnf HyperParameterAlgorithmSpecification' {..} =
    Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf metricDefinitions
      `Prelude.seq` Prelude.rnf trainingImage
      `Prelude.seq` Prelude.rnf trainingInputMode

instance
  Core.ToJSON
    HyperParameterAlgorithmSpecification
  where
  toJSON HyperParameterAlgorithmSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AlgorithmName" Core..=) Prelude.<$> algorithmName,
            ("MetricDefinitions" Core..=)
              Prelude.<$> metricDefinitions,
            ("TrainingImage" Core..=) Prelude.<$> trainingImage,
            Prelude.Just
              ("TrainingInputMode" Core..= trainingInputMode)
          ]
      )
