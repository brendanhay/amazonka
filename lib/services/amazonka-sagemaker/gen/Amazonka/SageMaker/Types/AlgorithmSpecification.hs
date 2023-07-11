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
-- Module      : Amazonka.SageMaker.Types.AlgorithmSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AlgorithmSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.MetricDefinition
import Amazonka.SageMaker.Types.TrainingInputMode

-- | Specifies the training algorithm to use in a CreateTrainingJob request.
--
-- For more information about algorithms provided by SageMaker, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
-- For information about using your own algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- /See:/ 'newAlgorithmSpecification' smart constructor.
data AlgorithmSpecification = AlgorithmSpecification'
  { -- | The name of the algorithm resource to use for the training job. This
    -- must be an algorithm resource that you created or subscribe to on Amazon
    -- Web Services Marketplace.
    --
    -- You must specify either the algorithm name to the @AlgorithmName@
    -- parameter or the image URI of the algorithm container to the
    -- @TrainingImage@ parameter.
    --
    -- Note that the @AlgorithmName@ parameter is mutually exclusive with the
    -- @TrainingImage@ parameter. If you specify a value for the
    -- @AlgorithmName@ parameter, you can\'t specify a value for
    -- @TrainingImage@, and vice versa.
    --
    -- If you specify values for both parameters, the training job might break;
    -- if you don\'t specify any value for both parameters, the training job
    -- might raise a @null@ error.
    algorithmName :: Prelude.Maybe Prelude.Text,
    -- | The arguments for a container used to run a training job. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-training-algo-dockerfile.html How Amazon SageMaker Runs Your Training Image>
    -- for additional information.
    containerArguments :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The
    -- <https://docs.docker.com/engine/reference/builder/ entrypoint script for a Docker container>
    -- used to run a training job. This script takes precedence over the
    -- default train processing instructions. See
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-training-algo-dockerfile.html How Amazon SageMaker Runs Your Training Image>
    -- for more information.
    containerEntrypoint :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | To generate and save time-series metrics during training, set to @true@.
    -- The default is @false@ and time-series metrics aren\'t generated except
    -- in the following cases:
    --
    -- -   You use one of the SageMaker built-in algorithms
    --
    -- -   You use one of the following
    --     <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt SageMaker Docker Images>:
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
    -- name and regular expressions used to parse algorithm logs. SageMaker
    -- publishes each metric to Amazon CloudWatch.
    metricDefinitions :: Prelude.Maybe [MetricDefinition],
    -- | The registry path of the Docker image that contains the training
    -- algorithm. For information about docker registry paths for SageMaker
    -- built-in algorithms, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Docker Registry Paths and Example Code>
    -- in the /Amazon SageMaker developer guide/. SageMaker supports both
    -- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
    -- path formats. For more information about using your custom training
    -- container, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
    --
    -- You must specify either the algorithm name to the @AlgorithmName@
    -- parameter or the image URI of the algorithm container to the
    -- @TrainingImage@ parameter.
    --
    -- For more information, see the note in the @AlgorithmName@ parameter
    -- description.
    trainingImage :: Prelude.Maybe Prelude.Text,
    trainingInputMode :: TrainingInputMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AlgorithmSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'algorithmName', 'algorithmSpecification_algorithmName' - The name of the algorithm resource to use for the training job. This
-- must be an algorithm resource that you created or subscribe to on Amazon
-- Web Services Marketplace.
--
-- You must specify either the algorithm name to the @AlgorithmName@
-- parameter or the image URI of the algorithm container to the
-- @TrainingImage@ parameter.
--
-- Note that the @AlgorithmName@ parameter is mutually exclusive with the
-- @TrainingImage@ parameter. If you specify a value for the
-- @AlgorithmName@ parameter, you can\'t specify a value for
-- @TrainingImage@, and vice versa.
--
-- If you specify values for both parameters, the training job might break;
-- if you don\'t specify any value for both parameters, the training job
-- might raise a @null@ error.
--
-- 'containerArguments', 'algorithmSpecification_containerArguments' - The arguments for a container used to run a training job. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-training-algo-dockerfile.html How Amazon SageMaker Runs Your Training Image>
-- for additional information.
--
-- 'containerEntrypoint', 'algorithmSpecification_containerEntrypoint' - The
-- <https://docs.docker.com/engine/reference/builder/ entrypoint script for a Docker container>
-- used to run a training job. This script takes precedence over the
-- default train processing instructions. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-training-algo-dockerfile.html How Amazon SageMaker Runs Your Training Image>
-- for more information.
--
-- 'enableSageMakerMetricsTimeSeries', 'algorithmSpecification_enableSageMakerMetricsTimeSeries' - To generate and save time-series metrics during training, set to @true@.
-- The default is @false@ and time-series metrics aren\'t generated except
-- in the following cases:
--
-- -   You use one of the SageMaker built-in algorithms
--
-- -   You use one of the following
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt SageMaker Docker Images>:
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
-- name and regular expressions used to parse algorithm logs. SageMaker
-- publishes each metric to Amazon CloudWatch.
--
-- 'trainingImage', 'algorithmSpecification_trainingImage' - The registry path of the Docker image that contains the training
-- algorithm. For information about docker registry paths for SageMaker
-- built-in algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Docker Registry Paths and Example Code>
-- in the /Amazon SageMaker developer guide/. SageMaker supports both
-- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
-- path formats. For more information about using your custom training
-- container, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- You must specify either the algorithm name to the @AlgorithmName@
-- parameter or the image URI of the algorithm container to the
-- @TrainingImage@ parameter.
--
-- For more information, see the note in the @AlgorithmName@ parameter
-- description.
--
-- 'trainingInputMode', 'algorithmSpecification_trainingInputMode' - Undocumented member.
newAlgorithmSpecification ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  AlgorithmSpecification
newAlgorithmSpecification pTrainingInputMode_ =
  AlgorithmSpecification'
    { algorithmName =
        Prelude.Nothing,
      containerArguments = Prelude.Nothing,
      containerEntrypoint = Prelude.Nothing,
      enableSageMakerMetricsTimeSeries = Prelude.Nothing,
      metricDefinitions = Prelude.Nothing,
      trainingImage = Prelude.Nothing,
      trainingInputMode = pTrainingInputMode_
    }

-- | The name of the algorithm resource to use for the training job. This
-- must be an algorithm resource that you created or subscribe to on Amazon
-- Web Services Marketplace.
--
-- You must specify either the algorithm name to the @AlgorithmName@
-- parameter or the image URI of the algorithm container to the
-- @TrainingImage@ parameter.
--
-- Note that the @AlgorithmName@ parameter is mutually exclusive with the
-- @TrainingImage@ parameter. If you specify a value for the
-- @AlgorithmName@ parameter, you can\'t specify a value for
-- @TrainingImage@, and vice versa.
--
-- If you specify values for both parameters, the training job might break;
-- if you don\'t specify any value for both parameters, the training job
-- might raise a @null@ error.
algorithmSpecification_algorithmName :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe Prelude.Text)
algorithmSpecification_algorithmName = Lens.lens (\AlgorithmSpecification' {algorithmName} -> algorithmName) (\s@AlgorithmSpecification' {} a -> s {algorithmName = a} :: AlgorithmSpecification)

-- | The arguments for a container used to run a training job. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-training-algo-dockerfile.html How Amazon SageMaker Runs Your Training Image>
-- for additional information.
algorithmSpecification_containerArguments :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
algorithmSpecification_containerArguments = Lens.lens (\AlgorithmSpecification' {containerArguments} -> containerArguments) (\s@AlgorithmSpecification' {} a -> s {containerArguments = a} :: AlgorithmSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The
-- <https://docs.docker.com/engine/reference/builder/ entrypoint script for a Docker container>
-- used to run a training job. This script takes precedence over the
-- default train processing instructions. See
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms-training-algo-dockerfile.html How Amazon SageMaker Runs Your Training Image>
-- for more information.
algorithmSpecification_containerEntrypoint :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
algorithmSpecification_containerEntrypoint = Lens.lens (\AlgorithmSpecification' {containerEntrypoint} -> containerEntrypoint) (\s@AlgorithmSpecification' {} a -> s {containerEntrypoint = a} :: AlgorithmSpecification) Prelude.. Lens.mapping Lens.coerced

-- | To generate and save time-series metrics during training, set to @true@.
-- The default is @false@ and time-series metrics aren\'t generated except
-- in the following cases:
--
-- -   You use one of the SageMaker built-in algorithms
--
-- -   You use one of the following
--     <https://docs.aws.amazon.com/sagemaker/latest/dg/pre-built-containers-frameworks-deep-learning.html Prebuilt SageMaker Docker Images>:
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
-- name and regular expressions used to parse algorithm logs. SageMaker
-- publishes each metric to Amazon CloudWatch.
algorithmSpecification_metricDefinitions :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe [MetricDefinition])
algorithmSpecification_metricDefinitions = Lens.lens (\AlgorithmSpecification' {metricDefinitions} -> metricDefinitions) (\s@AlgorithmSpecification' {} a -> s {metricDefinitions = a} :: AlgorithmSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The registry path of the Docker image that contains the training
-- algorithm. For information about docker registry paths for SageMaker
-- built-in algorithms, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-algo-docker-registry-paths.html Docker Registry Paths and Example Code>
-- in the /Amazon SageMaker developer guide/. SageMaker supports both
-- @registry\/repository[:tag]@ and @registry\/repository[\@digest]@ image
-- path formats. For more information about using your custom training
-- container, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/your-algorithms.html Using Your Own Algorithms with Amazon SageMaker>.
--
-- You must specify either the algorithm name to the @AlgorithmName@
-- parameter or the image URI of the algorithm container to the
-- @TrainingImage@ parameter.
--
-- For more information, see the note in the @AlgorithmName@ parameter
-- description.
algorithmSpecification_trainingImage :: Lens.Lens' AlgorithmSpecification (Prelude.Maybe Prelude.Text)
algorithmSpecification_trainingImage = Lens.lens (\AlgorithmSpecification' {trainingImage} -> trainingImage) (\s@AlgorithmSpecification' {} a -> s {trainingImage = a} :: AlgorithmSpecification)

-- | Undocumented member.
algorithmSpecification_trainingInputMode :: Lens.Lens' AlgorithmSpecification TrainingInputMode
algorithmSpecification_trainingInputMode = Lens.lens (\AlgorithmSpecification' {trainingInputMode} -> trainingInputMode) (\s@AlgorithmSpecification' {} a -> s {trainingInputMode = a} :: AlgorithmSpecification)

instance Data.FromJSON AlgorithmSpecification where
  parseJSON =
    Data.withObject
      "AlgorithmSpecification"
      ( \x ->
          AlgorithmSpecification'
            Prelude.<$> (x Data..:? "AlgorithmName")
            Prelude.<*> (x Data..:? "ContainerArguments")
            Prelude.<*> (x Data..:? "ContainerEntrypoint")
            Prelude.<*> (x Data..:? "EnableSageMakerMetricsTimeSeries")
            Prelude.<*> ( x
                            Data..:? "MetricDefinitions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TrainingImage")
            Prelude.<*> (x Data..: "TrainingInputMode")
      )

instance Prelude.Hashable AlgorithmSpecification where
  hashWithSalt _salt AlgorithmSpecification' {..} =
    _salt
      `Prelude.hashWithSalt` algorithmName
      `Prelude.hashWithSalt` containerArguments
      `Prelude.hashWithSalt` containerEntrypoint
      `Prelude.hashWithSalt` enableSageMakerMetricsTimeSeries
      `Prelude.hashWithSalt` metricDefinitions
      `Prelude.hashWithSalt` trainingImage
      `Prelude.hashWithSalt` trainingInputMode

instance Prelude.NFData AlgorithmSpecification where
  rnf AlgorithmSpecification' {..} =
    Prelude.rnf algorithmName
      `Prelude.seq` Prelude.rnf containerArguments
      `Prelude.seq` Prelude.rnf containerEntrypoint
      `Prelude.seq` Prelude.rnf enableSageMakerMetricsTimeSeries
      `Prelude.seq` Prelude.rnf metricDefinitions
      `Prelude.seq` Prelude.rnf trainingImage
      `Prelude.seq` Prelude.rnf trainingInputMode

instance Data.ToJSON AlgorithmSpecification where
  toJSON AlgorithmSpecification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AlgorithmName" Data..=) Prelude.<$> algorithmName,
            ("ContainerArguments" Data..=)
              Prelude.<$> containerArguments,
            ("ContainerEntrypoint" Data..=)
              Prelude.<$> containerEntrypoint,
            ("EnableSageMakerMetricsTimeSeries" Data..=)
              Prelude.<$> enableSageMakerMetricsTimeSeries,
            ("MetricDefinitions" Data..=)
              Prelude.<$> metricDefinitions,
            ("TrainingImage" Data..=) Prelude.<$> trainingImage,
            Prelude.Just
              ("TrainingInputMode" Data..= trainingInputMode)
          ]
      )
