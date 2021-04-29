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
-- Module      : Network.AWS.SageMaker.Types.TrainingJobDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrainingJobDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.TrainingInputMode

-- | Defines the input needed to run a training job using the algorithm.
--
-- /See:/ 'newTrainingJobDefinition' smart constructor.
data TrainingJobDefinition = TrainingJobDefinition'
  { -- | The hyperparameters used for the training job.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The input mode used by the algorithm for the training job. For the input
    -- modes that Amazon SageMaker algorithms support, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
    --
    -- If an algorithm supports the @File@ input mode, Amazon SageMaker
    -- downloads the training data from S3 to the provisioned ML storage
    -- Volume, and mounts the directory to docker volume for training
    -- container. If an algorithm supports the @Pipe@ input mode, Amazon
    -- SageMaker streams data directly from S3 to the container.
    trainingInputMode :: TrainingInputMode,
    -- | An array of @Channel@ objects, each of which specifies an input source.
    inputDataConfig :: Prelude.NonEmpty Channel,
    -- | the path to the S3 bucket where you want to store model artifacts.
    -- Amazon SageMaker creates subfolders for the artifacts.
    outputDataConfig :: OutputDataConfig,
    -- | The resources, including the ML compute instances and ML storage
    -- volumes, to use for model training.
    resourceConfig :: ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. When the job
    -- reaches the time limit, Amazon SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal,
    -- which delays job termination for 120 seconds. Algorithms can use this
    -- 120-second window to save the model artifacts.
    stoppingCondition :: StoppingCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TrainingJobDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hyperParameters', 'trainingJobDefinition_hyperParameters' - The hyperparameters used for the training job.
--
-- 'trainingInputMode', 'trainingJobDefinition_trainingInputMode' - The input mode used by the algorithm for the training job. For the input
-- modes that Amazon SageMaker algorithms support, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- If an algorithm supports the @File@ input mode, Amazon SageMaker
-- downloads the training data from S3 to the provisioned ML storage
-- Volume, and mounts the directory to docker volume for training
-- container. If an algorithm supports the @Pipe@ input mode, Amazon
-- SageMaker streams data directly from S3 to the container.
--
-- 'inputDataConfig', 'trainingJobDefinition_inputDataConfig' - An array of @Channel@ objects, each of which specifies an input source.
--
-- 'outputDataConfig', 'trainingJobDefinition_outputDataConfig' - the path to the S3 bucket where you want to store model artifacts.
-- Amazon SageMaker creates subfolders for the artifacts.
--
-- 'resourceConfig', 'trainingJobDefinition_resourceConfig' - The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
--
-- 'stoppingCondition', 'trainingJobDefinition_stoppingCondition' - Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal,
-- which delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts.
newTrainingJobDefinition ::
  -- | 'trainingInputMode'
  TrainingInputMode ->
  -- | 'inputDataConfig'
  Prelude.NonEmpty Channel ->
  -- | 'outputDataConfig'
  OutputDataConfig ->
  -- | 'resourceConfig'
  ResourceConfig ->
  -- | 'stoppingCondition'
  StoppingCondition ->
  TrainingJobDefinition
newTrainingJobDefinition
  pTrainingInputMode_
  pInputDataConfig_
  pOutputDataConfig_
  pResourceConfig_
  pStoppingCondition_ =
    TrainingJobDefinition'
      { hyperParameters =
          Prelude.Nothing,
        trainingInputMode = pTrainingInputMode_,
        inputDataConfig =
          Prelude._Coerce Lens.# pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | The hyperparameters used for the training job.
trainingJobDefinition_hyperParameters :: Lens.Lens' TrainingJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJobDefinition_hyperParameters = Lens.lens (\TrainingJobDefinition' {hyperParameters} -> hyperParameters) (\s@TrainingJobDefinition' {} a -> s {hyperParameters = a} :: TrainingJobDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | The input mode used by the algorithm for the training job. For the input
-- modes that Amazon SageMaker algorithms support, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/algos.html Algorithms>.
--
-- If an algorithm supports the @File@ input mode, Amazon SageMaker
-- downloads the training data from S3 to the provisioned ML storage
-- Volume, and mounts the directory to docker volume for training
-- container. If an algorithm supports the @Pipe@ input mode, Amazon
-- SageMaker streams data directly from S3 to the container.
trainingJobDefinition_trainingInputMode :: Lens.Lens' TrainingJobDefinition TrainingInputMode
trainingJobDefinition_trainingInputMode = Lens.lens (\TrainingJobDefinition' {trainingInputMode} -> trainingInputMode) (\s@TrainingJobDefinition' {} a -> s {trainingInputMode = a} :: TrainingJobDefinition)

-- | An array of @Channel@ objects, each of which specifies an input source.
trainingJobDefinition_inputDataConfig :: Lens.Lens' TrainingJobDefinition (Prelude.NonEmpty Channel)
trainingJobDefinition_inputDataConfig = Lens.lens (\TrainingJobDefinition' {inputDataConfig} -> inputDataConfig) (\s@TrainingJobDefinition' {} a -> s {inputDataConfig = a} :: TrainingJobDefinition) Prelude.. Prelude._Coerce

-- | the path to the S3 bucket where you want to store model artifacts.
-- Amazon SageMaker creates subfolders for the artifacts.
trainingJobDefinition_outputDataConfig :: Lens.Lens' TrainingJobDefinition OutputDataConfig
trainingJobDefinition_outputDataConfig = Lens.lens (\TrainingJobDefinition' {outputDataConfig} -> outputDataConfig) (\s@TrainingJobDefinition' {} a -> s {outputDataConfig = a} :: TrainingJobDefinition)

-- | The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
trainingJobDefinition_resourceConfig :: Lens.Lens' TrainingJobDefinition ResourceConfig
trainingJobDefinition_resourceConfig = Lens.lens (\TrainingJobDefinition' {resourceConfig} -> resourceConfig) (\s@TrainingJobDefinition' {} a -> s {resourceConfig = a} :: TrainingJobDefinition)

-- | Specifies a limit to how long a model training job can run. When the job
-- reaches the time limit, Amazon SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, Amazon SageMaker sends the algorithm the SIGTERM signal,
-- which delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts.
trainingJobDefinition_stoppingCondition :: Lens.Lens' TrainingJobDefinition StoppingCondition
trainingJobDefinition_stoppingCondition = Lens.lens (\TrainingJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@TrainingJobDefinition' {} a -> s {stoppingCondition = a} :: TrainingJobDefinition)

instance Prelude.FromJSON TrainingJobDefinition where
  parseJSON =
    Prelude.withObject
      "TrainingJobDefinition"
      ( \x ->
          TrainingJobDefinition'
            Prelude.<$> ( x Prelude..:? "HyperParameters"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "TrainingInputMode")
            Prelude.<*> (x Prelude..: "InputDataConfig")
            Prelude.<*> (x Prelude..: "OutputDataConfig")
            Prelude.<*> (x Prelude..: "ResourceConfig")
            Prelude.<*> (x Prelude..: "StoppingCondition")
      )

instance Prelude.Hashable TrainingJobDefinition

instance Prelude.NFData TrainingJobDefinition

instance Prelude.ToJSON TrainingJobDefinition where
  toJSON TrainingJobDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("HyperParameters" Prelude..=)
              Prelude.<$> hyperParameters,
            Prelude.Just
              ("TrainingInputMode" Prelude..= trainingInputMode),
            Prelude.Just
              ("InputDataConfig" Prelude..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Prelude..= outputDataConfig),
            Prelude.Just
              ("ResourceConfig" Prelude..= resourceConfig),
            Prelude.Just
              ("StoppingCondition" Prelude..= stoppingCondition)
          ]
      )
