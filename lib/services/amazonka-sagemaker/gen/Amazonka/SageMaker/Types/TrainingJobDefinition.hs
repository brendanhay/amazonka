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
-- Module      : Amazonka.SageMaker.Types.TrainingJobDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrainingJobDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.OutputDataConfig
import Amazonka.SageMaker.Types.ResourceConfig
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.TrainingInputMode

-- | Defines the input needed to run a training job using the algorithm.
--
-- /See:/ 'newTrainingJobDefinition' smart constructor.
data TrainingJobDefinition = TrainingJobDefinition'
  { -- | The hyperparameters used for the training job.
    hyperParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    trainingInputMode :: TrainingInputMode,
    -- | An array of @Channel@ objects, each of which specifies an input source.
    inputDataConfig :: Prelude.NonEmpty Channel,
    -- | the path to the S3 bucket where you want to store model artifacts.
    -- SageMaker creates subfolders for the artifacts.
    outputDataConfig :: OutputDataConfig,
    -- | The resources, including the ML compute instances and ML storage
    -- volumes, to use for model training.
    resourceConfig :: ResourceConfig,
    -- | Specifies a limit to how long a model training job can run. It also
    -- specifies how long a managed Spot training job has to complete. When the
    -- job reaches the time limit, SageMaker ends the training job. Use this
    -- API to cap model training costs.
    --
    -- To stop a job, SageMaker sends the algorithm the SIGTERM signal, which
    -- delays job termination for 120 seconds. Algorithms can use this
    -- 120-second window to save the model artifacts.
    stoppingCondition :: StoppingCondition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'trainingInputMode', 'trainingJobDefinition_trainingInputMode' - Undocumented member.
--
-- 'inputDataConfig', 'trainingJobDefinition_inputDataConfig' - An array of @Channel@ objects, each of which specifies an input source.
--
-- 'outputDataConfig', 'trainingJobDefinition_outputDataConfig' - the path to the S3 bucket where you want to store model artifacts.
-- SageMaker creates subfolders for the artifacts.
--
-- 'resourceConfig', 'trainingJobDefinition_resourceConfig' - The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
--
-- 'stoppingCondition', 'trainingJobDefinition_stoppingCondition' - Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, SageMaker sends the algorithm the SIGTERM signal, which
-- delays job termination for 120 seconds. Algorithms can use this
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
          Lens.coerced Lens.# pInputDataConfig_,
        outputDataConfig = pOutputDataConfig_,
        resourceConfig = pResourceConfig_,
        stoppingCondition = pStoppingCondition_
      }

-- | The hyperparameters used for the training job.
trainingJobDefinition_hyperParameters :: Lens.Lens' TrainingJobDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
trainingJobDefinition_hyperParameters = Lens.lens (\TrainingJobDefinition' {hyperParameters} -> hyperParameters) (\s@TrainingJobDefinition' {} a -> s {hyperParameters = a} :: TrainingJobDefinition) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
trainingJobDefinition_trainingInputMode :: Lens.Lens' TrainingJobDefinition TrainingInputMode
trainingJobDefinition_trainingInputMode = Lens.lens (\TrainingJobDefinition' {trainingInputMode} -> trainingInputMode) (\s@TrainingJobDefinition' {} a -> s {trainingInputMode = a} :: TrainingJobDefinition)

-- | An array of @Channel@ objects, each of which specifies an input source.
trainingJobDefinition_inputDataConfig :: Lens.Lens' TrainingJobDefinition (Prelude.NonEmpty Channel)
trainingJobDefinition_inputDataConfig = Lens.lens (\TrainingJobDefinition' {inputDataConfig} -> inputDataConfig) (\s@TrainingJobDefinition' {} a -> s {inputDataConfig = a} :: TrainingJobDefinition) Prelude.. Lens.coerced

-- | the path to the S3 bucket where you want to store model artifacts.
-- SageMaker creates subfolders for the artifacts.
trainingJobDefinition_outputDataConfig :: Lens.Lens' TrainingJobDefinition OutputDataConfig
trainingJobDefinition_outputDataConfig = Lens.lens (\TrainingJobDefinition' {outputDataConfig} -> outputDataConfig) (\s@TrainingJobDefinition' {} a -> s {outputDataConfig = a} :: TrainingJobDefinition)

-- | The resources, including the ML compute instances and ML storage
-- volumes, to use for model training.
trainingJobDefinition_resourceConfig :: Lens.Lens' TrainingJobDefinition ResourceConfig
trainingJobDefinition_resourceConfig = Lens.lens (\TrainingJobDefinition' {resourceConfig} -> resourceConfig) (\s@TrainingJobDefinition' {} a -> s {resourceConfig = a} :: TrainingJobDefinition)

-- | Specifies a limit to how long a model training job can run. It also
-- specifies how long a managed Spot training job has to complete. When the
-- job reaches the time limit, SageMaker ends the training job. Use this
-- API to cap model training costs.
--
-- To stop a job, SageMaker sends the algorithm the SIGTERM signal, which
-- delays job termination for 120 seconds. Algorithms can use this
-- 120-second window to save the model artifacts.
trainingJobDefinition_stoppingCondition :: Lens.Lens' TrainingJobDefinition StoppingCondition
trainingJobDefinition_stoppingCondition = Lens.lens (\TrainingJobDefinition' {stoppingCondition} -> stoppingCondition) (\s@TrainingJobDefinition' {} a -> s {stoppingCondition = a} :: TrainingJobDefinition)

instance Data.FromJSON TrainingJobDefinition where
  parseJSON =
    Data.withObject
      "TrainingJobDefinition"
      ( \x ->
          TrainingJobDefinition'
            Prelude.<$> ( x
                            Data..:? "HyperParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "TrainingInputMode")
            Prelude.<*> (x Data..: "InputDataConfig")
            Prelude.<*> (x Data..: "OutputDataConfig")
            Prelude.<*> (x Data..: "ResourceConfig")
            Prelude.<*> (x Data..: "StoppingCondition")
      )

instance Prelude.Hashable TrainingJobDefinition where
  hashWithSalt _salt TrainingJobDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` hyperParameters
      `Prelude.hashWithSalt` trainingInputMode
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` resourceConfig
      `Prelude.hashWithSalt` stoppingCondition

instance Prelude.NFData TrainingJobDefinition where
  rnf TrainingJobDefinition' {..} =
    Prelude.rnf hyperParameters
      `Prelude.seq` Prelude.rnf trainingInputMode
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf resourceConfig
      `Prelude.seq` Prelude.rnf stoppingCondition

instance Data.ToJSON TrainingJobDefinition where
  toJSON TrainingJobDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HyperParameters" Data..=)
              Prelude.<$> hyperParameters,
            Prelude.Just
              ("TrainingInputMode" Data..= trainingInputMode),
            Prelude.Just
              ("InputDataConfig" Data..= inputDataConfig),
            Prelude.Just
              ("OutputDataConfig" Data..= outputDataConfig),
            Prelude.Just
              ("ResourceConfig" Data..= resourceConfig),
            Prelude.Just
              ("StoppingCondition" Data..= stoppingCondition)
          ]
      )
