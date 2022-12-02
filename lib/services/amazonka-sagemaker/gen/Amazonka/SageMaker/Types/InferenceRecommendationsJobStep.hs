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
-- Module      : Amazonka.SageMaker.Types.InferenceRecommendationsJobStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.InferenceRecommendationsJobStep where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark
import Amazonka.SageMaker.Types.RecommendationJobStatus
import Amazonka.SageMaker.Types.RecommendationStepType

-- | A returned array object for the @Steps@ response field in the
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_InferenceRecommendationsJobStep.html ListInferenceRecommendationsJobSteps>
-- API command.
--
-- /See:/ 'newInferenceRecommendationsJobStep' smart constructor.
data InferenceRecommendationsJobStep = InferenceRecommendationsJobStep'
  { -- | The details for a specific benchmark.
    inferenceBenchmark :: Prelude.Maybe RecommendationJobInferenceBenchmark,
    -- | The type of the subtask.
    --
    -- @BENCHMARK@: Evaluate the performance of your model on different
    -- instance types.
    stepType :: RecommendationStepType,
    -- | The name of the Inference Recommender job.
    jobName :: Prelude.Text,
    -- | The current status of the benchmark.
    status :: RecommendationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceRecommendationsJobStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceBenchmark', 'inferenceRecommendationsJobStep_inferenceBenchmark' - The details for a specific benchmark.
--
-- 'stepType', 'inferenceRecommendationsJobStep_stepType' - The type of the subtask.
--
-- @BENCHMARK@: Evaluate the performance of your model on different
-- instance types.
--
-- 'jobName', 'inferenceRecommendationsJobStep_jobName' - The name of the Inference Recommender job.
--
-- 'status', 'inferenceRecommendationsJobStep_status' - The current status of the benchmark.
newInferenceRecommendationsJobStep ::
  -- | 'stepType'
  RecommendationStepType ->
  -- | 'jobName'
  Prelude.Text ->
  -- | 'status'
  RecommendationJobStatus ->
  InferenceRecommendationsJobStep
newInferenceRecommendationsJobStep
  pStepType_
  pJobName_
  pStatus_ =
    InferenceRecommendationsJobStep'
      { inferenceBenchmark =
          Prelude.Nothing,
        stepType = pStepType_,
        jobName = pJobName_,
        status = pStatus_
      }

-- | The details for a specific benchmark.
inferenceRecommendationsJobStep_inferenceBenchmark :: Lens.Lens' InferenceRecommendationsJobStep (Prelude.Maybe RecommendationJobInferenceBenchmark)
inferenceRecommendationsJobStep_inferenceBenchmark = Lens.lens (\InferenceRecommendationsJobStep' {inferenceBenchmark} -> inferenceBenchmark) (\s@InferenceRecommendationsJobStep' {} a -> s {inferenceBenchmark = a} :: InferenceRecommendationsJobStep)

-- | The type of the subtask.
--
-- @BENCHMARK@: Evaluate the performance of your model on different
-- instance types.
inferenceRecommendationsJobStep_stepType :: Lens.Lens' InferenceRecommendationsJobStep RecommendationStepType
inferenceRecommendationsJobStep_stepType = Lens.lens (\InferenceRecommendationsJobStep' {stepType} -> stepType) (\s@InferenceRecommendationsJobStep' {} a -> s {stepType = a} :: InferenceRecommendationsJobStep)

-- | The name of the Inference Recommender job.
inferenceRecommendationsJobStep_jobName :: Lens.Lens' InferenceRecommendationsJobStep Prelude.Text
inferenceRecommendationsJobStep_jobName = Lens.lens (\InferenceRecommendationsJobStep' {jobName} -> jobName) (\s@InferenceRecommendationsJobStep' {} a -> s {jobName = a} :: InferenceRecommendationsJobStep)

-- | The current status of the benchmark.
inferenceRecommendationsJobStep_status :: Lens.Lens' InferenceRecommendationsJobStep RecommendationJobStatus
inferenceRecommendationsJobStep_status = Lens.lens (\InferenceRecommendationsJobStep' {status} -> status) (\s@InferenceRecommendationsJobStep' {} a -> s {status = a} :: InferenceRecommendationsJobStep)

instance
  Data.FromJSON
    InferenceRecommendationsJobStep
  where
  parseJSON =
    Data.withObject
      "InferenceRecommendationsJobStep"
      ( \x ->
          InferenceRecommendationsJobStep'
            Prelude.<$> (x Data..:? "InferenceBenchmark")
            Prelude.<*> (x Data..: "StepType")
            Prelude.<*> (x Data..: "JobName")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    InferenceRecommendationsJobStep
  where
  hashWithSalt
    _salt
    InferenceRecommendationsJobStep' {..} =
      _salt `Prelude.hashWithSalt` inferenceBenchmark
        `Prelude.hashWithSalt` stepType
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    InferenceRecommendationsJobStep
  where
  rnf InferenceRecommendationsJobStep' {..} =
    Prelude.rnf inferenceBenchmark
      `Prelude.seq` Prelude.rnf stepType
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf status
