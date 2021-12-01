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
-- Module      : Amazonka.SageMaker.Types.PipelineExecutionStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineExecutionStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CallbackStepMetadata
import Amazonka.SageMaker.Types.ConditionStepMetadata
import Amazonka.SageMaker.Types.LambdaStepMetadata
import Amazonka.SageMaker.Types.ModelStepMetadata
import Amazonka.SageMaker.Types.ProcessingJobStepMetadata
import Amazonka.SageMaker.Types.RegisterModelStepMetadata
import Amazonka.SageMaker.Types.TrainingJobStepMetadata
import Amazonka.SageMaker.Types.TransformJobStepMetadata
import Amazonka.SageMaker.Types.TuningJobStepMetaData

-- | Metadata for a step execution.
--
-- /See:/ 'newPipelineExecutionStepMetadata' smart constructor.
data PipelineExecutionStepMetadata = PipelineExecutionStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the training job that was run by this
    -- step execution.
    trainingJob :: Prelude.Maybe TrainingJobStepMetadata,
    -- | The Amazon Resource Name (ARN) of the processing job that was run by
    -- this step execution.
    processingJob :: Prelude.Maybe ProcessingJobStepMetadata,
    -- | The Amazon Resource Name (ARN) of the model that was created by this
    -- step execution.
    model :: Prelude.Maybe ModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the Lambda function that was run by
    -- this step execution and a list of output parameters.
    lambda :: Prelude.Maybe LambdaStepMetadata,
    -- | The Amazon Resource Name (ARN) of the tuning job that was run by this
    -- step execution.
    tuningJob :: Prelude.Maybe TuningJobStepMetaData,
    -- | The outcome of the condition evaluation that was run by this step
    -- execution.
    condition :: Prelude.Maybe ConditionStepMetadata,
    -- | The Amazon Resource Name (ARN) of the transform job that was run by this
    -- step execution.
    transformJob :: Prelude.Maybe TransformJobStepMetadata,
    -- | The Amazon Resource Name (ARN) of the model package the model was
    -- registered to by this step execution.
    registerModel :: Prelude.Maybe RegisterModelStepMetadata,
    -- | The URL of the Amazon SQS queue used by this step execution, the
    -- pipeline generated token, and a list of output parameters.
    callback :: Prelude.Maybe CallbackStepMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PipelineExecutionStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trainingJob', 'pipelineExecutionStepMetadata_trainingJob' - The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
--
-- 'processingJob', 'pipelineExecutionStepMetadata_processingJob' - The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
--
-- 'model', 'pipelineExecutionStepMetadata_model' - The Amazon Resource Name (ARN) of the model that was created by this
-- step execution.
--
-- 'lambda', 'pipelineExecutionStepMetadata_lambda' - The Amazon Resource Name (ARN) of the Lambda function that was run by
-- this step execution and a list of output parameters.
--
-- 'tuningJob', 'pipelineExecutionStepMetadata_tuningJob' - The Amazon Resource Name (ARN) of the tuning job that was run by this
-- step execution.
--
-- 'condition', 'pipelineExecutionStepMetadata_condition' - The outcome of the condition evaluation that was run by this step
-- execution.
--
-- 'transformJob', 'pipelineExecutionStepMetadata_transformJob' - The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
--
-- 'registerModel', 'pipelineExecutionStepMetadata_registerModel' - The Amazon Resource Name (ARN) of the model package the model was
-- registered to by this step execution.
--
-- 'callback', 'pipelineExecutionStepMetadata_callback' - The URL of the Amazon SQS queue used by this step execution, the
-- pipeline generated token, and a list of output parameters.
newPipelineExecutionStepMetadata ::
  PipelineExecutionStepMetadata
newPipelineExecutionStepMetadata =
  PipelineExecutionStepMetadata'
    { trainingJob =
        Prelude.Nothing,
      processingJob = Prelude.Nothing,
      model = Prelude.Nothing,
      lambda = Prelude.Nothing,
      tuningJob = Prelude.Nothing,
      condition = Prelude.Nothing,
      transformJob = Prelude.Nothing,
      registerModel = Prelude.Nothing,
      callback = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
pipelineExecutionStepMetadata_trainingJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TrainingJobStepMetadata)
pipelineExecutionStepMetadata_trainingJob = Lens.lens (\PipelineExecutionStepMetadata' {trainingJob} -> trainingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {trainingJob = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
pipelineExecutionStepMetadata_processingJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ProcessingJobStepMetadata)
pipelineExecutionStepMetadata_processingJob = Lens.lens (\PipelineExecutionStepMetadata' {processingJob} -> processingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {processingJob = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the model that was created by this
-- step execution.
pipelineExecutionStepMetadata_model :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ModelStepMetadata)
pipelineExecutionStepMetadata_model = Lens.lens (\PipelineExecutionStepMetadata' {model} -> model) (\s@PipelineExecutionStepMetadata' {} a -> s {model = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the Lambda function that was run by
-- this step execution and a list of output parameters.
pipelineExecutionStepMetadata_lambda :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe LambdaStepMetadata)
pipelineExecutionStepMetadata_lambda = Lens.lens (\PipelineExecutionStepMetadata' {lambda} -> lambda) (\s@PipelineExecutionStepMetadata' {} a -> s {lambda = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the tuning job that was run by this
-- step execution.
pipelineExecutionStepMetadata_tuningJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TuningJobStepMetaData)
pipelineExecutionStepMetadata_tuningJob = Lens.lens (\PipelineExecutionStepMetadata' {tuningJob} -> tuningJob) (\s@PipelineExecutionStepMetadata' {} a -> s {tuningJob = a} :: PipelineExecutionStepMetadata)

-- | The outcome of the condition evaluation that was run by this step
-- execution.
pipelineExecutionStepMetadata_condition :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ConditionStepMetadata)
pipelineExecutionStepMetadata_condition = Lens.lens (\PipelineExecutionStepMetadata' {condition} -> condition) (\s@PipelineExecutionStepMetadata' {} a -> s {condition = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
pipelineExecutionStepMetadata_transformJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TransformJobStepMetadata)
pipelineExecutionStepMetadata_transformJob = Lens.lens (\PipelineExecutionStepMetadata' {transformJob} -> transformJob) (\s@PipelineExecutionStepMetadata' {} a -> s {transformJob = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the model package the model was
-- registered to by this step execution.
pipelineExecutionStepMetadata_registerModel :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe RegisterModelStepMetadata)
pipelineExecutionStepMetadata_registerModel = Lens.lens (\PipelineExecutionStepMetadata' {registerModel} -> registerModel) (\s@PipelineExecutionStepMetadata' {} a -> s {registerModel = a} :: PipelineExecutionStepMetadata)

-- | The URL of the Amazon SQS queue used by this step execution, the
-- pipeline generated token, and a list of output parameters.
pipelineExecutionStepMetadata_callback :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe CallbackStepMetadata)
pipelineExecutionStepMetadata_callback = Lens.lens (\PipelineExecutionStepMetadata' {callback} -> callback) (\s@PipelineExecutionStepMetadata' {} a -> s {callback = a} :: PipelineExecutionStepMetadata)

instance Core.FromJSON PipelineExecutionStepMetadata where
  parseJSON =
    Core.withObject
      "PipelineExecutionStepMetadata"
      ( \x ->
          PipelineExecutionStepMetadata'
            Prelude.<$> (x Core..:? "TrainingJob")
            Prelude.<*> (x Core..:? "ProcessingJob")
            Prelude.<*> (x Core..:? "Model")
            Prelude.<*> (x Core..:? "Lambda")
            Prelude.<*> (x Core..:? "TuningJob")
            Prelude.<*> (x Core..:? "Condition")
            Prelude.<*> (x Core..:? "TransformJob")
            Prelude.<*> (x Core..:? "RegisterModel")
            Prelude.<*> (x Core..:? "Callback")
      )

instance
  Prelude.Hashable
    PipelineExecutionStepMetadata
  where
  hashWithSalt salt' PipelineExecutionStepMetadata' {..} =
    salt' `Prelude.hashWithSalt` callback
      `Prelude.hashWithSalt` registerModel
      `Prelude.hashWithSalt` transformJob
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` tuningJob
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` processingJob
      `Prelude.hashWithSalt` trainingJob

instance Prelude.NFData PipelineExecutionStepMetadata where
  rnf PipelineExecutionStepMetadata' {..} =
    Prelude.rnf trainingJob
      `Prelude.seq` Prelude.rnf callback
      `Prelude.seq` Prelude.rnf registerModel
      `Prelude.seq` Prelude.rnf transformJob
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf tuningJob
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf processingJob
