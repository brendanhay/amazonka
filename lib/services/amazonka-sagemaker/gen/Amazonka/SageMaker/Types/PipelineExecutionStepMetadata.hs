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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.PipelineExecutionStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobStepMetadata
import Amazonka.SageMaker.Types.CallbackStepMetadata
import Amazonka.SageMaker.Types.ClarifyCheckStepMetadata
import Amazonka.SageMaker.Types.ConditionStepMetadata
import Amazonka.SageMaker.Types.EMRStepMetadata
import Amazonka.SageMaker.Types.FailStepMetadata
import Amazonka.SageMaker.Types.LambdaStepMetadata
import Amazonka.SageMaker.Types.ModelStepMetadata
import Amazonka.SageMaker.Types.ProcessingJobStepMetadata
import Amazonka.SageMaker.Types.QualityCheckStepMetadata
import Amazonka.SageMaker.Types.RegisterModelStepMetadata
import Amazonka.SageMaker.Types.TrainingJobStepMetadata
import Amazonka.SageMaker.Types.TransformJobStepMetadata
import Amazonka.SageMaker.Types.TuningJobStepMetaData

-- | Metadata for a step execution.
--
-- /See:/ 'newPipelineExecutionStepMetadata' smart constructor.
data PipelineExecutionStepMetadata = PipelineExecutionStepMetadata'
  { -- | The Amazon Resource Name (ARN) of the AutoML job that was run by this
    -- step.
    autoMLJob :: Prelude.Maybe AutoMLJobStepMetadata,
    -- | The URL of the Amazon SQS queue used by this step execution, the
    -- pipeline generated token, and a list of output parameters.
    callback :: Prelude.Maybe CallbackStepMetadata,
    -- | Container for the metadata for a Clarify check step. The configurations
    -- and outcomes of the check step execution. This includes:
    --
    -- -   The type of the check conducted,
    --
    -- -   The Amazon S3 URIs of baseline constraints and statistics files to
    --     be used for the drift check.
    --
    -- -   The Amazon S3 URIs of newly calculated baseline constraints and
    --     statistics.
    --
    -- -   The model package group name provided.
    --
    -- -   The Amazon S3 URI of the violation report if violations detected.
    --
    -- -   The Amazon Resource Name (ARN) of check processing job initiated by
    --     the step execution.
    --
    -- -   The boolean flags indicating if the drift check is skipped.
    --
    -- -   If step property @BaselineUsedForDriftCheck@ is set the same as
    --     @CalculatedBaseline@.
    clarifyCheck :: Prelude.Maybe ClarifyCheckStepMetadata,
    -- | The outcome of the condition evaluation that was run by this step
    -- execution.
    condition :: Prelude.Maybe ConditionStepMetadata,
    -- | The configurations and outcomes of an Amazon EMR step execution.
    emr :: Prelude.Maybe EMRStepMetadata,
    -- | The configurations and outcomes of a Fail step execution.
    fail :: Prelude.Maybe FailStepMetadata,
    -- | The Amazon Resource Name (ARN) of the Lambda function that was run by
    -- this step execution and a list of output parameters.
    lambda :: Prelude.Maybe LambdaStepMetadata,
    -- | The Amazon Resource Name (ARN) of the model that was created by this
    -- step execution.
    model :: Prelude.Maybe ModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the processing job that was run by
    -- this step execution.
    processingJob :: Prelude.Maybe ProcessingJobStepMetadata,
    -- | The configurations and outcomes of the check step execution. This
    -- includes:
    --
    -- -   The type of the check conducted.
    --
    -- -   The Amazon S3 URIs of baseline constraints and statistics files to
    --     be used for the drift check.
    --
    -- -   The Amazon S3 URIs of newly calculated baseline constraints and
    --     statistics.
    --
    -- -   The model package group name provided.
    --
    -- -   The Amazon S3 URI of the violation report if violations detected.
    --
    -- -   The Amazon Resource Name (ARN) of check processing job initiated by
    --     the step execution.
    --
    -- -   The Boolean flags indicating if the drift check is skipped.
    --
    -- -   If step property @BaselineUsedForDriftCheck@ is set the same as
    --     @CalculatedBaseline@.
    qualityCheck :: Prelude.Maybe QualityCheckStepMetadata,
    -- | The Amazon Resource Name (ARN) of the model package that the model was
    -- registered to by this step execution.
    registerModel :: Prelude.Maybe RegisterModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the training job that was run by this
    -- step execution.
    trainingJob :: Prelude.Maybe TrainingJobStepMetadata,
    -- | The Amazon Resource Name (ARN) of the transform job that was run by this
    -- step execution.
    transformJob :: Prelude.Maybe TransformJobStepMetadata,
    -- | The Amazon Resource Name (ARN) of the tuning job that was run by this
    -- step execution.
    tuningJob :: Prelude.Maybe TuningJobStepMetaData
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
-- 'autoMLJob', 'pipelineExecutionStepMetadata_autoMLJob' - The Amazon Resource Name (ARN) of the AutoML job that was run by this
-- step.
--
-- 'callback', 'pipelineExecutionStepMetadata_callback' - The URL of the Amazon SQS queue used by this step execution, the
-- pipeline generated token, and a list of output parameters.
--
-- 'clarifyCheck', 'pipelineExecutionStepMetadata_clarifyCheck' - Container for the metadata for a Clarify check step. The configurations
-- and outcomes of the check step execution. This includes:
--
-- -   The type of the check conducted,
--
-- -   The Amazon S3 URIs of baseline constraints and statistics files to
--     be used for the drift check.
--
-- -   The Amazon S3 URIs of newly calculated baseline constraints and
--     statistics.
--
-- -   The model package group name provided.
--
-- -   The Amazon S3 URI of the violation report if violations detected.
--
-- -   The Amazon Resource Name (ARN) of check processing job initiated by
--     the step execution.
--
-- -   The boolean flags indicating if the drift check is skipped.
--
-- -   If step property @BaselineUsedForDriftCheck@ is set the same as
--     @CalculatedBaseline@.
--
-- 'condition', 'pipelineExecutionStepMetadata_condition' - The outcome of the condition evaluation that was run by this step
-- execution.
--
-- 'emr', 'pipelineExecutionStepMetadata_emr' - The configurations and outcomes of an Amazon EMR step execution.
--
-- 'fail', 'pipelineExecutionStepMetadata_fail' - The configurations and outcomes of a Fail step execution.
--
-- 'lambda', 'pipelineExecutionStepMetadata_lambda' - The Amazon Resource Name (ARN) of the Lambda function that was run by
-- this step execution and a list of output parameters.
--
-- 'model', 'pipelineExecutionStepMetadata_model' - The Amazon Resource Name (ARN) of the model that was created by this
-- step execution.
--
-- 'processingJob', 'pipelineExecutionStepMetadata_processingJob' - The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
--
-- 'qualityCheck', 'pipelineExecutionStepMetadata_qualityCheck' - The configurations and outcomes of the check step execution. This
-- includes:
--
-- -   The type of the check conducted.
--
-- -   The Amazon S3 URIs of baseline constraints and statistics files to
--     be used for the drift check.
--
-- -   The Amazon S3 URIs of newly calculated baseline constraints and
--     statistics.
--
-- -   The model package group name provided.
--
-- -   The Amazon S3 URI of the violation report if violations detected.
--
-- -   The Amazon Resource Name (ARN) of check processing job initiated by
--     the step execution.
--
-- -   The Boolean flags indicating if the drift check is skipped.
--
-- -   If step property @BaselineUsedForDriftCheck@ is set the same as
--     @CalculatedBaseline@.
--
-- 'registerModel', 'pipelineExecutionStepMetadata_registerModel' - The Amazon Resource Name (ARN) of the model package that the model was
-- registered to by this step execution.
--
-- 'trainingJob', 'pipelineExecutionStepMetadata_trainingJob' - The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
--
-- 'transformJob', 'pipelineExecutionStepMetadata_transformJob' - The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
--
-- 'tuningJob', 'pipelineExecutionStepMetadata_tuningJob' - The Amazon Resource Name (ARN) of the tuning job that was run by this
-- step execution.
newPipelineExecutionStepMetadata ::
  PipelineExecutionStepMetadata
newPipelineExecutionStepMetadata =
  PipelineExecutionStepMetadata'
    { autoMLJob =
        Prelude.Nothing,
      callback = Prelude.Nothing,
      clarifyCheck = Prelude.Nothing,
      condition = Prelude.Nothing,
      emr = Prelude.Nothing,
      fail = Prelude.Nothing,
      lambda = Prelude.Nothing,
      model = Prelude.Nothing,
      processingJob = Prelude.Nothing,
      qualityCheck = Prelude.Nothing,
      registerModel = Prelude.Nothing,
      trainingJob = Prelude.Nothing,
      transformJob = Prelude.Nothing,
      tuningJob = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AutoML job that was run by this
-- step.
pipelineExecutionStepMetadata_autoMLJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe AutoMLJobStepMetadata)
pipelineExecutionStepMetadata_autoMLJob = Lens.lens (\PipelineExecutionStepMetadata' {autoMLJob} -> autoMLJob) (\s@PipelineExecutionStepMetadata' {} a -> s {autoMLJob = a} :: PipelineExecutionStepMetadata)

-- | The URL of the Amazon SQS queue used by this step execution, the
-- pipeline generated token, and a list of output parameters.
pipelineExecutionStepMetadata_callback :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe CallbackStepMetadata)
pipelineExecutionStepMetadata_callback = Lens.lens (\PipelineExecutionStepMetadata' {callback} -> callback) (\s@PipelineExecutionStepMetadata' {} a -> s {callback = a} :: PipelineExecutionStepMetadata)

-- | Container for the metadata for a Clarify check step. The configurations
-- and outcomes of the check step execution. This includes:
--
-- -   The type of the check conducted,
--
-- -   The Amazon S3 URIs of baseline constraints and statistics files to
--     be used for the drift check.
--
-- -   The Amazon S3 URIs of newly calculated baseline constraints and
--     statistics.
--
-- -   The model package group name provided.
--
-- -   The Amazon S3 URI of the violation report if violations detected.
--
-- -   The Amazon Resource Name (ARN) of check processing job initiated by
--     the step execution.
--
-- -   The boolean flags indicating if the drift check is skipped.
--
-- -   If step property @BaselineUsedForDriftCheck@ is set the same as
--     @CalculatedBaseline@.
pipelineExecutionStepMetadata_clarifyCheck :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ClarifyCheckStepMetadata)
pipelineExecutionStepMetadata_clarifyCheck = Lens.lens (\PipelineExecutionStepMetadata' {clarifyCheck} -> clarifyCheck) (\s@PipelineExecutionStepMetadata' {} a -> s {clarifyCheck = a} :: PipelineExecutionStepMetadata)

-- | The outcome of the condition evaluation that was run by this step
-- execution.
pipelineExecutionStepMetadata_condition :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ConditionStepMetadata)
pipelineExecutionStepMetadata_condition = Lens.lens (\PipelineExecutionStepMetadata' {condition} -> condition) (\s@PipelineExecutionStepMetadata' {} a -> s {condition = a} :: PipelineExecutionStepMetadata)

-- | The configurations and outcomes of an Amazon EMR step execution.
pipelineExecutionStepMetadata_emr :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe EMRStepMetadata)
pipelineExecutionStepMetadata_emr = Lens.lens (\PipelineExecutionStepMetadata' {emr} -> emr) (\s@PipelineExecutionStepMetadata' {} a -> s {emr = a} :: PipelineExecutionStepMetadata)

-- | The configurations and outcomes of a Fail step execution.
pipelineExecutionStepMetadata_fail :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe FailStepMetadata)
pipelineExecutionStepMetadata_fail = Lens.lens (\PipelineExecutionStepMetadata' {fail} -> fail) (\s@PipelineExecutionStepMetadata' {} a -> s {fail = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the Lambda function that was run by
-- this step execution and a list of output parameters.
pipelineExecutionStepMetadata_lambda :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe LambdaStepMetadata)
pipelineExecutionStepMetadata_lambda = Lens.lens (\PipelineExecutionStepMetadata' {lambda} -> lambda) (\s@PipelineExecutionStepMetadata' {} a -> s {lambda = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the model that was created by this
-- step execution.
pipelineExecutionStepMetadata_model :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ModelStepMetadata)
pipelineExecutionStepMetadata_model = Lens.lens (\PipelineExecutionStepMetadata' {model} -> model) (\s@PipelineExecutionStepMetadata' {} a -> s {model = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
pipelineExecutionStepMetadata_processingJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ProcessingJobStepMetadata)
pipelineExecutionStepMetadata_processingJob = Lens.lens (\PipelineExecutionStepMetadata' {processingJob} -> processingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {processingJob = a} :: PipelineExecutionStepMetadata)

-- | The configurations and outcomes of the check step execution. This
-- includes:
--
-- -   The type of the check conducted.
--
-- -   The Amazon S3 URIs of baseline constraints and statistics files to
--     be used for the drift check.
--
-- -   The Amazon S3 URIs of newly calculated baseline constraints and
--     statistics.
--
-- -   The model package group name provided.
--
-- -   The Amazon S3 URI of the violation report if violations detected.
--
-- -   The Amazon Resource Name (ARN) of check processing job initiated by
--     the step execution.
--
-- -   The Boolean flags indicating if the drift check is skipped.
--
-- -   If step property @BaselineUsedForDriftCheck@ is set the same as
--     @CalculatedBaseline@.
pipelineExecutionStepMetadata_qualityCheck :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe QualityCheckStepMetadata)
pipelineExecutionStepMetadata_qualityCheck = Lens.lens (\PipelineExecutionStepMetadata' {qualityCheck} -> qualityCheck) (\s@PipelineExecutionStepMetadata' {} a -> s {qualityCheck = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the model package that the model was
-- registered to by this step execution.
pipelineExecutionStepMetadata_registerModel :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe RegisterModelStepMetadata)
pipelineExecutionStepMetadata_registerModel = Lens.lens (\PipelineExecutionStepMetadata' {registerModel} -> registerModel) (\s@PipelineExecutionStepMetadata' {} a -> s {registerModel = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
pipelineExecutionStepMetadata_trainingJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TrainingJobStepMetadata)
pipelineExecutionStepMetadata_trainingJob = Lens.lens (\PipelineExecutionStepMetadata' {trainingJob} -> trainingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {trainingJob = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
pipelineExecutionStepMetadata_transformJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TransformJobStepMetadata)
pipelineExecutionStepMetadata_transformJob = Lens.lens (\PipelineExecutionStepMetadata' {transformJob} -> transformJob) (\s@PipelineExecutionStepMetadata' {} a -> s {transformJob = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the tuning job that was run by this
-- step execution.
pipelineExecutionStepMetadata_tuningJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TuningJobStepMetaData)
pipelineExecutionStepMetadata_tuningJob = Lens.lens (\PipelineExecutionStepMetadata' {tuningJob} -> tuningJob) (\s@PipelineExecutionStepMetadata' {} a -> s {tuningJob = a} :: PipelineExecutionStepMetadata)

instance Data.FromJSON PipelineExecutionStepMetadata where
  parseJSON =
    Data.withObject
      "PipelineExecutionStepMetadata"
      ( \x ->
          PipelineExecutionStepMetadata'
            Prelude.<$> (x Data..:? "AutoMLJob")
            Prelude.<*> (x Data..:? "Callback")
            Prelude.<*> (x Data..:? "ClarifyCheck")
            Prelude.<*> (x Data..:? "Condition")
            Prelude.<*> (x Data..:? "EMR")
            Prelude.<*> (x Data..:? "Fail")
            Prelude.<*> (x Data..:? "Lambda")
            Prelude.<*> (x Data..:? "Model")
            Prelude.<*> (x Data..:? "ProcessingJob")
            Prelude.<*> (x Data..:? "QualityCheck")
            Prelude.<*> (x Data..:? "RegisterModel")
            Prelude.<*> (x Data..:? "TrainingJob")
            Prelude.<*> (x Data..:? "TransformJob")
            Prelude.<*> (x Data..:? "TuningJob")
      )

instance
  Prelude.Hashable
    PipelineExecutionStepMetadata
  where
  hashWithSalt _salt PipelineExecutionStepMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` autoMLJob
      `Prelude.hashWithSalt` callback
      `Prelude.hashWithSalt` clarifyCheck
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` emr
      `Prelude.hashWithSalt` fail
      `Prelude.hashWithSalt` lambda
      `Prelude.hashWithSalt` model
      `Prelude.hashWithSalt` processingJob
      `Prelude.hashWithSalt` qualityCheck
      `Prelude.hashWithSalt` registerModel
      `Prelude.hashWithSalt` trainingJob
      `Prelude.hashWithSalt` transformJob
      `Prelude.hashWithSalt` tuningJob

instance Prelude.NFData PipelineExecutionStepMetadata where
  rnf PipelineExecutionStepMetadata' {..} =
    Prelude.rnf autoMLJob
      `Prelude.seq` Prelude.rnf callback
      `Prelude.seq` Prelude.rnf clarifyCheck
      `Prelude.seq` Prelude.rnf condition
      `Prelude.seq` Prelude.rnf emr
      `Prelude.seq` Prelude.rnf fail
      `Prelude.seq` Prelude.rnf lambda
      `Prelude.seq` Prelude.rnf model
      `Prelude.seq` Prelude.rnf processingJob
      `Prelude.seq` Prelude.rnf qualityCheck
      `Prelude.seq` Prelude.rnf registerModel
      `Prelude.seq` Prelude.rnf trainingJob
      `Prelude.seq` Prelude.rnf transformJob
      `Prelude.seq` Prelude.rnf tuningJob
