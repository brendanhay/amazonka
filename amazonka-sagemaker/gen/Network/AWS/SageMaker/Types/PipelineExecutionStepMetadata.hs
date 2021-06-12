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
-- Module      : Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SageMaker.Types.ConditionStepMetadata
import Network.AWS.SageMaker.Types.ModelStepMetadata
import Network.AWS.SageMaker.Types.ProcessingJobStepMetadata
import Network.AWS.SageMaker.Types.RegisterModelStepMetadata
import Network.AWS.SageMaker.Types.TrainingJobStepMetadata
import Network.AWS.SageMaker.Types.TransformJobStepMetadata

-- | Metadata for a step execution.
--
-- /See:/ 'newPipelineExecutionStepMetadata' smart constructor.
data PipelineExecutionStepMetadata = PipelineExecutionStepMetadata'
  { -- | Metadata for the Model step.
    model :: Core.Maybe ModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the processing job that was run by
    -- this step execution.
    processingJob :: Core.Maybe ProcessingJobStepMetadata,
    -- | If this is a Condition step metadata object, details on the condition.
    condition :: Core.Maybe ConditionStepMetadata,
    -- | The Amazon Resource Name (ARN) of the training job that was run by this
    -- step execution.
    trainingJob :: Core.Maybe TrainingJobStepMetadata,
    -- | Metadata for the RegisterModel step.
    registerModel :: Core.Maybe RegisterModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the transform job that was run by this
    -- step execution.
    transformJob :: Core.Maybe TransformJobStepMetadata
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PipelineExecutionStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'model', 'pipelineExecutionStepMetadata_model' - Metadata for the Model step.
--
-- 'processingJob', 'pipelineExecutionStepMetadata_processingJob' - The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
--
-- 'condition', 'pipelineExecutionStepMetadata_condition' - If this is a Condition step metadata object, details on the condition.
--
-- 'trainingJob', 'pipelineExecutionStepMetadata_trainingJob' - The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
--
-- 'registerModel', 'pipelineExecutionStepMetadata_registerModel' - Metadata for the RegisterModel step.
--
-- 'transformJob', 'pipelineExecutionStepMetadata_transformJob' - The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
newPipelineExecutionStepMetadata ::
  PipelineExecutionStepMetadata
newPipelineExecutionStepMetadata =
  PipelineExecutionStepMetadata'
    { model =
        Core.Nothing,
      processingJob = Core.Nothing,
      condition = Core.Nothing,
      trainingJob = Core.Nothing,
      registerModel = Core.Nothing,
      transformJob = Core.Nothing
    }

-- | Metadata for the Model step.
pipelineExecutionStepMetadata_model :: Lens.Lens' PipelineExecutionStepMetadata (Core.Maybe ModelStepMetadata)
pipelineExecutionStepMetadata_model = Lens.lens (\PipelineExecutionStepMetadata' {model} -> model) (\s@PipelineExecutionStepMetadata' {} a -> s {model = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
pipelineExecutionStepMetadata_processingJob :: Lens.Lens' PipelineExecutionStepMetadata (Core.Maybe ProcessingJobStepMetadata)
pipelineExecutionStepMetadata_processingJob = Lens.lens (\PipelineExecutionStepMetadata' {processingJob} -> processingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {processingJob = a} :: PipelineExecutionStepMetadata)

-- | If this is a Condition step metadata object, details on the condition.
pipelineExecutionStepMetadata_condition :: Lens.Lens' PipelineExecutionStepMetadata (Core.Maybe ConditionStepMetadata)
pipelineExecutionStepMetadata_condition = Lens.lens (\PipelineExecutionStepMetadata' {condition} -> condition) (\s@PipelineExecutionStepMetadata' {} a -> s {condition = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
pipelineExecutionStepMetadata_trainingJob :: Lens.Lens' PipelineExecutionStepMetadata (Core.Maybe TrainingJobStepMetadata)
pipelineExecutionStepMetadata_trainingJob = Lens.lens (\PipelineExecutionStepMetadata' {trainingJob} -> trainingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {trainingJob = a} :: PipelineExecutionStepMetadata)

-- | Metadata for the RegisterModel step.
pipelineExecutionStepMetadata_registerModel :: Lens.Lens' PipelineExecutionStepMetadata (Core.Maybe RegisterModelStepMetadata)
pipelineExecutionStepMetadata_registerModel = Lens.lens (\PipelineExecutionStepMetadata' {registerModel} -> registerModel) (\s@PipelineExecutionStepMetadata' {} a -> s {registerModel = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
pipelineExecutionStepMetadata_transformJob :: Lens.Lens' PipelineExecutionStepMetadata (Core.Maybe TransformJobStepMetadata)
pipelineExecutionStepMetadata_transformJob = Lens.lens (\PipelineExecutionStepMetadata' {transformJob} -> transformJob) (\s@PipelineExecutionStepMetadata' {} a -> s {transformJob = a} :: PipelineExecutionStepMetadata)

instance Core.FromJSON PipelineExecutionStepMetadata where
  parseJSON =
    Core.withObject
      "PipelineExecutionStepMetadata"
      ( \x ->
          PipelineExecutionStepMetadata'
            Core.<$> (x Core..:? "Model")
            Core.<*> (x Core..:? "ProcessingJob")
            Core.<*> (x Core..:? "Condition")
            Core.<*> (x Core..:? "TrainingJob")
            Core.<*> (x Core..:? "RegisterModel")
            Core.<*> (x Core..:? "TransformJob")
      )

instance Core.Hashable PipelineExecutionStepMetadata

instance Core.NFData PipelineExecutionStepMetadata
