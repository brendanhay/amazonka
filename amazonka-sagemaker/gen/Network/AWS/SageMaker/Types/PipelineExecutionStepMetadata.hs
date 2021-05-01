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
-- Module      : Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    model :: Prelude.Maybe ModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the processing job that was run by
    -- this step execution.
    processingJob :: Prelude.Maybe ProcessingJobStepMetadata,
    -- | If this is a Condition step metadata object, details on the condition.
    condition :: Prelude.Maybe ConditionStepMetadata,
    -- | The Amazon Resource Name (ARN) of the training job that was run by this
    -- step execution.
    trainingJob :: Prelude.Maybe TrainingJobStepMetadata,
    -- | Metadata for the RegisterModel step.
    registerModel :: Prelude.Maybe RegisterModelStepMetadata,
    -- | The Amazon Resource Name (ARN) of the transform job that was run by this
    -- step execution.
    transformJob :: Prelude.Maybe TransformJobStepMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      processingJob = Prelude.Nothing,
      condition = Prelude.Nothing,
      trainingJob = Prelude.Nothing,
      registerModel = Prelude.Nothing,
      transformJob = Prelude.Nothing
    }

-- | Metadata for the Model step.
pipelineExecutionStepMetadata_model :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ModelStepMetadata)
pipelineExecutionStepMetadata_model = Lens.lens (\PipelineExecutionStepMetadata' {model} -> model) (\s@PipelineExecutionStepMetadata' {} a -> s {model = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the processing job that was run by
-- this step execution.
pipelineExecutionStepMetadata_processingJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ProcessingJobStepMetadata)
pipelineExecutionStepMetadata_processingJob = Lens.lens (\PipelineExecutionStepMetadata' {processingJob} -> processingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {processingJob = a} :: PipelineExecutionStepMetadata)

-- | If this is a Condition step metadata object, details on the condition.
pipelineExecutionStepMetadata_condition :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe ConditionStepMetadata)
pipelineExecutionStepMetadata_condition = Lens.lens (\PipelineExecutionStepMetadata' {condition} -> condition) (\s@PipelineExecutionStepMetadata' {} a -> s {condition = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the training job that was run by this
-- step execution.
pipelineExecutionStepMetadata_trainingJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TrainingJobStepMetadata)
pipelineExecutionStepMetadata_trainingJob = Lens.lens (\PipelineExecutionStepMetadata' {trainingJob} -> trainingJob) (\s@PipelineExecutionStepMetadata' {} a -> s {trainingJob = a} :: PipelineExecutionStepMetadata)

-- | Metadata for the RegisterModel step.
pipelineExecutionStepMetadata_registerModel :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe RegisterModelStepMetadata)
pipelineExecutionStepMetadata_registerModel = Lens.lens (\PipelineExecutionStepMetadata' {registerModel} -> registerModel) (\s@PipelineExecutionStepMetadata' {} a -> s {registerModel = a} :: PipelineExecutionStepMetadata)

-- | The Amazon Resource Name (ARN) of the transform job that was run by this
-- step execution.
pipelineExecutionStepMetadata_transformJob :: Lens.Lens' PipelineExecutionStepMetadata (Prelude.Maybe TransformJobStepMetadata)
pipelineExecutionStepMetadata_transformJob = Lens.lens (\PipelineExecutionStepMetadata' {transformJob} -> transformJob) (\s@PipelineExecutionStepMetadata' {} a -> s {transformJob = a} :: PipelineExecutionStepMetadata)

instance
  Prelude.FromJSON
    PipelineExecutionStepMetadata
  where
  parseJSON =
    Prelude.withObject
      "PipelineExecutionStepMetadata"
      ( \x ->
          PipelineExecutionStepMetadata'
            Prelude.<$> (x Prelude..:? "Model")
            Prelude.<*> (x Prelude..:? "ProcessingJob")
            Prelude.<*> (x Prelude..:? "Condition")
            Prelude.<*> (x Prelude..:? "TrainingJob")
            Prelude.<*> (x Prelude..:? "RegisterModel")
            Prelude.<*> (x Prelude..:? "TransformJob")
      )

instance
  Prelude.Hashable
    PipelineExecutionStepMetadata

instance Prelude.NFData PipelineExecutionStepMetadata
