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
-- Module      : Amazonka.ImageBuilder.Types.WorkflowExecutionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.WorkflowExecutionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.WorkflowExecutionStatus
import Amazonka.ImageBuilder.Types.WorkflowType
import qualified Amazonka.Prelude as Prelude

-- | Metadata that includes details and status from this runtime instance of
-- the workflow.
--
-- /See:/ 'newWorkflowExecutionMetadata' smart constructor.
data WorkflowExecutionMetadata = WorkflowExecutionMetadata'
  { -- | The timestamp when this runtime instance of the workflow finished.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | The runtime output message from the workflow, if applicable.
    message :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the runtime instance of this workflow started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The current runtime status for this workflow.
    status :: Prelude.Maybe WorkflowExecutionStatus,
    -- | The total number of steps in the workflow. This should equal the sum of
    -- the step counts for steps that succeeded, were skipped, and failed.
    totalStepCount :: Prelude.Maybe Prelude.Int,
    -- | A runtime count for the number of steps in the workflow that failed.
    totalStepsFailed :: Prelude.Maybe Prelude.Int,
    -- | A runtime count for the number of steps in the workflow that were
    -- skipped.
    totalStepsSkipped :: Prelude.Maybe Prelude.Int,
    -- | A runtime count for the number of steps in the workflow that ran
    -- successfully.
    totalStepsSucceeded :: Prelude.Maybe Prelude.Int,
    -- | Indicates what type of workflow that Image Builder ran for this runtime
    -- instance of the workflow.
    type' :: Prelude.Maybe WorkflowType,
    -- | The Amazon Resource Name (ARN) of the workflow resource build version
    -- that ran.
    workflowBuildVersionArn :: Prelude.Maybe Prelude.Text,
    -- | Unique identifier that Image Builder assigns to keep track of runtime
    -- resources each time it runs a workflow.
    workflowExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'workflowExecutionMetadata_endTime' - The timestamp when this runtime instance of the workflow finished.
--
-- 'message', 'workflowExecutionMetadata_message' - The runtime output message from the workflow, if applicable.
--
-- 'startTime', 'workflowExecutionMetadata_startTime' - The timestamp when the runtime instance of this workflow started.
--
-- 'status', 'workflowExecutionMetadata_status' - The current runtime status for this workflow.
--
-- 'totalStepCount', 'workflowExecutionMetadata_totalStepCount' - The total number of steps in the workflow. This should equal the sum of
-- the step counts for steps that succeeded, were skipped, and failed.
--
-- 'totalStepsFailed', 'workflowExecutionMetadata_totalStepsFailed' - A runtime count for the number of steps in the workflow that failed.
--
-- 'totalStepsSkipped', 'workflowExecutionMetadata_totalStepsSkipped' - A runtime count for the number of steps in the workflow that were
-- skipped.
--
-- 'totalStepsSucceeded', 'workflowExecutionMetadata_totalStepsSucceeded' - A runtime count for the number of steps in the workflow that ran
-- successfully.
--
-- 'type'', 'workflowExecutionMetadata_type' - Indicates what type of workflow that Image Builder ran for this runtime
-- instance of the workflow.
--
-- 'workflowBuildVersionArn', 'workflowExecutionMetadata_workflowBuildVersionArn' - The Amazon Resource Name (ARN) of the workflow resource build version
-- that ran.
--
-- 'workflowExecutionId', 'workflowExecutionMetadata_workflowExecutionId' - Unique identifier that Image Builder assigns to keep track of runtime
-- resources each time it runs a workflow.
newWorkflowExecutionMetadata ::
  WorkflowExecutionMetadata
newWorkflowExecutionMetadata =
  WorkflowExecutionMetadata'
    { endTime =
        Prelude.Nothing,
      message = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      totalStepCount = Prelude.Nothing,
      totalStepsFailed = Prelude.Nothing,
      totalStepsSkipped = Prelude.Nothing,
      totalStepsSucceeded = Prelude.Nothing,
      type' = Prelude.Nothing,
      workflowBuildVersionArn = Prelude.Nothing,
      workflowExecutionId = Prelude.Nothing
    }

-- | The timestamp when this runtime instance of the workflow finished.
workflowExecutionMetadata_endTime :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Text)
workflowExecutionMetadata_endTime = Lens.lens (\WorkflowExecutionMetadata' {endTime} -> endTime) (\s@WorkflowExecutionMetadata' {} a -> s {endTime = a} :: WorkflowExecutionMetadata)

-- | The runtime output message from the workflow, if applicable.
workflowExecutionMetadata_message :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Text)
workflowExecutionMetadata_message = Lens.lens (\WorkflowExecutionMetadata' {message} -> message) (\s@WorkflowExecutionMetadata' {} a -> s {message = a} :: WorkflowExecutionMetadata)

-- | The timestamp when the runtime instance of this workflow started.
workflowExecutionMetadata_startTime :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Text)
workflowExecutionMetadata_startTime = Lens.lens (\WorkflowExecutionMetadata' {startTime} -> startTime) (\s@WorkflowExecutionMetadata' {} a -> s {startTime = a} :: WorkflowExecutionMetadata)

-- | The current runtime status for this workflow.
workflowExecutionMetadata_status :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe WorkflowExecutionStatus)
workflowExecutionMetadata_status = Lens.lens (\WorkflowExecutionMetadata' {status} -> status) (\s@WorkflowExecutionMetadata' {} a -> s {status = a} :: WorkflowExecutionMetadata)

-- | The total number of steps in the workflow. This should equal the sum of
-- the step counts for steps that succeeded, were skipped, and failed.
workflowExecutionMetadata_totalStepCount :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Int)
workflowExecutionMetadata_totalStepCount = Lens.lens (\WorkflowExecutionMetadata' {totalStepCount} -> totalStepCount) (\s@WorkflowExecutionMetadata' {} a -> s {totalStepCount = a} :: WorkflowExecutionMetadata)

-- | A runtime count for the number of steps in the workflow that failed.
workflowExecutionMetadata_totalStepsFailed :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Int)
workflowExecutionMetadata_totalStepsFailed = Lens.lens (\WorkflowExecutionMetadata' {totalStepsFailed} -> totalStepsFailed) (\s@WorkflowExecutionMetadata' {} a -> s {totalStepsFailed = a} :: WorkflowExecutionMetadata)

-- | A runtime count for the number of steps in the workflow that were
-- skipped.
workflowExecutionMetadata_totalStepsSkipped :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Int)
workflowExecutionMetadata_totalStepsSkipped = Lens.lens (\WorkflowExecutionMetadata' {totalStepsSkipped} -> totalStepsSkipped) (\s@WorkflowExecutionMetadata' {} a -> s {totalStepsSkipped = a} :: WorkflowExecutionMetadata)

-- | A runtime count for the number of steps in the workflow that ran
-- successfully.
workflowExecutionMetadata_totalStepsSucceeded :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Int)
workflowExecutionMetadata_totalStepsSucceeded = Lens.lens (\WorkflowExecutionMetadata' {totalStepsSucceeded} -> totalStepsSucceeded) (\s@WorkflowExecutionMetadata' {} a -> s {totalStepsSucceeded = a} :: WorkflowExecutionMetadata)

-- | Indicates what type of workflow that Image Builder ran for this runtime
-- instance of the workflow.
workflowExecutionMetadata_type :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe WorkflowType)
workflowExecutionMetadata_type = Lens.lens (\WorkflowExecutionMetadata' {type'} -> type') (\s@WorkflowExecutionMetadata' {} a -> s {type' = a} :: WorkflowExecutionMetadata)

-- | The Amazon Resource Name (ARN) of the workflow resource build version
-- that ran.
workflowExecutionMetadata_workflowBuildVersionArn :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Text)
workflowExecutionMetadata_workflowBuildVersionArn = Lens.lens (\WorkflowExecutionMetadata' {workflowBuildVersionArn} -> workflowBuildVersionArn) (\s@WorkflowExecutionMetadata' {} a -> s {workflowBuildVersionArn = a} :: WorkflowExecutionMetadata)

-- | Unique identifier that Image Builder assigns to keep track of runtime
-- resources each time it runs a workflow.
workflowExecutionMetadata_workflowExecutionId :: Lens.Lens' WorkflowExecutionMetadata (Prelude.Maybe Prelude.Text)
workflowExecutionMetadata_workflowExecutionId = Lens.lens (\WorkflowExecutionMetadata' {workflowExecutionId} -> workflowExecutionId) (\s@WorkflowExecutionMetadata' {} a -> s {workflowExecutionId = a} :: WorkflowExecutionMetadata)

instance Data.FromJSON WorkflowExecutionMetadata where
  parseJSON =
    Data.withObject
      "WorkflowExecutionMetadata"
      ( \x ->
          WorkflowExecutionMetadata'
            Prelude.<$> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "totalStepCount")
            Prelude.<*> (x Data..:? "totalStepsFailed")
            Prelude.<*> (x Data..:? "totalStepsSkipped")
            Prelude.<*> (x Data..:? "totalStepsSucceeded")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "workflowBuildVersionArn")
            Prelude.<*> (x Data..:? "workflowExecutionId")
      )

instance Prelude.Hashable WorkflowExecutionMetadata where
  hashWithSalt _salt WorkflowExecutionMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` totalStepCount
      `Prelude.hashWithSalt` totalStepsFailed
      `Prelude.hashWithSalt` totalStepsSkipped
      `Prelude.hashWithSalt` totalStepsSucceeded
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` workflowBuildVersionArn
      `Prelude.hashWithSalt` workflowExecutionId

instance Prelude.NFData WorkflowExecutionMetadata where
  rnf WorkflowExecutionMetadata' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf totalStepCount
      `Prelude.seq` Prelude.rnf totalStepsFailed
      `Prelude.seq` Prelude.rnf totalStepsSkipped
      `Prelude.seq` Prelude.rnf totalStepsSucceeded
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf workflowBuildVersionArn
      `Prelude.seq` Prelude.rnf workflowExecutionId
