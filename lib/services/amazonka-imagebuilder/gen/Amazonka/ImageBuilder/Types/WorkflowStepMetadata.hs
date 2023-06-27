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
-- Module      : Amazonka.ImageBuilder.Types.WorkflowStepMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.WorkflowStepMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types.WorkflowStepExecutionRollbackStatus
import Amazonka.ImageBuilder.Types.WorkflowStepExecutionStatus
import qualified Amazonka.Prelude as Prelude

-- | Runtime details and status for the workflow step.
--
-- /See:/ 'newWorkflowStepMetadata' smart constructor.
data WorkflowStepMetadata = WorkflowStepMetadata'
  { -- | The step action name.
    action :: Prelude.Maybe Prelude.Text,
    -- | Description of the workflow step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the workflow step finished.
    endTime :: Prelude.Maybe Prelude.Text,
    -- | Input parameters that Image Builder provides for the workflow step.
    inputs :: Prelude.Maybe Prelude.Text,
    -- | Detailed output message that the workflow step provides at runtime.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the workflow step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The file names that the workflow step created as output for this runtime
    -- instance of the workflow.
    outputs :: Prelude.Maybe Prelude.Text,
    -- | Reports on the rollback status of the step, if applicable.
    rollbackStatus :: Prelude.Maybe WorkflowStepExecutionRollbackStatus,
    -- | The timestamp when the workflow step started.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | Runtime status for the workflow step.
    status :: Prelude.Maybe WorkflowStepExecutionStatus,
    -- | A unique identifier for the workflow step, assigned at runtime.
    stepExecutionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowStepMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'workflowStepMetadata_action' - The step action name.
--
-- 'description', 'workflowStepMetadata_description' - Description of the workflow step.
--
-- 'endTime', 'workflowStepMetadata_endTime' - The timestamp when the workflow step finished.
--
-- 'inputs', 'workflowStepMetadata_inputs' - Input parameters that Image Builder provides for the workflow step.
--
-- 'message', 'workflowStepMetadata_message' - Detailed output message that the workflow step provides at runtime.
--
-- 'name', 'workflowStepMetadata_name' - The name of the workflow step.
--
-- 'outputs', 'workflowStepMetadata_outputs' - The file names that the workflow step created as output for this runtime
-- instance of the workflow.
--
-- 'rollbackStatus', 'workflowStepMetadata_rollbackStatus' - Reports on the rollback status of the step, if applicable.
--
-- 'startTime', 'workflowStepMetadata_startTime' - The timestamp when the workflow step started.
--
-- 'status', 'workflowStepMetadata_status' - Runtime status for the workflow step.
--
-- 'stepExecutionId', 'workflowStepMetadata_stepExecutionId' - A unique identifier for the workflow step, assigned at runtime.
newWorkflowStepMetadata ::
  WorkflowStepMetadata
newWorkflowStepMetadata =
  WorkflowStepMetadata'
    { action = Prelude.Nothing,
      description = Prelude.Nothing,
      endTime = Prelude.Nothing,
      inputs = Prelude.Nothing,
      message = Prelude.Nothing,
      name = Prelude.Nothing,
      outputs = Prelude.Nothing,
      rollbackStatus = Prelude.Nothing,
      startTime = Prelude.Nothing,
      status = Prelude.Nothing,
      stepExecutionId = Prelude.Nothing
    }

-- | The step action name.
workflowStepMetadata_action :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_action = Lens.lens (\WorkflowStepMetadata' {action} -> action) (\s@WorkflowStepMetadata' {} a -> s {action = a} :: WorkflowStepMetadata)

-- | Description of the workflow step.
workflowStepMetadata_description :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_description = Lens.lens (\WorkflowStepMetadata' {description} -> description) (\s@WorkflowStepMetadata' {} a -> s {description = a} :: WorkflowStepMetadata)

-- | The timestamp when the workflow step finished.
workflowStepMetadata_endTime :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_endTime = Lens.lens (\WorkflowStepMetadata' {endTime} -> endTime) (\s@WorkflowStepMetadata' {} a -> s {endTime = a} :: WorkflowStepMetadata)

-- | Input parameters that Image Builder provides for the workflow step.
workflowStepMetadata_inputs :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_inputs = Lens.lens (\WorkflowStepMetadata' {inputs} -> inputs) (\s@WorkflowStepMetadata' {} a -> s {inputs = a} :: WorkflowStepMetadata)

-- | Detailed output message that the workflow step provides at runtime.
workflowStepMetadata_message :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_message = Lens.lens (\WorkflowStepMetadata' {message} -> message) (\s@WorkflowStepMetadata' {} a -> s {message = a} :: WorkflowStepMetadata)

-- | The name of the workflow step.
workflowStepMetadata_name :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_name = Lens.lens (\WorkflowStepMetadata' {name} -> name) (\s@WorkflowStepMetadata' {} a -> s {name = a} :: WorkflowStepMetadata)

-- | The file names that the workflow step created as output for this runtime
-- instance of the workflow.
workflowStepMetadata_outputs :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_outputs = Lens.lens (\WorkflowStepMetadata' {outputs} -> outputs) (\s@WorkflowStepMetadata' {} a -> s {outputs = a} :: WorkflowStepMetadata)

-- | Reports on the rollback status of the step, if applicable.
workflowStepMetadata_rollbackStatus :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe WorkflowStepExecutionRollbackStatus)
workflowStepMetadata_rollbackStatus = Lens.lens (\WorkflowStepMetadata' {rollbackStatus} -> rollbackStatus) (\s@WorkflowStepMetadata' {} a -> s {rollbackStatus = a} :: WorkflowStepMetadata)

-- | The timestamp when the workflow step started.
workflowStepMetadata_startTime :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_startTime = Lens.lens (\WorkflowStepMetadata' {startTime} -> startTime) (\s@WorkflowStepMetadata' {} a -> s {startTime = a} :: WorkflowStepMetadata)

-- | Runtime status for the workflow step.
workflowStepMetadata_status :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe WorkflowStepExecutionStatus)
workflowStepMetadata_status = Lens.lens (\WorkflowStepMetadata' {status} -> status) (\s@WorkflowStepMetadata' {} a -> s {status = a} :: WorkflowStepMetadata)

-- | A unique identifier for the workflow step, assigned at runtime.
workflowStepMetadata_stepExecutionId :: Lens.Lens' WorkflowStepMetadata (Prelude.Maybe Prelude.Text)
workflowStepMetadata_stepExecutionId = Lens.lens (\WorkflowStepMetadata' {stepExecutionId} -> stepExecutionId) (\s@WorkflowStepMetadata' {} a -> s {stepExecutionId = a} :: WorkflowStepMetadata)

instance Data.FromJSON WorkflowStepMetadata where
  parseJSON =
    Data.withObject
      "WorkflowStepMetadata"
      ( \x ->
          WorkflowStepMetadata'
            Prelude.<$> (x Data..:? "action")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "endTime")
            Prelude.<*> (x Data..:? "inputs")
            Prelude.<*> (x Data..:? "message")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "outputs")
            Prelude.<*> (x Data..:? "rollbackStatus")
            Prelude.<*> (x Data..:? "startTime")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "stepExecutionId")
      )

instance Prelude.Hashable WorkflowStepMetadata where
  hashWithSalt _salt WorkflowStepMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` inputs
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` rollbackStatus
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stepExecutionId

instance Prelude.NFData WorkflowStepMetadata where
  rnf WorkflowStepMetadata' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputs
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf rollbackStatus
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stepExecutionId
