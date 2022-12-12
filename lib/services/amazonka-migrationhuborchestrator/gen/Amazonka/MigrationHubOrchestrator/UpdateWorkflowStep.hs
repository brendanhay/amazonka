{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubOrchestrator.UpdateWorkflowStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a step in a migration workflow.
module Amazonka.MigrationHubOrchestrator.UpdateWorkflowStep
  ( -- * Creating a Request
    UpdateWorkflowStep (..),
    newUpdateWorkflowStep,

    -- * Request Lenses
    updateWorkflowStep_description,
    updateWorkflowStep_name,
    updateWorkflowStep_next,
    updateWorkflowStep_outputs,
    updateWorkflowStep_previous,
    updateWorkflowStep_status,
    updateWorkflowStep_stepActionType,
    updateWorkflowStep_stepTarget,
    updateWorkflowStep_workflowStepAutomationConfiguration,
    updateWorkflowStep_id,
    updateWorkflowStep_stepGroupId,
    updateWorkflowStep_workflowId,

    -- * Destructuring the Response
    UpdateWorkflowStepResponse (..),
    newUpdateWorkflowStepResponse,

    -- * Response Lenses
    updateWorkflowStepResponse_id,
    updateWorkflowStepResponse_name,
    updateWorkflowStepResponse_stepGroupId,
    updateWorkflowStepResponse_workflowId,
    updateWorkflowStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateWorkflowStep' smart constructor.
data UpdateWorkflowStep = UpdateWorkflowStep'
  { -- | The description of the step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The outputs of a step.
    outputs :: Prelude.Maybe [WorkflowStepOutput],
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The status of the step.
    status :: Prelude.Maybe StepStatus,
    -- | The action type of the step. You must run and update the status of a
    -- manual step for the workflow to continue after the completion of the
    -- step.
    stepActionType :: Prelude.Maybe StepActionType,
    -- | The servers on which a step will be run.
    stepTarget :: Prelude.Maybe [Prelude.Text],
    -- | The custom script to run tests on the source and target environments.
    workflowStepAutomationConfiguration :: Prelude.Maybe WorkflowStepAutomationConfiguration,
    -- | The ID of the step.
    id :: Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateWorkflowStep_description' - The description of the step.
--
-- 'name', 'updateWorkflowStep_name' - The name of the step.
--
-- 'next', 'updateWorkflowStep_next' - The next step.
--
-- 'outputs', 'updateWorkflowStep_outputs' - The outputs of a step.
--
-- 'previous', 'updateWorkflowStep_previous' - The previous step.
--
-- 'status', 'updateWorkflowStep_status' - The status of the step.
--
-- 'stepActionType', 'updateWorkflowStep_stepActionType' - The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
--
-- 'stepTarget', 'updateWorkflowStep_stepTarget' - The servers on which a step will be run.
--
-- 'workflowStepAutomationConfiguration', 'updateWorkflowStep_workflowStepAutomationConfiguration' - The custom script to run tests on the source and target environments.
--
-- 'id', 'updateWorkflowStep_id' - The ID of the step.
--
-- 'stepGroupId', 'updateWorkflowStep_stepGroupId' - The ID of the step group.
--
-- 'workflowId', 'updateWorkflowStep_workflowId' - The ID of the migration workflow.
newUpdateWorkflowStep ::
  -- | 'id'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  UpdateWorkflowStep
newUpdateWorkflowStep pId_ pStepGroupId_ pWorkflowId_ =
  UpdateWorkflowStep'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      next = Prelude.Nothing,
      outputs = Prelude.Nothing,
      previous = Prelude.Nothing,
      status = Prelude.Nothing,
      stepActionType = Prelude.Nothing,
      stepTarget = Prelude.Nothing,
      workflowStepAutomationConfiguration =
        Prelude.Nothing,
      id = pId_,
      stepGroupId = pStepGroupId_,
      workflowId = pWorkflowId_
    }

-- | The description of the step.
updateWorkflowStep_description :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe Prelude.Text)
updateWorkflowStep_description = Lens.lens (\UpdateWorkflowStep' {description} -> description) (\s@UpdateWorkflowStep' {} a -> s {description = a} :: UpdateWorkflowStep)

-- | The name of the step.
updateWorkflowStep_name :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe Prelude.Text)
updateWorkflowStep_name = Lens.lens (\UpdateWorkflowStep' {name} -> name) (\s@UpdateWorkflowStep' {} a -> s {name = a} :: UpdateWorkflowStep)

-- | The next step.
updateWorkflowStep_next :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe [Prelude.Text])
updateWorkflowStep_next = Lens.lens (\UpdateWorkflowStep' {next} -> next) (\s@UpdateWorkflowStep' {} a -> s {next = a} :: UpdateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The outputs of a step.
updateWorkflowStep_outputs :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe [WorkflowStepOutput])
updateWorkflowStep_outputs = Lens.lens (\UpdateWorkflowStep' {outputs} -> outputs) (\s@UpdateWorkflowStep' {} a -> s {outputs = a} :: UpdateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The previous step.
updateWorkflowStep_previous :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe [Prelude.Text])
updateWorkflowStep_previous = Lens.lens (\UpdateWorkflowStep' {previous} -> previous) (\s@UpdateWorkflowStep' {} a -> s {previous = a} :: UpdateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The status of the step.
updateWorkflowStep_status :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe StepStatus)
updateWorkflowStep_status = Lens.lens (\UpdateWorkflowStep' {status} -> status) (\s@UpdateWorkflowStep' {} a -> s {status = a} :: UpdateWorkflowStep)

-- | The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
updateWorkflowStep_stepActionType :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe StepActionType)
updateWorkflowStep_stepActionType = Lens.lens (\UpdateWorkflowStep' {stepActionType} -> stepActionType) (\s@UpdateWorkflowStep' {} a -> s {stepActionType = a} :: UpdateWorkflowStep)

-- | The servers on which a step will be run.
updateWorkflowStep_stepTarget :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe [Prelude.Text])
updateWorkflowStep_stepTarget = Lens.lens (\UpdateWorkflowStep' {stepTarget} -> stepTarget) (\s@UpdateWorkflowStep' {} a -> s {stepTarget = a} :: UpdateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The custom script to run tests on the source and target environments.
updateWorkflowStep_workflowStepAutomationConfiguration :: Lens.Lens' UpdateWorkflowStep (Prelude.Maybe WorkflowStepAutomationConfiguration)
updateWorkflowStep_workflowStepAutomationConfiguration = Lens.lens (\UpdateWorkflowStep' {workflowStepAutomationConfiguration} -> workflowStepAutomationConfiguration) (\s@UpdateWorkflowStep' {} a -> s {workflowStepAutomationConfiguration = a} :: UpdateWorkflowStep)

-- | The ID of the step.
updateWorkflowStep_id :: Lens.Lens' UpdateWorkflowStep Prelude.Text
updateWorkflowStep_id = Lens.lens (\UpdateWorkflowStep' {id} -> id) (\s@UpdateWorkflowStep' {} a -> s {id = a} :: UpdateWorkflowStep)

-- | The ID of the step group.
updateWorkflowStep_stepGroupId :: Lens.Lens' UpdateWorkflowStep Prelude.Text
updateWorkflowStep_stepGroupId = Lens.lens (\UpdateWorkflowStep' {stepGroupId} -> stepGroupId) (\s@UpdateWorkflowStep' {} a -> s {stepGroupId = a} :: UpdateWorkflowStep)

-- | The ID of the migration workflow.
updateWorkflowStep_workflowId :: Lens.Lens' UpdateWorkflowStep Prelude.Text
updateWorkflowStep_workflowId = Lens.lens (\UpdateWorkflowStep' {workflowId} -> workflowId) (\s@UpdateWorkflowStep' {} a -> s {workflowId = a} :: UpdateWorkflowStep)

instance Core.AWSRequest UpdateWorkflowStep where
  type
    AWSResponse UpdateWorkflowStep =
      UpdateWorkflowStepResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateWorkflowStepResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "stepGroupId")
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateWorkflowStep where
  hashWithSalt _salt UpdateWorkflowStep' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stepActionType
      `Prelude.hashWithSalt` stepTarget
      `Prelude.hashWithSalt` workflowStepAutomationConfiguration
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData UpdateWorkflowStep where
  rnf UpdateWorkflowStep' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf stepActionType
      `Prelude.seq` Prelude.rnf stepTarget
      `Prelude.seq` Prelude.rnf workflowStepAutomationConfiguration
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf workflowId

instance Data.ToHeaders UpdateWorkflowStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateWorkflowStep where
  toJSON UpdateWorkflowStep' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("name" Data..=) Prelude.<$> name,
            ("next" Data..=) Prelude.<$> next,
            ("outputs" Data..=) Prelude.<$> outputs,
            ("previous" Data..=) Prelude.<$> previous,
            ("status" Data..=) Prelude.<$> status,
            ("stepActionType" Data..=)
              Prelude.<$> stepActionType,
            ("stepTarget" Data..=) Prelude.<$> stepTarget,
            ("workflowStepAutomationConfiguration" Data..=)
              Prelude.<$> workflowStepAutomationConfiguration,
            Prelude.Just ("stepGroupId" Data..= stepGroupId),
            Prelude.Just ("workflowId" Data..= workflowId)
          ]
      )

instance Data.ToPath UpdateWorkflowStep where
  toPath UpdateWorkflowStep' {..} =
    Prelude.mconcat ["/workflowstep/", Data.toBS id]

instance Data.ToQuery UpdateWorkflowStep where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateWorkflowStepResponse' smart constructor.
data UpdateWorkflowStepResponse = UpdateWorkflowStepResponse'
  { -- | The ID of the step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWorkflowStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateWorkflowStepResponse_id' - The ID of the step.
--
-- 'name', 'updateWorkflowStepResponse_name' - The name of the step.
--
-- 'stepGroupId', 'updateWorkflowStepResponse_stepGroupId' - The ID of the step group.
--
-- 'workflowId', 'updateWorkflowStepResponse_workflowId' - The ID of the migration workflow.
--
-- 'httpStatus', 'updateWorkflowStepResponse_httpStatus' - The response's http status code.
newUpdateWorkflowStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateWorkflowStepResponse
newUpdateWorkflowStepResponse pHttpStatus_ =
  UpdateWorkflowStepResponse'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the step.
updateWorkflowStepResponse_id :: Lens.Lens' UpdateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
updateWorkflowStepResponse_id = Lens.lens (\UpdateWorkflowStepResponse' {id} -> id) (\s@UpdateWorkflowStepResponse' {} a -> s {id = a} :: UpdateWorkflowStepResponse)

-- | The name of the step.
updateWorkflowStepResponse_name :: Lens.Lens' UpdateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
updateWorkflowStepResponse_name = Lens.lens (\UpdateWorkflowStepResponse' {name} -> name) (\s@UpdateWorkflowStepResponse' {} a -> s {name = a} :: UpdateWorkflowStepResponse)

-- | The ID of the step group.
updateWorkflowStepResponse_stepGroupId :: Lens.Lens' UpdateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
updateWorkflowStepResponse_stepGroupId = Lens.lens (\UpdateWorkflowStepResponse' {stepGroupId} -> stepGroupId) (\s@UpdateWorkflowStepResponse' {} a -> s {stepGroupId = a} :: UpdateWorkflowStepResponse)

-- | The ID of the migration workflow.
updateWorkflowStepResponse_workflowId :: Lens.Lens' UpdateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
updateWorkflowStepResponse_workflowId = Lens.lens (\UpdateWorkflowStepResponse' {workflowId} -> workflowId) (\s@UpdateWorkflowStepResponse' {} a -> s {workflowId = a} :: UpdateWorkflowStepResponse)

-- | The response's http status code.
updateWorkflowStepResponse_httpStatus :: Lens.Lens' UpdateWorkflowStepResponse Prelude.Int
updateWorkflowStepResponse_httpStatus = Lens.lens (\UpdateWorkflowStepResponse' {httpStatus} -> httpStatus) (\s@UpdateWorkflowStepResponse' {} a -> s {httpStatus = a} :: UpdateWorkflowStepResponse)

instance Prelude.NFData UpdateWorkflowStepResponse where
  rnf UpdateWorkflowStepResponse' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf httpStatus
