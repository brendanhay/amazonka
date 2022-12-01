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
-- Module      : Amazonka.MigrationHubOrchestrator.CreateWorkflowStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a step in the migration workflow.
module Amazonka.MigrationHubOrchestrator.CreateWorkflowStep
  ( -- * Creating a Request
    CreateWorkflowStep (..),
    newCreateWorkflowStep,

    -- * Request Lenses
    createWorkflowStep_next,
    createWorkflowStep_description,
    createWorkflowStep_workflowStepAutomationConfiguration,
    createWorkflowStep_stepTarget,
    createWorkflowStep_outputs,
    createWorkflowStep_previous,
    createWorkflowStep_name,
    createWorkflowStep_stepGroupId,
    createWorkflowStep_workflowId,
    createWorkflowStep_stepActionType,

    -- * Destructuring the Response
    CreateWorkflowStepResponse (..),
    newCreateWorkflowStepResponse,

    -- * Response Lenses
    createWorkflowStepResponse_name,
    createWorkflowStepResponse_workflowId,
    createWorkflowStepResponse_id,
    createWorkflowStepResponse_stepGroupId,
    createWorkflowStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkflowStep' smart constructor.
data CreateWorkflowStep = CreateWorkflowStep'
  { -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The description of the step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The custom script to run tests on source or target environments.
    workflowStepAutomationConfiguration :: Prelude.Maybe WorkflowStepAutomationConfiguration,
    -- | The servers on which a step will be run.
    stepTarget :: Prelude.Maybe [Prelude.Text],
    -- | The key value pairs added for the expected output.
    outputs :: Prelude.Maybe [WorkflowStepOutput],
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The name of the step.
    name :: Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Text,
    -- | The action type of the step. You must run and update the status of a
    -- manual step for the workflow to continue after the completion of the
    -- step.
    stepActionType :: StepActionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'next', 'createWorkflowStep_next' - The next step.
--
-- 'description', 'createWorkflowStep_description' - The description of the step.
--
-- 'workflowStepAutomationConfiguration', 'createWorkflowStep_workflowStepAutomationConfiguration' - The custom script to run tests on source or target environments.
--
-- 'stepTarget', 'createWorkflowStep_stepTarget' - The servers on which a step will be run.
--
-- 'outputs', 'createWorkflowStep_outputs' - The key value pairs added for the expected output.
--
-- 'previous', 'createWorkflowStep_previous' - The previous step.
--
-- 'name', 'createWorkflowStep_name' - The name of the step.
--
-- 'stepGroupId', 'createWorkflowStep_stepGroupId' - The ID of the step group.
--
-- 'workflowId', 'createWorkflowStep_workflowId' - The ID of the migration workflow.
--
-- 'stepActionType', 'createWorkflowStep_stepActionType' - The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
newCreateWorkflowStep ::
  -- | 'name'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'stepActionType'
  StepActionType ->
  CreateWorkflowStep
newCreateWorkflowStep
  pName_
  pStepGroupId_
  pWorkflowId_
  pStepActionType_ =
    CreateWorkflowStep'
      { next = Prelude.Nothing,
        description = Prelude.Nothing,
        workflowStepAutomationConfiguration =
          Prelude.Nothing,
        stepTarget = Prelude.Nothing,
        outputs = Prelude.Nothing,
        previous = Prelude.Nothing,
        name = pName_,
        stepGroupId = pStepGroupId_,
        workflowId = pWorkflowId_,
        stepActionType = pStepActionType_
      }

-- | The next step.
createWorkflowStep_next :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [Prelude.Text])
createWorkflowStep_next = Lens.lens (\CreateWorkflowStep' {next} -> next) (\s@CreateWorkflowStep' {} a -> s {next = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The description of the step.
createWorkflowStep_description :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe Prelude.Text)
createWorkflowStep_description = Lens.lens (\CreateWorkflowStep' {description} -> description) (\s@CreateWorkflowStep' {} a -> s {description = a} :: CreateWorkflowStep)

-- | The custom script to run tests on source or target environments.
createWorkflowStep_workflowStepAutomationConfiguration :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe WorkflowStepAutomationConfiguration)
createWorkflowStep_workflowStepAutomationConfiguration = Lens.lens (\CreateWorkflowStep' {workflowStepAutomationConfiguration} -> workflowStepAutomationConfiguration) (\s@CreateWorkflowStep' {} a -> s {workflowStepAutomationConfiguration = a} :: CreateWorkflowStep)

-- | The servers on which a step will be run.
createWorkflowStep_stepTarget :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [Prelude.Text])
createWorkflowStep_stepTarget = Lens.lens (\CreateWorkflowStep' {stepTarget} -> stepTarget) (\s@CreateWorkflowStep' {} a -> s {stepTarget = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The key value pairs added for the expected output.
createWorkflowStep_outputs :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [WorkflowStepOutput])
createWorkflowStep_outputs = Lens.lens (\CreateWorkflowStep' {outputs} -> outputs) (\s@CreateWorkflowStep' {} a -> s {outputs = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The previous step.
createWorkflowStep_previous :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [Prelude.Text])
createWorkflowStep_previous = Lens.lens (\CreateWorkflowStep' {previous} -> previous) (\s@CreateWorkflowStep' {} a -> s {previous = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The name of the step.
createWorkflowStep_name :: Lens.Lens' CreateWorkflowStep Prelude.Text
createWorkflowStep_name = Lens.lens (\CreateWorkflowStep' {name} -> name) (\s@CreateWorkflowStep' {} a -> s {name = a} :: CreateWorkflowStep)

-- | The ID of the step group.
createWorkflowStep_stepGroupId :: Lens.Lens' CreateWorkflowStep Prelude.Text
createWorkflowStep_stepGroupId = Lens.lens (\CreateWorkflowStep' {stepGroupId} -> stepGroupId) (\s@CreateWorkflowStep' {} a -> s {stepGroupId = a} :: CreateWorkflowStep)

-- | The ID of the migration workflow.
createWorkflowStep_workflowId :: Lens.Lens' CreateWorkflowStep Prelude.Text
createWorkflowStep_workflowId = Lens.lens (\CreateWorkflowStep' {workflowId} -> workflowId) (\s@CreateWorkflowStep' {} a -> s {workflowId = a} :: CreateWorkflowStep)

-- | The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
createWorkflowStep_stepActionType :: Lens.Lens' CreateWorkflowStep StepActionType
createWorkflowStep_stepActionType = Lens.lens (\CreateWorkflowStep' {stepActionType} -> stepActionType) (\s@CreateWorkflowStep' {} a -> s {stepActionType = a} :: CreateWorkflowStep)

instance Core.AWSRequest CreateWorkflowStep where
  type
    AWSResponse CreateWorkflowStep =
      CreateWorkflowStepResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkflowStepResponse'
            Prelude.<$> (x Core..?> "name")
            Prelude.<*> (x Core..?> "workflowId")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "stepGroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkflowStep where
  hashWithSalt _salt CreateWorkflowStep' {..} =
    _salt `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` workflowStepAutomationConfiguration
      `Prelude.hashWithSalt` stepTarget
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` stepActionType

instance Prelude.NFData CreateWorkflowStep where
  rnf CreateWorkflowStep' {..} =
    Prelude.rnf next
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf workflowStepAutomationConfiguration
      `Prelude.seq` Prelude.rnf stepTarget
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf stepActionType

instance Core.ToHeaders CreateWorkflowStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorkflowStep where
  toJSON CreateWorkflowStep' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("next" Core..=) Prelude.<$> next,
            ("description" Core..=) Prelude.<$> description,
            ("workflowStepAutomationConfiguration" Core..=)
              Prelude.<$> workflowStepAutomationConfiguration,
            ("stepTarget" Core..=) Prelude.<$> stepTarget,
            ("outputs" Core..=) Prelude.<$> outputs,
            ("previous" Core..=) Prelude.<$> previous,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("stepGroupId" Core..= stepGroupId),
            Prelude.Just ("workflowId" Core..= workflowId),
            Prelude.Just
              ("stepActionType" Core..= stepActionType)
          ]
      )

instance Core.ToPath CreateWorkflowStep where
  toPath = Prelude.const "/workflowstep"

instance Core.ToQuery CreateWorkflowStep where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkflowStepResponse' smart constructor.
data CreateWorkflowStepResponse = CreateWorkflowStepResponse'
  { -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step group.
    stepGroupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkflowStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createWorkflowStepResponse_name' - The name of the step.
--
-- 'workflowId', 'createWorkflowStepResponse_workflowId' - The ID of the migration workflow.
--
-- 'id', 'createWorkflowStepResponse_id' - The ID of the step.
--
-- 'stepGroupId', 'createWorkflowStepResponse_stepGroupId' - The ID of the step group.
--
-- 'httpStatus', 'createWorkflowStepResponse_httpStatus' - The response's http status code.
newCreateWorkflowStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkflowStepResponse
newCreateWorkflowStepResponse pHttpStatus_ =
  CreateWorkflowStepResponse'
    { name = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      id = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the step.
createWorkflowStepResponse_name :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_name = Lens.lens (\CreateWorkflowStepResponse' {name} -> name) (\s@CreateWorkflowStepResponse' {} a -> s {name = a} :: CreateWorkflowStepResponse)

-- | The ID of the migration workflow.
createWorkflowStepResponse_workflowId :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_workflowId = Lens.lens (\CreateWorkflowStepResponse' {workflowId} -> workflowId) (\s@CreateWorkflowStepResponse' {} a -> s {workflowId = a} :: CreateWorkflowStepResponse)

-- | The ID of the step.
createWorkflowStepResponse_id :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_id = Lens.lens (\CreateWorkflowStepResponse' {id} -> id) (\s@CreateWorkflowStepResponse' {} a -> s {id = a} :: CreateWorkflowStepResponse)

-- | The ID of the step group.
createWorkflowStepResponse_stepGroupId :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_stepGroupId = Lens.lens (\CreateWorkflowStepResponse' {stepGroupId} -> stepGroupId) (\s@CreateWorkflowStepResponse' {} a -> s {stepGroupId = a} :: CreateWorkflowStepResponse)

-- | The response's http status code.
createWorkflowStepResponse_httpStatus :: Lens.Lens' CreateWorkflowStepResponse Prelude.Int
createWorkflowStepResponse_httpStatus = Lens.lens (\CreateWorkflowStepResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowStepResponse' {} a -> s {httpStatus = a} :: CreateWorkflowStepResponse)

instance Prelude.NFData CreateWorkflowStepResponse where
  rnf CreateWorkflowStepResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf httpStatus
