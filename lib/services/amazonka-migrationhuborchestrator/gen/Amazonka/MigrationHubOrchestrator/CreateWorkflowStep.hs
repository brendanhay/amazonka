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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a step in the migration workflow.
module Amazonka.MigrationHubOrchestrator.CreateWorkflowStep
  ( -- * Creating a Request
    CreateWorkflowStep (..),
    newCreateWorkflowStep,

    -- * Request Lenses
    createWorkflowStep_description,
    createWorkflowStep_next,
    createWorkflowStep_outputs,
    createWorkflowStep_previous,
    createWorkflowStep_stepTarget,
    createWorkflowStep_workflowStepAutomationConfiguration,
    createWorkflowStep_name,
    createWorkflowStep_stepGroupId,
    createWorkflowStep_workflowId,
    createWorkflowStep_stepActionType,

    -- * Destructuring the Response
    CreateWorkflowStepResponse (..),
    newCreateWorkflowStepResponse,

    -- * Response Lenses
    createWorkflowStepResponse_id,
    createWorkflowStepResponse_name,
    createWorkflowStepResponse_stepGroupId,
    createWorkflowStepResponse_workflowId,
    createWorkflowStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkflowStep' smart constructor.
data CreateWorkflowStep = CreateWorkflowStep'
  { -- | The description of the step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The key value pairs added for the expected output.
    outputs :: Prelude.Maybe [WorkflowStepOutput],
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The servers on which a step will be run.
    stepTarget :: Prelude.Maybe [Prelude.Text],
    -- | The custom script to run tests on source or target environments.
    workflowStepAutomationConfiguration :: Prelude.Maybe WorkflowStepAutomationConfiguration,
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
-- 'description', 'createWorkflowStep_description' - The description of the step.
--
-- 'next', 'createWorkflowStep_next' - The next step.
--
-- 'outputs', 'createWorkflowStep_outputs' - The key value pairs added for the expected output.
--
-- 'previous', 'createWorkflowStep_previous' - The previous step.
--
-- 'stepTarget', 'createWorkflowStep_stepTarget' - The servers on which a step will be run.
--
-- 'workflowStepAutomationConfiguration', 'createWorkflowStep_workflowStepAutomationConfiguration' - The custom script to run tests on source or target environments.
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
      { description = Prelude.Nothing,
        next = Prelude.Nothing,
        outputs = Prelude.Nothing,
        previous = Prelude.Nothing,
        stepTarget = Prelude.Nothing,
        workflowStepAutomationConfiguration =
          Prelude.Nothing,
        name = pName_,
        stepGroupId = pStepGroupId_,
        workflowId = pWorkflowId_,
        stepActionType = pStepActionType_
      }

-- | The description of the step.
createWorkflowStep_description :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe Prelude.Text)
createWorkflowStep_description = Lens.lens (\CreateWorkflowStep' {description} -> description) (\s@CreateWorkflowStep' {} a -> s {description = a} :: CreateWorkflowStep)

-- | The next step.
createWorkflowStep_next :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [Prelude.Text])
createWorkflowStep_next = Lens.lens (\CreateWorkflowStep' {next} -> next) (\s@CreateWorkflowStep' {} a -> s {next = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The key value pairs added for the expected output.
createWorkflowStep_outputs :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [WorkflowStepOutput])
createWorkflowStep_outputs = Lens.lens (\CreateWorkflowStep' {outputs} -> outputs) (\s@CreateWorkflowStep' {} a -> s {outputs = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The previous step.
createWorkflowStep_previous :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [Prelude.Text])
createWorkflowStep_previous = Lens.lens (\CreateWorkflowStep' {previous} -> previous) (\s@CreateWorkflowStep' {} a -> s {previous = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The servers on which a step will be run.
createWorkflowStep_stepTarget :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe [Prelude.Text])
createWorkflowStep_stepTarget = Lens.lens (\CreateWorkflowStep' {stepTarget} -> stepTarget) (\s@CreateWorkflowStep' {} a -> s {stepTarget = a} :: CreateWorkflowStep) Prelude.. Lens.mapping Lens.coerced

-- | The custom script to run tests on source or target environments.
createWorkflowStep_workflowStepAutomationConfiguration :: Lens.Lens' CreateWorkflowStep (Prelude.Maybe WorkflowStepAutomationConfiguration)
createWorkflowStep_workflowStepAutomationConfiguration = Lens.lens (\CreateWorkflowStep' {workflowStepAutomationConfiguration} -> workflowStepAutomationConfiguration) (\s@CreateWorkflowStep' {} a -> s {workflowStepAutomationConfiguration = a} :: CreateWorkflowStep)

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
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "stepGroupId")
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWorkflowStep where
  hashWithSalt _salt CreateWorkflowStep' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` next
      `Prelude.hashWithSalt` outputs
      `Prelude.hashWithSalt` previous
      `Prelude.hashWithSalt` stepTarget
      `Prelude.hashWithSalt` workflowStepAutomationConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` stepActionType

instance Prelude.NFData CreateWorkflowStep where
  rnf CreateWorkflowStep' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf next `Prelude.seq`
        Prelude.rnf outputs `Prelude.seq`
          Prelude.rnf previous `Prelude.seq`
            Prelude.rnf stepTarget `Prelude.seq`
              Prelude.rnf workflowStepAutomationConfiguration `Prelude.seq`
                Prelude.rnf name `Prelude.seq`
                  Prelude.rnf stepGroupId `Prelude.seq`
                    Prelude.rnf workflowId `Prelude.seq`
                      Prelude.rnf stepActionType

instance Data.ToHeaders CreateWorkflowStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkflowStep where
  toJSON CreateWorkflowStep' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("next" Data..=) Prelude.<$> next,
            ("outputs" Data..=) Prelude.<$> outputs,
            ("previous" Data..=) Prelude.<$> previous,
            ("stepTarget" Data..=) Prelude.<$> stepTarget,
            ("workflowStepAutomationConfiguration" Data..=)
              Prelude.<$> workflowStepAutomationConfiguration,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("stepGroupId" Data..= stepGroupId),
            Prelude.Just ("workflowId" Data..= workflowId),
            Prelude.Just
              ("stepActionType" Data..= stepActionType)
          ]
      )

instance Data.ToPath CreateWorkflowStep where
  toPath = Prelude.const "/workflowstep"

instance Data.ToQuery CreateWorkflowStep where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkflowStepResponse' smart constructor.
data CreateWorkflowStepResponse = CreateWorkflowStepResponse'
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
-- Create a value of 'CreateWorkflowStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'createWorkflowStepResponse_id' - The ID of the step.
--
-- 'name', 'createWorkflowStepResponse_name' - The name of the step.
--
-- 'stepGroupId', 'createWorkflowStepResponse_stepGroupId' - The ID of the step group.
--
-- 'workflowId', 'createWorkflowStepResponse_workflowId' - The ID of the migration workflow.
--
-- 'httpStatus', 'createWorkflowStepResponse_httpStatus' - The response's http status code.
newCreateWorkflowStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWorkflowStepResponse
newCreateWorkflowStepResponse pHttpStatus_ =
  CreateWorkflowStepResponse'
    { id = Prelude.Nothing,
      name = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the step.
createWorkflowStepResponse_id :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_id = Lens.lens (\CreateWorkflowStepResponse' {id} -> id) (\s@CreateWorkflowStepResponse' {} a -> s {id = a} :: CreateWorkflowStepResponse)

-- | The name of the step.
createWorkflowStepResponse_name :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_name = Lens.lens (\CreateWorkflowStepResponse' {name} -> name) (\s@CreateWorkflowStepResponse' {} a -> s {name = a} :: CreateWorkflowStepResponse)

-- | The ID of the step group.
createWorkflowStepResponse_stepGroupId :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_stepGroupId = Lens.lens (\CreateWorkflowStepResponse' {stepGroupId} -> stepGroupId) (\s@CreateWorkflowStepResponse' {} a -> s {stepGroupId = a} :: CreateWorkflowStepResponse)

-- | The ID of the migration workflow.
createWorkflowStepResponse_workflowId :: Lens.Lens' CreateWorkflowStepResponse (Prelude.Maybe Prelude.Text)
createWorkflowStepResponse_workflowId = Lens.lens (\CreateWorkflowStepResponse' {workflowId} -> workflowId) (\s@CreateWorkflowStepResponse' {} a -> s {workflowId = a} :: CreateWorkflowStepResponse)

-- | The response's http status code.
createWorkflowStepResponse_httpStatus :: Lens.Lens' CreateWorkflowStepResponse Prelude.Int
createWorkflowStepResponse_httpStatus = Lens.lens (\CreateWorkflowStepResponse' {httpStatus} -> httpStatus) (\s@CreateWorkflowStepResponse' {} a -> s {httpStatus = a} :: CreateWorkflowStepResponse)

instance Prelude.NFData CreateWorkflowStepResponse where
  rnf CreateWorkflowStepResponse' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf stepGroupId `Prelude.seq`
          Prelude.rnf workflowId `Prelude.seq`
            Prelude.rnf httpStatus
