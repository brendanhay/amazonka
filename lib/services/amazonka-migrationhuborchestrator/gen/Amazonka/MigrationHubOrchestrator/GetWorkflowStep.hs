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
-- Module      : Amazonka.MigrationHubOrchestrator.GetWorkflowStep
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a step in the migration workflow.
module Amazonka.MigrationHubOrchestrator.GetWorkflowStep
  ( -- * Creating a Request
    GetWorkflowStep (..),
    newGetWorkflowStep,

    -- * Request Lenses
    getWorkflowStep_workflowId,
    getWorkflowStep_stepGroupId,
    getWorkflowStep_id,

    -- * Destructuring the Response
    GetWorkflowStepResponse (..),
    newGetWorkflowStepResponse,

    -- * Response Lenses
    getWorkflowStepResponse_name,
    getWorkflowStepResponse_workflowId,
    getWorkflowStepResponse_scriptOutputLocation,
    getWorkflowStepResponse_noOfSrvFailed,
    getWorkflowStepResponse_next,
    getWorkflowStepResponse_lastStartTime,
    getWorkflowStepResponse_status,
    getWorkflowStepResponse_owner,
    getWorkflowStepResponse_noOfSrvCompleted,
    getWorkflowStepResponse_stepActionType,
    getWorkflowStepResponse_description,
    getWorkflowStepResponse_workflowStepAutomationConfiguration,
    getWorkflowStepResponse_endTime,
    getWorkflowStepResponse_stepTarget,
    getWorkflowStepResponse_stepGroupId,
    getWorkflowStepResponse_outputs,
    getWorkflowStepResponse_creationTime,
    getWorkflowStepResponse_statusMessage,
    getWorkflowStepResponse_stepId,
    getWorkflowStepResponse_previous,
    getWorkflowStepResponse_totalNoOfSrv,
    getWorkflowStepResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubOrchestrator.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkflowStep' smart constructor.
data GetWorkflowStep = GetWorkflowStep'
  { -- | The ID of the migration workflow.
    workflowId :: Prelude.Text,
    -- | desThe ID of the step group.
    stepGroupId :: Prelude.Text,
    -- | The ID of the step.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStep' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'getWorkflowStep_workflowId' - The ID of the migration workflow.
--
-- 'stepGroupId', 'getWorkflowStep_stepGroupId' - desThe ID of the step group.
--
-- 'id', 'getWorkflowStep_id' - The ID of the step.
newGetWorkflowStep ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'stepGroupId'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  GetWorkflowStep
newGetWorkflowStep pWorkflowId_ pStepGroupId_ pId_ =
  GetWorkflowStep'
    { workflowId = pWorkflowId_,
      stepGroupId = pStepGroupId_,
      id = pId_
    }

-- | The ID of the migration workflow.
getWorkflowStep_workflowId :: Lens.Lens' GetWorkflowStep Prelude.Text
getWorkflowStep_workflowId = Lens.lens (\GetWorkflowStep' {workflowId} -> workflowId) (\s@GetWorkflowStep' {} a -> s {workflowId = a} :: GetWorkflowStep)

-- | desThe ID of the step group.
getWorkflowStep_stepGroupId :: Lens.Lens' GetWorkflowStep Prelude.Text
getWorkflowStep_stepGroupId = Lens.lens (\GetWorkflowStep' {stepGroupId} -> stepGroupId) (\s@GetWorkflowStep' {} a -> s {stepGroupId = a} :: GetWorkflowStep)

-- | The ID of the step.
getWorkflowStep_id :: Lens.Lens' GetWorkflowStep Prelude.Text
getWorkflowStep_id = Lens.lens (\GetWorkflowStep' {id} -> id) (\s@GetWorkflowStep' {} a -> s {id = a} :: GetWorkflowStep)

instance Core.AWSRequest GetWorkflowStep where
  type
    AWSResponse GetWorkflowStep =
      GetWorkflowStepResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkflowStepResponse'
            Prelude.<$> (x Data..?> "name")
            Prelude.<*> (x Data..?> "workflowId")
            Prelude.<*> (x Data..?> "scriptOutputLocation")
            Prelude.<*> (x Data..?> "noOfSrvFailed")
            Prelude.<*> (x Data..?> "next" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "lastStartTime")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (x Data..?> "owner")
            Prelude.<*> (x Data..?> "noOfSrvCompleted")
            Prelude.<*> (x Data..?> "stepActionType")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "workflowStepAutomationConfiguration")
            Prelude.<*> (x Data..?> "endTime")
            Prelude.<*> (x Data..?> "stepTarget" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "stepGroupId")
            Prelude.<*> (x Data..?> "outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "statusMessage")
            Prelude.<*> (x Data..?> "stepId")
            Prelude.<*> (x Data..?> "previous" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "totalNoOfSrv")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetWorkflowStep where
  hashWithSalt _salt GetWorkflowStep' {..} =
    _salt `Prelude.hashWithSalt` workflowId
      `Prelude.hashWithSalt` stepGroupId
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetWorkflowStep where
  rnf GetWorkflowStep' {..} =
    Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetWorkflowStep where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetWorkflowStep where
  toPath GetWorkflowStep' {..} =
    Prelude.mconcat ["/workflowstep/", Data.toBS id]

instance Data.ToQuery GetWorkflowStep where
  toQuery GetWorkflowStep' {..} =
    Prelude.mconcat
      [ "workflowId" Data.=: workflowId,
        "stepGroupId" Data.=: stepGroupId
      ]

-- | /See:/ 'newGetWorkflowStepResponse' smart constructor.
data GetWorkflowStepResponse = GetWorkflowStepResponse'
  { -- | The name of the step.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the migration workflow.
    workflowId :: Prelude.Maybe Prelude.Text,
    -- | The output location of the script.
    scriptOutputLocation :: Prelude.Maybe Prelude.Text,
    -- | The number of servers that have failed to migrate.
    noOfSrvFailed :: Prelude.Maybe Prelude.Int,
    -- | The next step.
    next :: Prelude.Maybe [Prelude.Text],
    -- | The time at which the workflow was last started.
    lastStartTime :: Prelude.Maybe Data.POSIX,
    -- | The status of the step.
    status :: Prelude.Maybe StepStatus,
    -- | The owner of the step.
    owner :: Prelude.Maybe Owner,
    -- | The number of servers that have been migrated.
    noOfSrvCompleted :: Prelude.Maybe Prelude.Int,
    -- | The action type of the step. You must run and update the status of a
    -- manual step for the workflow to continue after the completion of the
    -- step.
    stepActionType :: Prelude.Maybe StepActionType,
    -- | The description of the step.
    description :: Prelude.Maybe Prelude.Text,
    -- | The custom script to run tests on source or target environments.
    workflowStepAutomationConfiguration :: Prelude.Maybe WorkflowStepAutomationConfiguration,
    -- | The time at which the step ended.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | The servers on which a step will be run.
    stepTarget :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the step group.
    stepGroupId :: Prelude.Maybe Prelude.Text,
    -- | The outputs of the step.
    outputs :: Prelude.Maybe [WorkflowStepOutput],
    -- | The time at which the step was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The status message of the migration workflow.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the step.
    stepId :: Prelude.Maybe Prelude.Text,
    -- | The previous step.
    previous :: Prelude.Maybe [Prelude.Text],
    -- | The total number of servers that have been migrated.
    totalNoOfSrv :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkflowStepResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getWorkflowStepResponse_name' - The name of the step.
--
-- 'workflowId', 'getWorkflowStepResponse_workflowId' - The ID of the migration workflow.
--
-- 'scriptOutputLocation', 'getWorkflowStepResponse_scriptOutputLocation' - The output location of the script.
--
-- 'noOfSrvFailed', 'getWorkflowStepResponse_noOfSrvFailed' - The number of servers that have failed to migrate.
--
-- 'next', 'getWorkflowStepResponse_next' - The next step.
--
-- 'lastStartTime', 'getWorkflowStepResponse_lastStartTime' - The time at which the workflow was last started.
--
-- 'status', 'getWorkflowStepResponse_status' - The status of the step.
--
-- 'owner', 'getWorkflowStepResponse_owner' - The owner of the step.
--
-- 'noOfSrvCompleted', 'getWorkflowStepResponse_noOfSrvCompleted' - The number of servers that have been migrated.
--
-- 'stepActionType', 'getWorkflowStepResponse_stepActionType' - The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
--
-- 'description', 'getWorkflowStepResponse_description' - The description of the step.
--
-- 'workflowStepAutomationConfiguration', 'getWorkflowStepResponse_workflowStepAutomationConfiguration' - The custom script to run tests on source or target environments.
--
-- 'endTime', 'getWorkflowStepResponse_endTime' - The time at which the step ended.
--
-- 'stepTarget', 'getWorkflowStepResponse_stepTarget' - The servers on which a step will be run.
--
-- 'stepGroupId', 'getWorkflowStepResponse_stepGroupId' - The ID of the step group.
--
-- 'outputs', 'getWorkflowStepResponse_outputs' - The outputs of the step.
--
-- 'creationTime', 'getWorkflowStepResponse_creationTime' - The time at which the step was created.
--
-- 'statusMessage', 'getWorkflowStepResponse_statusMessage' - The status message of the migration workflow.
--
-- 'stepId', 'getWorkflowStepResponse_stepId' - The ID of the step.
--
-- 'previous', 'getWorkflowStepResponse_previous' - The previous step.
--
-- 'totalNoOfSrv', 'getWorkflowStepResponse_totalNoOfSrv' - The total number of servers that have been migrated.
--
-- 'httpStatus', 'getWorkflowStepResponse_httpStatus' - The response's http status code.
newGetWorkflowStepResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWorkflowStepResponse
newGetWorkflowStepResponse pHttpStatus_ =
  GetWorkflowStepResponse'
    { name = Prelude.Nothing,
      workflowId = Prelude.Nothing,
      scriptOutputLocation = Prelude.Nothing,
      noOfSrvFailed = Prelude.Nothing,
      next = Prelude.Nothing,
      lastStartTime = Prelude.Nothing,
      status = Prelude.Nothing,
      owner = Prelude.Nothing,
      noOfSrvCompleted = Prelude.Nothing,
      stepActionType = Prelude.Nothing,
      description = Prelude.Nothing,
      workflowStepAutomationConfiguration =
        Prelude.Nothing,
      endTime = Prelude.Nothing,
      stepTarget = Prelude.Nothing,
      stepGroupId = Prelude.Nothing,
      outputs = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      stepId = Prelude.Nothing,
      previous = Prelude.Nothing,
      totalNoOfSrv = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the step.
getWorkflowStepResponse_name :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_name = Lens.lens (\GetWorkflowStepResponse' {name} -> name) (\s@GetWorkflowStepResponse' {} a -> s {name = a} :: GetWorkflowStepResponse)

-- | The ID of the migration workflow.
getWorkflowStepResponse_workflowId :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_workflowId = Lens.lens (\GetWorkflowStepResponse' {workflowId} -> workflowId) (\s@GetWorkflowStepResponse' {} a -> s {workflowId = a} :: GetWorkflowStepResponse)

-- | The output location of the script.
getWorkflowStepResponse_scriptOutputLocation :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_scriptOutputLocation = Lens.lens (\GetWorkflowStepResponse' {scriptOutputLocation} -> scriptOutputLocation) (\s@GetWorkflowStepResponse' {} a -> s {scriptOutputLocation = a} :: GetWorkflowStepResponse)

-- | The number of servers that have failed to migrate.
getWorkflowStepResponse_noOfSrvFailed :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Int)
getWorkflowStepResponse_noOfSrvFailed = Lens.lens (\GetWorkflowStepResponse' {noOfSrvFailed} -> noOfSrvFailed) (\s@GetWorkflowStepResponse' {} a -> s {noOfSrvFailed = a} :: GetWorkflowStepResponse)

-- | The next step.
getWorkflowStepResponse_next :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepResponse_next = Lens.lens (\GetWorkflowStepResponse' {next} -> next) (\s@GetWorkflowStepResponse' {} a -> s {next = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the workflow was last started.
getWorkflowStepResponse_lastStartTime :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepResponse_lastStartTime = Lens.lens (\GetWorkflowStepResponse' {lastStartTime} -> lastStartTime) (\s@GetWorkflowStepResponse' {} a -> s {lastStartTime = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the step.
getWorkflowStepResponse_status :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe StepStatus)
getWorkflowStepResponse_status = Lens.lens (\GetWorkflowStepResponse' {status} -> status) (\s@GetWorkflowStepResponse' {} a -> s {status = a} :: GetWorkflowStepResponse)

-- | The owner of the step.
getWorkflowStepResponse_owner :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Owner)
getWorkflowStepResponse_owner = Lens.lens (\GetWorkflowStepResponse' {owner} -> owner) (\s@GetWorkflowStepResponse' {} a -> s {owner = a} :: GetWorkflowStepResponse)

-- | The number of servers that have been migrated.
getWorkflowStepResponse_noOfSrvCompleted :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Int)
getWorkflowStepResponse_noOfSrvCompleted = Lens.lens (\GetWorkflowStepResponse' {noOfSrvCompleted} -> noOfSrvCompleted) (\s@GetWorkflowStepResponse' {} a -> s {noOfSrvCompleted = a} :: GetWorkflowStepResponse)

-- | The action type of the step. You must run and update the status of a
-- manual step for the workflow to continue after the completion of the
-- step.
getWorkflowStepResponse_stepActionType :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe StepActionType)
getWorkflowStepResponse_stepActionType = Lens.lens (\GetWorkflowStepResponse' {stepActionType} -> stepActionType) (\s@GetWorkflowStepResponse' {} a -> s {stepActionType = a} :: GetWorkflowStepResponse)

-- | The description of the step.
getWorkflowStepResponse_description :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_description = Lens.lens (\GetWorkflowStepResponse' {description} -> description) (\s@GetWorkflowStepResponse' {} a -> s {description = a} :: GetWorkflowStepResponse)

-- | The custom script to run tests on source or target environments.
getWorkflowStepResponse_workflowStepAutomationConfiguration :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe WorkflowStepAutomationConfiguration)
getWorkflowStepResponse_workflowStepAutomationConfiguration = Lens.lens (\GetWorkflowStepResponse' {workflowStepAutomationConfiguration} -> workflowStepAutomationConfiguration) (\s@GetWorkflowStepResponse' {} a -> s {workflowStepAutomationConfiguration = a} :: GetWorkflowStepResponse)

-- | The time at which the step ended.
getWorkflowStepResponse_endTime :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepResponse_endTime = Lens.lens (\GetWorkflowStepResponse' {endTime} -> endTime) (\s@GetWorkflowStepResponse' {} a -> s {endTime = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Data._Time

-- | The servers on which a step will be run.
getWorkflowStepResponse_stepTarget :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepResponse_stepTarget = Lens.lens (\GetWorkflowStepResponse' {stepTarget} -> stepTarget) (\s@GetWorkflowStepResponse' {} a -> s {stepTarget = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the step group.
getWorkflowStepResponse_stepGroupId :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_stepGroupId = Lens.lens (\GetWorkflowStepResponse' {stepGroupId} -> stepGroupId) (\s@GetWorkflowStepResponse' {} a -> s {stepGroupId = a} :: GetWorkflowStepResponse)

-- | The outputs of the step.
getWorkflowStepResponse_outputs :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe [WorkflowStepOutput])
getWorkflowStepResponse_outputs = Lens.lens (\GetWorkflowStepResponse' {outputs} -> outputs) (\s@GetWorkflowStepResponse' {} a -> s {outputs = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the step was created.
getWorkflowStepResponse_creationTime :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.UTCTime)
getWorkflowStepResponse_creationTime = Lens.lens (\GetWorkflowStepResponse' {creationTime} -> creationTime) (\s@GetWorkflowStepResponse' {} a -> s {creationTime = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Data._Time

-- | The status message of the migration workflow.
getWorkflowStepResponse_statusMessage :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_statusMessage = Lens.lens (\GetWorkflowStepResponse' {statusMessage} -> statusMessage) (\s@GetWorkflowStepResponse' {} a -> s {statusMessage = a} :: GetWorkflowStepResponse)

-- | The ID of the step.
getWorkflowStepResponse_stepId :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Text)
getWorkflowStepResponse_stepId = Lens.lens (\GetWorkflowStepResponse' {stepId} -> stepId) (\s@GetWorkflowStepResponse' {} a -> s {stepId = a} :: GetWorkflowStepResponse)

-- | The previous step.
getWorkflowStepResponse_previous :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe [Prelude.Text])
getWorkflowStepResponse_previous = Lens.lens (\GetWorkflowStepResponse' {previous} -> previous) (\s@GetWorkflowStepResponse' {} a -> s {previous = a} :: GetWorkflowStepResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of servers that have been migrated.
getWorkflowStepResponse_totalNoOfSrv :: Lens.Lens' GetWorkflowStepResponse (Prelude.Maybe Prelude.Int)
getWorkflowStepResponse_totalNoOfSrv = Lens.lens (\GetWorkflowStepResponse' {totalNoOfSrv} -> totalNoOfSrv) (\s@GetWorkflowStepResponse' {} a -> s {totalNoOfSrv = a} :: GetWorkflowStepResponse)

-- | The response's http status code.
getWorkflowStepResponse_httpStatus :: Lens.Lens' GetWorkflowStepResponse Prelude.Int
getWorkflowStepResponse_httpStatus = Lens.lens (\GetWorkflowStepResponse' {httpStatus} -> httpStatus) (\s@GetWorkflowStepResponse' {} a -> s {httpStatus = a} :: GetWorkflowStepResponse)

instance Prelude.NFData GetWorkflowStepResponse where
  rnf GetWorkflowStepResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf scriptOutputLocation
      `Prelude.seq` Prelude.rnf noOfSrvFailed
      `Prelude.seq` Prelude.rnf next
      `Prelude.seq` Prelude.rnf lastStartTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf noOfSrvCompleted
      `Prelude.seq` Prelude.rnf stepActionType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf
        workflowStepAutomationConfiguration
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf stepTarget
      `Prelude.seq` Prelude.rnf stepGroupId
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf stepId
      `Prelude.seq` Prelude.rnf previous
      `Prelude.seq` Prelude.rnf totalNoOfSrv
      `Prelude.seq` Prelude.rnf httpStatus
