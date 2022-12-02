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
-- Module      : Amazonka.SWF.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionTerminated@ event and forces closure of the
-- workflow execution identified by the given domain, runId, and
-- workflowId. The child policy, registered with the workflow type or
-- specified when starting this execution, is applied to any open child
-- workflow executions of this workflow execution.
--
-- If the identified workflow execution was in progress, it is terminated
-- immediately.
--
-- If a runId isn\'t specified, then the @WorkflowExecutionTerminated@
-- event is recorded in the history of the current open workflow with the
-- matching workflowId in the domain.
--
-- You should consider using RequestCancelWorkflowExecution action instead
-- because it allows the workflow to gracefully close while
-- TerminateWorkflowExecution doesn\'t.
--
-- __Access Control__
--
-- You can use IAM policies to control this action\'s access to Amazon SWF
-- resources as follows:
--
-- -   Use a @Resource@ element with the domain name to limit the action to
--     only specified domains.
--
-- -   Use an @Action@ element to allow or deny permission to call this
--     action.
--
-- -   You cannot use an IAM policy to constrain this action\'s parameters.
--
-- If the caller doesn\'t have sufficient permissions to invoke the action,
-- or the parameter values fall outside the specified constraints, the
-- action fails. The associated event attribute\'s @cause@ parameter is set
-- to @OPERATION_NOT_PERMITTED@. For details and example IAM policies, see
-- <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows>
-- in the /Amazon SWF Developer Guide/.
module Amazonka.SWF.TerminateWorkflowExecution
  ( -- * Creating a Request
    TerminateWorkflowExecution (..),
    newTerminateWorkflowExecution,

    -- * Request Lenses
    terminateWorkflowExecution_details,
    terminateWorkflowExecution_reason,
    terminateWorkflowExecution_childPolicy,
    terminateWorkflowExecution_runId,
    terminateWorkflowExecution_domain,
    terminateWorkflowExecution_workflowId,

    -- * Destructuring the Response
    TerminateWorkflowExecutionResponse (..),
    newTerminateWorkflowExecutionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SWF.Types

-- | /See:/ 'newTerminateWorkflowExecution' smart constructor.
data TerminateWorkflowExecution = TerminateWorkflowExecution'
  { -- | Details for terminating the workflow execution.
    details :: Prelude.Maybe Prelude.Text,
    -- | A descriptive reason for terminating the workflow execution.
    reason :: Prelude.Maybe Prelude.Text,
    -- | If set, specifies the policy to use for the child workflow executions of
    -- the workflow execution being terminated. This policy overrides the child
    -- policy specified for the workflow execution at registration time or when
    -- starting the execution.
    --
    -- The supported child policies are:
    --
    -- -   @TERMINATE@ – The child executions are terminated.
    --
    -- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
    --     execution by recording a @WorkflowExecutionCancelRequested@ event in
    --     its history. It is up to the decider to take appropriate actions
    --     when it receives an execution history with this event.
    --
    -- -   @ABANDON@ – No action is taken. The child executions continue to
    --     run.
    --
    -- A child policy for this workflow execution must be specified either as a
    -- default for the workflow type or through this parameter. If neither this
    -- parameter is set nor a default child policy was specified at
    -- registration time then a fault is returned.
    childPolicy :: Prelude.Maybe ChildPolicy,
    -- | The runId of the workflow execution to terminate.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The domain of the workflow execution to terminate.
    domain :: Prelude.Text,
    -- | The workflowId of the workflow execution to terminate.
    workflowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'details', 'terminateWorkflowExecution_details' - Details for terminating the workflow execution.
--
-- 'reason', 'terminateWorkflowExecution_reason' - A descriptive reason for terminating the workflow execution.
--
-- 'childPolicy', 'terminateWorkflowExecution_childPolicy' - If set, specifies the policy to use for the child workflow executions of
-- the workflow execution being terminated. This policy overrides the child
-- policy specified for the workflow execution at registration time or when
-- starting the execution.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault is returned.
--
-- 'runId', 'terminateWorkflowExecution_runId' - The runId of the workflow execution to terminate.
--
-- 'domain', 'terminateWorkflowExecution_domain' - The domain of the workflow execution to terminate.
--
-- 'workflowId', 'terminateWorkflowExecution_workflowId' - The workflowId of the workflow execution to terminate.
newTerminateWorkflowExecution ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'workflowId'
  Prelude.Text ->
  TerminateWorkflowExecution
newTerminateWorkflowExecution pDomain_ pWorkflowId_ =
  TerminateWorkflowExecution'
    { details =
        Prelude.Nothing,
      reason = Prelude.Nothing,
      childPolicy = Prelude.Nothing,
      runId = Prelude.Nothing,
      domain = pDomain_,
      workflowId = pWorkflowId_
    }

-- | Details for terminating the workflow execution.
terminateWorkflowExecution_details :: Lens.Lens' TerminateWorkflowExecution (Prelude.Maybe Prelude.Text)
terminateWorkflowExecution_details = Lens.lens (\TerminateWorkflowExecution' {details} -> details) (\s@TerminateWorkflowExecution' {} a -> s {details = a} :: TerminateWorkflowExecution)

-- | A descriptive reason for terminating the workflow execution.
terminateWorkflowExecution_reason :: Lens.Lens' TerminateWorkflowExecution (Prelude.Maybe Prelude.Text)
terminateWorkflowExecution_reason = Lens.lens (\TerminateWorkflowExecution' {reason} -> reason) (\s@TerminateWorkflowExecution' {} a -> s {reason = a} :: TerminateWorkflowExecution)

-- | If set, specifies the policy to use for the child workflow executions of
-- the workflow execution being terminated. This policy overrides the child
-- policy specified for the workflow execution at registration time or when
-- starting the execution.
--
-- The supported child policies are:
--
-- -   @TERMINATE@ – The child executions are terminated.
--
-- -   @REQUEST_CANCEL@ – A request to cancel is attempted for each child
--     execution by recording a @WorkflowExecutionCancelRequested@ event in
--     its history. It is up to the decider to take appropriate actions
--     when it receives an execution history with this event.
--
-- -   @ABANDON@ – No action is taken. The child executions continue to
--     run.
--
-- A child policy for this workflow execution must be specified either as a
-- default for the workflow type or through this parameter. If neither this
-- parameter is set nor a default child policy was specified at
-- registration time then a fault is returned.
terminateWorkflowExecution_childPolicy :: Lens.Lens' TerminateWorkflowExecution (Prelude.Maybe ChildPolicy)
terminateWorkflowExecution_childPolicy = Lens.lens (\TerminateWorkflowExecution' {childPolicy} -> childPolicy) (\s@TerminateWorkflowExecution' {} a -> s {childPolicy = a} :: TerminateWorkflowExecution)

-- | The runId of the workflow execution to terminate.
terminateWorkflowExecution_runId :: Lens.Lens' TerminateWorkflowExecution (Prelude.Maybe Prelude.Text)
terminateWorkflowExecution_runId = Lens.lens (\TerminateWorkflowExecution' {runId} -> runId) (\s@TerminateWorkflowExecution' {} a -> s {runId = a} :: TerminateWorkflowExecution)

-- | The domain of the workflow execution to terminate.
terminateWorkflowExecution_domain :: Lens.Lens' TerminateWorkflowExecution Prelude.Text
terminateWorkflowExecution_domain = Lens.lens (\TerminateWorkflowExecution' {domain} -> domain) (\s@TerminateWorkflowExecution' {} a -> s {domain = a} :: TerminateWorkflowExecution)

-- | The workflowId of the workflow execution to terminate.
terminateWorkflowExecution_workflowId :: Lens.Lens' TerminateWorkflowExecution Prelude.Text
terminateWorkflowExecution_workflowId = Lens.lens (\TerminateWorkflowExecution' {workflowId} -> workflowId) (\s@TerminateWorkflowExecution' {} a -> s {workflowId = a} :: TerminateWorkflowExecution)

instance Core.AWSRequest TerminateWorkflowExecution where
  type
    AWSResponse TerminateWorkflowExecution =
      TerminateWorkflowExecutionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      TerminateWorkflowExecutionResponse'

instance Prelude.Hashable TerminateWorkflowExecution where
  hashWithSalt _salt TerminateWorkflowExecution' {..} =
    _salt `Prelude.hashWithSalt` details
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` childPolicy
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData TerminateWorkflowExecution where
  rnf TerminateWorkflowExecution' {..} =
    Prelude.rnf details
      `Prelude.seq` Prelude.rnf reason
      `Prelude.seq` Prelude.rnf childPolicy
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf workflowId

instance Data.ToHeaders TerminateWorkflowExecution where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SimpleWorkflowService.TerminateWorkflowExecution" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON TerminateWorkflowExecution where
  toJSON TerminateWorkflowExecution' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("details" Data..=) Prelude.<$> details,
            ("reason" Data..=) Prelude.<$> reason,
            ("childPolicy" Data..=) Prelude.<$> childPolicy,
            ("runId" Data..=) Prelude.<$> runId,
            Prelude.Just ("domain" Data..= domain),
            Prelude.Just ("workflowId" Data..= workflowId)
          ]
      )

instance Data.ToPath TerminateWorkflowExecution where
  toPath = Prelude.const "/"

instance Data.ToQuery TerminateWorkflowExecution where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTerminateWorkflowExecutionResponse' smart constructor.
data TerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TerminateWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTerminateWorkflowExecutionResponse ::
  TerminateWorkflowExecutionResponse
newTerminateWorkflowExecutionResponse =
  TerminateWorkflowExecutionResponse'

instance
  Prelude.NFData
    TerminateWorkflowExecutionResponse
  where
  rnf _ = ()
