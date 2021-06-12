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
-- Module      : Network.AWS.SWF.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SWF.TerminateWorkflowExecution
  ( -- * Creating a Request
    TerminateWorkflowExecution (..),
    newTerminateWorkflowExecution,

    -- * Request Lenses
    terminateWorkflowExecution_runId,
    terminateWorkflowExecution_childPolicy,
    terminateWorkflowExecution_details,
    terminateWorkflowExecution_reason,
    terminateWorkflowExecution_domain,
    terminateWorkflowExecution_workflowId,

    -- * Destructuring the Response
    TerminateWorkflowExecutionResponse (..),
    newTerminateWorkflowExecutionResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SWF.Types

-- | /See:/ 'newTerminateWorkflowExecution' smart constructor.
data TerminateWorkflowExecution = TerminateWorkflowExecution'
  { -- | The runId of the workflow execution to terminate.
    runId :: Core.Maybe Core.Text,
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
    childPolicy :: Core.Maybe ChildPolicy,
    -- | Details for terminating the workflow execution.
    details :: Core.Maybe Core.Text,
    -- | A descriptive reason for terminating the workflow execution.
    reason :: Core.Maybe Core.Text,
    -- | The domain of the workflow execution to terminate.
    domain :: Core.Text,
    -- | The workflowId of the workflow execution to terminate.
    workflowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateWorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runId', 'terminateWorkflowExecution_runId' - The runId of the workflow execution to terminate.
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
-- 'details', 'terminateWorkflowExecution_details' - Details for terminating the workflow execution.
--
-- 'reason', 'terminateWorkflowExecution_reason' - A descriptive reason for terminating the workflow execution.
--
-- 'domain', 'terminateWorkflowExecution_domain' - The domain of the workflow execution to terminate.
--
-- 'workflowId', 'terminateWorkflowExecution_workflowId' - The workflowId of the workflow execution to terminate.
newTerminateWorkflowExecution ::
  -- | 'domain'
  Core.Text ->
  -- | 'workflowId'
  Core.Text ->
  TerminateWorkflowExecution
newTerminateWorkflowExecution pDomain_ pWorkflowId_ =
  TerminateWorkflowExecution'
    { runId = Core.Nothing,
      childPolicy = Core.Nothing,
      details = Core.Nothing,
      reason = Core.Nothing,
      domain = pDomain_,
      workflowId = pWorkflowId_
    }

-- | The runId of the workflow execution to terminate.
terminateWorkflowExecution_runId :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Core.Text)
terminateWorkflowExecution_runId = Lens.lens (\TerminateWorkflowExecution' {runId} -> runId) (\s@TerminateWorkflowExecution' {} a -> s {runId = a} :: TerminateWorkflowExecution)

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
terminateWorkflowExecution_childPolicy :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe ChildPolicy)
terminateWorkflowExecution_childPolicy = Lens.lens (\TerminateWorkflowExecution' {childPolicy} -> childPolicy) (\s@TerminateWorkflowExecution' {} a -> s {childPolicy = a} :: TerminateWorkflowExecution)

-- | Details for terminating the workflow execution.
terminateWorkflowExecution_details :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Core.Text)
terminateWorkflowExecution_details = Lens.lens (\TerminateWorkflowExecution' {details} -> details) (\s@TerminateWorkflowExecution' {} a -> s {details = a} :: TerminateWorkflowExecution)

-- | A descriptive reason for terminating the workflow execution.
terminateWorkflowExecution_reason :: Lens.Lens' TerminateWorkflowExecution (Core.Maybe Core.Text)
terminateWorkflowExecution_reason = Lens.lens (\TerminateWorkflowExecution' {reason} -> reason) (\s@TerminateWorkflowExecution' {} a -> s {reason = a} :: TerminateWorkflowExecution)

-- | The domain of the workflow execution to terminate.
terminateWorkflowExecution_domain :: Lens.Lens' TerminateWorkflowExecution Core.Text
terminateWorkflowExecution_domain = Lens.lens (\TerminateWorkflowExecution' {domain} -> domain) (\s@TerminateWorkflowExecution' {} a -> s {domain = a} :: TerminateWorkflowExecution)

-- | The workflowId of the workflow execution to terminate.
terminateWorkflowExecution_workflowId :: Lens.Lens' TerminateWorkflowExecution Core.Text
terminateWorkflowExecution_workflowId = Lens.lens (\TerminateWorkflowExecution' {workflowId} -> workflowId) (\s@TerminateWorkflowExecution' {} a -> s {workflowId = a} :: TerminateWorkflowExecution)

instance Core.AWSRequest TerminateWorkflowExecution where
  type
    AWSResponse TerminateWorkflowExecution =
      TerminateWorkflowExecutionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      TerminateWorkflowExecutionResponse'

instance Core.Hashable TerminateWorkflowExecution

instance Core.NFData TerminateWorkflowExecution

instance Core.ToHeaders TerminateWorkflowExecution where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SimpleWorkflowService.TerminateWorkflowExecution" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.0" :: Core.ByteString)
          ]
      )

instance Core.ToJSON TerminateWorkflowExecution where
  toJSON TerminateWorkflowExecution' {..} =
    Core.object
      ( Core.catMaybes
          [ ("runId" Core..=) Core.<$> runId,
            ("childPolicy" Core..=) Core.<$> childPolicy,
            ("details" Core..=) Core.<$> details,
            ("reason" Core..=) Core.<$> reason,
            Core.Just ("domain" Core..= domain),
            Core.Just ("workflowId" Core..= workflowId)
          ]
      )

instance Core.ToPath TerminateWorkflowExecution where
  toPath = Core.const "/"

instance Core.ToQuery TerminateWorkflowExecution where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newTerminateWorkflowExecutionResponse' smart constructor.
data TerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TerminateWorkflowExecutionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newTerminateWorkflowExecutionResponse ::
  TerminateWorkflowExecutionResponse
newTerminateWorkflowExecutionResponse =
  TerminateWorkflowExecutionResponse'

instance
  Core.NFData
    TerminateWorkflowExecutionResponse
