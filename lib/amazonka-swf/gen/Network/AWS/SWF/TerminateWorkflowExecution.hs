{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.TerminateWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionTerminated@ event and forces closure of the workflow execution identified by the given domain, runId, and workflowId. The child policy, registered with the workflow type or specified when starting this execution, is applied to any open child workflow executions of this workflow execution.
--
-- /Important:/ If the identified workflow execution was in progress, it is terminated immediately.
-- __Access Control__
-- You can use IAM policies to control this action's access to Amazon SWF resources as follows:
--
--     * Use a @Resource@ element with the domain name to limit the action to only specified domains.
--
--
--     * Use an @Action@ element to allow or deny permission to call this action.
--
--
--     * You cannot use an IAM policy to constrain this action's parameters.
--
--
-- If the caller doesn't have sufficient permissions to invoke the action, or the parameter values fall outside the specified constraints, the action fails. The associated event attribute's @cause@ parameter is set to @OPERATION_NOT_PERMITTED@ . For details and example IAM policies, see <https://docs.aws.amazon.com/amazonswf/latest/developerguide/swf-dev-iam.html Using IAM to Manage Access to Amazon SWF Workflows> in the /Amazon SWF Developer Guide/ .
module Network.AWS.SWF.TerminateWorkflowExecution
  ( -- * Creating a request
    TerminateWorkflowExecution (..),
    mkTerminateWorkflowExecution,

    -- ** Request lenses
    tweReason,
    tweRunId,
    tweChildPolicy,
    tweDetails,
    tweDomain,
    tweWorkflowId,

    -- * Destructuring the response
    TerminateWorkflowExecutionResponse (..),
    mkTerminateWorkflowExecutionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkTerminateWorkflowExecution' smart constructor.
data TerminateWorkflowExecution = TerminateWorkflowExecution'
  { reason ::
      Lude.Maybe Lude.Text,
    runId :: Lude.Maybe Lude.Text,
    childPolicy :: Lude.Maybe ChildPolicy,
    details :: Lude.Maybe Lude.Text,
    domain :: Lude.Text,
    workflowId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateWorkflowExecution' with the minimum fields required to make a request.
--
-- * 'childPolicy' - If set, specifies the policy to use for the child workflow executions of the workflow execution being terminated. This policy overrides the child policy specified for the workflow execution at registration time or when starting the execution.
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
-- * 'details' - Details for terminating the workflow execution.
-- * 'domain' - The domain of the workflow execution to terminate.
-- * 'reason' - A descriptive reason for terminating the workflow execution.
-- * 'runId' - The runId of the workflow execution to terminate.
-- * 'workflowId' - The workflowId of the workflow execution to terminate.
mkTerminateWorkflowExecution ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowId'
  Lude.Text ->
  TerminateWorkflowExecution
mkTerminateWorkflowExecution pDomain_ pWorkflowId_ =
  TerminateWorkflowExecution'
    { reason = Lude.Nothing,
      runId = Lude.Nothing,
      childPolicy = Lude.Nothing,
      details = Lude.Nothing,
      domain = pDomain_,
      workflowId = pWorkflowId_
    }

-- | A descriptive reason for terminating the workflow execution.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweReason :: Lens.Lens' TerminateWorkflowExecution (Lude.Maybe Lude.Text)
tweReason = Lens.lens (reason :: TerminateWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: TerminateWorkflowExecution)
{-# DEPRECATED tweReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The runId of the workflow execution to terminate.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweRunId :: Lens.Lens' TerminateWorkflowExecution (Lude.Maybe Lude.Text)
tweRunId = Lens.lens (runId :: TerminateWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: TerminateWorkflowExecution)
{-# DEPRECATED tweRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | If set, specifies the policy to use for the child workflow executions of the workflow execution being terminated. This policy overrides the child policy specified for the workflow execution at registration time or when starting the execution.
--
-- The supported child policies are:
--
--     * @TERMINATE@ – The child executions are terminated.
--
--
--     * @REQUEST_CANCEL@ – A request to cancel is attempted for each child execution by recording a @WorkflowExecutionCancelRequested@ event in its history. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
--
--     * @ABANDON@ – No action is taken. The child executions continue to run.
--
--
--
-- /Note:/ Consider using 'childPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweChildPolicy :: Lens.Lens' TerminateWorkflowExecution (Lude.Maybe ChildPolicy)
tweChildPolicy = Lens.lens (childPolicy :: TerminateWorkflowExecution -> Lude.Maybe ChildPolicy) (\s a -> s {childPolicy = a} :: TerminateWorkflowExecution)
{-# DEPRECATED tweChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | Details for terminating the workflow execution.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweDetails :: Lens.Lens' TerminateWorkflowExecution (Lude.Maybe Lude.Text)
tweDetails = Lens.lens (details :: TerminateWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: TerminateWorkflowExecution)
{-# DEPRECATED tweDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The domain of the workflow execution to terminate.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweDomain :: Lens.Lens' TerminateWorkflowExecution Lude.Text
tweDomain = Lens.lens (domain :: TerminateWorkflowExecution -> Lude.Text) (\s a -> s {domain = a} :: TerminateWorkflowExecution)
{-# DEPRECATED tweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflowId of the workflow execution to terminate.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tweWorkflowId :: Lens.Lens' TerminateWorkflowExecution Lude.Text
tweWorkflowId = Lens.lens (workflowId :: TerminateWorkflowExecution -> Lude.Text) (\s a -> s {workflowId = a} :: TerminateWorkflowExecution)
{-# DEPRECATED tweWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

instance Lude.AWSRequest TerminateWorkflowExecution where
  type
    Rs TerminateWorkflowExecution =
      TerminateWorkflowExecutionResponse
  request = Req.postJSON swfService
  response = Res.receiveNull TerminateWorkflowExecutionResponse'

instance Lude.ToHeaders TerminateWorkflowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.TerminateWorkflowExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON TerminateWorkflowExecution where
  toJSON TerminateWorkflowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("reason" Lude..=) Lude.<$> reason,
            ("runId" Lude..=) Lude.<$> runId,
            ("childPolicy" Lude..=) Lude.<$> childPolicy,
            ("details" Lude..=) Lude.<$> details,
            Lude.Just ("domain" Lude..= domain),
            Lude.Just ("workflowId" Lude..= workflowId)
          ]
      )

instance Lude.ToPath TerminateWorkflowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery TerminateWorkflowExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkTerminateWorkflowExecutionResponse' smart constructor.
data TerminateWorkflowExecutionResponse = TerminateWorkflowExecutionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TerminateWorkflowExecutionResponse' with the minimum fields required to make a request.
mkTerminateWorkflowExecutionResponse ::
  TerminateWorkflowExecutionResponse
mkTerminateWorkflowExecutionResponse =
  TerminateWorkflowExecutionResponse'
