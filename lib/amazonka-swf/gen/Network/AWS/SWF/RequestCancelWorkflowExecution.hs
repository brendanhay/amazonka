{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.RequestCancelWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionCancelRequested@ event in the currently running workflow execution identified by the given domain, workflowId, and runId. This logically requests the cancellation of the workflow execution as a whole. It is up to the decider to take appropriate actions when it receives an execution history with this event.
--
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
module Network.AWS.SWF.RequestCancelWorkflowExecution
  ( -- * Creating a request
    RequestCancelWorkflowExecution (..),
    mkRequestCancelWorkflowExecution,

    -- ** Request lenses
    rcweRunId,
    rcweDomain,
    rcweWorkflowId,

    -- * Destructuring the response
    RequestCancelWorkflowExecutionResponse (..),
    mkRequestCancelWorkflowExecutionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkRequestCancelWorkflowExecution' smart constructor.
data RequestCancelWorkflowExecution = RequestCancelWorkflowExecution'
  { runId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'RequestCancelWorkflowExecution' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain containing the workflow execution to cancel.
-- * 'runId' - The runId of the workflow execution to cancel.
-- * 'workflowId' - The workflowId of the workflow execution to cancel.
mkRequestCancelWorkflowExecution ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowId'
  Lude.Text ->
  RequestCancelWorkflowExecution
mkRequestCancelWorkflowExecution pDomain_ pWorkflowId_ =
  RequestCancelWorkflowExecution'
    { runId = Lude.Nothing,
      domain = pDomain_,
      workflowId = pWorkflowId_
    }

-- | The runId of the workflow execution to cancel.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcweRunId :: Lens.Lens' RequestCancelWorkflowExecution (Lude.Maybe Lude.Text)
rcweRunId = Lens.lens (runId :: RequestCancelWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: RequestCancelWorkflowExecution)
{-# DEPRECATED rcweRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The name of the domain containing the workflow execution to cancel.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcweDomain :: Lens.Lens' RequestCancelWorkflowExecution Lude.Text
rcweDomain = Lens.lens (domain :: RequestCancelWorkflowExecution -> Lude.Text) (\s a -> s {domain = a} :: RequestCancelWorkflowExecution)
{-# DEPRECATED rcweDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The workflowId of the workflow execution to cancel.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcweWorkflowId :: Lens.Lens' RequestCancelWorkflowExecution Lude.Text
rcweWorkflowId = Lens.lens (workflowId :: RequestCancelWorkflowExecution -> Lude.Text) (\s a -> s {workflowId = a} :: RequestCancelWorkflowExecution)
{-# DEPRECATED rcweWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

instance Lude.AWSRequest RequestCancelWorkflowExecution where
  type
    Rs RequestCancelWorkflowExecution =
      RequestCancelWorkflowExecutionResponse
  request = Req.postJSON swfService
  response = Res.receiveNull RequestCancelWorkflowExecutionResponse'

instance Lude.ToHeaders RequestCancelWorkflowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.RequestCancelWorkflowExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RequestCancelWorkflowExecution where
  toJSON RequestCancelWorkflowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("runId" Lude..=) Lude.<$> runId,
            Lude.Just ("domain" Lude..= domain),
            Lude.Just ("workflowId" Lude..= workflowId)
          ]
      )

instance Lude.ToPath RequestCancelWorkflowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery RequestCancelWorkflowExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRequestCancelWorkflowExecutionResponse' smart constructor.
data RequestCancelWorkflowExecutionResponse = RequestCancelWorkflowExecutionResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RequestCancelWorkflowExecutionResponse' with the minimum fields required to make a request.
mkRequestCancelWorkflowExecutionResponse ::
  RequestCancelWorkflowExecutionResponse
mkRequestCancelWorkflowExecutionResponse =
  RequestCancelWorkflowExecutionResponse'
