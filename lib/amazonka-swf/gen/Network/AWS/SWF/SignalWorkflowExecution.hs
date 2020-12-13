{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.SignalWorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Records a @WorkflowExecutionSignaled@ event in the workflow execution history and creates a decision task for the workflow execution identified by the given domain, workflowId and runId. The event is recorded with the specified user defined signalName and input (if provided).
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
module Network.AWS.SWF.SignalWorkflowExecution
  ( -- * Creating a request
    SignalWorkflowExecution (..),
    mkSignalWorkflowExecution,

    -- ** Request lenses
    sDomain,
    sInput,
    sRunId,
    sWorkflowId,
    sSignalName,

    -- * Destructuring the response
    SignalWorkflowExecutionResponse (..),
    mkSignalWorkflowExecutionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SWF.Types

-- | /See:/ 'mkSignalWorkflowExecution' smart constructor.
data SignalWorkflowExecution = SignalWorkflowExecution'
  { -- | The name of the domain containing the workflow execution to signal.
    domain :: Lude.Text,
    -- | Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
    input :: Lude.Maybe Lude.Text,
    -- | The runId of the workflow execution to signal.
    runId :: Lude.Maybe Lude.Text,
    -- | The workflowId of the workflow execution to signal.
    workflowId :: Lude.Text,
    -- | The name of the signal. This name must be meaningful to the target workflow.
    signalName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SignalWorkflowExecution' with the minimum fields required to make a request.
--
-- * 'domain' - The name of the domain containing the workflow execution to signal.
-- * 'input' - Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
-- * 'runId' - The runId of the workflow execution to signal.
-- * 'workflowId' - The workflowId of the workflow execution to signal.
-- * 'signalName' - The name of the signal. This name must be meaningful to the target workflow.
mkSignalWorkflowExecution ::
  -- | 'domain'
  Lude.Text ->
  -- | 'workflowId'
  Lude.Text ->
  -- | 'signalName'
  Lude.Text ->
  SignalWorkflowExecution
mkSignalWorkflowExecution pDomain_ pWorkflowId_ pSignalName_ =
  SignalWorkflowExecution'
    { domain = pDomain_,
      input = Lude.Nothing,
      runId = Lude.Nothing,
      workflowId = pWorkflowId_,
      signalName = pSignalName_
    }

-- | The name of the domain containing the workflow execution to signal.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sDomain :: Lens.Lens' SignalWorkflowExecution Lude.Text
sDomain = Lens.lens (domain :: SignalWorkflowExecution -> Lude.Text) (\s a -> s {domain = a} :: SignalWorkflowExecution)
{-# DEPRECATED sDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | Data to attach to the @WorkflowExecutionSignaled@ event in the target workflow execution's history.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sInput :: Lens.Lens' SignalWorkflowExecution (Lude.Maybe Lude.Text)
sInput = Lens.lens (input :: SignalWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: SignalWorkflowExecution)
{-# DEPRECATED sInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The runId of the workflow execution to signal.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sRunId :: Lens.Lens' SignalWorkflowExecution (Lude.Maybe Lude.Text)
sRunId = Lens.lens (runId :: SignalWorkflowExecution -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: SignalWorkflowExecution)
{-# DEPRECATED sRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The workflowId of the workflow execution to signal.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sWorkflowId :: Lens.Lens' SignalWorkflowExecution Lude.Text
sWorkflowId = Lens.lens (workflowId :: SignalWorkflowExecution -> Lude.Text) (\s a -> s {workflowId = a} :: SignalWorkflowExecution)
{-# DEPRECATED sWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The name of the signal. This name must be meaningful to the target workflow.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSignalName :: Lens.Lens' SignalWorkflowExecution Lude.Text
sSignalName = Lens.lens (signalName :: SignalWorkflowExecution -> Lude.Text) (\s a -> s {signalName = a} :: SignalWorkflowExecution)
{-# DEPRECATED sSignalName "Use generic-lens or generic-optics with 'signalName' instead." #-}

instance Lude.AWSRequest SignalWorkflowExecution where
  type Rs SignalWorkflowExecution = SignalWorkflowExecutionResponse
  request = Req.postJSON swfService
  response = Res.receiveNull SignalWorkflowExecutionResponse'

instance Lude.ToHeaders SignalWorkflowExecution where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "SimpleWorkflowService.SignalWorkflowExecution" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SignalWorkflowExecution where
  toJSON SignalWorkflowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("domain" Lude..= domain),
            ("input" Lude..=) Lude.<$> input,
            ("runId" Lude..=) Lude.<$> runId,
            Lude.Just ("workflowId" Lude..= workflowId),
            Lude.Just ("signalName" Lude..= signalName)
          ]
      )

instance Lude.ToPath SignalWorkflowExecution where
  toPath = Lude.const "/"

instance Lude.ToQuery SignalWorkflowExecution where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSignalWorkflowExecutionResponse' smart constructor.
data SignalWorkflowExecutionResponse = SignalWorkflowExecutionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SignalWorkflowExecutionResponse' with the minimum fields required to make a request.
mkSignalWorkflowExecutionResponse ::
  SignalWorkflowExecutionResponse
mkSignalWorkflowExecutionResponse =
  SignalWorkflowExecutionResponse'
