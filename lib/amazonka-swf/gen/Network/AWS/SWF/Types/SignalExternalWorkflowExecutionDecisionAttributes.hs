-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionDecisionAttributes
  ( SignalExternalWorkflowExecutionDecisionAttributes (..),

    -- * Smart constructor
    mkSignalExternalWorkflowExecutionDecisionAttributes,

    -- * Lenses
    sewedaControl,
    sewedaInput,
    sewedaRunId,
    sewedaWorkflowId,
    sewedaSignalName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @SignalExternalWorkflowExecution@ decision.
--
-- __Access Control__
-- You can use IAM policies to control this decision's access to Amazon SWF resources as follows:
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
--
-- /See:/ 'mkSignalExternalWorkflowExecutionDecisionAttributes' smart constructor.
data SignalExternalWorkflowExecutionDecisionAttributes = SignalExternalWorkflowExecutionDecisionAttributes'
  { control ::
      Lude.Maybe
        Lude.Text,
    input ::
      Lude.Maybe
        Lude.Text,
    runId ::
      Lude.Maybe
        Lude.Text,
    workflowId ::
      Lude.Text,
    signalName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'SignalExternalWorkflowExecutionDecisionAttributes' with the minimum fields required to make a request.
--
-- * 'control' - The data attached to the event that can be used by the decider in subsequent decision tasks.
-- * 'input' - The input data to be provided with the signal. The target workflow execution uses the signal name and input data to process the signal.
-- * 'runId' - The @runId@ of the workflow execution to be signaled.
-- * 'signalName' - The name of the signal.The target workflow execution uses the signal name and input to process the signal.
-- * 'workflowId' - The @workflowId@ of the workflow execution to be signaled.
mkSignalExternalWorkflowExecutionDecisionAttributes ::
  -- | 'workflowId'
  Lude.Text ->
  -- | 'signalName'
  Lude.Text ->
  SignalExternalWorkflowExecutionDecisionAttributes
mkSignalExternalWorkflowExecutionDecisionAttributes
  pWorkflowId_
  pSignalName_ =
    SignalExternalWorkflowExecutionDecisionAttributes'
      { control =
          Lude.Nothing,
        input = Lude.Nothing,
        runId = Lude.Nothing,
        workflowId = pWorkflowId_,
        signalName = pSignalName_
      }

-- | The data attached to the event that can be used by the decider in subsequent decision tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaControl :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
sewedaControl = Lens.lens (control :: SignalExternalWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: SignalExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED sewedaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The input data to be provided with the signal. The target workflow execution uses the signal name and input data to process the signal.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaInput :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
sewedaInput = Lens.lens (input :: SignalExternalWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: SignalExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED sewedaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The @runId@ of the workflow execution to be signaled.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaRunId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes (Lude.Maybe Lude.Text)
sewedaRunId = Lens.lens (runId :: SignalExternalWorkflowExecutionDecisionAttributes -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: SignalExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED sewedaRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The @workflowId@ of the workflow execution to be signaled.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaWorkflowId :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Lude.Text
sewedaWorkflowId = Lens.lens (workflowId :: SignalExternalWorkflowExecutionDecisionAttributes -> Lude.Text) (\s a -> s {workflowId = a} :: SignalExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED sewedaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The name of the signal.The target workflow execution uses the signal name and input to process the signal.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sewedaSignalName :: Lens.Lens' SignalExternalWorkflowExecutionDecisionAttributes Lude.Text
sewedaSignalName = Lens.lens (signalName :: SignalExternalWorkflowExecutionDecisionAttributes -> Lude.Text) (\s a -> s {signalName = a} :: SignalExternalWorkflowExecutionDecisionAttributes)
{-# DEPRECATED sewedaSignalName "Use generic-lens or generic-optics with 'signalName' instead." #-}

instance
  Lude.ToJSON
    SignalExternalWorkflowExecutionDecisionAttributes
  where
  toJSON SignalExternalWorkflowExecutionDecisionAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("control" Lude..=) Lude.<$> control,
            ("input" Lude..=) Lude.<$> input,
            ("runId" Lude..=) Lude.<$> runId,
            Lude.Just ("workflowId" Lude..= workflowId),
            Lude.Just ("signalName" Lude..= signalName)
          ]
      )
