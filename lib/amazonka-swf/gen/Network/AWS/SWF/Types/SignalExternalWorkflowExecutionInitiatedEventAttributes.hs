{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
  ( SignalExternalWorkflowExecutionInitiatedEventAttributes (..),

    -- * Smart constructor
    mkSignalExternalWorkflowExecutionInitiatedEventAttributes,

    -- * Lenses
    seweieaControl,
    seweieaInput,
    seweieaRunId,
    seweieaWorkflowId,
    seweieaSignalName,
    seweieaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @SignalExternalWorkflowExecutionInitiated@ event.
--
-- /See:/ 'mkSignalExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes'
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
      Lude.Text,
    decisionTaskCompletedEventId ::
      Lude.Integer
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

-- | Creates a value of 'SignalExternalWorkflowExecutionInitiatedEventAttributes' with the minimum fields required to make a request.
--
-- * 'control' - Data attached to the event that can be used by the decider in subsequent decision tasks.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @SignalExternalWorkflowExecution@ decision for this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'input' - The input provided to the signal.
-- * 'runId' - The @runId@ of the external workflow execution to send the signal to.
-- * 'signalName' - The name of the signal.
-- * 'workflowId' - The @workflowId@ of the external workflow execution.
mkSignalExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Lude.Text ->
  -- | 'signalName'
  Lude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  SignalExternalWorkflowExecutionInitiatedEventAttributes
mkSignalExternalWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pSignalName_
  pDecisionTaskCompletedEventId_ =
    SignalExternalWorkflowExecutionInitiatedEventAttributes'
      { control =
          Lude.Nothing,
        input = Lude.Nothing,
        runId = Lude.Nothing,
        workflowId = pWorkflowId_,
        signalName = pSignalName_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent decision tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaControl :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
seweieaControl = Lens.lens (control :: SignalExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED seweieaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The input provided to the signal.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaInput :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
seweieaInput = Lens.lens (input :: SignalExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED seweieaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The @runId@ of the external workflow execution to send the signal to.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaRunId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
seweieaRunId = Lens.lens (runId :: SignalExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED seweieaRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The @workflowId@ of the external workflow execution.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaWorkflowId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Lude.Text
seweieaWorkflowId = Lens.lens (workflowId :: SignalExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Text) (\s a -> s {workflowId = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED seweieaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The name of the signal.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaSignalName :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Lude.Text
seweieaSignalName = Lens.lens (signalName :: SignalExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Text) (\s a -> s {signalName = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED seweieaSignalName "Use generic-lens or generic-optics with 'signalName' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @SignalExternalWorkflowExecution@ decision for this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaDecisionTaskCompletedEventId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Lude.Integer
seweieaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: SignalExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: SignalExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED seweieaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance
  Lude.FromJSON
    SignalExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Lude.withObject
      "SignalExternalWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          SignalExternalWorkflowExecutionInitiatedEventAttributes'
            Lude.<$> (x Lude..:? "control")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..:? "runId")
            Lude.<*> (x Lude..: "workflowId")
            Lude.<*> (x Lude..: "signalName")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
