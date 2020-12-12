{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionSignaledEventAttributes
  ( WorkflowExecutionSignaledEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionSignaledEventAttributes,

    -- * Lenses
    wExternalWorkflowExecution,
    wExternalInitiatedEventId,
    wInput,
    wSignalName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @WorkflowExecutionSignaled@ event.
--
-- /See:/ 'mkWorkflowExecutionSignaledEventAttributes' smart constructor.
data WorkflowExecutionSignaledEventAttributes = WorkflowExecutionSignaledEventAttributes'
  { externalWorkflowExecution ::
      Lude.Maybe
        WorkflowExecution,
    externalInitiatedEventId ::
      Lude.Maybe
        Lude.Integer,
    input ::
      Lude.Maybe
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionSignaledEventAttributes' with the minimum fields required to make a request.
--
-- * 'externalInitiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflow@ decision to signal this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event. This field is set only if the signal was initiated by another workflow execution.
-- * 'externalWorkflowExecution' - The workflow execution that sent the signal. This is set only of the signal was sent by another workflow execution.
-- * 'input' - The inputs provided with the signal. The decider can use the signal name and inputs to determine how to process the signal.
-- * 'signalName' - The name of the signal received. The decider can use the signal name and inputs to determine how to the process the signal.
mkWorkflowExecutionSignaledEventAttributes ::
  -- | 'signalName'
  Lude.Text ->
  WorkflowExecutionSignaledEventAttributes
mkWorkflowExecutionSignaledEventAttributes pSignalName_ =
  WorkflowExecutionSignaledEventAttributes'
    { externalWorkflowExecution =
        Lude.Nothing,
      externalInitiatedEventId = Lude.Nothing,
      input = Lude.Nothing,
      signalName = pSignalName_
    }

-- | The workflow execution that sent the signal. This is set only of the signal was sent by another workflow execution.
--
-- /Note:/ Consider using 'externalWorkflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wExternalWorkflowExecution :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Lude.Maybe WorkflowExecution)
wExternalWorkflowExecution = Lens.lens (externalWorkflowExecution :: WorkflowExecutionSignaledEventAttributes -> Lude.Maybe WorkflowExecution) (\s a -> s {externalWorkflowExecution = a} :: WorkflowExecutionSignaledEventAttributes)
{-# DEPRECATED wExternalWorkflowExecution "Use generic-lens or generic-optics with 'externalWorkflowExecution' instead." #-}

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflow@ decision to signal this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event. This field is set only if the signal was initiated by another workflow execution.
--
-- /Note:/ Consider using 'externalInitiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wExternalInitiatedEventId :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Lude.Maybe Lude.Integer)
wExternalInitiatedEventId = Lens.lens (externalInitiatedEventId :: WorkflowExecutionSignaledEventAttributes -> Lude.Maybe Lude.Integer) (\s a -> s {externalInitiatedEventId = a} :: WorkflowExecutionSignaledEventAttributes)
{-# DEPRECATED wExternalInitiatedEventId "Use generic-lens or generic-optics with 'externalInitiatedEventId' instead." #-}

-- | The inputs provided with the signal. The decider can use the signal name and inputs to determine how to process the signal.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wInput :: Lens.Lens' WorkflowExecutionSignaledEventAttributes (Lude.Maybe Lude.Text)
wInput = Lens.lens (input :: WorkflowExecutionSignaledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {input = a} :: WorkflowExecutionSignaledEventAttributes)
{-# DEPRECATED wInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the signal received. The decider can use the signal name and inputs to determine how to the process the signal.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wSignalName :: Lens.Lens' WorkflowExecutionSignaledEventAttributes Lude.Text
wSignalName = Lens.lens (signalName :: WorkflowExecutionSignaledEventAttributes -> Lude.Text) (\s a -> s {signalName = a} :: WorkflowExecutionSignaledEventAttributes)
{-# DEPRECATED wSignalName "Use generic-lens or generic-optics with 'signalName' instead." #-}

instance Lude.FromJSON WorkflowExecutionSignaledEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionSignaledEventAttributes"
      ( \x ->
          WorkflowExecutionSignaledEventAttributes'
            Lude.<$> (x Lude..:? "externalWorkflowExecution")
            Lude.<*> (x Lude..:? "externalInitiatedEventId")
            Lude.<*> (x Lude..:? "input")
            Lude.<*> (x Lude..: "signalName")
      )
