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
    seweieaWorkflowId,
    seweieaSignalName,
    seweieaDecisionTaskCompletedEventId,
    seweieaControl,
    seweieaInput,
    seweieaRunId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.RunId as Types
import qualified Network.AWS.SWF.Types.SignalName as Types
import qualified Network.AWS.SWF.Types.WorkflowId as Types

-- | Provides the details of the @SignalExternalWorkflowExecutionInitiated@ event.
--
-- /See:/ 'mkSignalExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data SignalExternalWorkflowExecutionInitiatedEventAttributes = SignalExternalWorkflowExecutionInitiatedEventAttributes'
  { -- | The @workflowId@ of the external workflow execution.
    workflowId :: Types.WorkflowId,
    -- | The name of the signal.
    signalName :: Types.SignalName,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @SignalExternalWorkflowExecution@ decision for this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | Data attached to the event that can be used by the decider in subsequent decision tasks.
    control :: Core.Maybe Types.Data,
    -- | The input provided to the signal.
    input :: Core.Maybe Types.Data,
    -- | The @runId@ of the external workflow execution to send the signal to.
    runId :: Core.Maybe Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SignalExternalWorkflowExecutionInitiatedEventAttributes' value with any optional fields omitted.
mkSignalExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Types.WorkflowId ->
  -- | 'signalName'
  Types.SignalName ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  SignalExternalWorkflowExecutionInitiatedEventAttributes
mkSignalExternalWorkflowExecutionInitiatedEventAttributes
  workflowId
  signalName
  decisionTaskCompletedEventId =
    SignalExternalWorkflowExecutionInitiatedEventAttributes'
      { workflowId,
        signalName,
        decisionTaskCompletedEventId,
        control = Core.Nothing,
        input = Core.Nothing,
        runId = Core.Nothing
      }

-- | The @workflowId@ of the external workflow execution.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaWorkflowId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Types.WorkflowId
seweieaWorkflowId = Lens.field @"workflowId"
{-# DEPRECATED seweieaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The name of the signal.
--
-- /Note:/ Consider using 'signalName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaSignalName :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Types.SignalName
seweieaSignalName = Lens.field @"signalName"
{-# DEPRECATED seweieaSignalName "Use generic-lens or generic-optics with 'signalName' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @SignalExternalWorkflowExecution@ decision for this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaDecisionTaskCompletedEventId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes Core.Integer
seweieaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED seweieaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | Data attached to the event that can be used by the decider in subsequent decision tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaControl :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.Data)
seweieaControl = Lens.field @"control"
{-# DEPRECATED seweieaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The input provided to the signal.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaInput :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.Data)
seweieaInput = Lens.field @"input"
{-# DEPRECATED seweieaInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The @runId@ of the external workflow execution to send the signal to.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
seweieaRunId :: Lens.Lens' SignalExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.RunId)
seweieaRunId = Lens.field @"runId"
{-# DEPRECATED seweieaRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance
  Core.FromJSON
    SignalExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Core.withObject
      "SignalExternalWorkflowExecutionInitiatedEventAttributes"
      Core.$ \x ->
        SignalExternalWorkflowExecutionInitiatedEventAttributes'
          Core.<$> (x Core..: "workflowId")
          Core.<*> (x Core..: "signalName")
          Core.<*> (x Core..: "decisionTaskCompletedEventId")
          Core.<*> (x Core..:? "control")
          Core.<*> (x Core..:? "input")
          Core.<*> (x Core..:? "runId")
