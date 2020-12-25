{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  ( RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),

    -- * Smart constructor
    mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,

    -- * Lenses
    rceweieaWorkflowId,
    rceweieaDecisionTaskCompletedEventId,
    rceweieaControl,
    rceweieaRunId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Control as Types
import qualified Network.AWS.SWF.Types.RunId as Types
import qualified Network.AWS.SWF.Types.WorkflowId as Types

-- | Provides the details of the @RequestCancelExternalWorkflowExecutionInitiated@ event.
--
-- /See:/ 'mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
  { -- | The @workflowId@ of the external workflow execution to be canceled.
    workflowId :: Types.WorkflowId,
    -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelExternalWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | Data attached to the event that can be used by the decider in subsequent workflow tasks.
    control :: Core.Maybe Types.Control,
    -- | The @runId@ of the external workflow execution to be canceled.
    runId :: Core.Maybe Types.RunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' value with any optional fields omitted.
mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Types.WorkflowId ->
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  workflowId
  decisionTaskCompletedEventId =
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
      { workflowId,
        decisionTaskCompletedEventId,
        control = Core.Nothing,
        runId = Core.Nothing
      }

-- | The @workflowId@ of the external workflow execution to be canceled.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaWorkflowId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Types.WorkflowId
rceweieaWorkflowId = Lens.field @"workflowId"
{-# DEPRECATED rceweieaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelExternalWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaDecisionTaskCompletedEventId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Core.Integer
rceweieaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED rceweieaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaControl :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.Control)
rceweieaControl = Lens.field @"control"
{-# DEPRECATED rceweieaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The @runId@ of the external workflow execution to be canceled.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaRunId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Core.Maybe Types.RunId)
rceweieaRunId = Lens.field @"runId"
{-# DEPRECATED rceweieaRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance
  Core.FromJSON
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Core.withObject
      "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
      Core.$ \x ->
        RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
          Core.<$> (x Core..: "workflowId")
            Core.<*> (x Core..: "decisionTaskCompletedEventId")
            Core.<*> (x Core..:? "control")
            Core.<*> (x Core..:? "runId")
