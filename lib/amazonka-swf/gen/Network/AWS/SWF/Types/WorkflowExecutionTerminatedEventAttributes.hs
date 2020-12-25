{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTerminatedEventAttributes
  ( WorkflowExecutionTerminatedEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionTerminatedEventAttributes,

    -- * Lenses
    weteaChildPolicy,
    weteaCause,
    weteaDetails,
    weteaReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.TerminateReason as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause as Types

-- | Provides the details of the @WorkflowExecutionTerminated@ event.
--
-- /See:/ 'mkWorkflowExecutionTerminatedEventAttributes' smart constructor.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes'
  { -- | The policy used for the child workflow executions of this workflow execution.
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
    childPolicy :: Types.ChildPolicy,
    -- | If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
    cause :: Core.Maybe Types.WorkflowExecutionTerminatedCause,
    -- | The details provided for the termination.
    details :: Core.Maybe Types.Data,
    -- | The reason provided for the termination.
    reason :: Core.Maybe Types.TerminateReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionTerminatedEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionTerminatedEventAttributes ::
  -- | 'childPolicy'
  Types.ChildPolicy ->
  WorkflowExecutionTerminatedEventAttributes
mkWorkflowExecutionTerminatedEventAttributes childPolicy =
  WorkflowExecutionTerminatedEventAttributes'
    { childPolicy,
      cause = Core.Nothing,
      details = Core.Nothing,
      reason = Core.Nothing
    }

-- | The policy used for the child workflow executions of this workflow execution.
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
weteaChildPolicy :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes Types.ChildPolicy
weteaChildPolicy = Lens.field @"childPolicy"
{-# DEPRECATED weteaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weteaCause :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Core.Maybe Types.WorkflowExecutionTerminatedCause)
weteaCause = Lens.field @"cause"
{-# DEPRECATED weteaCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The details provided for the termination.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weteaDetails :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Core.Maybe Types.Data)
weteaDetails = Lens.field @"details"
{-# DEPRECATED weteaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The reason provided for the termination.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weteaReason :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Core.Maybe Types.TerminateReason)
weteaReason = Lens.field @"reason"
{-# DEPRECATED weteaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON WorkflowExecutionTerminatedEventAttributes where
  parseJSON =
    Core.withObject "WorkflowExecutionTerminatedEventAttributes" Core.$
      \x ->
        WorkflowExecutionTerminatedEventAttributes'
          Core.<$> (x Core..: "childPolicy")
          Core.<*> (x Core..:? "cause")
          Core.<*> (x Core..:? "details")
          Core.<*> (x Core..:? "reason")
