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
    weteaCause,
    weteaReason,
    weteaChildPolicy,
    weteaDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.WorkflowExecutionTerminatedCause

-- | Provides the details of the @WorkflowExecutionTerminated@ event.
--
-- /See:/ 'mkWorkflowExecutionTerminatedEventAttributes' smart constructor.
data WorkflowExecutionTerminatedEventAttributes = WorkflowExecutionTerminatedEventAttributes'
  { -- | If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
    cause :: Lude.Maybe WorkflowExecutionTerminatedCause,
    -- | The reason provided for the termination.
    reason :: Lude.Maybe Lude.Text,
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
    childPolicy :: ChildPolicy,
    -- | The details provided for the termination.
    details :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionTerminatedEventAttributes' with the minimum fields required to make a request.
--
-- * 'cause' - If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
-- * 'reason' - The reason provided for the termination.
-- * 'childPolicy' - The policy used for the child workflow executions of this workflow execution.
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
-- * 'details' - The details provided for the termination.
mkWorkflowExecutionTerminatedEventAttributes ::
  -- | 'childPolicy'
  ChildPolicy ->
  WorkflowExecutionTerminatedEventAttributes
mkWorkflowExecutionTerminatedEventAttributes pChildPolicy_ =
  WorkflowExecutionTerminatedEventAttributes'
    { cause = Lude.Nothing,
      reason = Lude.Nothing,
      childPolicy = pChildPolicy_,
      details = Lude.Nothing
    }

-- | If set, indicates that the workflow execution was automatically terminated, and specifies the cause. This happens if the parent workflow execution times out or is terminated and the child policy is set to terminate child executions.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weteaCause :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Lude.Maybe WorkflowExecutionTerminatedCause)
weteaCause = Lens.lens (cause :: WorkflowExecutionTerminatedEventAttributes -> Lude.Maybe WorkflowExecutionTerminatedCause) (\s a -> s {cause = a} :: WorkflowExecutionTerminatedEventAttributes)
{-# DEPRECATED weteaCause "Use generic-lens or generic-optics with 'cause' instead." #-}

-- | The reason provided for the termination.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weteaReason :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Lude.Maybe Lude.Text)
weteaReason = Lens.lens (reason :: WorkflowExecutionTerminatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: WorkflowExecutionTerminatedEventAttributes)
{-# DEPRECATED weteaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

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
weteaChildPolicy :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes ChildPolicy
weteaChildPolicy = Lens.lens (childPolicy :: WorkflowExecutionTerminatedEventAttributes -> ChildPolicy) (\s a -> s {childPolicy = a} :: WorkflowExecutionTerminatedEventAttributes)
{-# DEPRECATED weteaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

-- | The details provided for the termination.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weteaDetails :: Lens.Lens' WorkflowExecutionTerminatedEventAttributes (Lude.Maybe Lude.Text)
weteaDetails = Lens.lens (details :: WorkflowExecutionTerminatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: WorkflowExecutionTerminatedEventAttributes)
{-# DEPRECATED weteaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance Lude.FromJSON WorkflowExecutionTerminatedEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionTerminatedEventAttributes"
      ( \x ->
          WorkflowExecutionTerminatedEventAttributes'
            Lude.<$> (x Lude..:? "cause")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..: "childPolicy")
            Lude.<*> (x Lude..:? "details")
      )
