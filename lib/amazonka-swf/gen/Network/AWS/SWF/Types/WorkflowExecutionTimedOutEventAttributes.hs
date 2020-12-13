{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionTimedOutEventAttributes
  ( WorkflowExecutionTimedOutEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionTimedOutEventAttributes,

    -- * Lenses
    wetoeaTimeoutType,
    wetoeaChildPolicy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.ChildPolicy
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType

-- | Provides the details of the @WorkflowExecutionTimedOut@ event.
--
-- /See:/ 'mkWorkflowExecutionTimedOutEventAttributes' smart constructor.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes'
  { -- | The type of timeout that caused this event.
    timeoutType :: WorkflowExecutionTimeoutType,
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
    childPolicy :: ChildPolicy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- * 'timeoutType' - The type of timeout that caused this event.
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
mkWorkflowExecutionTimedOutEventAttributes ::
  -- | 'timeoutType'
  WorkflowExecutionTimeoutType ->
  -- | 'childPolicy'
  ChildPolicy ->
  WorkflowExecutionTimedOutEventAttributes
mkWorkflowExecutionTimedOutEventAttributes
  pTimeoutType_
  pChildPolicy_ =
    WorkflowExecutionTimedOutEventAttributes'
      { timeoutType =
          pTimeoutType_,
        childPolicy = pChildPolicy_
      }

-- | The type of timeout that caused this event.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wetoeaTimeoutType :: Lens.Lens' WorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
wetoeaTimeoutType = Lens.lens (timeoutType :: WorkflowExecutionTimedOutEventAttributes -> WorkflowExecutionTimeoutType) (\s a -> s {timeoutType = a} :: WorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED wetoeaTimeoutType "Use generic-lens or generic-optics with 'timeoutType' instead." #-}

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
wetoeaChildPolicy :: Lens.Lens' WorkflowExecutionTimedOutEventAttributes ChildPolicy
wetoeaChildPolicy = Lens.lens (childPolicy :: WorkflowExecutionTimedOutEventAttributes -> ChildPolicy) (\s a -> s {childPolicy = a} :: WorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED wetoeaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

instance Lude.FromJSON WorkflowExecutionTimedOutEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          WorkflowExecutionTimedOutEventAttributes'
            Lude.<$> (x Lude..: "timeoutType") Lude.<*> (x Lude..: "childPolicy")
      )
