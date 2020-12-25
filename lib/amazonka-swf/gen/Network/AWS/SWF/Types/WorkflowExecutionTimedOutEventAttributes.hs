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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.ChildPolicy as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionTimeoutType as Types

-- | Provides the details of the @WorkflowExecutionTimedOut@ event.
--
-- /See:/ 'mkWorkflowExecutionTimedOutEventAttributes' smart constructor.
data WorkflowExecutionTimedOutEventAttributes = WorkflowExecutionTimedOutEventAttributes'
  { -- | The type of timeout that caused this event.
    timeoutType :: Types.WorkflowExecutionTimeoutType,
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
    childPolicy :: Types.ChildPolicy
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionTimedOutEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionTimedOutEventAttributes ::
  -- | 'timeoutType'
  Types.WorkflowExecutionTimeoutType ->
  -- | 'childPolicy'
  Types.ChildPolicy ->
  WorkflowExecutionTimedOutEventAttributes
mkWorkflowExecutionTimedOutEventAttributes timeoutType childPolicy =
  WorkflowExecutionTimedOutEventAttributes'
    { timeoutType,
      childPolicy
    }

-- | The type of timeout that caused this event.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wetoeaTimeoutType :: Lens.Lens' WorkflowExecutionTimedOutEventAttributes Types.WorkflowExecutionTimeoutType
wetoeaTimeoutType = Lens.field @"timeoutType"
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
wetoeaChildPolicy :: Lens.Lens' WorkflowExecutionTimedOutEventAttributes Types.ChildPolicy
wetoeaChildPolicy = Lens.field @"childPolicy"
{-# DEPRECATED wetoeaChildPolicy "Use generic-lens or generic-optics with 'childPolicy' instead." #-}

instance Core.FromJSON WorkflowExecutionTimedOutEventAttributes where
  parseJSON =
    Core.withObject "WorkflowExecutionTimedOutEventAttributes" Core.$
      \x ->
        WorkflowExecutionTimedOutEventAttributes'
          Core.<$> (x Core..: "timeoutType") Core.<*> (x Core..: "childPolicy")
