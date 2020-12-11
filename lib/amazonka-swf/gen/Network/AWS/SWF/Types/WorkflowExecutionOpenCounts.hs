-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionOpenCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionOpenCounts
  ( WorkflowExecutionOpenCounts (..),

    -- * Smart constructor
    mkWorkflowExecutionOpenCounts,

    -- * Lenses
    weocOpenLambdaFunctions,
    weocOpenActivityTasks,
    weocOpenDecisionTasks,
    weocOpenTimers,
    weocOpenChildWorkflowExecutions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the counts of open tasks, child workflow executions and timers for a workflow execution.
--
-- /See:/ 'mkWorkflowExecutionOpenCounts' smart constructor.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts'
  { openLambdaFunctions ::
      Lude.Maybe Lude.Natural,
    openActivityTasks :: Lude.Natural,
    openDecisionTasks :: Lude.Natural,
    openTimers :: Lude.Natural,
    openChildWorkflowExecutions ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionOpenCounts' with the minimum fields required to make a request.
--
-- * 'openActivityTasks' - The count of activity tasks whose status is @OPEN@ .
-- * 'openChildWorkflowExecutions' - The count of child workflow executions whose status is @OPEN@ .
-- * 'openDecisionTasks' - The count of decision tasks whose status is OPEN. A workflow execution can have at most one open decision task.
-- * 'openLambdaFunctions' - The count of Lambda tasks whose status is @OPEN@ .
-- * 'openTimers' - The count of timers started by this workflow execution that have not fired yet.
mkWorkflowExecutionOpenCounts ::
  -- | 'openActivityTasks'
  Lude.Natural ->
  -- | 'openDecisionTasks'
  Lude.Natural ->
  -- | 'openTimers'
  Lude.Natural ->
  -- | 'openChildWorkflowExecutions'
  Lude.Natural ->
  WorkflowExecutionOpenCounts
mkWorkflowExecutionOpenCounts
  pOpenActivityTasks_
  pOpenDecisionTasks_
  pOpenTimers_
  pOpenChildWorkflowExecutions_ =
    WorkflowExecutionOpenCounts'
      { openLambdaFunctions = Lude.Nothing,
        openActivityTasks = pOpenActivityTasks_,
        openDecisionTasks = pOpenDecisionTasks_,
        openTimers = pOpenTimers_,
        openChildWorkflowExecutions = pOpenChildWorkflowExecutions_
      }

-- | The count of Lambda tasks whose status is @OPEN@ .
--
-- /Note:/ Consider using 'openLambdaFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenLambdaFunctions :: Lens.Lens' WorkflowExecutionOpenCounts (Lude.Maybe Lude.Natural)
weocOpenLambdaFunctions = Lens.lens (openLambdaFunctions :: WorkflowExecutionOpenCounts -> Lude.Maybe Lude.Natural) (\s a -> s {openLambdaFunctions = a} :: WorkflowExecutionOpenCounts)
{-# DEPRECATED weocOpenLambdaFunctions "Use generic-lens or generic-optics with 'openLambdaFunctions' instead." #-}

-- | The count of activity tasks whose status is @OPEN@ .
--
-- /Note:/ Consider using 'openActivityTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenActivityTasks :: Lens.Lens' WorkflowExecutionOpenCounts Lude.Natural
weocOpenActivityTasks = Lens.lens (openActivityTasks :: WorkflowExecutionOpenCounts -> Lude.Natural) (\s a -> s {openActivityTasks = a} :: WorkflowExecutionOpenCounts)
{-# DEPRECATED weocOpenActivityTasks "Use generic-lens or generic-optics with 'openActivityTasks' instead." #-}

-- | The count of decision tasks whose status is OPEN. A workflow execution can have at most one open decision task.
--
-- /Note:/ Consider using 'openDecisionTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenDecisionTasks :: Lens.Lens' WorkflowExecutionOpenCounts Lude.Natural
weocOpenDecisionTasks = Lens.lens (openDecisionTasks :: WorkflowExecutionOpenCounts -> Lude.Natural) (\s a -> s {openDecisionTasks = a} :: WorkflowExecutionOpenCounts)
{-# DEPRECATED weocOpenDecisionTasks "Use generic-lens or generic-optics with 'openDecisionTasks' instead." #-}

-- | The count of timers started by this workflow execution that have not fired yet.
--
-- /Note:/ Consider using 'openTimers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenTimers :: Lens.Lens' WorkflowExecutionOpenCounts Lude.Natural
weocOpenTimers = Lens.lens (openTimers :: WorkflowExecutionOpenCounts -> Lude.Natural) (\s a -> s {openTimers = a} :: WorkflowExecutionOpenCounts)
{-# DEPRECATED weocOpenTimers "Use generic-lens or generic-optics with 'openTimers' instead." #-}

-- | The count of child workflow executions whose status is @OPEN@ .
--
-- /Note:/ Consider using 'openChildWorkflowExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenChildWorkflowExecutions :: Lens.Lens' WorkflowExecutionOpenCounts Lude.Natural
weocOpenChildWorkflowExecutions = Lens.lens (openChildWorkflowExecutions :: WorkflowExecutionOpenCounts -> Lude.Natural) (\s a -> s {openChildWorkflowExecutions = a} :: WorkflowExecutionOpenCounts)
{-# DEPRECATED weocOpenChildWorkflowExecutions "Use generic-lens or generic-optics with 'openChildWorkflowExecutions' instead." #-}

instance Lude.FromJSON WorkflowExecutionOpenCounts where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionOpenCounts"
      ( \x ->
          WorkflowExecutionOpenCounts'
            Lude.<$> (x Lude..:? "openLambdaFunctions")
            Lude.<*> (x Lude..: "openActivityTasks")
            Lude.<*> (x Lude..: "openDecisionTasks")
            Lude.<*> (x Lude..: "openTimers")
            Lude.<*> (x Lude..: "openChildWorkflowExecutions")
      )
