{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    weocOpenActivityTasks,
    weocOpenDecisionTasks,
    weocOpenTimers,
    weocOpenChildWorkflowExecutions,
    weocOpenLambdaFunctions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the counts of open tasks, child workflow executions and timers for a workflow execution.
--
-- /See:/ 'mkWorkflowExecutionOpenCounts' smart constructor.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts'
  { -- | The count of activity tasks whose status is @OPEN@ .
    openActivityTasks :: Core.Natural,
    -- | The count of decision tasks whose status is OPEN. A workflow execution can have at most one open decision task.
    openDecisionTasks :: Core.Natural,
    -- | The count of timers started by this workflow execution that have not fired yet.
    openTimers :: Core.Natural,
    -- | The count of child workflow executions whose status is @OPEN@ .
    openChildWorkflowExecutions :: Core.Natural,
    -- | The count of Lambda tasks whose status is @OPEN@ .
    openLambdaFunctions :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionOpenCounts' value with any optional fields omitted.
mkWorkflowExecutionOpenCounts ::
  -- | 'openActivityTasks'
  Core.Natural ->
  -- | 'openDecisionTasks'
  Core.Natural ->
  -- | 'openTimers'
  Core.Natural ->
  -- | 'openChildWorkflowExecutions'
  Core.Natural ->
  WorkflowExecutionOpenCounts
mkWorkflowExecutionOpenCounts
  openActivityTasks
  openDecisionTasks
  openTimers
  openChildWorkflowExecutions =
    WorkflowExecutionOpenCounts'
      { openActivityTasks,
        openDecisionTasks,
        openTimers,
        openChildWorkflowExecutions,
        openLambdaFunctions = Core.Nothing
      }

-- | The count of activity tasks whose status is @OPEN@ .
--
-- /Note:/ Consider using 'openActivityTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenActivityTasks :: Lens.Lens' WorkflowExecutionOpenCounts Core.Natural
weocOpenActivityTasks = Lens.field @"openActivityTasks"
{-# DEPRECATED weocOpenActivityTasks "Use generic-lens or generic-optics with 'openActivityTasks' instead." #-}

-- | The count of decision tasks whose status is OPEN. A workflow execution can have at most one open decision task.
--
-- /Note:/ Consider using 'openDecisionTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenDecisionTasks :: Lens.Lens' WorkflowExecutionOpenCounts Core.Natural
weocOpenDecisionTasks = Lens.field @"openDecisionTasks"
{-# DEPRECATED weocOpenDecisionTasks "Use generic-lens or generic-optics with 'openDecisionTasks' instead." #-}

-- | The count of timers started by this workflow execution that have not fired yet.
--
-- /Note:/ Consider using 'openTimers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenTimers :: Lens.Lens' WorkflowExecutionOpenCounts Core.Natural
weocOpenTimers = Lens.field @"openTimers"
{-# DEPRECATED weocOpenTimers "Use generic-lens or generic-optics with 'openTimers' instead." #-}

-- | The count of child workflow executions whose status is @OPEN@ .
--
-- /Note:/ Consider using 'openChildWorkflowExecutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenChildWorkflowExecutions :: Lens.Lens' WorkflowExecutionOpenCounts Core.Natural
weocOpenChildWorkflowExecutions = Lens.field @"openChildWorkflowExecutions"
{-# DEPRECATED weocOpenChildWorkflowExecutions "Use generic-lens or generic-optics with 'openChildWorkflowExecutions' instead." #-}

-- | The count of Lambda tasks whose status is @OPEN@ .
--
-- /Note:/ Consider using 'openLambdaFunctions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weocOpenLambdaFunctions :: Lens.Lens' WorkflowExecutionOpenCounts (Core.Maybe Core.Natural)
weocOpenLambdaFunctions = Lens.field @"openLambdaFunctions"
{-# DEPRECATED weocOpenLambdaFunctions "Use generic-lens or generic-optics with 'openLambdaFunctions' instead." #-}

instance Core.FromJSON WorkflowExecutionOpenCounts where
  parseJSON =
    Core.withObject "WorkflowExecutionOpenCounts" Core.$
      \x ->
        WorkflowExecutionOpenCounts'
          Core.<$> (x Core..: "openActivityTasks")
          Core.<*> (x Core..: "openDecisionTasks")
          Core.<*> (x Core..: "openTimers")
          Core.<*> (x Core..: "openChildWorkflowExecutions")
          Core.<*> (x Core..:? "openLambdaFunctions")
