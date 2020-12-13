{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRunStatistics
  ( WorkflowRunStatistics (..),

    -- * Smart constructor
    mkWorkflowRunStatistics,

    -- * Lenses
    wrsRunningActions,
    wrsStoppedActions,
    wrsTotalActions,
    wrsFailedActions,
    wrsTimeoutActions,
    wrsSucceededActions,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Workflow run statistics provides statistics about the workflow run.
--
-- /See:/ 'mkWorkflowRunStatistics' smart constructor.
data WorkflowRunStatistics = WorkflowRunStatistics'
  { -- | Total number Actions in running state.
    runningActions :: Lude.Maybe Lude.Int,
    -- | Total number of Actions that have stopped.
    stoppedActions :: Lude.Maybe Lude.Int,
    -- | Total number of Actions in the workflow run.
    totalActions :: Lude.Maybe Lude.Int,
    -- | Total number of Actions that have failed.
    failedActions :: Lude.Maybe Lude.Int,
    -- | Total number of Actions that timed out.
    timeoutActions :: Lude.Maybe Lude.Int,
    -- | Total number of Actions that have succeeded.
    succeededActions :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowRunStatistics' with the minimum fields required to make a request.
--
-- * 'runningActions' - Total number Actions in running state.
-- * 'stoppedActions' - Total number of Actions that have stopped.
-- * 'totalActions' - Total number of Actions in the workflow run.
-- * 'failedActions' - Total number of Actions that have failed.
-- * 'timeoutActions' - Total number of Actions that timed out.
-- * 'succeededActions' - Total number of Actions that have succeeded.
mkWorkflowRunStatistics ::
  WorkflowRunStatistics
mkWorkflowRunStatistics =
  WorkflowRunStatistics'
    { runningActions = Lude.Nothing,
      stoppedActions = Lude.Nothing,
      totalActions = Lude.Nothing,
      failedActions = Lude.Nothing,
      timeoutActions = Lude.Nothing,
      succeededActions = Lude.Nothing
    }

-- | Total number Actions in running state.
--
-- /Note:/ Consider using 'runningActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsRunningActions :: Lens.Lens' WorkflowRunStatistics (Lude.Maybe Lude.Int)
wrsRunningActions = Lens.lens (runningActions :: WorkflowRunStatistics -> Lude.Maybe Lude.Int) (\s a -> s {runningActions = a} :: WorkflowRunStatistics)
{-# DEPRECATED wrsRunningActions "Use generic-lens or generic-optics with 'runningActions' instead." #-}

-- | Total number of Actions that have stopped.
--
-- /Note:/ Consider using 'stoppedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsStoppedActions :: Lens.Lens' WorkflowRunStatistics (Lude.Maybe Lude.Int)
wrsStoppedActions = Lens.lens (stoppedActions :: WorkflowRunStatistics -> Lude.Maybe Lude.Int) (\s a -> s {stoppedActions = a} :: WorkflowRunStatistics)
{-# DEPRECATED wrsStoppedActions "Use generic-lens or generic-optics with 'stoppedActions' instead." #-}

-- | Total number of Actions in the workflow run.
--
-- /Note:/ Consider using 'totalActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsTotalActions :: Lens.Lens' WorkflowRunStatistics (Lude.Maybe Lude.Int)
wrsTotalActions = Lens.lens (totalActions :: WorkflowRunStatistics -> Lude.Maybe Lude.Int) (\s a -> s {totalActions = a} :: WorkflowRunStatistics)
{-# DEPRECATED wrsTotalActions "Use generic-lens or generic-optics with 'totalActions' instead." #-}

-- | Total number of Actions that have failed.
--
-- /Note:/ Consider using 'failedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsFailedActions :: Lens.Lens' WorkflowRunStatistics (Lude.Maybe Lude.Int)
wrsFailedActions = Lens.lens (failedActions :: WorkflowRunStatistics -> Lude.Maybe Lude.Int) (\s a -> s {failedActions = a} :: WorkflowRunStatistics)
{-# DEPRECATED wrsFailedActions "Use generic-lens or generic-optics with 'failedActions' instead." #-}

-- | Total number of Actions that timed out.
--
-- /Note:/ Consider using 'timeoutActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsTimeoutActions :: Lens.Lens' WorkflowRunStatistics (Lude.Maybe Lude.Int)
wrsTimeoutActions = Lens.lens (timeoutActions :: WorkflowRunStatistics -> Lude.Maybe Lude.Int) (\s a -> s {timeoutActions = a} :: WorkflowRunStatistics)
{-# DEPRECATED wrsTimeoutActions "Use generic-lens or generic-optics with 'timeoutActions' instead." #-}

-- | Total number of Actions that have succeeded.
--
-- /Note:/ Consider using 'succeededActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsSucceededActions :: Lens.Lens' WorkflowRunStatistics (Lude.Maybe Lude.Int)
wrsSucceededActions = Lens.lens (succeededActions :: WorkflowRunStatistics -> Lude.Maybe Lude.Int) (\s a -> s {succeededActions = a} :: WorkflowRunStatistics)
{-# DEPRECATED wrsSucceededActions "Use generic-lens or generic-optics with 'succeededActions' instead." #-}

instance Lude.FromJSON WorkflowRunStatistics where
  parseJSON =
    Lude.withObject
      "WorkflowRunStatistics"
      ( \x ->
          WorkflowRunStatistics'
            Lude.<$> (x Lude..:? "RunningActions")
            Lude.<*> (x Lude..:? "StoppedActions")
            Lude.<*> (x Lude..:? "TotalActions")
            Lude.<*> (x Lude..:? "FailedActions")
            Lude.<*> (x Lude..:? "TimeoutActions")
            Lude.<*> (x Lude..:? "SucceededActions")
      )
