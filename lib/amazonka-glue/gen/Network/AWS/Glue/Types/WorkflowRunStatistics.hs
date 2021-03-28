{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.WorkflowRunStatistics
  ( WorkflowRunStatistics (..)
  -- * Smart constructor
  , mkWorkflowRunStatistics
  -- * Lenses
  , wrsFailedActions
  , wrsRunningActions
  , wrsStoppedActions
  , wrsSucceededActions
  , wrsTimeoutActions
  , wrsTotalActions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Workflow run statistics provides statistics about the workflow run.
--
-- /See:/ 'mkWorkflowRunStatistics' smart constructor.
data WorkflowRunStatistics = WorkflowRunStatistics'
  { failedActions :: Core.Maybe Core.Int
    -- ^ Total number of Actions that have failed.
  , runningActions :: Core.Maybe Core.Int
    -- ^ Total number Actions in running state.
  , stoppedActions :: Core.Maybe Core.Int
    -- ^ Total number of Actions that have stopped.
  , succeededActions :: Core.Maybe Core.Int
    -- ^ Total number of Actions that have succeeded.
  , timeoutActions :: Core.Maybe Core.Int
    -- ^ Total number of Actions that timed out.
  , totalActions :: Core.Maybe Core.Int
    -- ^ Total number of Actions in the workflow run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowRunStatistics' value with any optional fields omitted.
mkWorkflowRunStatistics
    :: WorkflowRunStatistics
mkWorkflowRunStatistics
  = WorkflowRunStatistics'{failedActions = Core.Nothing,
                           runningActions = Core.Nothing, stoppedActions = Core.Nothing,
                           succeededActions = Core.Nothing, timeoutActions = Core.Nothing,
                           totalActions = Core.Nothing}

-- | Total number of Actions that have failed.
--
-- /Note:/ Consider using 'failedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsFailedActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
wrsFailedActions = Lens.field @"failedActions"
{-# INLINEABLE wrsFailedActions #-}
{-# DEPRECATED failedActions "Use generic-lens or generic-optics with 'failedActions' instead"  #-}

-- | Total number Actions in running state.
--
-- /Note:/ Consider using 'runningActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsRunningActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
wrsRunningActions = Lens.field @"runningActions"
{-# INLINEABLE wrsRunningActions #-}
{-# DEPRECATED runningActions "Use generic-lens or generic-optics with 'runningActions' instead"  #-}

-- | Total number of Actions that have stopped.
--
-- /Note:/ Consider using 'stoppedActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsStoppedActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
wrsStoppedActions = Lens.field @"stoppedActions"
{-# INLINEABLE wrsStoppedActions #-}
{-# DEPRECATED stoppedActions "Use generic-lens or generic-optics with 'stoppedActions' instead"  #-}

-- | Total number of Actions that have succeeded.
--
-- /Note:/ Consider using 'succeededActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsSucceededActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
wrsSucceededActions = Lens.field @"succeededActions"
{-# INLINEABLE wrsSucceededActions #-}
{-# DEPRECATED succeededActions "Use generic-lens or generic-optics with 'succeededActions' instead"  #-}

-- | Total number of Actions that timed out.
--
-- /Note:/ Consider using 'timeoutActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsTimeoutActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
wrsTimeoutActions = Lens.field @"timeoutActions"
{-# INLINEABLE wrsTimeoutActions #-}
{-# DEPRECATED timeoutActions "Use generic-lens or generic-optics with 'timeoutActions' instead"  #-}

-- | Total number of Actions in the workflow run.
--
-- /Note:/ Consider using 'totalActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrsTotalActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
wrsTotalActions = Lens.field @"totalActions"
{-# INLINEABLE wrsTotalActions #-}
{-# DEPRECATED totalActions "Use generic-lens or generic-optics with 'totalActions' instead"  #-}

instance Core.FromJSON WorkflowRunStatistics where
        parseJSON
          = Core.withObject "WorkflowRunStatistics" Core.$
              \ x ->
                WorkflowRunStatistics' Core.<$>
                  (x Core..:? "FailedActions") Core.<*> x Core..:? "RunningActions"
                    Core.<*> x Core..:? "StoppedActions"
                    Core.<*> x Core..:? "SucceededActions"
                    Core.<*> x Core..:? "TimeoutActions"
                    Core.<*> x Core..:? "TotalActions"
