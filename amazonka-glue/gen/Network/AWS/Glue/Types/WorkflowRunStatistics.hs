{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRunStatistics where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Workflow run statistics provides statistics about the workflow run.
--
-- /See:/ 'newWorkflowRunStatistics' smart constructor.
data WorkflowRunStatistics = WorkflowRunStatistics'
  { -- | Total number of Actions that timed out.
    timeoutActions :: Core.Maybe Core.Int,
    -- | Total number of Actions that have succeeded.
    succeededActions :: Core.Maybe Core.Int,
    -- | Total number Actions in running state.
    runningActions :: Core.Maybe Core.Int,
    -- | Total number of Actions in the workflow run.
    totalActions :: Core.Maybe Core.Int,
    -- | Total number of Actions that have stopped.
    stoppedActions :: Core.Maybe Core.Int,
    -- | Total number of Actions that have failed.
    failedActions :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'WorkflowRunStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutActions', 'workflowRunStatistics_timeoutActions' - Total number of Actions that timed out.
--
-- 'succeededActions', 'workflowRunStatistics_succeededActions' - Total number of Actions that have succeeded.
--
-- 'runningActions', 'workflowRunStatistics_runningActions' - Total number Actions in running state.
--
-- 'totalActions', 'workflowRunStatistics_totalActions' - Total number of Actions in the workflow run.
--
-- 'stoppedActions', 'workflowRunStatistics_stoppedActions' - Total number of Actions that have stopped.
--
-- 'failedActions', 'workflowRunStatistics_failedActions' - Total number of Actions that have failed.
newWorkflowRunStatistics ::
  WorkflowRunStatistics
newWorkflowRunStatistics =
  WorkflowRunStatistics'
    { timeoutActions =
        Core.Nothing,
      succeededActions = Core.Nothing,
      runningActions = Core.Nothing,
      totalActions = Core.Nothing,
      stoppedActions = Core.Nothing,
      failedActions = Core.Nothing
    }

-- | Total number of Actions that timed out.
workflowRunStatistics_timeoutActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
workflowRunStatistics_timeoutActions = Lens.lens (\WorkflowRunStatistics' {timeoutActions} -> timeoutActions) (\s@WorkflowRunStatistics' {} a -> s {timeoutActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions that have succeeded.
workflowRunStatistics_succeededActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
workflowRunStatistics_succeededActions = Lens.lens (\WorkflowRunStatistics' {succeededActions} -> succeededActions) (\s@WorkflowRunStatistics' {} a -> s {succeededActions = a} :: WorkflowRunStatistics)

-- | Total number Actions in running state.
workflowRunStatistics_runningActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
workflowRunStatistics_runningActions = Lens.lens (\WorkflowRunStatistics' {runningActions} -> runningActions) (\s@WorkflowRunStatistics' {} a -> s {runningActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions in the workflow run.
workflowRunStatistics_totalActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
workflowRunStatistics_totalActions = Lens.lens (\WorkflowRunStatistics' {totalActions} -> totalActions) (\s@WorkflowRunStatistics' {} a -> s {totalActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions that have stopped.
workflowRunStatistics_stoppedActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
workflowRunStatistics_stoppedActions = Lens.lens (\WorkflowRunStatistics' {stoppedActions} -> stoppedActions) (\s@WorkflowRunStatistics' {} a -> s {stoppedActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions that have failed.
workflowRunStatistics_failedActions :: Lens.Lens' WorkflowRunStatistics (Core.Maybe Core.Int)
workflowRunStatistics_failedActions = Lens.lens (\WorkflowRunStatistics' {failedActions} -> failedActions) (\s@WorkflowRunStatistics' {} a -> s {failedActions = a} :: WorkflowRunStatistics)

instance Core.FromJSON WorkflowRunStatistics where
  parseJSON =
    Core.withObject
      "WorkflowRunStatistics"
      ( \x ->
          WorkflowRunStatistics'
            Core.<$> (x Core..:? "TimeoutActions")
            Core.<*> (x Core..:? "SucceededActions")
            Core.<*> (x Core..:? "RunningActions")
            Core.<*> (x Core..:? "TotalActions")
            Core.<*> (x Core..:? "StoppedActions")
            Core.<*> (x Core..:? "FailedActions")
      )

instance Core.Hashable WorkflowRunStatistics

instance Core.NFData WorkflowRunStatistics
