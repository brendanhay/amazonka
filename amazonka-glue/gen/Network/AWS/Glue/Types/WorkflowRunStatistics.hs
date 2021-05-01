{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Workflow run statistics provides statistics about the workflow run.
--
-- /See:/ 'newWorkflowRunStatistics' smart constructor.
data WorkflowRunStatistics = WorkflowRunStatistics'
  { -- | Total number of Actions that timed out.
    timeoutActions :: Prelude.Maybe Prelude.Int,
    -- | Total number of Actions that have succeeded.
    succeededActions :: Prelude.Maybe Prelude.Int,
    -- | Total number Actions in running state.
    runningActions :: Prelude.Maybe Prelude.Int,
    -- | Total number of Actions in the workflow run.
    totalActions :: Prelude.Maybe Prelude.Int,
    -- | Total number of Actions that have stopped.
    stoppedActions :: Prelude.Maybe Prelude.Int,
    -- | Total number of Actions that have failed.
    failedActions :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      succeededActions = Prelude.Nothing,
      runningActions = Prelude.Nothing,
      totalActions = Prelude.Nothing,
      stoppedActions = Prelude.Nothing,
      failedActions = Prelude.Nothing
    }

-- | Total number of Actions that timed out.
workflowRunStatistics_timeoutActions :: Lens.Lens' WorkflowRunStatistics (Prelude.Maybe Prelude.Int)
workflowRunStatistics_timeoutActions = Lens.lens (\WorkflowRunStatistics' {timeoutActions} -> timeoutActions) (\s@WorkflowRunStatistics' {} a -> s {timeoutActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions that have succeeded.
workflowRunStatistics_succeededActions :: Lens.Lens' WorkflowRunStatistics (Prelude.Maybe Prelude.Int)
workflowRunStatistics_succeededActions = Lens.lens (\WorkflowRunStatistics' {succeededActions} -> succeededActions) (\s@WorkflowRunStatistics' {} a -> s {succeededActions = a} :: WorkflowRunStatistics)

-- | Total number Actions in running state.
workflowRunStatistics_runningActions :: Lens.Lens' WorkflowRunStatistics (Prelude.Maybe Prelude.Int)
workflowRunStatistics_runningActions = Lens.lens (\WorkflowRunStatistics' {runningActions} -> runningActions) (\s@WorkflowRunStatistics' {} a -> s {runningActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions in the workflow run.
workflowRunStatistics_totalActions :: Lens.Lens' WorkflowRunStatistics (Prelude.Maybe Prelude.Int)
workflowRunStatistics_totalActions = Lens.lens (\WorkflowRunStatistics' {totalActions} -> totalActions) (\s@WorkflowRunStatistics' {} a -> s {totalActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions that have stopped.
workflowRunStatistics_stoppedActions :: Lens.Lens' WorkflowRunStatistics (Prelude.Maybe Prelude.Int)
workflowRunStatistics_stoppedActions = Lens.lens (\WorkflowRunStatistics' {stoppedActions} -> stoppedActions) (\s@WorkflowRunStatistics' {} a -> s {stoppedActions = a} :: WorkflowRunStatistics)

-- | Total number of Actions that have failed.
workflowRunStatistics_failedActions :: Lens.Lens' WorkflowRunStatistics (Prelude.Maybe Prelude.Int)
workflowRunStatistics_failedActions = Lens.lens (\WorkflowRunStatistics' {failedActions} -> failedActions) (\s@WorkflowRunStatistics' {} a -> s {failedActions = a} :: WorkflowRunStatistics)

instance Prelude.FromJSON WorkflowRunStatistics where
  parseJSON =
    Prelude.withObject
      "WorkflowRunStatistics"
      ( \x ->
          WorkflowRunStatistics'
            Prelude.<$> (x Prelude..:? "TimeoutActions")
            Prelude.<*> (x Prelude..:? "SucceededActions")
            Prelude.<*> (x Prelude..:? "RunningActions")
            Prelude.<*> (x Prelude..:? "TotalActions")
            Prelude.<*> (x Prelude..:? "StoppedActions")
            Prelude.<*> (x Prelude..:? "FailedActions")
      )

instance Prelude.Hashable WorkflowRunStatistics

instance Prelude.NFData WorkflowRunStatistics
