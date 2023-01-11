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
-- Module      : Amazonka.SWF.Types.WorkflowExecutionOpenCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecutionOpenCounts where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the counts of open tasks, child workflow executions and timers
-- for a workflow execution.
--
-- /See:/ 'newWorkflowExecutionOpenCounts' smart constructor.
data WorkflowExecutionOpenCounts = WorkflowExecutionOpenCounts'
  { -- | The count of Lambda tasks whose status is @OPEN@.
    openLambdaFunctions :: Prelude.Maybe Prelude.Natural,
    -- | The count of activity tasks whose status is @OPEN@.
    openActivityTasks :: Prelude.Natural,
    -- | The count of decision tasks whose status is OPEN. A workflow execution
    -- can have at most one open decision task.
    openDecisionTasks :: Prelude.Natural,
    -- | The count of timers started by this workflow execution that have not
    -- fired yet.
    openTimers :: Prelude.Natural,
    -- | The count of child workflow executions whose status is @OPEN@.
    openChildWorkflowExecutions :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecutionOpenCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openLambdaFunctions', 'workflowExecutionOpenCounts_openLambdaFunctions' - The count of Lambda tasks whose status is @OPEN@.
--
-- 'openActivityTasks', 'workflowExecutionOpenCounts_openActivityTasks' - The count of activity tasks whose status is @OPEN@.
--
-- 'openDecisionTasks', 'workflowExecutionOpenCounts_openDecisionTasks' - The count of decision tasks whose status is OPEN. A workflow execution
-- can have at most one open decision task.
--
-- 'openTimers', 'workflowExecutionOpenCounts_openTimers' - The count of timers started by this workflow execution that have not
-- fired yet.
--
-- 'openChildWorkflowExecutions', 'workflowExecutionOpenCounts_openChildWorkflowExecutions' - The count of child workflow executions whose status is @OPEN@.
newWorkflowExecutionOpenCounts ::
  -- | 'openActivityTasks'
  Prelude.Natural ->
  -- | 'openDecisionTasks'
  Prelude.Natural ->
  -- | 'openTimers'
  Prelude.Natural ->
  -- | 'openChildWorkflowExecutions'
  Prelude.Natural ->
  WorkflowExecutionOpenCounts
newWorkflowExecutionOpenCounts
  pOpenActivityTasks_
  pOpenDecisionTasks_
  pOpenTimers_
  pOpenChildWorkflowExecutions_ =
    WorkflowExecutionOpenCounts'
      { openLambdaFunctions =
          Prelude.Nothing,
        openActivityTasks = pOpenActivityTasks_,
        openDecisionTasks = pOpenDecisionTasks_,
        openTimers = pOpenTimers_,
        openChildWorkflowExecutions =
          pOpenChildWorkflowExecutions_
      }

-- | The count of Lambda tasks whose status is @OPEN@.
workflowExecutionOpenCounts_openLambdaFunctions :: Lens.Lens' WorkflowExecutionOpenCounts (Prelude.Maybe Prelude.Natural)
workflowExecutionOpenCounts_openLambdaFunctions = Lens.lens (\WorkflowExecutionOpenCounts' {openLambdaFunctions} -> openLambdaFunctions) (\s@WorkflowExecutionOpenCounts' {} a -> s {openLambdaFunctions = a} :: WorkflowExecutionOpenCounts)

-- | The count of activity tasks whose status is @OPEN@.
workflowExecutionOpenCounts_openActivityTasks :: Lens.Lens' WorkflowExecutionOpenCounts Prelude.Natural
workflowExecutionOpenCounts_openActivityTasks = Lens.lens (\WorkflowExecutionOpenCounts' {openActivityTasks} -> openActivityTasks) (\s@WorkflowExecutionOpenCounts' {} a -> s {openActivityTasks = a} :: WorkflowExecutionOpenCounts)

-- | The count of decision tasks whose status is OPEN. A workflow execution
-- can have at most one open decision task.
workflowExecutionOpenCounts_openDecisionTasks :: Lens.Lens' WorkflowExecutionOpenCounts Prelude.Natural
workflowExecutionOpenCounts_openDecisionTasks = Lens.lens (\WorkflowExecutionOpenCounts' {openDecisionTasks} -> openDecisionTasks) (\s@WorkflowExecutionOpenCounts' {} a -> s {openDecisionTasks = a} :: WorkflowExecutionOpenCounts)

-- | The count of timers started by this workflow execution that have not
-- fired yet.
workflowExecutionOpenCounts_openTimers :: Lens.Lens' WorkflowExecutionOpenCounts Prelude.Natural
workflowExecutionOpenCounts_openTimers = Lens.lens (\WorkflowExecutionOpenCounts' {openTimers} -> openTimers) (\s@WorkflowExecutionOpenCounts' {} a -> s {openTimers = a} :: WorkflowExecutionOpenCounts)

-- | The count of child workflow executions whose status is @OPEN@.
workflowExecutionOpenCounts_openChildWorkflowExecutions :: Lens.Lens' WorkflowExecutionOpenCounts Prelude.Natural
workflowExecutionOpenCounts_openChildWorkflowExecutions = Lens.lens (\WorkflowExecutionOpenCounts' {openChildWorkflowExecutions} -> openChildWorkflowExecutions) (\s@WorkflowExecutionOpenCounts' {} a -> s {openChildWorkflowExecutions = a} :: WorkflowExecutionOpenCounts)

instance Data.FromJSON WorkflowExecutionOpenCounts where
  parseJSON =
    Data.withObject
      "WorkflowExecutionOpenCounts"
      ( \x ->
          WorkflowExecutionOpenCounts'
            Prelude.<$> (x Data..:? "openLambdaFunctions")
            Prelude.<*> (x Data..: "openActivityTasks")
            Prelude.<*> (x Data..: "openDecisionTasks")
            Prelude.<*> (x Data..: "openTimers")
            Prelude.<*> (x Data..: "openChildWorkflowExecutions")
      )

instance Prelude.Hashable WorkflowExecutionOpenCounts where
  hashWithSalt _salt WorkflowExecutionOpenCounts' {..} =
    _salt `Prelude.hashWithSalt` openLambdaFunctions
      `Prelude.hashWithSalt` openActivityTasks
      `Prelude.hashWithSalt` openDecisionTasks
      `Prelude.hashWithSalt` openTimers
      `Prelude.hashWithSalt` openChildWorkflowExecutions

instance Prelude.NFData WorkflowExecutionOpenCounts where
  rnf WorkflowExecutionOpenCounts' {..} =
    Prelude.rnf openLambdaFunctions
      `Prelude.seq` Prelude.rnf openActivityTasks
      `Prelude.seq` Prelude.rnf openDecisionTasks
      `Prelude.seq` Prelude.rnf openTimers
      `Prelude.seq` Prelude.rnf openChildWorkflowExecutions
