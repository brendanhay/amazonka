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
-- Module      : Network.AWS.Glue.Types.WorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRun where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.StartingEventBatchCondition
import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.WorkflowRunStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A workflow run is an execution of a workflow providing all the runtime
-- information.
--
-- /See:/ 'newWorkflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { -- | The ID of this workflow run.
    workflowRunId :: Prelude.Maybe Prelude.Text,
    -- | The status of the workflow run.
    status :: Prelude.Maybe WorkflowRunStatus,
    -- | The batch condition that started the workflow run.
    startingEventBatchCondition :: Prelude.Maybe StartingEventBatchCondition,
    -- | The workflow run properties which were set during the run.
    workflowRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The statistics of the run.
    statistics :: Prelude.Maybe WorkflowRunStatistics,
    -- | Name of the workflow that was run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The graph representing all the Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Prelude.Maybe WorkflowGraph,
    -- | The date and time when the workflow run completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | This error message describes any error that may have occurred in
    -- starting the workflow run. Currently the only error message is
    -- \"Concurrent runs exceeded for workflow: @foo@.\"
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the workflow run was started.
    startedOn :: Prelude.Maybe Core.POSIX,
    -- | The ID of the previous workflow run.
    previousRunId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowRunId', 'workflowRun_workflowRunId' - The ID of this workflow run.
--
-- 'status', 'workflowRun_status' - The status of the workflow run.
--
-- 'startingEventBatchCondition', 'workflowRun_startingEventBatchCondition' - The batch condition that started the workflow run.
--
-- 'workflowRunProperties', 'workflowRun_workflowRunProperties' - The workflow run properties which were set during the run.
--
-- 'statistics', 'workflowRun_statistics' - The statistics of the run.
--
-- 'name', 'workflowRun_name' - Name of the workflow that was run.
--
-- 'graph', 'workflowRun_graph' - The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
--
-- 'completedOn', 'workflowRun_completedOn' - The date and time when the workflow run completed.
--
-- 'errorMessage', 'workflowRun_errorMessage' - This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
--
-- 'startedOn', 'workflowRun_startedOn' - The date and time when the workflow run was started.
--
-- 'previousRunId', 'workflowRun_previousRunId' - The ID of the previous workflow run.
newWorkflowRun ::
  WorkflowRun
newWorkflowRun =
  WorkflowRun'
    { workflowRunId = Prelude.Nothing,
      status = Prelude.Nothing,
      startingEventBatchCondition = Prelude.Nothing,
      workflowRunProperties = Prelude.Nothing,
      statistics = Prelude.Nothing,
      name = Prelude.Nothing,
      graph = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      previousRunId = Prelude.Nothing
    }

-- | The ID of this workflow run.
workflowRun_workflowRunId :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_workflowRunId = Lens.lens (\WorkflowRun' {workflowRunId} -> workflowRunId) (\s@WorkflowRun' {} a -> s {workflowRunId = a} :: WorkflowRun)

-- | The status of the workflow run.
workflowRun_status :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowRunStatus)
workflowRun_status = Lens.lens (\WorkflowRun' {status} -> status) (\s@WorkflowRun' {} a -> s {status = a} :: WorkflowRun)

-- | The batch condition that started the workflow run.
workflowRun_startingEventBatchCondition :: Lens.Lens' WorkflowRun (Prelude.Maybe StartingEventBatchCondition)
workflowRun_startingEventBatchCondition = Lens.lens (\WorkflowRun' {startingEventBatchCondition} -> startingEventBatchCondition) (\s@WorkflowRun' {} a -> s {startingEventBatchCondition = a} :: WorkflowRun)

-- | The workflow run properties which were set during the run.
workflowRun_workflowRunProperties :: Lens.Lens' WorkflowRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workflowRun_workflowRunProperties = Lens.lens (\WorkflowRun' {workflowRunProperties} -> workflowRunProperties) (\s@WorkflowRun' {} a -> s {workflowRunProperties = a} :: WorkflowRun) Prelude.. Lens.mapping Lens._Coerce

-- | The statistics of the run.
workflowRun_statistics :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowRunStatistics)
workflowRun_statistics = Lens.lens (\WorkflowRun' {statistics} -> statistics) (\s@WorkflowRun' {} a -> s {statistics = a} :: WorkflowRun)

-- | Name of the workflow that was run.
workflowRun_name :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_name = Lens.lens (\WorkflowRun' {name} -> name) (\s@WorkflowRun' {} a -> s {name = a} :: WorkflowRun)

-- | The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflowRun_graph :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowGraph)
workflowRun_graph = Lens.lens (\WorkflowRun' {graph} -> graph) (\s@WorkflowRun' {} a -> s {graph = a} :: WorkflowRun)

-- | The date and time when the workflow run completed.
workflowRun_completedOn :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.UTCTime)
workflowRun_completedOn = Lens.lens (\WorkflowRun' {completedOn} -> completedOn) (\s@WorkflowRun' {} a -> s {completedOn = a} :: WorkflowRun) Prelude.. Lens.mapping Core._Time

-- | This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
workflowRun_errorMessage :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_errorMessage = Lens.lens (\WorkflowRun' {errorMessage} -> errorMessage) (\s@WorkflowRun' {} a -> s {errorMessage = a} :: WorkflowRun)

-- | The date and time when the workflow run was started.
workflowRun_startedOn :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.UTCTime)
workflowRun_startedOn = Lens.lens (\WorkflowRun' {startedOn} -> startedOn) (\s@WorkflowRun' {} a -> s {startedOn = a} :: WorkflowRun) Prelude.. Lens.mapping Core._Time

-- | The ID of the previous workflow run.
workflowRun_previousRunId :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_previousRunId = Lens.lens (\WorkflowRun' {previousRunId} -> previousRunId) (\s@WorkflowRun' {} a -> s {previousRunId = a} :: WorkflowRun)

instance Core.FromJSON WorkflowRun where
  parseJSON =
    Core.withObject
      "WorkflowRun"
      ( \x ->
          WorkflowRun'
            Prelude.<$> (x Core..:? "WorkflowRunId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "StartingEventBatchCondition")
            Prelude.<*> ( x Core..:? "WorkflowRunProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Statistics")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Graph")
            Prelude.<*> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "PreviousRunId")
      )

instance Prelude.Hashable WorkflowRun

instance Prelude.NFData WorkflowRun
