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
import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.WorkflowRunStatus
import qualified Network.AWS.Lens as Lens

-- | A workflow run is an execution of a workflow providing all the runtime
-- information.
--
-- /See:/ 'newWorkflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { -- | The ID of this workflow run.
    workflowRunId :: Core.Maybe Core.Text,
    -- | The status of the workflow run.
    status :: Core.Maybe WorkflowRunStatus,
    -- | The workflow run properties which were set during the run.
    workflowRunProperties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The statistics of the run.
    statistics :: Core.Maybe WorkflowRunStatistics,
    -- | Name of the workflow that was executed.
    name :: Core.Maybe Core.Text,
    -- | The date and time when the workflow run completed.
    completedOn :: Core.Maybe Core.POSIX,
    -- | The graph representing all the AWS Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Core.Maybe WorkflowGraph,
    -- | This error message describes any error that may have occurred in
    -- starting the workflow run. Currently the only error message is
    -- \"Concurrent runs exceeded for workflow: @foo@.\"
    errorMessage :: Core.Maybe Core.Text,
    -- | The date and time when the workflow run was started.
    startedOn :: Core.Maybe Core.POSIX,
    -- | The ID of the previous workflow run.
    previousRunId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'workflowRunProperties', 'workflowRun_workflowRunProperties' - The workflow run properties which were set during the run.
--
-- 'statistics', 'workflowRun_statistics' - The statistics of the run.
--
-- 'name', 'workflowRun_name' - Name of the workflow that was executed.
--
-- 'completedOn', 'workflowRun_completedOn' - The date and time when the workflow run completed.
--
-- 'graph', 'workflowRun_graph' - The graph representing all the AWS Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
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
    { workflowRunId = Core.Nothing,
      status = Core.Nothing,
      workflowRunProperties = Core.Nothing,
      statistics = Core.Nothing,
      name = Core.Nothing,
      completedOn = Core.Nothing,
      graph = Core.Nothing,
      errorMessage = Core.Nothing,
      startedOn = Core.Nothing,
      previousRunId = Core.Nothing
    }

-- | The ID of this workflow run.
workflowRun_workflowRunId :: Lens.Lens' WorkflowRun (Core.Maybe Core.Text)
workflowRun_workflowRunId = Lens.lens (\WorkflowRun' {workflowRunId} -> workflowRunId) (\s@WorkflowRun' {} a -> s {workflowRunId = a} :: WorkflowRun)

-- | The status of the workflow run.
workflowRun_status :: Lens.Lens' WorkflowRun (Core.Maybe WorkflowRunStatus)
workflowRun_status = Lens.lens (\WorkflowRun' {status} -> status) (\s@WorkflowRun' {} a -> s {status = a} :: WorkflowRun)

-- | The workflow run properties which were set during the run.
workflowRun_workflowRunProperties :: Lens.Lens' WorkflowRun (Core.Maybe (Core.HashMap Core.Text Core.Text))
workflowRun_workflowRunProperties = Lens.lens (\WorkflowRun' {workflowRunProperties} -> workflowRunProperties) (\s@WorkflowRun' {} a -> s {workflowRunProperties = a} :: WorkflowRun) Core.. Lens.mapping Lens._Coerce

-- | The statistics of the run.
workflowRun_statistics :: Lens.Lens' WorkflowRun (Core.Maybe WorkflowRunStatistics)
workflowRun_statistics = Lens.lens (\WorkflowRun' {statistics} -> statistics) (\s@WorkflowRun' {} a -> s {statistics = a} :: WorkflowRun)

-- | Name of the workflow that was executed.
workflowRun_name :: Lens.Lens' WorkflowRun (Core.Maybe Core.Text)
workflowRun_name = Lens.lens (\WorkflowRun' {name} -> name) (\s@WorkflowRun' {} a -> s {name = a} :: WorkflowRun)

-- | The date and time when the workflow run completed.
workflowRun_completedOn :: Lens.Lens' WorkflowRun (Core.Maybe Core.UTCTime)
workflowRun_completedOn = Lens.lens (\WorkflowRun' {completedOn} -> completedOn) (\s@WorkflowRun' {} a -> s {completedOn = a} :: WorkflowRun) Core.. Lens.mapping Core._Time

-- | The graph representing all the AWS Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflowRun_graph :: Lens.Lens' WorkflowRun (Core.Maybe WorkflowGraph)
workflowRun_graph = Lens.lens (\WorkflowRun' {graph} -> graph) (\s@WorkflowRun' {} a -> s {graph = a} :: WorkflowRun)

-- | This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
workflowRun_errorMessage :: Lens.Lens' WorkflowRun (Core.Maybe Core.Text)
workflowRun_errorMessage = Lens.lens (\WorkflowRun' {errorMessage} -> errorMessage) (\s@WorkflowRun' {} a -> s {errorMessage = a} :: WorkflowRun)

-- | The date and time when the workflow run was started.
workflowRun_startedOn :: Lens.Lens' WorkflowRun (Core.Maybe Core.UTCTime)
workflowRun_startedOn = Lens.lens (\WorkflowRun' {startedOn} -> startedOn) (\s@WorkflowRun' {} a -> s {startedOn = a} :: WorkflowRun) Core.. Lens.mapping Core._Time

-- | The ID of the previous workflow run.
workflowRun_previousRunId :: Lens.Lens' WorkflowRun (Core.Maybe Core.Text)
workflowRun_previousRunId = Lens.lens (\WorkflowRun' {previousRunId} -> previousRunId) (\s@WorkflowRun' {} a -> s {previousRunId = a} :: WorkflowRun)

instance Core.FromJSON WorkflowRun where
  parseJSON =
    Core.withObject
      "WorkflowRun"
      ( \x ->
          WorkflowRun'
            Core.<$> (x Core..:? "WorkflowRunId")
            Core.<*> (x Core..:? "Status")
            Core.<*> ( x Core..:? "WorkflowRunProperties"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Statistics")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "CompletedOn")
            Core.<*> (x Core..:? "Graph")
            Core.<*> (x Core..:? "ErrorMessage")
            Core.<*> (x Core..:? "StartedOn")
            Core.<*> (x Core..:? "PreviousRunId")
      )

instance Core.Hashable WorkflowRun

instance Core.NFData WorkflowRun
