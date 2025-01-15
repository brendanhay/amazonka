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
-- Module      : Amazonka.Glue.Types.WorkflowRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.WorkflowRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.StartingEventBatchCondition
import Amazonka.Glue.Types.WorkflowGraph
import Amazonka.Glue.Types.WorkflowRunStatistics
import Amazonka.Glue.Types.WorkflowRunStatus
import qualified Amazonka.Prelude as Prelude

-- | A workflow run is an execution of a workflow providing all the runtime
-- information.
--
-- /See:/ 'newWorkflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { -- | The date and time when the workflow run completed.
    completedOn :: Prelude.Maybe Data.POSIX,
    -- | This error message describes any error that may have occurred in
    -- starting the workflow run. Currently the only error message is
    -- \"Concurrent runs exceeded for workflow: @foo@.\"
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The graph representing all the Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Prelude.Maybe WorkflowGraph,
    -- | Name of the workflow that was run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the previous workflow run.
    previousRunId :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the workflow run was started.
    startedOn :: Prelude.Maybe Data.POSIX,
    -- | The batch condition that started the workflow run.
    startingEventBatchCondition :: Prelude.Maybe StartingEventBatchCondition,
    -- | The statistics of the run.
    statistics :: Prelude.Maybe WorkflowRunStatistics,
    -- | The status of the workflow run.
    status :: Prelude.Maybe WorkflowRunStatus,
    -- | The ID of this workflow run.
    workflowRunId :: Prelude.Maybe Prelude.Text,
    -- | The workflow run properties which were set during the run.
    workflowRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
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
-- 'completedOn', 'workflowRun_completedOn' - The date and time when the workflow run completed.
--
-- 'errorMessage', 'workflowRun_errorMessage' - This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
--
-- 'graph', 'workflowRun_graph' - The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
--
-- 'name', 'workflowRun_name' - Name of the workflow that was run.
--
-- 'previousRunId', 'workflowRun_previousRunId' - The ID of the previous workflow run.
--
-- 'startedOn', 'workflowRun_startedOn' - The date and time when the workflow run was started.
--
-- 'startingEventBatchCondition', 'workflowRun_startingEventBatchCondition' - The batch condition that started the workflow run.
--
-- 'statistics', 'workflowRun_statistics' - The statistics of the run.
--
-- 'status', 'workflowRun_status' - The status of the workflow run.
--
-- 'workflowRunId', 'workflowRun_workflowRunId' - The ID of this workflow run.
--
-- 'workflowRunProperties', 'workflowRun_workflowRunProperties' - The workflow run properties which were set during the run.
newWorkflowRun ::
  WorkflowRun
newWorkflowRun =
  WorkflowRun'
    { completedOn = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      graph = Prelude.Nothing,
      name = Prelude.Nothing,
      previousRunId = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      startingEventBatchCondition = Prelude.Nothing,
      statistics = Prelude.Nothing,
      status = Prelude.Nothing,
      workflowRunId = Prelude.Nothing,
      workflowRunProperties = Prelude.Nothing
    }

-- | The date and time when the workflow run completed.
workflowRun_completedOn :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.UTCTime)
workflowRun_completedOn = Lens.lens (\WorkflowRun' {completedOn} -> completedOn) (\s@WorkflowRun' {} a -> s {completedOn = a} :: WorkflowRun) Prelude.. Lens.mapping Data._Time

-- | This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
workflowRun_errorMessage :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_errorMessage = Lens.lens (\WorkflowRun' {errorMessage} -> errorMessage) (\s@WorkflowRun' {} a -> s {errorMessage = a} :: WorkflowRun)

-- | The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflowRun_graph :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowGraph)
workflowRun_graph = Lens.lens (\WorkflowRun' {graph} -> graph) (\s@WorkflowRun' {} a -> s {graph = a} :: WorkflowRun)

-- | Name of the workflow that was run.
workflowRun_name :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_name = Lens.lens (\WorkflowRun' {name} -> name) (\s@WorkflowRun' {} a -> s {name = a} :: WorkflowRun)

-- | The ID of the previous workflow run.
workflowRun_previousRunId :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_previousRunId = Lens.lens (\WorkflowRun' {previousRunId} -> previousRunId) (\s@WorkflowRun' {} a -> s {previousRunId = a} :: WorkflowRun)

-- | The date and time when the workflow run was started.
workflowRun_startedOn :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.UTCTime)
workflowRun_startedOn = Lens.lens (\WorkflowRun' {startedOn} -> startedOn) (\s@WorkflowRun' {} a -> s {startedOn = a} :: WorkflowRun) Prelude.. Lens.mapping Data._Time

-- | The batch condition that started the workflow run.
workflowRun_startingEventBatchCondition :: Lens.Lens' WorkflowRun (Prelude.Maybe StartingEventBatchCondition)
workflowRun_startingEventBatchCondition = Lens.lens (\WorkflowRun' {startingEventBatchCondition} -> startingEventBatchCondition) (\s@WorkflowRun' {} a -> s {startingEventBatchCondition = a} :: WorkflowRun)

-- | The statistics of the run.
workflowRun_statistics :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowRunStatistics)
workflowRun_statistics = Lens.lens (\WorkflowRun' {statistics} -> statistics) (\s@WorkflowRun' {} a -> s {statistics = a} :: WorkflowRun)

-- | The status of the workflow run.
workflowRun_status :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowRunStatus)
workflowRun_status = Lens.lens (\WorkflowRun' {status} -> status) (\s@WorkflowRun' {} a -> s {status = a} :: WorkflowRun)

-- | The ID of this workflow run.
workflowRun_workflowRunId :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_workflowRunId = Lens.lens (\WorkflowRun' {workflowRunId} -> workflowRunId) (\s@WorkflowRun' {} a -> s {workflowRunId = a} :: WorkflowRun)

-- | The workflow run properties which were set during the run.
workflowRun_workflowRunProperties :: Lens.Lens' WorkflowRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workflowRun_workflowRunProperties = Lens.lens (\WorkflowRun' {workflowRunProperties} -> workflowRunProperties) (\s@WorkflowRun' {} a -> s {workflowRunProperties = a} :: WorkflowRun) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON WorkflowRun where
  parseJSON =
    Data.withObject
      "WorkflowRun"
      ( \x ->
          WorkflowRun'
            Prelude.<$> (x Data..:? "CompletedOn")
            Prelude.<*> (x Data..:? "ErrorMessage")
            Prelude.<*> (x Data..:? "Graph")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PreviousRunId")
            Prelude.<*> (x Data..:? "StartedOn")
            Prelude.<*> (x Data..:? "StartingEventBatchCondition")
            Prelude.<*> (x Data..:? "Statistics")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "WorkflowRunId")
            Prelude.<*> ( x
                            Data..:? "WorkflowRunProperties"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable WorkflowRun where
  hashWithSalt _salt WorkflowRun' {..} =
    _salt
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` graph
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` previousRunId
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` startingEventBatchCondition
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` workflowRunId
      `Prelude.hashWithSalt` workflowRunProperties

instance Prelude.NFData WorkflowRun where
  rnf WorkflowRun' {..} =
    Prelude.rnf completedOn `Prelude.seq`
      Prelude.rnf errorMessage `Prelude.seq`
        Prelude.rnf graph `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf previousRunId `Prelude.seq`
              Prelude.rnf startedOn `Prelude.seq`
                Prelude.rnf startingEventBatchCondition `Prelude.seq`
                  Prelude.rnf statistics `Prelude.seq`
                    Prelude.rnf status `Prelude.seq`
                      Prelude.rnf workflowRunId `Prelude.seq`
                        Prelude.rnf workflowRunProperties
