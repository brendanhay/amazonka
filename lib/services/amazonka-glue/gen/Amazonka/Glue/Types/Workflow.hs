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
-- Module      : Amazonka.Glue.Types.Workflow
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Workflow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.BlueprintDetails
import Amazonka.Glue.Types.WorkflowGraph
import Amazonka.Glue.Types.WorkflowRun
import qualified Amazonka.Prelude as Prelude

-- | A workflow is a collection of multiple dependent Glue jobs and crawlers
-- that are run to complete a complex ETL task. A workflow manages the
-- execution and monitoring of all its jobs and crawlers.
--
-- /See:/ 'newWorkflow' smart constructor.
data Workflow = Workflow'
  { -- | This structure indicates the details of the blueprint that this
    -- particular workflow is created from.
    blueprintDetails :: Prelude.Maybe BlueprintDetails,
    -- | The date and time when the workflow was created.
    createdOn :: Prelude.Maybe Data.POSIX,
    -- | A collection of properties to be used as part of each execution of the
    -- workflow. The run properties are made available to each job in the
    -- workflow. A job can modify the properties for the next jobs in the flow.
    defaultRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A description of the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The graph representing all the Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Prelude.Maybe WorkflowGraph,
    -- | The date and time when the workflow was last modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The information about the last execution of the workflow.
    lastRun :: Prelude.Maybe WorkflowRun,
    -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int,
    -- | The name of the workflow.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Workflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintDetails', 'workflow_blueprintDetails' - This structure indicates the details of the blueprint that this
-- particular workflow is created from.
--
-- 'createdOn', 'workflow_createdOn' - The date and time when the workflow was created.
--
-- 'defaultRunProperties', 'workflow_defaultRunProperties' - A collection of properties to be used as part of each execution of the
-- workflow. The run properties are made available to each job in the
-- workflow. A job can modify the properties for the next jobs in the flow.
--
-- 'description', 'workflow_description' - A description of the workflow.
--
-- 'graph', 'workflow_graph' - The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
--
-- 'lastModifiedOn', 'workflow_lastModifiedOn' - The date and time when the workflow was last modified.
--
-- 'lastRun', 'workflow_lastRun' - The information about the last execution of the workflow.
--
-- 'maxConcurrentRuns', 'workflow_maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
--
-- 'name', 'workflow_name' - The name of the workflow.
newWorkflow ::
  Workflow
newWorkflow =
  Workflow'
    { blueprintDetails = Prelude.Nothing,
      createdOn = Prelude.Nothing,
      defaultRunProperties = Prelude.Nothing,
      description = Prelude.Nothing,
      graph = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      lastRun = Prelude.Nothing,
      maxConcurrentRuns = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | This structure indicates the details of the blueprint that this
-- particular workflow is created from.
workflow_blueprintDetails :: Lens.Lens' Workflow (Prelude.Maybe BlueprintDetails)
workflow_blueprintDetails = Lens.lens (\Workflow' {blueprintDetails} -> blueprintDetails) (\s@Workflow' {} a -> s {blueprintDetails = a} :: Workflow)

-- | The date and time when the workflow was created.
workflow_createdOn :: Lens.Lens' Workflow (Prelude.Maybe Prelude.UTCTime)
workflow_createdOn = Lens.lens (\Workflow' {createdOn} -> createdOn) (\s@Workflow' {} a -> s {createdOn = a} :: Workflow) Prelude.. Lens.mapping Data._Time

-- | A collection of properties to be used as part of each execution of the
-- workflow. The run properties are made available to each job in the
-- workflow. A job can modify the properties for the next jobs in the flow.
workflow_defaultRunProperties :: Lens.Lens' Workflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workflow_defaultRunProperties = Lens.lens (\Workflow' {defaultRunProperties} -> defaultRunProperties) (\s@Workflow' {} a -> s {defaultRunProperties = a} :: Workflow) Prelude.. Lens.mapping Lens.coerced

-- | A description of the workflow.
workflow_description :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Text)
workflow_description = Lens.lens (\Workflow' {description} -> description) (\s@Workflow' {} a -> s {description = a} :: Workflow)

-- | The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflow_graph :: Lens.Lens' Workflow (Prelude.Maybe WorkflowGraph)
workflow_graph = Lens.lens (\Workflow' {graph} -> graph) (\s@Workflow' {} a -> s {graph = a} :: Workflow)

-- | The date and time when the workflow was last modified.
workflow_lastModifiedOn :: Lens.Lens' Workflow (Prelude.Maybe Prelude.UTCTime)
workflow_lastModifiedOn = Lens.lens (\Workflow' {lastModifiedOn} -> lastModifiedOn) (\s@Workflow' {} a -> s {lastModifiedOn = a} :: Workflow) Prelude.. Lens.mapping Data._Time

-- | The information about the last execution of the workflow.
workflow_lastRun :: Lens.Lens' Workflow (Prelude.Maybe WorkflowRun)
workflow_lastRun = Lens.lens (\Workflow' {lastRun} -> lastRun) (\s@Workflow' {} a -> s {lastRun = a} :: Workflow)

-- | You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
workflow_maxConcurrentRuns :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Int)
workflow_maxConcurrentRuns = Lens.lens (\Workflow' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@Workflow' {} a -> s {maxConcurrentRuns = a} :: Workflow)

-- | The name of the workflow.
workflow_name :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Text)
workflow_name = Lens.lens (\Workflow' {name} -> name) (\s@Workflow' {} a -> s {name = a} :: Workflow)

instance Data.FromJSON Workflow where
  parseJSON =
    Data.withObject
      "Workflow"
      ( \x ->
          Workflow'
            Prelude.<$> (x Data..:? "BlueprintDetails")
            Prelude.<*> (x Data..:? "CreatedOn")
            Prelude.<*> ( x
                            Data..:? "DefaultRunProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Graph")
            Prelude.<*> (x Data..:? "LastModifiedOn")
            Prelude.<*> (x Data..:? "LastRun")
            Prelude.<*> (x Data..:? "MaxConcurrentRuns")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable Workflow where
  hashWithSalt _salt Workflow' {..} =
    _salt
      `Prelude.hashWithSalt` blueprintDetails
      `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` defaultRunProperties
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` graph
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` lastRun
      `Prelude.hashWithSalt` maxConcurrentRuns
      `Prelude.hashWithSalt` name

instance Prelude.NFData Workflow where
  rnf Workflow' {..} =
    Prelude.rnf blueprintDetails
      `Prelude.seq` Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf defaultRunProperties
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf graph
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf lastRun
      `Prelude.seq` Prelude.rnf maxConcurrentRuns
      `Prelude.seq` Prelude.rnf name
