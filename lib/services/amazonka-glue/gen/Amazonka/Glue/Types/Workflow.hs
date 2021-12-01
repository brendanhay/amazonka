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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Workflow where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.BlueprintDetails
import Amazonka.Glue.Types.WorkflowGraph
import Amazonka.Glue.Types.WorkflowRun
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A workflow is a collection of multiple dependent Glue jobs and crawlers
-- that are run to complete a complex ETL task. A workflow manages the
-- execution and monitoring of all its jobs and crawlers.
--
-- /See:/ 'newWorkflow' smart constructor.
data Workflow = Workflow'
  { -- | The graph representing all the Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Prelude.Maybe WorkflowGraph,
    -- | The date and time when the workflow was last modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | This structure indicates the details of the blueprint that this
    -- particular workflow is created from.
    blueprintDetails :: Prelude.Maybe BlueprintDetails,
    -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int,
    -- | A collection of properties to be used as part of each execution of the
    -- workflow. The run properties are made available to each job in the
    -- workflow. A job can modify the properties for the next jobs in the flow.
    defaultRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the workflow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The information about the last execution of the workflow.
    lastRun :: Prelude.Maybe WorkflowRun,
    -- | A description of the workflow.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the workflow was created.
    createdOn :: Prelude.Maybe Core.POSIX
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
-- 'graph', 'workflow_graph' - The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
--
-- 'lastModifiedOn', 'workflow_lastModifiedOn' - The date and time when the workflow was last modified.
--
-- 'blueprintDetails', 'workflow_blueprintDetails' - This structure indicates the details of the blueprint that this
-- particular workflow is created from.
--
-- 'maxConcurrentRuns', 'workflow_maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
--
-- 'defaultRunProperties', 'workflow_defaultRunProperties' - A collection of properties to be used as part of each execution of the
-- workflow. The run properties are made available to each job in the
-- workflow. A job can modify the properties for the next jobs in the flow.
--
-- 'name', 'workflow_name' - The name of the workflow.
--
-- 'lastRun', 'workflow_lastRun' - The information about the last execution of the workflow.
--
-- 'description', 'workflow_description' - A description of the workflow.
--
-- 'createdOn', 'workflow_createdOn' - The date and time when the workflow was created.
newWorkflow ::
  Workflow
newWorkflow =
  Workflow'
    { graph = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      blueprintDetails = Prelude.Nothing,
      maxConcurrentRuns = Prelude.Nothing,
      defaultRunProperties = Prelude.Nothing,
      name = Prelude.Nothing,
      lastRun = Prelude.Nothing,
      description = Prelude.Nothing,
      createdOn = Prelude.Nothing
    }

-- | The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflow_graph :: Lens.Lens' Workflow (Prelude.Maybe WorkflowGraph)
workflow_graph = Lens.lens (\Workflow' {graph} -> graph) (\s@Workflow' {} a -> s {graph = a} :: Workflow)

-- | The date and time when the workflow was last modified.
workflow_lastModifiedOn :: Lens.Lens' Workflow (Prelude.Maybe Prelude.UTCTime)
workflow_lastModifiedOn = Lens.lens (\Workflow' {lastModifiedOn} -> lastModifiedOn) (\s@Workflow' {} a -> s {lastModifiedOn = a} :: Workflow) Prelude.. Lens.mapping Core._Time

-- | This structure indicates the details of the blueprint that this
-- particular workflow is created from.
workflow_blueprintDetails :: Lens.Lens' Workflow (Prelude.Maybe BlueprintDetails)
workflow_blueprintDetails = Lens.lens (\Workflow' {blueprintDetails} -> blueprintDetails) (\s@Workflow' {} a -> s {blueprintDetails = a} :: Workflow)

-- | You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
workflow_maxConcurrentRuns :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Int)
workflow_maxConcurrentRuns = Lens.lens (\Workflow' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@Workflow' {} a -> s {maxConcurrentRuns = a} :: Workflow)

-- | A collection of properties to be used as part of each execution of the
-- workflow. The run properties are made available to each job in the
-- workflow. A job can modify the properties for the next jobs in the flow.
workflow_defaultRunProperties :: Lens.Lens' Workflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workflow_defaultRunProperties = Lens.lens (\Workflow' {defaultRunProperties} -> defaultRunProperties) (\s@Workflow' {} a -> s {defaultRunProperties = a} :: Workflow) Prelude.. Lens.mapping Lens.coerced

-- | The name of the workflow.
workflow_name :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Text)
workflow_name = Lens.lens (\Workflow' {name} -> name) (\s@Workflow' {} a -> s {name = a} :: Workflow)

-- | The information about the last execution of the workflow.
workflow_lastRun :: Lens.Lens' Workflow (Prelude.Maybe WorkflowRun)
workflow_lastRun = Lens.lens (\Workflow' {lastRun} -> lastRun) (\s@Workflow' {} a -> s {lastRun = a} :: Workflow)

-- | A description of the workflow.
workflow_description :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Text)
workflow_description = Lens.lens (\Workflow' {description} -> description) (\s@Workflow' {} a -> s {description = a} :: Workflow)

-- | The date and time when the workflow was created.
workflow_createdOn :: Lens.Lens' Workflow (Prelude.Maybe Prelude.UTCTime)
workflow_createdOn = Lens.lens (\Workflow' {createdOn} -> createdOn) (\s@Workflow' {} a -> s {createdOn = a} :: Workflow) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Workflow where
  parseJSON =
    Core.withObject
      "Workflow"
      ( \x ->
          Workflow'
            Prelude.<$> (x Core..:? "Graph")
            Prelude.<*> (x Core..:? "LastModifiedOn")
            Prelude.<*> (x Core..:? "BlueprintDetails")
            Prelude.<*> (x Core..:? "MaxConcurrentRuns")
            Prelude.<*> ( x Core..:? "DefaultRunProperties"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "LastRun")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "CreatedOn")
      )

instance Prelude.Hashable Workflow where
  hashWithSalt salt' Workflow' {..} =
    salt' `Prelude.hashWithSalt` createdOn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastRun
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` defaultRunProperties
      `Prelude.hashWithSalt` maxConcurrentRuns
      `Prelude.hashWithSalt` blueprintDetails
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` graph

instance Prelude.NFData Workflow where
  rnf Workflow' {..} =
    Prelude.rnf graph
      `Prelude.seq` Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastRun
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf defaultRunProperties
      `Prelude.seq` Prelude.rnf maxConcurrentRuns
      `Prelude.seq` Prelude.rnf blueprintDetails
      `Prelude.seq` Prelude.rnf lastModifiedOn
