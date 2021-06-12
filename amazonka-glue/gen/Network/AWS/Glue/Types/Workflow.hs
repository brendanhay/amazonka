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
-- Module      : Network.AWS.Glue.Types.Workflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Workflow where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import qualified Network.AWS.Lens as Lens

-- | A workflow represents a flow in which AWS Glue components should be
-- executed to complete a logical task.
--
-- /See:/ 'newWorkflow' smart constructor.
data Workflow = Workflow'
  { -- | The date and time when the workflow was created.
    createdOn :: Core.Maybe Core.POSIX,
    -- | A collection of properties to be used as part of each execution of the
    -- workflow.
    defaultRunProperties :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The information about the last execution of the workflow.
    lastRun :: Core.Maybe WorkflowRun,
    -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Core.Maybe Core.Int,
    -- | The date and time when the workflow was last modified.
    lastModifiedOn :: Core.Maybe Core.POSIX,
    -- | The name of the workflow representing the flow.
    name :: Core.Maybe Core.Text,
    -- | The graph representing all the AWS Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Core.Maybe WorkflowGraph,
    -- | A description of the workflow.
    description :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Workflow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdOn', 'workflow_createdOn' - The date and time when the workflow was created.
--
-- 'defaultRunProperties', 'workflow_defaultRunProperties' - A collection of properties to be used as part of each execution of the
-- workflow.
--
-- 'lastRun', 'workflow_lastRun' - The information about the last execution of the workflow.
--
-- 'maxConcurrentRuns', 'workflow_maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
--
-- 'lastModifiedOn', 'workflow_lastModifiedOn' - The date and time when the workflow was last modified.
--
-- 'name', 'workflow_name' - The name of the workflow representing the flow.
--
-- 'graph', 'workflow_graph' - The graph representing all the AWS Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
--
-- 'description', 'workflow_description' - A description of the workflow.
newWorkflow ::
  Workflow
newWorkflow =
  Workflow'
    { createdOn = Core.Nothing,
      defaultRunProperties = Core.Nothing,
      lastRun = Core.Nothing,
      maxConcurrentRuns = Core.Nothing,
      lastModifiedOn = Core.Nothing,
      name = Core.Nothing,
      graph = Core.Nothing,
      description = Core.Nothing
    }

-- | The date and time when the workflow was created.
workflow_createdOn :: Lens.Lens' Workflow (Core.Maybe Core.UTCTime)
workflow_createdOn = Lens.lens (\Workflow' {createdOn} -> createdOn) (\s@Workflow' {} a -> s {createdOn = a} :: Workflow) Core.. Lens.mapping Core._Time

-- | A collection of properties to be used as part of each execution of the
-- workflow.
workflow_defaultRunProperties :: Lens.Lens' Workflow (Core.Maybe (Core.HashMap Core.Text Core.Text))
workflow_defaultRunProperties = Lens.lens (\Workflow' {defaultRunProperties} -> defaultRunProperties) (\s@Workflow' {} a -> s {defaultRunProperties = a} :: Workflow) Core.. Lens.mapping Lens._Coerce

-- | The information about the last execution of the workflow.
workflow_lastRun :: Lens.Lens' Workflow (Core.Maybe WorkflowRun)
workflow_lastRun = Lens.lens (\Workflow' {lastRun} -> lastRun) (\s@Workflow' {} a -> s {lastRun = a} :: Workflow)

-- | You can use this parameter to prevent unwanted multiple updates to data,
-- to control costs, or in some cases, to prevent exceeding the maximum
-- number of concurrent runs of any of the component jobs. If you leave
-- this parameter blank, there is no limit to the number of concurrent
-- workflow runs.
workflow_maxConcurrentRuns :: Lens.Lens' Workflow (Core.Maybe Core.Int)
workflow_maxConcurrentRuns = Lens.lens (\Workflow' {maxConcurrentRuns} -> maxConcurrentRuns) (\s@Workflow' {} a -> s {maxConcurrentRuns = a} :: Workflow)

-- | The date and time when the workflow was last modified.
workflow_lastModifiedOn :: Lens.Lens' Workflow (Core.Maybe Core.UTCTime)
workflow_lastModifiedOn = Lens.lens (\Workflow' {lastModifiedOn} -> lastModifiedOn) (\s@Workflow' {} a -> s {lastModifiedOn = a} :: Workflow) Core.. Lens.mapping Core._Time

-- | The name of the workflow representing the flow.
workflow_name :: Lens.Lens' Workflow (Core.Maybe Core.Text)
workflow_name = Lens.lens (\Workflow' {name} -> name) (\s@Workflow' {} a -> s {name = a} :: Workflow)

-- | The graph representing all the AWS Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflow_graph :: Lens.Lens' Workflow (Core.Maybe WorkflowGraph)
workflow_graph = Lens.lens (\Workflow' {graph} -> graph) (\s@Workflow' {} a -> s {graph = a} :: Workflow)

-- | A description of the workflow.
workflow_description :: Lens.Lens' Workflow (Core.Maybe Core.Text)
workflow_description = Lens.lens (\Workflow' {description} -> description) (\s@Workflow' {} a -> s {description = a} :: Workflow)

instance Core.FromJSON Workflow where
  parseJSON =
    Core.withObject
      "Workflow"
      ( \x ->
          Workflow'
            Core.<$> (x Core..:? "CreatedOn")
            Core.<*> ( x Core..:? "DefaultRunProperties"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "LastRun")
            Core.<*> (x Core..:? "MaxConcurrentRuns")
            Core.<*> (x Core..:? "LastModifiedOn")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Graph")
            Core.<*> (x Core..:? "Description")
      )

instance Core.Hashable Workflow

instance Core.NFData Workflow
