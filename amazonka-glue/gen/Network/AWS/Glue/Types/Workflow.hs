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
-- Module      : Network.AWS.Glue.Types.Workflow
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Workflow where

import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A workflow represents a flow in which AWS Glue components should be
-- executed to complete a logical task.
--
-- /See:/ 'newWorkflow' smart constructor.
data Workflow = Workflow'
  { -- | The date and time when the workflow was created.
    createdOn :: Prelude.Maybe Prelude.POSIX,
    -- | A collection of properties to be used as part of each execution of the
    -- workflow.
    defaultRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The information about the last execution of the workflow.
    lastRun :: Prelude.Maybe WorkflowRun,
    -- | You can use this parameter to prevent unwanted multiple updates to data,
    -- to control costs, or in some cases, to prevent exceeding the maximum
    -- number of concurrent runs of any of the component jobs. If you leave
    -- this parameter blank, there is no limit to the number of concurrent
    -- workflow runs.
    maxConcurrentRuns :: Prelude.Maybe Prelude.Int,
    -- | The date and time when the workflow was last modified.
    lastModifiedOn :: Prelude.Maybe Prelude.POSIX,
    -- | The name of the workflow representing the flow.
    name :: Prelude.Maybe Prelude.Text,
    -- | The graph representing all the AWS Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Prelude.Maybe WorkflowGraph,
    -- | A description of the workflow.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { createdOn = Prelude.Nothing,
      defaultRunProperties = Prelude.Nothing,
      lastRun = Prelude.Nothing,
      maxConcurrentRuns = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      name = Prelude.Nothing,
      graph = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The date and time when the workflow was created.
workflow_createdOn :: Lens.Lens' Workflow (Prelude.Maybe Prelude.UTCTime)
workflow_createdOn = Lens.lens (\Workflow' {createdOn} -> createdOn) (\s@Workflow' {} a -> s {createdOn = a} :: Workflow) Prelude.. Lens.mapping Prelude._Time

-- | A collection of properties to be used as part of each execution of the
-- workflow.
workflow_defaultRunProperties :: Lens.Lens' Workflow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workflow_defaultRunProperties = Lens.lens (\Workflow' {defaultRunProperties} -> defaultRunProperties) (\s@Workflow' {} a -> s {defaultRunProperties = a} :: Workflow) Prelude.. Lens.mapping Prelude._Coerce

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

-- | The date and time when the workflow was last modified.
workflow_lastModifiedOn :: Lens.Lens' Workflow (Prelude.Maybe Prelude.UTCTime)
workflow_lastModifiedOn = Lens.lens (\Workflow' {lastModifiedOn} -> lastModifiedOn) (\s@Workflow' {} a -> s {lastModifiedOn = a} :: Workflow) Prelude.. Lens.mapping Prelude._Time

-- | The name of the workflow representing the flow.
workflow_name :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Text)
workflow_name = Lens.lens (\Workflow' {name} -> name) (\s@Workflow' {} a -> s {name = a} :: Workflow)

-- | The graph representing all the AWS Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflow_graph :: Lens.Lens' Workflow (Prelude.Maybe WorkflowGraph)
workflow_graph = Lens.lens (\Workflow' {graph} -> graph) (\s@Workflow' {} a -> s {graph = a} :: Workflow)

-- | A description of the workflow.
workflow_description :: Lens.Lens' Workflow (Prelude.Maybe Prelude.Text)
workflow_description = Lens.lens (\Workflow' {description} -> description) (\s@Workflow' {} a -> s {description = a} :: Workflow)

instance Prelude.FromJSON Workflow where
  parseJSON =
    Prelude.withObject
      "Workflow"
      ( \x ->
          Workflow'
            Prelude.<$> (x Prelude..:? "CreatedOn")
            Prelude.<*> ( x Prelude..:? "DefaultRunProperties"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "LastRun")
            Prelude.<*> (x Prelude..:? "MaxConcurrentRuns")
            Prelude.<*> (x Prelude..:? "LastModifiedOn")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Graph")
            Prelude.<*> (x Prelude..:? "Description")
      )

instance Prelude.Hashable Workflow

instance Prelude.NFData Workflow
