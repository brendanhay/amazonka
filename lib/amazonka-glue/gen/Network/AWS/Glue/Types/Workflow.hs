{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Workflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Workflow
  ( Workflow (..),

    -- * Smart constructor
    mkWorkflow,

    -- * Lenses
    wGraph,
    wLastModifiedOn,
    wMaxConcurrentRuns,
    wDefaultRunProperties,
    wName,
    wLastRun,
    wDescription,
    wCreatedOn,
  )
where

import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A workflow represents a flow in which AWS Glue components should be executed to complete a logical task.
--
-- /See:/ 'mkWorkflow' smart constructor.
data Workflow = Workflow'
  { -- | The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
    graph :: Lude.Maybe WorkflowGraph,
    -- | The date and time when the workflow was last modified.
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    -- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
    maxConcurrentRuns :: Lude.Maybe Lude.Int,
    -- | A collection of properties to be used as part of each execution of the workflow.
    defaultRunProperties :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The name of the workflow representing the flow.
    name :: Lude.Maybe Lude.Text,
    -- | The information about the last execution of the workflow.
    lastRun :: Lude.Maybe WorkflowRun,
    -- | A description of the workflow.
    description :: Lude.Maybe Lude.Text,
    -- | The date and time when the workflow was created.
    createdOn :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Workflow' with the minimum fields required to make a request.
--
-- * 'graph' - The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
-- * 'lastModifiedOn' - The date and time when the workflow was last modified.
-- * 'maxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
-- * 'defaultRunProperties' - A collection of properties to be used as part of each execution of the workflow.
-- * 'name' - The name of the workflow representing the flow.
-- * 'lastRun' - The information about the last execution of the workflow.
-- * 'description' - A description of the workflow.
-- * 'createdOn' - The date and time when the workflow was created.
mkWorkflow ::
  Workflow
mkWorkflow =
  Workflow'
    { graph = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      maxConcurrentRuns = Lude.Nothing,
      defaultRunProperties = Lude.Nothing,
      name = Lude.Nothing,
      lastRun = Lude.Nothing,
      description = Lude.Nothing,
      createdOn = Lude.Nothing
    }

-- | The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
--
-- /Note:/ Consider using 'graph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wGraph :: Lens.Lens' Workflow (Lude.Maybe WorkflowGraph)
wGraph = Lens.lens (graph :: Workflow -> Lude.Maybe WorkflowGraph) (\s a -> s {graph = a} :: Workflow)
{-# DEPRECATED wGraph "Use generic-lens or generic-optics with 'graph' instead." #-}

-- | The date and time when the workflow was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wLastModifiedOn :: Lens.Lens' Workflow (Lude.Maybe Lude.Timestamp)
wLastModifiedOn = Lens.lens (lastModifiedOn :: Workflow -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: Workflow)
{-# DEPRECATED wLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- /Note:/ Consider using 'maxConcurrentRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wMaxConcurrentRuns :: Lens.Lens' Workflow (Lude.Maybe Lude.Int)
wMaxConcurrentRuns = Lens.lens (maxConcurrentRuns :: Workflow -> Lude.Maybe Lude.Int) (\s a -> s {maxConcurrentRuns = a} :: Workflow)
{-# DEPRECATED wMaxConcurrentRuns "Use generic-lens or generic-optics with 'maxConcurrentRuns' instead." #-}

-- | A collection of properties to be used as part of each execution of the workflow.
--
-- /Note:/ Consider using 'defaultRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDefaultRunProperties :: Lens.Lens' Workflow (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wDefaultRunProperties = Lens.lens (defaultRunProperties :: Workflow -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {defaultRunProperties = a} :: Workflow)
{-# DEPRECATED wDefaultRunProperties "Use generic-lens or generic-optics with 'defaultRunProperties' instead." #-}

-- | The name of the workflow representing the flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wName :: Lens.Lens' Workflow (Lude.Maybe Lude.Text)
wName = Lens.lens (name :: Workflow -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Workflow)
{-# DEPRECATED wName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The information about the last execution of the workflow.
--
-- /Note:/ Consider using 'lastRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wLastRun :: Lens.Lens' Workflow (Lude.Maybe WorkflowRun)
wLastRun = Lens.lens (lastRun :: Workflow -> Lude.Maybe WorkflowRun) (\s a -> s {lastRun = a} :: Workflow)
{-# DEPRECATED wLastRun "Use generic-lens or generic-optics with 'lastRun' instead." #-}

-- | A description of the workflow.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDescription :: Lens.Lens' Workflow (Lude.Maybe Lude.Text)
wDescription = Lens.lens (description :: Workflow -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Workflow)
{-# DEPRECATED wDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The date and time when the workflow was created.
--
-- /Note:/ Consider using 'createdOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wCreatedOn :: Lens.Lens' Workflow (Lude.Maybe Lude.Timestamp)
wCreatedOn = Lens.lens (createdOn :: Workflow -> Lude.Maybe Lude.Timestamp) (\s a -> s {createdOn = a} :: Workflow)
{-# DEPRECATED wCreatedOn "Use generic-lens or generic-optics with 'createdOn' instead." #-}

instance Lude.FromJSON Workflow where
  parseJSON =
    Lude.withObject
      "Workflow"
      ( \x ->
          Workflow'
            Lude.<$> (x Lude..:? "Graph")
            Lude.<*> (x Lude..:? "LastModifiedOn")
            Lude.<*> (x Lude..:? "MaxConcurrentRuns")
            Lude.<*> (x Lude..:? "DefaultRunProperties" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "LastRun")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "CreatedOn")
      )
