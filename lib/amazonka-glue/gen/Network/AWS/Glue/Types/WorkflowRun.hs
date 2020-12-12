{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRun
  ( WorkflowRun (..),

    -- * Smart constructor
    mkWorkflowRun,

    -- * Lenses
    wrCompletedOn,
    wrStatus,
    wrGraph,
    wrStartedOn,
    wrWorkflowRunId,
    wrName,
    wrPreviousRunId,
    wrStatistics,
    wrErrorMessage,
    wrWorkflowRunProperties,
  )
where

import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRunStatistics
import Network.AWS.Glue.Types.WorkflowRunStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A workflow run is an execution of a workflow providing all the runtime information.
--
-- /See:/ 'mkWorkflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { completedOn ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe WorkflowRunStatus,
    graph :: Lude.Maybe WorkflowGraph,
    startedOn :: Lude.Maybe Lude.Timestamp,
    workflowRunId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    previousRunId :: Lude.Maybe Lude.Text,
    statistics :: Lude.Maybe WorkflowRunStatistics,
    errorMessage :: Lude.Maybe Lude.Text,
    workflowRunProperties ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowRun' with the minimum fields required to make a request.
--
-- * 'completedOn' - The date and time when the workflow run completed.
-- * 'errorMessage' - This error message describes any error that may have occurred in starting the workflow run. Currently the only error message is "Concurrent runs exceeded for workflow: @foo@ ."
-- * 'graph' - The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
-- * 'name' - Name of the workflow that was executed.
-- * 'previousRunId' - The ID of the previous workflow run.
-- * 'startedOn' - The date and time when the workflow run was started.
-- * 'statistics' - The statistics of the run.
-- * 'status' - The status of the workflow run.
-- * 'workflowRunId' - The ID of this workflow run.
-- * 'workflowRunProperties' - The workflow run properties which were set during the run.
mkWorkflowRun ::
  WorkflowRun
mkWorkflowRun =
  WorkflowRun'
    { completedOn = Lude.Nothing,
      status = Lude.Nothing,
      graph = Lude.Nothing,
      startedOn = Lude.Nothing,
      workflowRunId = Lude.Nothing,
      name = Lude.Nothing,
      previousRunId = Lude.Nothing,
      statistics = Lude.Nothing,
      errorMessage = Lude.Nothing,
      workflowRunProperties = Lude.Nothing
    }

-- | The date and time when the workflow run completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrCompletedOn :: Lens.Lens' WorkflowRun (Lude.Maybe Lude.Timestamp)
wrCompletedOn = Lens.lens (completedOn :: WorkflowRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedOn = a} :: WorkflowRun)
{-# DEPRECATED wrCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | The status of the workflow run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrStatus :: Lens.Lens' WorkflowRun (Lude.Maybe WorkflowRunStatus)
wrStatus = Lens.lens (status :: WorkflowRun -> Lude.Maybe WorkflowRunStatus) (\s a -> s {status = a} :: WorkflowRun)
{-# DEPRECATED wrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
--
-- /Note:/ Consider using 'graph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrGraph :: Lens.Lens' WorkflowRun (Lude.Maybe WorkflowGraph)
wrGraph = Lens.lens (graph :: WorkflowRun -> Lude.Maybe WorkflowGraph) (\s a -> s {graph = a} :: WorkflowRun)
{-# DEPRECATED wrGraph "Use generic-lens or generic-optics with 'graph' instead." #-}

-- | The date and time when the workflow run was started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrStartedOn :: Lens.Lens' WorkflowRun (Lude.Maybe Lude.Timestamp)
wrStartedOn = Lens.lens (startedOn :: WorkflowRun -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedOn = a} :: WorkflowRun)
{-# DEPRECATED wrStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

-- | The ID of this workflow run.
--
-- /Note:/ Consider using 'workflowRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrWorkflowRunId :: Lens.Lens' WorkflowRun (Lude.Maybe Lude.Text)
wrWorkflowRunId = Lens.lens (workflowRunId :: WorkflowRun -> Lude.Maybe Lude.Text) (\s a -> s {workflowRunId = a} :: WorkflowRun)
{-# DEPRECATED wrWorkflowRunId "Use generic-lens or generic-optics with 'workflowRunId' instead." #-}

-- | Name of the workflow that was executed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrName :: Lens.Lens' WorkflowRun (Lude.Maybe Lude.Text)
wrName = Lens.lens (name :: WorkflowRun -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: WorkflowRun)
{-# DEPRECATED wrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The ID of the previous workflow run.
--
-- /Note:/ Consider using 'previousRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrPreviousRunId :: Lens.Lens' WorkflowRun (Lude.Maybe Lude.Text)
wrPreviousRunId = Lens.lens (previousRunId :: WorkflowRun -> Lude.Maybe Lude.Text) (\s a -> s {previousRunId = a} :: WorkflowRun)
{-# DEPRECATED wrPreviousRunId "Use generic-lens or generic-optics with 'previousRunId' instead." #-}

-- | The statistics of the run.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrStatistics :: Lens.Lens' WorkflowRun (Lude.Maybe WorkflowRunStatistics)
wrStatistics = Lens.lens (statistics :: WorkflowRun -> Lude.Maybe WorkflowRunStatistics) (\s a -> s {statistics = a} :: WorkflowRun)
{-# DEPRECATED wrStatistics "Use generic-lens or generic-optics with 'statistics' instead." #-}

-- | This error message describes any error that may have occurred in starting the workflow run. Currently the only error message is "Concurrent runs exceeded for workflow: @foo@ ."
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrErrorMessage :: Lens.Lens' WorkflowRun (Lude.Maybe Lude.Text)
wrErrorMessage = Lens.lens (errorMessage :: WorkflowRun -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: WorkflowRun)
{-# DEPRECATED wrErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The workflow run properties which were set during the run.
--
-- /Note:/ Consider using 'workflowRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrWorkflowRunProperties :: Lens.Lens' WorkflowRun (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
wrWorkflowRunProperties = Lens.lens (workflowRunProperties :: WorkflowRun -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {workflowRunProperties = a} :: WorkflowRun)
{-# DEPRECATED wrWorkflowRunProperties "Use generic-lens or generic-optics with 'workflowRunProperties' instead." #-}

instance Lude.FromJSON WorkflowRun where
  parseJSON =
    Lude.withObject
      "WorkflowRun"
      ( \x ->
          WorkflowRun'
            Lude.<$> (x Lude..:? "CompletedOn")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "Graph")
            Lude.<*> (x Lude..:? "StartedOn")
            Lude.<*> (x Lude..:? "WorkflowRunId")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "PreviousRunId")
            Lude.<*> (x Lude..:? "Statistics")
            Lude.<*> (x Lude..:? "ErrorMessage")
            Lude.<*> (x Lude..:? "WorkflowRunProperties" Lude..!= Lude.mempty)
      )
