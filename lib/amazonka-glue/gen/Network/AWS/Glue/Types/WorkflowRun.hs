{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.WorkflowRun
  ( WorkflowRun (..)
  -- * Smart constructor
  , mkWorkflowRun
  -- * Lenses
  , wrCompletedOn
  , wrErrorMessage
  , wrGraph
  , wrName
  , wrPreviousRunId
  , wrStartedOn
  , wrStatistics
  , wrStatus
  , wrWorkflowRunId
  , wrWorkflowRunProperties
  ) where

import qualified Network.AWS.Glue.Types.ErrorMessage as Types
import qualified Network.AWS.Glue.Types.GenericString as Types
import qualified Network.AWS.Glue.Types.IdString as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.PreviousRunId as Types
import qualified Network.AWS.Glue.Types.WorkflowGraph as Types
import qualified Network.AWS.Glue.Types.WorkflowRunId as Types
import qualified Network.AWS.Glue.Types.WorkflowRunStatistics as Types
import qualified Network.AWS.Glue.Types.WorkflowRunStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A workflow run is an execution of a workflow providing all the runtime information.
--
-- /See:/ 'mkWorkflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { completedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the workflow run completed.
  , errorMessage :: Core.Maybe Types.ErrorMessage
    -- ^ This error message describes any error that may have occurred in starting the workflow run. Currently the only error message is "Concurrent runs exceeded for workflow: @foo@ ."
  , graph :: Core.Maybe Types.WorkflowGraph
    -- ^ The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
  , name :: Core.Maybe Types.Name
    -- ^ Name of the workflow that was executed.
  , previousRunId :: Core.Maybe Types.PreviousRunId
    -- ^ The ID of the previous workflow run.
  , startedOn :: Core.Maybe Core.NominalDiffTime
    -- ^ The date and time when the workflow run was started.
  , statistics :: Core.Maybe Types.WorkflowRunStatistics
    -- ^ The statistics of the run.
  , status :: Core.Maybe Types.WorkflowRunStatus
    -- ^ The status of the workflow run.
  , workflowRunId :: Core.Maybe Types.WorkflowRunId
    -- ^ The ID of this workflow run.
  , workflowRunProperties :: Core.Maybe (Core.HashMap Types.IdString Types.GenericString)
    -- ^ The workflow run properties which were set during the run.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'WorkflowRun' value with any optional fields omitted.
mkWorkflowRun
    :: WorkflowRun
mkWorkflowRun
  = WorkflowRun'{completedOn = Core.Nothing,
                 errorMessage = Core.Nothing, graph = Core.Nothing,
                 name = Core.Nothing, previousRunId = Core.Nothing,
                 startedOn = Core.Nothing, statistics = Core.Nothing,
                 status = Core.Nothing, workflowRunId = Core.Nothing,
                 workflowRunProperties = Core.Nothing}

-- | The date and time when the workflow run completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrCompletedOn :: Lens.Lens' WorkflowRun (Core.Maybe Core.NominalDiffTime)
wrCompletedOn = Lens.field @"completedOn"
{-# INLINEABLE wrCompletedOn #-}
{-# DEPRECATED completedOn "Use generic-lens or generic-optics with 'completedOn' instead"  #-}

-- | This error message describes any error that may have occurred in starting the workflow run. Currently the only error message is "Concurrent runs exceeded for workflow: @foo@ ."
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrErrorMessage :: Lens.Lens' WorkflowRun (Core.Maybe Types.ErrorMessage)
wrErrorMessage = Lens.field @"errorMessage"
{-# INLINEABLE wrErrorMessage #-}
{-# DEPRECATED errorMessage "Use generic-lens or generic-optics with 'errorMessage' instead"  #-}

-- | The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
--
-- /Note:/ Consider using 'graph' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrGraph :: Lens.Lens' WorkflowRun (Core.Maybe Types.WorkflowGraph)
wrGraph = Lens.field @"graph"
{-# INLINEABLE wrGraph #-}
{-# DEPRECATED graph "Use generic-lens or generic-optics with 'graph' instead"  #-}

-- | Name of the workflow that was executed.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrName :: Lens.Lens' WorkflowRun (Core.Maybe Types.Name)
wrName = Lens.field @"name"
{-# INLINEABLE wrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The ID of the previous workflow run.
--
-- /Note:/ Consider using 'previousRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrPreviousRunId :: Lens.Lens' WorkflowRun (Core.Maybe Types.PreviousRunId)
wrPreviousRunId = Lens.field @"previousRunId"
{-# INLINEABLE wrPreviousRunId #-}
{-# DEPRECATED previousRunId "Use generic-lens or generic-optics with 'previousRunId' instead"  #-}

-- | The date and time when the workflow run was started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrStartedOn :: Lens.Lens' WorkflowRun (Core.Maybe Core.NominalDiffTime)
wrStartedOn = Lens.field @"startedOn"
{-# INLINEABLE wrStartedOn #-}
{-# DEPRECATED startedOn "Use generic-lens or generic-optics with 'startedOn' instead"  #-}

-- | The statistics of the run.
--
-- /Note:/ Consider using 'statistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrStatistics :: Lens.Lens' WorkflowRun (Core.Maybe Types.WorkflowRunStatistics)
wrStatistics = Lens.field @"statistics"
{-# INLINEABLE wrStatistics #-}
{-# DEPRECATED statistics "Use generic-lens or generic-optics with 'statistics' instead"  #-}

-- | The status of the workflow run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrStatus :: Lens.Lens' WorkflowRun (Core.Maybe Types.WorkflowRunStatus)
wrStatus = Lens.field @"status"
{-# INLINEABLE wrStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of this workflow run.
--
-- /Note:/ Consider using 'workflowRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrWorkflowRunId :: Lens.Lens' WorkflowRun (Core.Maybe Types.WorkflowRunId)
wrWorkflowRunId = Lens.field @"workflowRunId"
{-# INLINEABLE wrWorkflowRunId #-}
{-# DEPRECATED workflowRunId "Use generic-lens or generic-optics with 'workflowRunId' instead"  #-}

-- | The workflow run properties which were set during the run.
--
-- /Note:/ Consider using 'workflowRunProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wrWorkflowRunProperties :: Lens.Lens' WorkflowRun (Core.Maybe (Core.HashMap Types.IdString Types.GenericString))
wrWorkflowRunProperties = Lens.field @"workflowRunProperties"
{-# INLINEABLE wrWorkflowRunProperties #-}
{-# DEPRECATED workflowRunProperties "Use generic-lens or generic-optics with 'workflowRunProperties' instead"  #-}

instance Core.FromJSON WorkflowRun where
        parseJSON
          = Core.withObject "WorkflowRun" Core.$
              \ x ->
                WorkflowRun' Core.<$>
                  (x Core..:? "CompletedOn") Core.<*> x Core..:? "ErrorMessage"
                    Core.<*> x Core..:? "Graph"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "PreviousRunId"
                    Core.<*> x Core..:? "StartedOn"
                    Core.<*> x Core..:? "Statistics"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "WorkflowRunId"
                    Core.<*> x Core..:? "WorkflowRunProperties"
