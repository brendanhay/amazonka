{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionInfo
  ( WorkflowExecutionInfo (..)
  -- * Smart constructor
  , mkWorkflowExecutionInfo
  -- * Lenses
  , weiExecution
  , weiWorkflowType
  , weiStartTimestamp
  , weiExecutionStatus
  , weiCancelRequested
  , weiCloseStatus
  , weiCloseTimestamp
  , weiParent
  , weiTagList
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.CloseStatus as Types
import qualified Network.AWS.SWF.Types.ExecutionStatus as Types
import qualified Network.AWS.SWF.Types.Tag as Types
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Contains information about a workflow execution.
--
-- /See:/ 'mkWorkflowExecutionInfo' smart constructor.
data WorkflowExecutionInfo = WorkflowExecutionInfo'
  { execution :: Types.WorkflowExecution
    -- ^ The workflow execution this information is about.
  , workflowType :: Types.WorkflowType
    -- ^ The type of the workflow execution.
  , startTimestamp :: Core.NominalDiffTime
    -- ^ The time when the execution was started.
  , executionStatus :: Types.ExecutionStatus
    -- ^ The current status of the execution.
  , cancelRequested :: Core.Maybe Core.Bool
    -- ^ Set to true if a cancellation is requested for this workflow execution.
  , closeStatus :: Core.Maybe Types.CloseStatus
    -- ^ If the execution status is closed then this specifies how the execution was closed:
--
--
--     * @COMPLETED@ – the execution was successfully completed.
--
--
--     * @CANCELED@ – the execution was canceled.Cancellation allows the implementation to gracefully clean up before the execution is closed.
--
--
--     * @TERMINATED@ – the execution was force terminated.
--
--
--     * @FAILED@ – the execution failed to complete.
--
--
--     * @TIMED_OUT@ – the execution did not complete in the alloted time and was automatically timed out.
--
--
--     * @CONTINUED_AS_NEW@ – the execution is logically continued. This means the current execution was completed and a new execution was started to carry on the workflow.
--
--
  , closeTimestamp :: Core.Maybe Core.NominalDiffTime
    -- ^ The time when the workflow execution was closed. Set only if the execution status is CLOSED.
  , parent :: Core.Maybe Types.WorkflowExecution
    -- ^ If this workflow execution is a child of another execution then contains the workflow execution that started this execution.
  , tagList :: Core.Maybe [Types.Tag]
    -- ^ The list of tags associated with the workflow execution. Tags can be used to identify and list workflow executions of interest through the visibility APIs. A workflow execution can have a maximum of 5 tags.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'WorkflowExecutionInfo' value with any optional fields omitted.
mkWorkflowExecutionInfo
    :: Types.WorkflowExecution -- ^ 'execution'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> Core.NominalDiffTime -- ^ 'startTimestamp'
    -> Types.ExecutionStatus -- ^ 'executionStatus'
    -> WorkflowExecutionInfo
mkWorkflowExecutionInfo execution workflowType startTimestamp
  executionStatus
  = WorkflowExecutionInfo'{execution, workflowType, startTimestamp,
                           executionStatus, cancelRequested = Core.Nothing,
                           closeStatus = Core.Nothing, closeTimestamp = Core.Nothing,
                           parent = Core.Nothing, tagList = Core.Nothing}

-- | The workflow execution this information is about.
--
-- /Note:/ Consider using 'execution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiExecution :: Lens.Lens' WorkflowExecutionInfo Types.WorkflowExecution
weiExecution = Lens.field @"execution"
{-# INLINEABLE weiExecution #-}
{-# DEPRECATED execution "Use generic-lens or generic-optics with 'execution' instead"  #-}

-- | The type of the workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiWorkflowType :: Lens.Lens' WorkflowExecutionInfo Types.WorkflowType
weiWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE weiWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The time when the execution was started.
--
-- /Note:/ Consider using 'startTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiStartTimestamp :: Lens.Lens' WorkflowExecutionInfo Core.NominalDiffTime
weiStartTimestamp = Lens.field @"startTimestamp"
{-# INLINEABLE weiStartTimestamp #-}
{-# DEPRECATED startTimestamp "Use generic-lens or generic-optics with 'startTimestamp' instead"  #-}

-- | The current status of the execution.
--
-- /Note:/ Consider using 'executionStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiExecutionStatus :: Lens.Lens' WorkflowExecutionInfo Types.ExecutionStatus
weiExecutionStatus = Lens.field @"executionStatus"
{-# INLINEABLE weiExecutionStatus #-}
{-# DEPRECATED executionStatus "Use generic-lens or generic-optics with 'executionStatus' instead"  #-}

-- | Set to true if a cancellation is requested for this workflow execution.
--
-- /Note:/ Consider using 'cancelRequested' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiCancelRequested :: Lens.Lens' WorkflowExecutionInfo (Core.Maybe Core.Bool)
weiCancelRequested = Lens.field @"cancelRequested"
{-# INLINEABLE weiCancelRequested #-}
{-# DEPRECATED cancelRequested "Use generic-lens or generic-optics with 'cancelRequested' instead"  #-}

-- | If the execution status is closed then this specifies how the execution was closed:
--
--
--     * @COMPLETED@ – the execution was successfully completed.
--
--
--     * @CANCELED@ – the execution was canceled.Cancellation allows the implementation to gracefully clean up before the execution is closed.
--
--
--     * @TERMINATED@ – the execution was force terminated.
--
--
--     * @FAILED@ – the execution failed to complete.
--
--
--     * @TIMED_OUT@ – the execution did not complete in the alloted time and was automatically timed out.
--
--
--     * @CONTINUED_AS_NEW@ – the execution is logically continued. This means the current execution was completed and a new execution was started to carry on the workflow.
--
--
--
-- /Note:/ Consider using 'closeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiCloseStatus :: Lens.Lens' WorkflowExecutionInfo (Core.Maybe Types.CloseStatus)
weiCloseStatus = Lens.field @"closeStatus"
{-# INLINEABLE weiCloseStatus #-}
{-# DEPRECATED closeStatus "Use generic-lens or generic-optics with 'closeStatus' instead"  #-}

-- | The time when the workflow execution was closed. Set only if the execution status is CLOSED.
--
-- /Note:/ Consider using 'closeTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiCloseTimestamp :: Lens.Lens' WorkflowExecutionInfo (Core.Maybe Core.NominalDiffTime)
weiCloseTimestamp = Lens.field @"closeTimestamp"
{-# INLINEABLE weiCloseTimestamp #-}
{-# DEPRECATED closeTimestamp "Use generic-lens or generic-optics with 'closeTimestamp' instead"  #-}

-- | If this workflow execution is a child of another execution then contains the workflow execution that started this execution.
--
-- /Note:/ Consider using 'parent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiParent :: Lens.Lens' WorkflowExecutionInfo (Core.Maybe Types.WorkflowExecution)
weiParent = Lens.field @"parent"
{-# INLINEABLE weiParent #-}
{-# DEPRECATED parent "Use generic-lens or generic-optics with 'parent' instead"  #-}

-- | The list of tags associated with the workflow execution. Tags can be used to identify and list workflow executions of interest through the visibility APIs. A workflow execution can have a maximum of 5 tags.
--
-- /Note:/ Consider using 'tagList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiTagList :: Lens.Lens' WorkflowExecutionInfo (Core.Maybe [Types.Tag])
weiTagList = Lens.field @"tagList"
{-# INLINEABLE weiTagList #-}
{-# DEPRECATED tagList "Use generic-lens or generic-optics with 'tagList' instead"  #-}

instance Core.FromJSON WorkflowExecutionInfo where
        parseJSON
          = Core.withObject "WorkflowExecutionInfo" Core.$
              \ x ->
                WorkflowExecutionInfo' Core.<$>
                  (x Core..: "execution") Core.<*> x Core..: "workflowType" Core.<*>
                    x Core..: "startTimestamp"
                    Core.<*> x Core..: "executionStatus"
                    Core.<*> x Core..:? "cancelRequested"
                    Core.<*> x Core..:? "closeStatus"
                    Core.<*> x Core..:? "closeTimestamp"
                    Core.<*> x Core..:? "parent"
                    Core.<*> x Core..:? "tagList"
