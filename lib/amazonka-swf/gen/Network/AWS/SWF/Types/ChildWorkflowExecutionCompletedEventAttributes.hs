{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
  ( ChildWorkflowExecutionCompletedEventAttributes (..)
  -- * Smart constructor
  , mkChildWorkflowExecutionCompletedEventAttributes
  -- * Lenses
  , cWorkflowExecution
  , cWorkflowType
  , cInitiatedEventId
  , cStartedEventId
  , cResult
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Result as Types
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @ChildWorkflowExecutionCompleted@ event.
--
-- /See:/ 'mkChildWorkflowExecutionCompletedEventAttributes' smart constructor.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes'
  { workflowExecution :: Types.WorkflowExecution
    -- ^ The child workflow execution that was completed.
  , workflowType :: Types.WorkflowType
    -- ^ The type of the child workflow execution.
  , initiatedEventId :: Core.Integer
    -- ^ The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , result :: Core.Maybe Types.Result
    -- ^ The result of the child workflow execution.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildWorkflowExecutionCompletedEventAttributes' value with any optional fields omitted.
mkChildWorkflowExecutionCompletedEventAttributes
    :: Types.WorkflowExecution -- ^ 'workflowExecution'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> Core.Integer -- ^ 'initiatedEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> ChildWorkflowExecutionCompletedEventAttributes
mkChildWorkflowExecutionCompletedEventAttributes workflowExecution
  workflowType initiatedEventId startedEventId
  = ChildWorkflowExecutionCompletedEventAttributes'{workflowExecution,
                                                    workflowType, initiatedEventId, startedEventId,
                                                    result = Core.Nothing}

-- | The child workflow execution that was completed.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Types.WorkflowExecution
cWorkflowExecution = Lens.field @"workflowExecution"
{-# INLINEABLE cWorkflowExecution #-}
{-# DEPRECATED workflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead"  #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWorkflowType :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Types.WorkflowType
cWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE cWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Core.Integer
cInitiatedEventId = Lens.field @"initiatedEventId"
{-# INLINEABLE cInitiatedEventId #-}
{-# DEPRECATED initiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead"  #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStartedEventId :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Core.Integer
cStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE cStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

-- | The result of the child workflow execution.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cResult :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes (Core.Maybe Types.Result)
cResult = Lens.field @"result"
{-# INLINEABLE cResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

instance Core.FromJSON
           ChildWorkflowExecutionCompletedEventAttributes
         where
        parseJSON
          = Core.withObject "ChildWorkflowExecutionCompletedEventAttributes"
              Core.$
              \ x ->
                ChildWorkflowExecutionCompletedEventAttributes' Core.<$>
                  (x Core..: "workflowExecution") Core.<*> x Core..: "workflowType"
                    Core.<*> x Core..: "initiatedEventId"
                    Core.<*> x Core..: "startedEventId"
                    Core.<*> x Core..:? "result"
