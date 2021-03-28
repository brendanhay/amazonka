{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
  ( ChildWorkflowExecutionTimedOutEventAttributes (..)
  -- * Smart constructor
  , mkChildWorkflowExecutionTimedOutEventAttributes
  -- * Lenses
  , cwetoeaWorkflowExecution
  , cwetoeaWorkflowType
  , cwetoeaTimeoutType
  , cwetoeaInitiatedEventId
  , cwetoeaStartedEventId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionTimeoutType as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @ChildWorkflowExecutionTimedOut@ event.
--
-- /See:/ 'mkChildWorkflowExecutionTimedOutEventAttributes' smart constructor.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes'
  { workflowExecution :: Types.WorkflowExecution
    -- ^ The child workflow execution that timed out.
  , workflowType :: Types.WorkflowType
    -- ^ The type of the child workflow execution.
  , timeoutType :: Types.WorkflowExecutionTimeoutType
    -- ^ The type of the timeout that caused the child workflow execution to time out.
  , initiatedEventId :: Core.Integer
    -- ^ The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildWorkflowExecutionTimedOutEventAttributes' value with any optional fields omitted.
mkChildWorkflowExecutionTimedOutEventAttributes
    :: Types.WorkflowExecution -- ^ 'workflowExecution'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> Types.WorkflowExecutionTimeoutType -- ^ 'timeoutType'
    -> Core.Integer -- ^ 'initiatedEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> ChildWorkflowExecutionTimedOutEventAttributes
mkChildWorkflowExecutionTimedOutEventAttributes workflowExecution
  workflowType timeoutType initiatedEventId startedEventId
  = ChildWorkflowExecutionTimedOutEventAttributes'{workflowExecution,
                                                   workflowType, timeoutType, initiatedEventId,
                                                   startedEventId}

-- | The child workflow execution that timed out.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Types.WorkflowExecution
cwetoeaWorkflowExecution = Lens.field @"workflowExecution"
{-# INLINEABLE cwetoeaWorkflowExecution #-}
{-# DEPRECATED workflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead"  #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaWorkflowType :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Types.WorkflowType
cwetoeaWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE cwetoeaWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The type of the timeout that caused the child workflow execution to time out.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaTimeoutType :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Types.WorkflowExecutionTimeoutType
cwetoeaTimeoutType = Lens.field @"timeoutType"
{-# INLINEABLE cwetoeaTimeoutType #-}
{-# DEPRECATED timeoutType "Use generic-lens or generic-optics with 'timeoutType' instead"  #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Core.Integer
cwetoeaInitiatedEventId = Lens.field @"initiatedEventId"
{-# INLINEABLE cwetoeaInitiatedEventId #-}
{-# DEPRECATED initiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead"  #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaStartedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Core.Integer
cwetoeaStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE cwetoeaStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

instance Core.FromJSON
           ChildWorkflowExecutionTimedOutEventAttributes
         where
        parseJSON
          = Core.withObject "ChildWorkflowExecutionTimedOutEventAttributes"
              Core.$
              \ x ->
                ChildWorkflowExecutionTimedOutEventAttributes' Core.<$>
                  (x Core..: "workflowExecution") Core.<*> x Core..: "workflowType"
                    Core.<*> x Core..: "timeoutType"
                    Core.<*> x Core..: "initiatedEventId"
                    Core.<*> x Core..: "startedEventId"
