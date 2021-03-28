{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
  ( ChildWorkflowExecutionFailedEventAttributes (..)
  -- * Smart constructor
  , mkChildWorkflowExecutionFailedEventAttributes
  -- * Lenses
  , cwefeaWorkflowExecution
  , cwefeaWorkflowType
  , cwefeaInitiatedEventId
  , cwefeaStartedEventId
  , cwefeaDetails
  , cwefeaReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.FailureReason as Types
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @ChildWorkflowExecutionFailed@ event.
--
-- /See:/ 'mkChildWorkflowExecutionFailedEventAttributes' smart constructor.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes'
  { workflowExecution :: Types.WorkflowExecution
    -- ^ The child workflow execution that failed.
  , workflowType :: Types.WorkflowType
    -- ^ The type of the child workflow execution.
  , initiatedEventId :: Core.Integer
    -- ^ The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , startedEventId :: Core.Integer
    -- ^ The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , details :: Core.Maybe Types.Data
    -- ^ The details of the failure (if provided).
  , reason :: Core.Maybe Types.FailureReason
    -- ^ The reason for the failure (if provided).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildWorkflowExecutionFailedEventAttributes' value with any optional fields omitted.
mkChildWorkflowExecutionFailedEventAttributes
    :: Types.WorkflowExecution -- ^ 'workflowExecution'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> Core.Integer -- ^ 'initiatedEventId'
    -> Core.Integer -- ^ 'startedEventId'
    -> ChildWorkflowExecutionFailedEventAttributes
mkChildWorkflowExecutionFailedEventAttributes workflowExecution
  workflowType initiatedEventId startedEventId
  = ChildWorkflowExecutionFailedEventAttributes'{workflowExecution,
                                                 workflowType, initiatedEventId, startedEventId,
                                                 details = Core.Nothing, reason = Core.Nothing}

-- | The child workflow execution that failed.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Types.WorkflowExecution
cwefeaWorkflowExecution = Lens.field @"workflowExecution"
{-# INLINEABLE cwefeaWorkflowExecution #-}
{-# DEPRECATED workflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead"  #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaWorkflowType :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Types.WorkflowType
cwefeaWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE cwefeaWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Core.Integer
cwefeaInitiatedEventId = Lens.field @"initiatedEventId"
{-# INLINEABLE cwefeaInitiatedEventId #-}
{-# DEPRECATED initiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead"  #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaStartedEventId :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Core.Integer
cwefeaStartedEventId = Lens.field @"startedEventId"
{-# INLINEABLE cwefeaStartedEventId #-}
{-# DEPRECATED startedEventId "Use generic-lens or generic-optics with 'startedEventId' instead"  #-}

-- | The details of the failure (if provided).
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaDetails :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes (Core.Maybe Types.Data)
cwefeaDetails = Lens.field @"details"
{-# INLINEABLE cwefeaDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

-- | The reason for the failure (if provided).
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaReason :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes (Core.Maybe Types.FailureReason)
cwefeaReason = Lens.field @"reason"
{-# INLINEABLE cwefeaReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.FromJSON ChildWorkflowExecutionFailedEventAttributes
         where
        parseJSON
          = Core.withObject "ChildWorkflowExecutionFailedEventAttributes"
              Core.$
              \ x ->
                ChildWorkflowExecutionFailedEventAttributes' Core.<$>
                  (x Core..: "workflowExecution") Core.<*> x Core..: "workflowType"
                    Core.<*> x Core..: "initiatedEventId"
                    Core.<*> x Core..: "startedEventId"
                    Core.<*> x Core..:? "details"
                    Core.<*> x Core..:? "reason"
