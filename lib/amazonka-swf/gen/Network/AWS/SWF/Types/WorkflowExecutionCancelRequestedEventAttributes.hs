{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
  ( WorkflowExecutionCancelRequestedEventAttributes (..)
  -- * Smart constructor
  , mkWorkflowExecutionCancelRequestedEventAttributes
  -- * Lenses
  , wecreaCause
  , wecreaExternalInitiatedEventId
  , wecreaExternalWorkflowExecution
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause as Types

-- | Provides the details of the @WorkflowExecutionCancelRequested@ event.
--
-- /See:/ 'mkWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes'
  { cause :: Core.Maybe Types.WorkflowExecutionCancelRequestedCause
    -- ^ If set, indicates that the request to cancel the workflow execution was automatically generated, and specifies the cause. This happens if the parent workflow execution times out or is terminated, and the child policy is set to cancel child executions.
  , externalInitiatedEventId :: Core.Maybe Core.Integer
    -- ^ The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , externalWorkflowExecution :: Core.Maybe Types.WorkflowExecution
    -- ^ The external workflow execution for which the cancellation was requested.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionCancelRequestedEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionCancelRequestedEventAttributes
    :: WorkflowExecutionCancelRequestedEventAttributes
mkWorkflowExecutionCancelRequestedEventAttributes
  = WorkflowExecutionCancelRequestedEventAttributes'{cause =
                                                       Core.Nothing,
                                                     externalInitiatedEventId = Core.Nothing,
                                                     externalWorkflowExecution = Core.Nothing}

-- | If set, indicates that the request to cancel the workflow execution was automatically generated, and specifies the cause. This happens if the parent workflow execution times out or is terminated, and the child policy is set to cancel child executions.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecreaCause :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Core.Maybe Types.WorkflowExecutionCancelRequestedCause)
wecreaCause = Lens.field @"cause"
{-# INLINEABLE wecreaCause #-}
{-# DEPRECATED cause "Use generic-lens or generic-optics with 'cause' instead"  #-}

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'externalInitiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecreaExternalInitiatedEventId :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Core.Maybe Core.Integer)
wecreaExternalInitiatedEventId = Lens.field @"externalInitiatedEventId"
{-# INLINEABLE wecreaExternalInitiatedEventId #-}
{-# DEPRECATED externalInitiatedEventId "Use generic-lens or generic-optics with 'externalInitiatedEventId' instead"  #-}

-- | The external workflow execution for which the cancellation was requested.
--
-- /Note:/ Consider using 'externalWorkflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecreaExternalWorkflowExecution :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Core.Maybe Types.WorkflowExecution)
wecreaExternalWorkflowExecution = Lens.field @"externalWorkflowExecution"
{-# INLINEABLE wecreaExternalWorkflowExecution #-}
{-# DEPRECATED externalWorkflowExecution "Use generic-lens or generic-optics with 'externalWorkflowExecution' instead"  #-}

instance Core.FromJSON
           WorkflowExecutionCancelRequestedEventAttributes
         where
        parseJSON
          = Core.withObject "WorkflowExecutionCancelRequestedEventAttributes"
              Core.$
              \ x ->
                WorkflowExecutionCancelRequestedEventAttributes' Core.<$>
                  (x Core..:? "cause") Core.<*> x Core..:? "externalInitiatedEventId"
                    Core.<*> x Core..:? "externalWorkflowExecution"
