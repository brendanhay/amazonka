{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
  ( WorkflowExecutionCanceledEventAttributes (..)
  -- * Smart constructor
  , mkWorkflowExecutionCanceledEventAttributes
  -- * Lenses
  , weceaDecisionTaskCompletedEventId
  , weceaDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types

-- | Provides the details of the @WorkflowExecutionCanceled@ event.
--
-- /See:/ 'mkWorkflowExecutionCanceledEventAttributes' smart constructor.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes'
  { decisionTaskCompletedEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , details :: Core.Maybe Types.Data
    -- ^ The details of the cancellation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionCanceledEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionCanceledEventAttributes
    :: Core.Integer -- ^ 'decisionTaskCompletedEventId'
    -> WorkflowExecutionCanceledEventAttributes
mkWorkflowExecutionCanceledEventAttributes
  decisionTaskCompletedEventId
  = WorkflowExecutionCanceledEventAttributes'{decisionTaskCompletedEventId,
                                              details = Core.Nothing}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weceaDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCanceledEventAttributes Core.Integer
weceaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# INLINEABLE weceaDecisionTaskCompletedEventId #-}
{-# DEPRECATED decisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead"  #-}

-- | The details of the cancellation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weceaDetails :: Lens.Lens' WorkflowExecutionCanceledEventAttributes (Core.Maybe Types.Data)
weceaDetails = Lens.field @"details"
{-# INLINEABLE weceaDetails #-}
{-# DEPRECATED details "Use generic-lens or generic-optics with 'details' instead"  #-}

instance Core.FromJSON WorkflowExecutionCanceledEventAttributes
         where
        parseJSON
          = Core.withObject "WorkflowExecutionCanceledEventAttributes" Core.$
              \ x ->
                WorkflowExecutionCanceledEventAttributes' Core.<$>
                  (x Core..: "decisionTaskCompletedEventId") Core.<*>
                    x Core..:? "details"
