{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
  ( WorkflowExecutionCompletedEventAttributes (..)
  -- * Smart constructor
  , mkWorkflowExecutionCompletedEventAttributes
  -- * Lenses
  , wDecisionTaskCompletedEventId
  , wResult
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Result as Types

-- | Provides the details of the @WorkflowExecutionCompleted@ event.
--
-- /See:/ 'mkWorkflowExecutionCompletedEventAttributes' smart constructor.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes'
  { decisionTaskCompletedEventId :: Core.Integer
    -- ^ The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  , result :: Core.Maybe Types.Result
    -- ^ The result produced by the workflow execution upon successful completion.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionCompletedEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionCompletedEventAttributes
    :: Core.Integer -- ^ 'decisionTaskCompletedEventId'
    -> WorkflowExecutionCompletedEventAttributes
mkWorkflowExecutionCompletedEventAttributes
  decisionTaskCompletedEventId
  = WorkflowExecutionCompletedEventAttributes'{decisionTaskCompletedEventId,
                                               result = Core.Nothing}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCompletedEventAttributes Core.Integer
wDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# INLINEABLE wDecisionTaskCompletedEventId #-}
{-# DEPRECATED decisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead"  #-}

-- | The result produced by the workflow execution upon successful completion.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wResult :: Lens.Lens' WorkflowExecutionCompletedEventAttributes (Core.Maybe Types.Result)
wResult = Lens.field @"result"
{-# INLINEABLE wResult #-}
{-# DEPRECATED result "Use generic-lens or generic-optics with 'result' instead"  #-}

instance Core.FromJSON WorkflowExecutionCompletedEventAttributes
         where
        parseJSON
          = Core.withObject "WorkflowExecutionCompletedEventAttributes"
              Core.$
              \ x ->
                WorkflowExecutionCompletedEventAttributes' Core.<$>
                  (x Core..: "decisionTaskCompletedEventId") Core.<*>
                    x Core..:? "result"
