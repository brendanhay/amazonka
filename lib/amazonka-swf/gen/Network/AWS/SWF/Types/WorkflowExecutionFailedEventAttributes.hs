{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFailedEventAttributes
  ( WorkflowExecutionFailedEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionFailedEventAttributes,

    -- * Lenses
    wefeaDecisionTaskCompletedEventId,
    wefeaDetails,
    wefeaReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.FailureReason as Types

-- | Provides the details of the @WorkflowExecutionFailed@ event.
--
-- /See:/ 'mkWorkflowExecutionFailedEventAttributes' smart constructor.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes'
  { -- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @FailWorkflowExecution@ decision to fail this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    decisionTaskCompletedEventId :: Core.Integer,
    -- | The details of the failure.
    details :: Core.Maybe Types.Data,
    -- | The descriptive reason provided for the failure.
    reason :: Core.Maybe Types.FailureReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionFailedEventAttributes' value with any optional fields omitted.
mkWorkflowExecutionFailedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Core.Integer ->
  WorkflowExecutionFailedEventAttributes
mkWorkflowExecutionFailedEventAttributes
  decisionTaskCompletedEventId =
    WorkflowExecutionFailedEventAttributes'
      { decisionTaskCompletedEventId,
        details = Core.Nothing,
        reason = Core.Nothing
      }

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @FailWorkflowExecution@ decision to fail this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefeaDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionFailedEventAttributes Core.Integer
wefeaDecisionTaskCompletedEventId = Lens.field @"decisionTaskCompletedEventId"
{-# DEPRECATED wefeaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefeaDetails :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Core.Maybe Types.Data)
wefeaDetails = Lens.field @"details"
{-# DEPRECATED wefeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The descriptive reason provided for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefeaReason :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Core.Maybe Types.FailureReason)
wefeaReason = Lens.field @"reason"
{-# DEPRECATED wefeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON WorkflowExecutionFailedEventAttributes where
  parseJSON =
    Core.withObject "WorkflowExecutionFailedEventAttributes" Core.$
      \x ->
        WorkflowExecutionFailedEventAttributes'
          Core.<$> (x Core..: "decisionTaskCompletedEventId")
          Core.<*> (x Core..:? "details")
          Core.<*> (x Core..:? "reason")
