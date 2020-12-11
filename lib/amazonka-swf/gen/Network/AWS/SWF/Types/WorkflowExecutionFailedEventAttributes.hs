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
    wefeaReason,
    wefeaDetails,
    wefeaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @WorkflowExecutionFailed@ event.
--
-- /See:/ 'mkWorkflowExecutionFailedEventAttributes' smart constructor.
data WorkflowExecutionFailedEventAttributes = WorkflowExecutionFailedEventAttributes'
  { reason ::
      Lude.Maybe
        Lude.Text,
    details ::
      Lude.Maybe
        Lude.Text,
    decisionTaskCompletedEventId ::
      Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @FailWorkflowExecution@ decision to fail this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'details' - The details of the failure.
-- * 'reason' - The descriptive reason provided for the failure.
mkWorkflowExecutionFailedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  WorkflowExecutionFailedEventAttributes
mkWorkflowExecutionFailedEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionFailedEventAttributes'
      { reason = Lude.Nothing,
        details = Lude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The descriptive reason provided for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefeaReason :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Lude.Maybe Lude.Text)
wefeaReason = Lens.lens (reason :: WorkflowExecutionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: WorkflowExecutionFailedEventAttributes)
{-# DEPRECATED wefeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefeaDetails :: Lens.Lens' WorkflowExecutionFailedEventAttributes (Lude.Maybe Lude.Text)
wefeaDetails = Lens.lens (details :: WorkflowExecutionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: WorkflowExecutionFailedEventAttributes)
{-# DEPRECATED wefeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @FailWorkflowExecution@ decision to fail this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefeaDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionFailedEventAttributes Lude.Integer
wefeaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: WorkflowExecutionFailedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionFailedEventAttributes)
{-# DEPRECATED wefeaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON WorkflowExecutionFailedEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionFailedEventAttributes"
      ( \x ->
          WorkflowExecutionFailedEventAttributes'
            Lude.<$> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "details")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
