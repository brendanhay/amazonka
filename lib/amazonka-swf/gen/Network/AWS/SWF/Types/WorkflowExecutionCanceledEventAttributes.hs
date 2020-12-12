{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCanceledEventAttributes
  ( WorkflowExecutionCanceledEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionCanceledEventAttributes,

    -- * Lenses
    wDetails,
    wDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @WorkflowExecutionCanceled@ event.
--
-- /See:/ 'mkWorkflowExecutionCanceledEventAttributes' smart constructor.
data WorkflowExecutionCanceledEventAttributes = WorkflowExecutionCanceledEventAttributes'
  { details ::
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

-- | Creates a value of 'WorkflowExecutionCanceledEventAttributes' with the minimum fields required to make a request.
--
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'details' - The details of the cancellation.
mkWorkflowExecutionCanceledEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  WorkflowExecutionCanceledEventAttributes
mkWorkflowExecutionCanceledEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCanceledEventAttributes'
      { details = Lude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The details of the cancellation.
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDetails :: Lens.Lens' WorkflowExecutionCanceledEventAttributes (Lude.Maybe Lude.Text)
wDetails = Lens.lens (details :: WorkflowExecutionCanceledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: WorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED wDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CancelWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCanceledEventAttributes Lude.Integer
wDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: WorkflowExecutionCanceledEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED wDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON WorkflowExecutionCanceledEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionCanceledEventAttributes"
      ( \x ->
          WorkflowExecutionCanceledEventAttributes'
            Lude.<$> (x Lude..:? "details")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
