{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCompletedEventAttributes
  ( WorkflowExecutionCompletedEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionCompletedEventAttributes,

    -- * Lenses
    weceaResult,
    weceaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @WorkflowExecutionCompleted@ event.
--
-- /See:/ 'mkWorkflowExecutionCompletedEventAttributes' smart constructor.
data WorkflowExecutionCompletedEventAttributes = WorkflowExecutionCompletedEventAttributes'
  { result ::
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

-- | Creates a value of 'WorkflowExecutionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'result' - The result produced by the workflow execution upon successful completion.
mkWorkflowExecutionCompletedEventAttributes ::
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  WorkflowExecutionCompletedEventAttributes
mkWorkflowExecutionCompletedEventAttributes
  pDecisionTaskCompletedEventId_ =
    WorkflowExecutionCompletedEventAttributes'
      { result = Lude.Nothing,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | The result produced by the workflow execution upon successful completion.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weceaResult :: Lens.Lens' WorkflowExecutionCompletedEventAttributes (Lude.Maybe Lude.Text)
weceaResult = Lens.lens (result :: WorkflowExecutionCompletedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: WorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED weceaResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @CompleteWorkflowExecution@ decision to complete this execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weceaDecisionTaskCompletedEventId :: Lens.Lens' WorkflowExecutionCompletedEventAttributes Lude.Integer
weceaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: WorkflowExecutionCompletedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: WorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED weceaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance Lude.FromJSON WorkflowExecutionCompletedEventAttributes where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionCompletedEventAttributes"
      ( \x ->
          WorkflowExecutionCompletedEventAttributes'
            Lude.<$> (x Lude..:? "result")
            Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
