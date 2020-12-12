{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  ( RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (..),

    -- * Smart constructor
    mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes,

    -- * Lenses
    rceweieaControl,
    rceweieaRunId,
    rceweieaWorkflowId,
    rceweieaDecisionTaskCompletedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides the details of the @RequestCancelExternalWorkflowExecutionInitiated@ event.
--
-- /See:/ 'mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes' smart constructor.
data RequestCancelExternalWorkflowExecutionInitiatedEventAttributes = RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
  { control ::
      Lude.Maybe
        Lude.Text,
    runId ::
      Lude.Maybe
        Lude.Text,
    workflowId ::
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'RequestCancelExternalWorkflowExecutionInitiatedEventAttributes' with the minimum fields required to make a request.
--
-- * 'control' - Data attached to the event that can be used by the decider in subsequent workflow tasks.
-- * 'decisionTaskCompletedEventId' - The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelExternalWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'runId' - The @runId@ of the external workflow execution to be canceled.
-- * 'workflowId' - The @workflowId@ of the external workflow execution to be canceled.
mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes ::
  -- | 'workflowId'
  Lude.Text ->
  -- | 'decisionTaskCompletedEventId'
  Lude.Integer ->
  RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
mkRequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  pWorkflowId_
  pDecisionTaskCompletedEventId_ =
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
      { control =
          Lude.Nothing,
        runId = Lude.Nothing,
        workflowId = pWorkflowId_,
        decisionTaskCompletedEventId =
          pDecisionTaskCompletedEventId_
      }

-- | Data attached to the event that can be used by the decider in subsequent workflow tasks.
--
-- /Note:/ Consider using 'control' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaControl :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
rceweieaControl = Lens.lens (control :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {control = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED rceweieaControl "Use generic-lens or generic-optics with 'control' instead." #-}

-- | The @runId@ of the external workflow execution to be canceled.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaRunId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes (Lude.Maybe Lude.Text)
rceweieaRunId = Lens.lens (runId :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {runId = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED rceweieaRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

-- | The @workflowId@ of the external workflow execution to be canceled.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaWorkflowId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Lude.Text
rceweieaWorkflowId = Lens.lens (workflowId :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Text) (\s a -> s {workflowId = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED rceweieaWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | The ID of the @DecisionTaskCompleted@ event corresponding to the decision task that resulted in the @RequestCancelExternalWorkflowExecution@ decision for this cancellation request. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'decisionTaskCompletedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rceweieaDecisionTaskCompletedEventId :: Lens.Lens' RequestCancelExternalWorkflowExecutionInitiatedEventAttributes Lude.Integer
rceweieaDecisionTaskCompletedEventId = Lens.lens (decisionTaskCompletedEventId :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes -> Lude.Integer) (\s a -> s {decisionTaskCompletedEventId = a} :: RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
{-# DEPRECATED rceweieaDecisionTaskCompletedEventId "Use generic-lens or generic-optics with 'decisionTaskCompletedEventId' instead." #-}

instance
  Lude.FromJSON
    RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
  where
  parseJSON =
    Lude.withObject
      "RequestCancelExternalWorkflowExecutionInitiatedEventAttributes"
      ( \x ->
          RequestCancelExternalWorkflowExecutionInitiatedEventAttributes'
            Lude.<$> (x Lude..:? "control") Lude.<*> (x Lude..:? "runId")
              Lude.<*> (x Lude..: "workflowId")
              Lude.<*> (x Lude..: "decisionTaskCompletedEventId")
      )
