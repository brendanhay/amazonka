-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
  ( WorkflowExecutionCancelRequestedEventAttributes (..),

    -- * Smart constructor
    mkWorkflowExecutionCancelRequestedEventAttributes,

    -- * Lenses
    wecreaExternalWorkflowExecution,
    wecreaExternalInitiatedEventId,
    wecreaCause,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowExecutionCancelRequestedCause

-- | Provides the details of the @WorkflowExecutionCancelRequested@ event.
--
-- /See:/ 'mkWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data WorkflowExecutionCancelRequestedEventAttributes = WorkflowExecutionCancelRequestedEventAttributes'
  { externalWorkflowExecution ::
      Lude.Maybe
        WorkflowExecution,
    externalInitiatedEventId ::
      Lude.Maybe
        Lude.Integer,
    cause ::
      Lude.Maybe
        WorkflowExecutionCancelRequestedCause
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

-- | Creates a value of 'WorkflowExecutionCancelRequestedEventAttributes' with the minimum fields required to make a request.
--
-- * 'cause' - If set, indicates that the request to cancel the workflow execution was automatically generated, and specifies the cause. This happens if the parent workflow execution times out or is terminated, and the child policy is set to cancel child executions.
-- * 'externalInitiatedEventId' - The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'externalWorkflowExecution' - The external workflow execution for which the cancellation was requested.
mkWorkflowExecutionCancelRequestedEventAttributes ::
  WorkflowExecutionCancelRequestedEventAttributes
mkWorkflowExecutionCancelRequestedEventAttributes =
  WorkflowExecutionCancelRequestedEventAttributes'
    { externalWorkflowExecution =
        Lude.Nothing,
      externalInitiatedEventId = Lude.Nothing,
      cause = Lude.Nothing
    }

-- | The external workflow execution for which the cancellation was requested.
--
-- /Note:/ Consider using 'externalWorkflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecreaExternalWorkflowExecution :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Lude.Maybe WorkflowExecution)
wecreaExternalWorkflowExecution = Lens.lens (externalWorkflowExecution :: WorkflowExecutionCancelRequestedEventAttributes -> Lude.Maybe WorkflowExecution) (\s a -> s {externalWorkflowExecution = a} :: WorkflowExecutionCancelRequestedEventAttributes)
{-# DEPRECATED wecreaExternalWorkflowExecution "Use generic-lens or generic-optics with 'externalWorkflowExecution' instead." #-}

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this workflow execution.The source event with this ID can be found in the history of the source workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'externalInitiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecreaExternalInitiatedEventId :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Lude.Maybe Lude.Integer)
wecreaExternalInitiatedEventId = Lens.lens (externalInitiatedEventId :: WorkflowExecutionCancelRequestedEventAttributes -> Lude.Maybe Lude.Integer) (\s a -> s {externalInitiatedEventId = a} :: WorkflowExecutionCancelRequestedEventAttributes)
{-# DEPRECATED wecreaExternalInitiatedEventId "Use generic-lens or generic-optics with 'externalInitiatedEventId' instead." #-}

-- | If set, indicates that the request to cancel the workflow execution was automatically generated, and specifies the cause. This happens if the parent workflow execution times out or is terminated, and the child policy is set to cancel child executions.
--
-- /Note:/ Consider using 'cause' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecreaCause :: Lens.Lens' WorkflowExecutionCancelRequestedEventAttributes (Lude.Maybe WorkflowExecutionCancelRequestedCause)
wecreaCause = Lens.lens (cause :: WorkflowExecutionCancelRequestedEventAttributes -> Lude.Maybe WorkflowExecutionCancelRequestedCause) (\s a -> s {cause = a} :: WorkflowExecutionCancelRequestedEventAttributes)
{-# DEPRECATED wecreaCause "Use generic-lens or generic-optics with 'cause' instead." #-}

instance
  Lude.FromJSON
    WorkflowExecutionCancelRequestedEventAttributes
  where
  parseJSON =
    Lude.withObject
      "WorkflowExecutionCancelRequestedEventAttributes"
      ( \x ->
          WorkflowExecutionCancelRequestedEventAttributes'
            Lude.<$> (x Lude..:? "externalWorkflowExecution")
            Lude.<*> (x Lude..:? "externalInitiatedEventId")
            Lude.<*> (x Lude..:? "cause")
      )
