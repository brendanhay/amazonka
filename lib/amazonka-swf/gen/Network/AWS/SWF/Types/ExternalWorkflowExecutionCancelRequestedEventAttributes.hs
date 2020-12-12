{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
  ( ExternalWorkflowExecutionCancelRequestedEventAttributes (..),

    -- * Smart constructor
    mkExternalWorkflowExecutionCancelRequestedEventAttributes,

    -- * Lenses
    ewecreaWorkflowExecution,
    ewecreaInitiatedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @ExternalWorkflowExecutionCancelRequested@ event.
--
-- /See:/ 'mkExternalWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes'
  { workflowExecution ::
      WorkflowExecution,
    initiatedEventId ::
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

-- | Creates a value of 'ExternalWorkflowExecutionCancelRequestedEventAttributes' with the minimum fields required to make a request.
--
-- * 'initiatedEventId' - The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this external workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The external workflow execution to which the cancellation request was delivered.
mkExternalWorkflowExecutionCancelRequestedEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  ExternalWorkflowExecutionCancelRequestedEventAttributes
mkExternalWorkflowExecutionCancelRequestedEventAttributes
  pWorkflowExecution_
  pInitiatedEventId_ =
    ExternalWorkflowExecutionCancelRequestedEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        initiatedEventId = pInitiatedEventId_
      }

-- | The external workflow execution to which the cancellation request was delivered.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ewecreaWorkflowExecution :: Lens.Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes WorkflowExecution
ewecreaWorkflowExecution = Lens.lens (workflowExecution :: ExternalWorkflowExecutionCancelRequestedEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ExternalWorkflowExecutionCancelRequestedEventAttributes)
{-# DEPRECATED ewecreaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this external workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ewecreaInitiatedEventId :: Lens.Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Lude.Integer
ewecreaInitiatedEventId = Lens.lens (initiatedEventId :: ExternalWorkflowExecutionCancelRequestedEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ExternalWorkflowExecutionCancelRequestedEventAttributes)
{-# DEPRECATED ewecreaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

instance
  Lude.FromJSON
    ExternalWorkflowExecutionCancelRequestedEventAttributes
  where
  parseJSON =
    Lude.withObject
      "ExternalWorkflowExecutionCancelRequestedEventAttributes"
      ( \x ->
          ExternalWorkflowExecutionCancelRequestedEventAttributes'
            Lude.<$> (x Lude..: "workflowExecution")
            Lude.<*> (x Lude..: "initiatedEventId")
      )
