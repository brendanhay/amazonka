{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
  ( ExternalWorkflowExecutionSignaledEventAttributes (..),

    -- * Smart constructor
    mkExternalWorkflowExecutionSignaledEventAttributes,

    -- * Lenses
    eweseaWorkflowExecution,
    eweseaInitiatedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution

-- | Provides the details of the @ExternalWorkflowExecutionSignaled@ event.
--
-- /See:/ 'mkExternalWorkflowExecutionSignaledEventAttributes' smart constructor.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes'
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

-- | Creates a value of 'ExternalWorkflowExecutionSignaledEventAttributes' with the minimum fields required to make a request.
--
-- * 'initiatedEventId' - The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflowExecution@ decision to request this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The external workflow execution that the signal was delivered to.
mkExternalWorkflowExecutionSignaledEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  ExternalWorkflowExecutionSignaledEventAttributes
mkExternalWorkflowExecutionSignaledEventAttributes
  pWorkflowExecution_
  pInitiatedEventId_ =
    ExternalWorkflowExecutionSignaledEventAttributes'
      { workflowExecution =
          pWorkflowExecution_,
        initiatedEventId = pInitiatedEventId_
      }

-- | The external workflow execution that the signal was delivered to.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eweseaWorkflowExecution :: Lens.Lens' ExternalWorkflowExecutionSignaledEventAttributes WorkflowExecution
eweseaWorkflowExecution = Lens.lens (workflowExecution :: ExternalWorkflowExecutionSignaledEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ExternalWorkflowExecutionSignaledEventAttributes)
{-# DEPRECATED eweseaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflowExecution@ decision to request this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eweseaInitiatedEventId :: Lens.Lens' ExternalWorkflowExecutionSignaledEventAttributes Lude.Integer
eweseaInitiatedEventId = Lens.lens (initiatedEventId :: ExternalWorkflowExecutionSignaledEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ExternalWorkflowExecutionSignaledEventAttributes)
{-# DEPRECATED eweseaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

instance
  Lude.FromJSON
    ExternalWorkflowExecutionSignaledEventAttributes
  where
  parseJSON =
    Lude.withObject
      "ExternalWorkflowExecutionSignaledEventAttributes"
      ( \x ->
          ExternalWorkflowExecutionSignaledEventAttributes'
            Lude.<$> (x Lude..: "workflowExecution")
            Lude.<*> (x Lude..: "initiatedEventId")
      )
