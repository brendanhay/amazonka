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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types

-- | Provides the details of the @ExternalWorkflowExecutionSignaled@ event.
--
-- /See:/ 'mkExternalWorkflowExecutionSignaledEventAttributes' smart constructor.
data ExternalWorkflowExecutionSignaledEventAttributes = ExternalWorkflowExecutionSignaledEventAttributes'
  { -- | The external workflow execution that the signal was delivered to.
    workflowExecution :: Types.WorkflowExecution,
    -- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflowExecution@ decision to request this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExternalWorkflowExecutionSignaledEventAttributes' value with any optional fields omitted.
mkExternalWorkflowExecutionSignaledEventAttributes ::
  -- | 'workflowExecution'
  Types.WorkflowExecution ->
  -- | 'initiatedEventId'
  Core.Integer ->
  ExternalWorkflowExecutionSignaledEventAttributes
mkExternalWorkflowExecutionSignaledEventAttributes
  workflowExecution
  initiatedEventId =
    ExternalWorkflowExecutionSignaledEventAttributes'
      { workflowExecution,
        initiatedEventId
      }

-- | The external workflow execution that the signal was delivered to.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eweseaWorkflowExecution :: Lens.Lens' ExternalWorkflowExecutionSignaledEventAttributes Types.WorkflowExecution
eweseaWorkflowExecution = Lens.field @"workflowExecution"
{-# DEPRECATED eweseaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The ID of the @SignalExternalWorkflowExecutionInitiated@ event corresponding to the @SignalExternalWorkflowExecution@ decision to request this signal. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eweseaInitiatedEventId :: Lens.Lens' ExternalWorkflowExecutionSignaledEventAttributes Core.Integer
eweseaInitiatedEventId = Lens.field @"initiatedEventId"
{-# DEPRECATED eweseaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

instance
  Core.FromJSON
    ExternalWorkflowExecutionSignaledEventAttributes
  where
  parseJSON =
    Core.withObject
      "ExternalWorkflowExecutionSignaledEventAttributes"
      Core.$ \x ->
        ExternalWorkflowExecutionSignaledEventAttributes'
          Core.<$> (x Core..: "workflowExecution")
          Core.<*> (x Core..: "initiatedEventId")
