{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
  ( ExternalWorkflowExecutionCancelRequestedEventAttributes (..)
  -- * Smart constructor
  , mkExternalWorkflowExecutionCancelRequestedEventAttributes
  -- * Lenses
  , ewecreaWorkflowExecution
  , ewecreaInitiatedEventId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types

-- | Provides the details of the @ExternalWorkflowExecutionCancelRequested@ event.
--
-- /See:/ 'mkExternalWorkflowExecutionCancelRequestedEventAttributes' smart constructor.
data ExternalWorkflowExecutionCancelRequestedEventAttributes = ExternalWorkflowExecutionCancelRequestedEventAttributes'
  { workflowExecution :: Types.WorkflowExecution
    -- ^ The external workflow execution to which the cancellation request was delivered.
  , initiatedEventId :: Core.Integer
    -- ^ The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this external workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExternalWorkflowExecutionCancelRequestedEventAttributes' value with any optional fields omitted.
mkExternalWorkflowExecutionCancelRequestedEventAttributes
    :: Types.WorkflowExecution -- ^ 'workflowExecution'
    -> Core.Integer -- ^ 'initiatedEventId'
    -> ExternalWorkflowExecutionCancelRequestedEventAttributes
mkExternalWorkflowExecutionCancelRequestedEventAttributes
  workflowExecution initiatedEventId
  = ExternalWorkflowExecutionCancelRequestedEventAttributes'{workflowExecution,
                                                             initiatedEventId}

-- | The external workflow execution to which the cancellation request was delivered.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ewecreaWorkflowExecution :: Lens.Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Types.WorkflowExecution
ewecreaWorkflowExecution = Lens.field @"workflowExecution"
{-# INLINEABLE ewecreaWorkflowExecution #-}
{-# DEPRECATED workflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead"  #-}

-- | The ID of the @RequestCancelExternalWorkflowExecutionInitiated@ event corresponding to the @RequestCancelExternalWorkflowExecution@ decision to cancel this external workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ewecreaInitiatedEventId :: Lens.Lens' ExternalWorkflowExecutionCancelRequestedEventAttributes Core.Integer
ewecreaInitiatedEventId = Lens.field @"initiatedEventId"
{-# INLINEABLE ewecreaInitiatedEventId #-}
{-# DEPRECATED initiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead"  #-}

instance Core.FromJSON
           ExternalWorkflowExecutionCancelRequestedEventAttributes
         where
        parseJSON
          = Core.withObject
              "ExternalWorkflowExecutionCancelRequestedEventAttributes"
              Core.$
              \ x ->
                ExternalWorkflowExecutionCancelRequestedEventAttributes' Core.<$>
                  (x Core..: "workflowExecution") Core.<*>
                    x Core..: "initiatedEventId"
