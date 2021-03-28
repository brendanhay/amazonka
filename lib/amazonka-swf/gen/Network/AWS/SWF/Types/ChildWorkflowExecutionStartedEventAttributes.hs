{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
  ( ChildWorkflowExecutionStartedEventAttributes (..)
  -- * Smart constructor
  , mkChildWorkflowExecutionStartedEventAttributes
  -- * Lenses
  , cweseaWorkflowExecution
  , cweseaWorkflowType
  , cweseaInitiatedEventId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @ChildWorkflowExecutionStarted@ event.
--
-- /See:/ 'mkChildWorkflowExecutionStartedEventAttributes' smart constructor.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes'
  { workflowExecution :: Types.WorkflowExecution
    -- ^ The child workflow execution that was started.
  , workflowType :: Types.WorkflowType
    -- ^ The type of the child workflow execution.
  , initiatedEventId :: Core.Integer
    -- ^ The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildWorkflowExecutionStartedEventAttributes' value with any optional fields omitted.
mkChildWorkflowExecutionStartedEventAttributes
    :: Types.WorkflowExecution -- ^ 'workflowExecution'
    -> Types.WorkflowType -- ^ 'workflowType'
    -> Core.Integer -- ^ 'initiatedEventId'
    -> ChildWorkflowExecutionStartedEventAttributes
mkChildWorkflowExecutionStartedEventAttributes workflowExecution
  workflowType initiatedEventId
  = ChildWorkflowExecutionStartedEventAttributes'{workflowExecution,
                                                  workflowType, initiatedEventId}

-- | The child workflow execution that was started.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweseaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes Types.WorkflowExecution
cweseaWorkflowExecution = Lens.field @"workflowExecution"
{-# INLINEABLE cweseaWorkflowExecution #-}
{-# DEPRECATED workflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead"  #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweseaWorkflowType :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes Types.WorkflowType
cweseaWorkflowType = Lens.field @"workflowType"
{-# INLINEABLE cweseaWorkflowType #-}
{-# DEPRECATED workflowType "Use generic-lens or generic-optics with 'workflowType' instead"  #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweseaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes Core.Integer
cweseaInitiatedEventId = Lens.field @"initiatedEventId"
{-# INLINEABLE cweseaInitiatedEventId #-}
{-# DEPRECATED initiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead"  #-}

instance Core.FromJSON ChildWorkflowExecutionStartedEventAttributes
         where
        parseJSON
          = Core.withObject "ChildWorkflowExecutionStartedEventAttributes"
              Core.$
              \ x ->
                ChildWorkflowExecutionStartedEventAttributes' Core.<$>
                  (x Core..: "workflowExecution") Core.<*> x Core..: "workflowType"
                    Core.<*> x Core..: "initiatedEventId"
