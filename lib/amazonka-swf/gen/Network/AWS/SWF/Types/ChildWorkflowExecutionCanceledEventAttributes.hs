{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
  ( ChildWorkflowExecutionCanceledEventAttributes (..),

    -- * Smart constructor
    mkChildWorkflowExecutionCanceledEventAttributes,

    -- * Lenses
    cweceaWorkflowExecution,
    cweceaWorkflowType,
    cweceaInitiatedEventId,
    cweceaStartedEventId,
    cweceaDetails,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Data as Types
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provide details of the @ChildWorkflowExecutionCanceled@ event.
--
-- /See:/ 'mkChildWorkflowExecutionCanceledEventAttributes' smart constructor.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes'
  { -- | The child workflow execution that was canceled.
    workflowExecution :: Types.WorkflowExecution,
    -- | The type of the child workflow execution.
    workflowType :: Types.WorkflowType,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Core.Integer,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer,
    -- | Details of the cancellation (if provided).
    details :: Core.Maybe Types.Data
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildWorkflowExecutionCanceledEventAttributes' value with any optional fields omitted.
mkChildWorkflowExecutionCanceledEventAttributes ::
  -- | 'workflowExecution'
  Types.WorkflowExecution ->
  -- | 'workflowType'
  Types.WorkflowType ->
  -- | 'initiatedEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  ChildWorkflowExecutionCanceledEventAttributes
mkChildWorkflowExecutionCanceledEventAttributes
  workflowExecution
  workflowType
  initiatedEventId
  startedEventId =
    ChildWorkflowExecutionCanceledEventAttributes'
      { workflowExecution,
        workflowType,
        initiatedEventId,
        startedEventId,
        details = Core.Nothing
      }

-- | The child workflow execution that was canceled.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Types.WorkflowExecution
cweceaWorkflowExecution = Lens.field @"workflowExecution"
{-# DEPRECATED cweceaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaWorkflowType :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Types.WorkflowType
cweceaWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED cweceaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Core.Integer
cweceaInitiatedEventId = Lens.field @"initiatedEventId"
{-# DEPRECATED cweceaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaStartedEventId :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Core.Integer
cweceaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED cweceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | Details of the cancellation (if provided).
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaDetails :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes (Core.Maybe Types.Data)
cweceaDetails = Lens.field @"details"
{-# DEPRECATED cweceaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

instance
  Core.FromJSON
    ChildWorkflowExecutionCanceledEventAttributes
  where
  parseJSON =
    Core.withObject "ChildWorkflowExecutionCanceledEventAttributes" Core.$
      \x ->
        ChildWorkflowExecutionCanceledEventAttributes'
          Core.<$> (x Core..: "workflowExecution")
          Core.<*> (x Core..: "workflowType")
          Core.<*> (x Core..: "initiatedEventId")
          Core.<*> (x Core..: "startedEventId")
          Core.<*> (x Core..:? "details")
