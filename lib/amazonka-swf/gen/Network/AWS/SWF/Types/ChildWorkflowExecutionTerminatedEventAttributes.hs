{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
  ( ChildWorkflowExecutionTerminatedEventAttributes (..),

    -- * Smart constructor
    mkChildWorkflowExecutionTerminatedEventAttributes,

    -- * Lenses
    cweteaWorkflowExecution,
    cweteaWorkflowType,
    cweteaInitiatedEventId,
    cweteaStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowExecution as Types
import qualified Network.AWS.SWF.Types.WorkflowType as Types

-- | Provides the details of the @ChildWorkflowExecutionTerminated@ event.
--
-- /See:/ 'mkChildWorkflowExecutionTerminatedEventAttributes' smart constructor.
data ChildWorkflowExecutionTerminatedEventAttributes = ChildWorkflowExecutionTerminatedEventAttributes'
  { -- | The child workflow execution that was terminated.
    workflowExecution :: Types.WorkflowExecution,
    -- | The type of the child workflow execution.
    workflowType :: Types.WorkflowType,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Core.Integer,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChildWorkflowExecutionTerminatedEventAttributes' value with any optional fields omitted.
mkChildWorkflowExecutionTerminatedEventAttributes ::
  -- | 'workflowExecution'
  Types.WorkflowExecution ->
  -- | 'workflowType'
  Types.WorkflowType ->
  -- | 'initiatedEventId'
  Core.Integer ->
  -- | 'startedEventId'
  Core.Integer ->
  ChildWorkflowExecutionTerminatedEventAttributes
mkChildWorkflowExecutionTerminatedEventAttributes
  workflowExecution
  workflowType
  initiatedEventId
  startedEventId =
    ChildWorkflowExecutionTerminatedEventAttributes'
      { workflowExecution,
        workflowType,
        initiatedEventId,
        startedEventId
      }

-- | The child workflow execution that was terminated.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweteaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes Types.WorkflowExecution
cweteaWorkflowExecution = Lens.field @"workflowExecution"
{-# DEPRECATED cweteaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweteaWorkflowType :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes Types.WorkflowType
cweteaWorkflowType = Lens.field @"workflowType"
{-# DEPRECATED cweteaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweteaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes Core.Integer
cweteaInitiatedEventId = Lens.field @"initiatedEventId"
{-# DEPRECATED cweteaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweteaStartedEventId :: Lens.Lens' ChildWorkflowExecutionTerminatedEventAttributes Core.Integer
cweteaStartedEventId = Lens.field @"startedEventId"
{-# DEPRECATED cweteaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance
  Core.FromJSON
    ChildWorkflowExecutionTerminatedEventAttributes
  where
  parseJSON =
    Core.withObject "ChildWorkflowExecutionTerminatedEventAttributes" Core.$
      \x ->
        ChildWorkflowExecutionTerminatedEventAttributes'
          Core.<$> (x Core..: "workflowExecution")
          Core.<*> (x Core..: "workflowType")
          Core.<*> (x Core..: "initiatedEventId")
          Core.<*> (x Core..: "startedEventId")
