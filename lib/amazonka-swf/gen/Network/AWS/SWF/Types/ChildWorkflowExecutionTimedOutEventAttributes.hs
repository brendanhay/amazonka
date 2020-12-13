{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
  ( ChildWorkflowExecutionTimedOutEventAttributes (..),

    -- * Smart constructor
    mkChildWorkflowExecutionTimedOutEventAttributes,

    -- * Lenses
    cwetoeaWorkflowType,
    cwetoeaTimeoutType,
    cwetoeaStartedEventId,
    cwetoeaInitiatedEventId,
    cwetoeaWorkflowExecution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowExecutionTimeoutType
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionTimedOut@ event.
--
-- /See:/ 'mkChildWorkflowExecutionTimedOutEventAttributes' smart constructor.
data ChildWorkflowExecutionTimedOutEventAttributes = ChildWorkflowExecutionTimedOutEventAttributes'
  { -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The type of the timeout that caused the child workflow execution to time out.
    timeoutType :: WorkflowExecutionTimeoutType,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Lude.Integer,
    -- | The child workflow execution that timed out.
    workflowExecution :: WorkflowExecution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChildWorkflowExecutionTimedOutEventAttributes' with the minimum fields required to make a request.
--
-- * 'workflowType' - The type of the child workflow execution.
-- * 'timeoutType' - The type of the timeout that caused the child workflow execution to time out.
-- * 'startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The child workflow execution that timed out.
mkChildWorkflowExecutionTimedOutEventAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'timeoutType'
  WorkflowExecutionTimeoutType ->
  -- | 'startedEventId'
  Lude.Integer ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  -- | 'workflowExecution'
  WorkflowExecution ->
  ChildWorkflowExecutionTimedOutEventAttributes
mkChildWorkflowExecutionTimedOutEventAttributes
  pWorkflowType_
  pTimeoutType_
  pStartedEventId_
  pInitiatedEventId_
  pWorkflowExecution_ =
    ChildWorkflowExecutionTimedOutEventAttributes'
      { workflowType =
          pWorkflowType_,
        timeoutType = pTimeoutType_,
        startedEventId = pStartedEventId_,
        initiatedEventId = pInitiatedEventId_,
        workflowExecution = pWorkflowExecution_
      }

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaWorkflowType :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowType
cwetoeaWorkflowType = Lens.lens (workflowType :: ChildWorkflowExecutionTimedOutEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: ChildWorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED cwetoeaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The type of the timeout that caused the child workflow execution to time out.
--
-- /Note:/ Consider using 'timeoutType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaTimeoutType :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecutionTimeoutType
cwetoeaTimeoutType = Lens.lens (timeoutType :: ChildWorkflowExecutionTimedOutEventAttributes -> WorkflowExecutionTimeoutType) (\s a -> s {timeoutType = a} :: ChildWorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED cwetoeaTimeoutType "Use generic-lens or generic-optics with 'timeoutType' instead." #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaStartedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Lude.Integer
cwetoeaStartedEventId = Lens.lens (startedEventId :: ChildWorkflowExecutionTimedOutEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ChildWorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED cwetoeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes Lude.Integer
cwetoeaInitiatedEventId = Lens.lens (initiatedEventId :: ChildWorkflowExecutionTimedOutEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ChildWorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED cwetoeaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The child workflow execution that timed out.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwetoeaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionTimedOutEventAttributes WorkflowExecution
cwetoeaWorkflowExecution = Lens.lens (workflowExecution :: ChildWorkflowExecutionTimedOutEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ChildWorkflowExecutionTimedOutEventAttributes)
{-# DEPRECATED cwetoeaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

instance
  Lude.FromJSON
    ChildWorkflowExecutionTimedOutEventAttributes
  where
  parseJSON =
    Lude.withObject
      "ChildWorkflowExecutionTimedOutEventAttributes"
      ( \x ->
          ChildWorkflowExecutionTimedOutEventAttributes'
            Lude.<$> (x Lude..: "workflowType")
            Lude.<*> (x Lude..: "timeoutType")
            Lude.<*> (x Lude..: "startedEventId")
            Lude.<*> (x Lude..: "initiatedEventId")
            Lude.<*> (x Lude..: "workflowExecution")
      )
