{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
  ( ChildWorkflowExecutionCompletedEventAttributes (..),

    -- * Smart constructor
    mkChildWorkflowExecutionCompletedEventAttributes,

    -- * Lenses
    cweceaWorkflowType,
    cweceaResult,
    cweceaStartedEventId,
    cweceaInitiatedEventId,
    cweceaWorkflowExecution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionCompleted@ event.
--
-- /See:/ 'mkChildWorkflowExecutionCompletedEventAttributes' smart constructor.
data ChildWorkflowExecutionCompletedEventAttributes = ChildWorkflowExecutionCompletedEventAttributes'
  { -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The result of the child workflow execution.
    result :: Lude.Maybe Lude.Text,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Lude.Integer,
    -- | The child workflow execution that was completed.
    workflowExecution :: WorkflowExecution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChildWorkflowExecutionCompletedEventAttributes' with the minimum fields required to make a request.
--
-- * 'workflowType' - The type of the child workflow execution.
-- * 'result' - The result of the child workflow execution.
-- * 'startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The child workflow execution that was completed.
mkChildWorkflowExecutionCompletedEventAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'startedEventId'
  Lude.Integer ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  -- | 'workflowExecution'
  WorkflowExecution ->
  ChildWorkflowExecutionCompletedEventAttributes
mkChildWorkflowExecutionCompletedEventAttributes
  pWorkflowType_
  pStartedEventId_
  pInitiatedEventId_
  pWorkflowExecution_ =
    ChildWorkflowExecutionCompletedEventAttributes'
      { workflowType =
          pWorkflowType_,
        result = Lude.Nothing,
        startedEventId = pStartedEventId_,
        initiatedEventId = pInitiatedEventId_,
        workflowExecution = pWorkflowExecution_
      }

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaWorkflowType :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowType
cweceaWorkflowType = Lens.lens (workflowType :: ChildWorkflowExecutionCompletedEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: ChildWorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED cweceaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The result of the child workflow execution.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaResult :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes (Lude.Maybe Lude.Text)
cweceaResult = Lens.lens (result :: ChildWorkflowExecutionCompletedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {result = a} :: ChildWorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED cweceaResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaStartedEventId :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Lude.Integer
cweceaStartedEventId = Lens.lens (startedEventId :: ChildWorkflowExecutionCompletedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ChildWorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED cweceaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes Lude.Integer
cweceaInitiatedEventId = Lens.lens (initiatedEventId :: ChildWorkflowExecutionCompletedEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ChildWorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED cweceaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The child workflow execution that was completed.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweceaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionCompletedEventAttributes WorkflowExecution
cweceaWorkflowExecution = Lens.lens (workflowExecution :: ChildWorkflowExecutionCompletedEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ChildWorkflowExecutionCompletedEventAttributes)
{-# DEPRECATED cweceaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

instance
  Lude.FromJSON
    ChildWorkflowExecutionCompletedEventAttributes
  where
  parseJSON =
    Lude.withObject
      "ChildWorkflowExecutionCompletedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionCompletedEventAttributes'
            Lude.<$> (x Lude..: "workflowType")
            Lude.<*> (x Lude..:? "result")
            Lude.<*> (x Lude..: "startedEventId")
            Lude.<*> (x Lude..: "initiatedEventId")
            Lude.<*> (x Lude..: "workflowExecution")
      )
