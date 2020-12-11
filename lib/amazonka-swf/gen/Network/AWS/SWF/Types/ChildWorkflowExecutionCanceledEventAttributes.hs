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
    cDetails,
    cWorkflowExecution,
    cWorkflowType,
    cInitiatedEventId,
    cStartedEventId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provide details of the @ChildWorkflowExecutionCanceled@ event.
--
-- /See:/ 'mkChildWorkflowExecutionCanceledEventAttributes' smart constructor.
data ChildWorkflowExecutionCanceledEventAttributes = ChildWorkflowExecutionCanceledEventAttributes'
  { details ::
      Lude.Maybe
        Lude.Text,
    workflowExecution ::
      WorkflowExecution,
    workflowType ::
      WorkflowType,
    initiatedEventId ::
      Lude.Integer,
    startedEventId ::
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

-- | Creates a value of 'ChildWorkflowExecutionCanceledEventAttributes' with the minimum fields required to make a request.
--
-- * 'details' - Details of the cancellation (if provided).
-- * 'initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The child workflow execution that was canceled.
-- * 'workflowType' - The type of the child workflow execution.
mkChildWorkflowExecutionCanceledEventAttributes ::
  -- | 'workflowExecution'
  WorkflowExecution ->
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  -- | 'startedEventId'
  Lude.Integer ->
  ChildWorkflowExecutionCanceledEventAttributes
mkChildWorkflowExecutionCanceledEventAttributes
  pWorkflowExecution_
  pWorkflowType_
  pInitiatedEventId_
  pStartedEventId_ =
    ChildWorkflowExecutionCanceledEventAttributes'
      { details =
          Lude.Nothing,
        workflowExecution = pWorkflowExecution_,
        workflowType = pWorkflowType_,
        initiatedEventId = pInitiatedEventId_,
        startedEventId = pStartedEventId_
      }

-- | Details of the cancellation (if provided).
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cDetails :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes (Lude.Maybe Lude.Text)
cDetails = Lens.lens (details :: ChildWorkflowExecutionCanceledEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: ChildWorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED cDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The child workflow execution that was canceled.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowExecution
cWorkflowExecution = Lens.lens (workflowExecution :: ChildWorkflowExecutionCanceledEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ChildWorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED cWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cWorkflowType :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes WorkflowType
cWorkflowType = Lens.lens (workflowType :: ChildWorkflowExecutionCanceledEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: ChildWorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED cWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Lude.Integer
cInitiatedEventId = Lens.lens (initiatedEventId :: ChildWorkflowExecutionCanceledEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ChildWorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED cInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStartedEventId :: Lens.Lens' ChildWorkflowExecutionCanceledEventAttributes Lude.Integer
cStartedEventId = Lens.lens (startedEventId :: ChildWorkflowExecutionCanceledEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ChildWorkflowExecutionCanceledEventAttributes)
{-# DEPRECATED cStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

instance
  Lude.FromJSON
    ChildWorkflowExecutionCanceledEventAttributes
  where
  parseJSON =
    Lude.withObject
      "ChildWorkflowExecutionCanceledEventAttributes"
      ( \x ->
          ChildWorkflowExecutionCanceledEventAttributes'
            Lude.<$> (x Lude..:? "details")
            Lude.<*> (x Lude..: "workflowExecution")
            Lude.<*> (x Lude..: "workflowType")
            Lude.<*> (x Lude..: "initiatedEventId")
            Lude.<*> (x Lude..: "startedEventId")
      )
