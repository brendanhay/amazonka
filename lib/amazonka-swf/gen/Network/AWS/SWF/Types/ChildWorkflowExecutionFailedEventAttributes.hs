{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
  ( ChildWorkflowExecutionFailedEventAttributes (..),

    -- * Smart constructor
    mkChildWorkflowExecutionFailedEventAttributes,

    -- * Lenses
    cwefeaWorkflowType,
    cwefeaReason,
    cwefeaDetails,
    cwefeaStartedEventId,
    cwefeaInitiatedEventId,
    cwefeaWorkflowExecution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionFailed@ event.
--
-- /See:/ 'mkChildWorkflowExecutionFailedEventAttributes' smart constructor.
data ChildWorkflowExecutionFailedEventAttributes = ChildWorkflowExecutionFailedEventAttributes'
  { -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The reason for the failure (if provided).
    reason :: Lude.Maybe Lude.Text,
    -- | The details of the failure (if provided).
    details :: Lude.Maybe Lude.Text,
    -- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    startedEventId :: Lude.Integer,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Lude.Integer,
    -- | The child workflow execution that failed.
    workflowExecution :: WorkflowExecution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChildWorkflowExecutionFailedEventAttributes' with the minimum fields required to make a request.
--
-- * 'workflowType' - The type of the child workflow execution.
-- * 'reason' - The reason for the failure (if provided).
-- * 'details' - The details of the failure (if provided).
-- * 'startedEventId' - The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The child workflow execution that failed.
mkChildWorkflowExecutionFailedEventAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'startedEventId'
  Lude.Integer ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  -- | 'workflowExecution'
  WorkflowExecution ->
  ChildWorkflowExecutionFailedEventAttributes
mkChildWorkflowExecutionFailedEventAttributes
  pWorkflowType_
  pStartedEventId_
  pInitiatedEventId_
  pWorkflowExecution_ =
    ChildWorkflowExecutionFailedEventAttributes'
      { workflowType =
          pWorkflowType_,
        reason = Lude.Nothing,
        details = Lude.Nothing,
        startedEventId = pStartedEventId_,
        initiatedEventId = pInitiatedEventId_,
        workflowExecution = pWorkflowExecution_
      }

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaWorkflowType :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowType
cwefeaWorkflowType = Lens.lens (workflowType :: ChildWorkflowExecutionFailedEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: ChildWorkflowExecutionFailedEventAttributes)
{-# DEPRECATED cwefeaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The reason for the failure (if provided).
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaReason :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes (Lude.Maybe Lude.Text)
cwefeaReason = Lens.lens (reason :: ChildWorkflowExecutionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: ChildWorkflowExecutionFailedEventAttributes)
{-# DEPRECATED cwefeaReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The details of the failure (if provided).
--
-- /Note:/ Consider using 'details' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaDetails :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes (Lude.Maybe Lude.Text)
cwefeaDetails = Lens.lens (details :: ChildWorkflowExecutionFailedEventAttributes -> Lude.Maybe Lude.Text) (\s a -> s {details = a} :: ChildWorkflowExecutionFailedEventAttributes)
{-# DEPRECATED cwefeaDetails "Use generic-lens or generic-optics with 'details' instead." #-}

-- | The ID of the @ChildWorkflowExecutionStarted@ event recorded when this child workflow execution was started. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'startedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaStartedEventId :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Lude.Integer
cwefeaStartedEventId = Lens.lens (startedEventId :: ChildWorkflowExecutionFailedEventAttributes -> Lude.Integer) (\s a -> s {startedEventId = a} :: ChildWorkflowExecutionFailedEventAttributes)
{-# DEPRECATED cwefeaStartedEventId "Use generic-lens or generic-optics with 'startedEventId' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes Lude.Integer
cwefeaInitiatedEventId = Lens.lens (initiatedEventId :: ChildWorkflowExecutionFailedEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ChildWorkflowExecutionFailedEventAttributes)
{-# DEPRECATED cwefeaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The child workflow execution that failed.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cwefeaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionFailedEventAttributes WorkflowExecution
cwefeaWorkflowExecution = Lens.lens (workflowExecution :: ChildWorkflowExecutionFailedEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ChildWorkflowExecutionFailedEventAttributes)
{-# DEPRECATED cwefeaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

instance Lude.FromJSON ChildWorkflowExecutionFailedEventAttributes where
  parseJSON =
    Lude.withObject
      "ChildWorkflowExecutionFailedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionFailedEventAttributes'
            Lude.<$> (x Lude..: "workflowType")
            Lude.<*> (x Lude..:? "reason")
            Lude.<*> (x Lude..:? "details")
            Lude.<*> (x Lude..: "startedEventId")
            Lude.<*> (x Lude..: "initiatedEventId")
            Lude.<*> (x Lude..: "workflowExecution")
      )
