{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
  ( ChildWorkflowExecutionStartedEventAttributes (..),

    -- * Smart constructor
    mkChildWorkflowExecutionStartedEventAttributes,

    -- * Lenses
    cweseaWorkflowType,
    cweseaInitiatedEventId,
    cweseaWorkflowExecution,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.WorkflowExecution
import Network.AWS.SWF.Types.WorkflowType

-- | Provides the details of the @ChildWorkflowExecutionStarted@ event.
--
-- /See:/ 'mkChildWorkflowExecutionStartedEventAttributes' smart constructor.
data ChildWorkflowExecutionStartedEventAttributes = ChildWorkflowExecutionStartedEventAttributes'
  { -- | The type of the child workflow execution.
    workflowType :: WorkflowType,
    -- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
    initiatedEventId :: Lude.Integer,
    -- | The child workflow execution that was started.
    workflowExecution :: WorkflowExecution
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ChildWorkflowExecutionStartedEventAttributes' with the minimum fields required to make a request.
--
-- * 'workflowType' - The type of the child workflow execution.
-- * 'initiatedEventId' - The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
-- * 'workflowExecution' - The child workflow execution that was started.
mkChildWorkflowExecutionStartedEventAttributes ::
  -- | 'workflowType'
  WorkflowType ->
  -- | 'initiatedEventId'
  Lude.Integer ->
  -- | 'workflowExecution'
  WorkflowExecution ->
  ChildWorkflowExecutionStartedEventAttributes
mkChildWorkflowExecutionStartedEventAttributes
  pWorkflowType_
  pInitiatedEventId_
  pWorkflowExecution_ =
    ChildWorkflowExecutionStartedEventAttributes'
      { workflowType =
          pWorkflowType_,
        initiatedEventId = pInitiatedEventId_,
        workflowExecution = pWorkflowExecution_
      }

-- | The type of the child workflow execution.
--
-- /Note:/ Consider using 'workflowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweseaWorkflowType :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowType
cweseaWorkflowType = Lens.lens (workflowType :: ChildWorkflowExecutionStartedEventAttributes -> WorkflowType) (\s a -> s {workflowType = a} :: ChildWorkflowExecutionStartedEventAttributes)
{-# DEPRECATED cweseaWorkflowType "Use generic-lens or generic-optics with 'workflowType' instead." #-}

-- | The ID of the @StartChildWorkflowExecutionInitiated@ event corresponding to the @StartChildWorkflowExecution@ 'Decision' to start this child workflow execution. This information can be useful for diagnosing problems by tracing back the chain of events leading up to this event.
--
-- /Note:/ Consider using 'initiatedEventId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweseaInitiatedEventId :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes Lude.Integer
cweseaInitiatedEventId = Lens.lens (initiatedEventId :: ChildWorkflowExecutionStartedEventAttributes -> Lude.Integer) (\s a -> s {initiatedEventId = a} :: ChildWorkflowExecutionStartedEventAttributes)
{-# DEPRECATED cweseaInitiatedEventId "Use generic-lens or generic-optics with 'initiatedEventId' instead." #-}

-- | The child workflow execution that was started.
--
-- /Note:/ Consider using 'workflowExecution' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cweseaWorkflowExecution :: Lens.Lens' ChildWorkflowExecutionStartedEventAttributes WorkflowExecution
cweseaWorkflowExecution = Lens.lens (workflowExecution :: ChildWorkflowExecutionStartedEventAttributes -> WorkflowExecution) (\s a -> s {workflowExecution = a} :: ChildWorkflowExecutionStartedEventAttributes)
{-# DEPRECATED cweseaWorkflowExecution "Use generic-lens or generic-optics with 'workflowExecution' instead." #-}

instance Lude.FromJSON ChildWorkflowExecutionStartedEventAttributes where
  parseJSON =
    Lude.withObject
      "ChildWorkflowExecutionStartedEventAttributes"
      ( \x ->
          ChildWorkflowExecutionStartedEventAttributes'
            Lude.<$> (x Lude..: "workflowType")
            Lude.<*> (x Lude..: "initiatedEventId")
            Lude.<*> (x Lude..: "workflowExecution")
      )
