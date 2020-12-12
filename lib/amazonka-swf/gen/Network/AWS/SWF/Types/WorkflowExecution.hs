{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecution
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecution
  ( WorkflowExecution (..),

    -- * Smart constructor
    mkWorkflowExecution,

    -- * Lenses
    weWorkflowId,
    weRunId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a workflow execution.
--
-- /See:/ 'mkWorkflowExecution' smart constructor.
data WorkflowExecution = WorkflowExecution'
  { workflowId ::
      Lude.Text,
    runId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecution' with the minimum fields required to make a request.
--
-- * 'runId' - A system-generated unique identifier for the workflow execution.
-- * 'workflowId' - The user defined identifier associated with the workflow execution.
mkWorkflowExecution ::
  -- | 'workflowId'
  Lude.Text ->
  -- | 'runId'
  Lude.Text ->
  WorkflowExecution
mkWorkflowExecution pWorkflowId_ pRunId_ =
  WorkflowExecution' {workflowId = pWorkflowId_, runId = pRunId_}

-- | The user defined identifier associated with the workflow execution.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weWorkflowId :: Lens.Lens' WorkflowExecution Lude.Text
weWorkflowId = Lens.lens (workflowId :: WorkflowExecution -> Lude.Text) (\s a -> s {workflowId = a} :: WorkflowExecution)
{-# DEPRECATED weWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | A system-generated unique identifier for the workflow execution.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weRunId :: Lens.Lens' WorkflowExecution Lude.Text
weRunId = Lens.lens (runId :: WorkflowExecution -> Lude.Text) (\s a -> s {runId = a} :: WorkflowExecution)
{-# DEPRECATED weRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Lude.FromJSON WorkflowExecution where
  parseJSON =
    Lude.withObject
      "WorkflowExecution"
      ( \x ->
          WorkflowExecution'
            Lude.<$> (x Lude..: "workflowId") Lude.<*> (x Lude..: "runId")
      )

instance Lude.ToJSON WorkflowExecution where
  toJSON WorkflowExecution' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("workflowId" Lude..= workflowId),
            Lude.Just ("runId" Lude..= runId)
          ]
      )
