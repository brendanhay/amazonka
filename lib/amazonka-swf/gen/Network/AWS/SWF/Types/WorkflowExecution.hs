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
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowId as Types
import qualified Network.AWS.SWF.Types.WorkflowRunId as Types

-- | Represents a workflow execution.
--
-- /See:/ 'mkWorkflowExecution' smart constructor.
data WorkflowExecution = WorkflowExecution'
  { -- | The user defined identifier associated with the workflow execution.
    workflowId :: Types.WorkflowId,
    -- | A system-generated unique identifier for the workflow execution.
    runId :: Types.WorkflowRunId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecution' value with any optional fields omitted.
mkWorkflowExecution ::
  -- | 'workflowId'
  Types.WorkflowId ->
  -- | 'runId'
  Types.WorkflowRunId ->
  WorkflowExecution
mkWorkflowExecution workflowId runId =
  WorkflowExecution' {workflowId, runId}

-- | The user defined identifier associated with the workflow execution.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weWorkflowId :: Lens.Lens' WorkflowExecution Types.WorkflowId
weWorkflowId = Lens.field @"workflowId"
{-# DEPRECATED weWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

-- | A system-generated unique identifier for the workflow execution.
--
-- /Note:/ Consider using 'runId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weRunId :: Lens.Lens' WorkflowExecution Types.WorkflowRunId
weRunId = Lens.field @"runId"
{-# DEPRECATED weRunId "Use generic-lens or generic-optics with 'runId' instead." #-}

instance Core.FromJSON WorkflowExecution where
  toJSON WorkflowExecution {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("workflowId" Core..= workflowId),
            Core.Just ("runId" Core..= runId)
          ]
      )

instance Core.FromJSON WorkflowExecution where
  parseJSON =
    Core.withObject "WorkflowExecution" Core.$
      \x ->
        WorkflowExecution'
          Core.<$> (x Core..: "workflowId") Core.<*> (x Core..: "runId")
