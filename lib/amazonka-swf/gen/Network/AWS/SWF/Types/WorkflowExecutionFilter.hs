{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionFilter
  ( WorkflowExecutionFilter (..),

    -- * Smart constructor
    mkWorkflowExecutionFilter,

    -- * Lenses
    wefWorkflowId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Used to filter the workflow executions in visibility APIs by their @workflowId@ .
--
-- /See:/ 'mkWorkflowExecutionFilter' smart constructor.
newtype WorkflowExecutionFilter = WorkflowExecutionFilter'
  { -- | The workflowId to pass of match the criteria of this filter.
    workflowId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'WorkflowExecutionFilter' with the minimum fields required to make a request.
--
-- * 'workflowId' - The workflowId to pass of match the criteria of this filter.
mkWorkflowExecutionFilter ::
  -- | 'workflowId'
  Lude.Text ->
  WorkflowExecutionFilter
mkWorkflowExecutionFilter pWorkflowId_ =
  WorkflowExecutionFilter' {workflowId = pWorkflowId_}

-- | The workflowId to pass of match the criteria of this filter.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefWorkflowId :: Lens.Lens' WorkflowExecutionFilter Lude.Text
wefWorkflowId = Lens.lens (workflowId :: WorkflowExecutionFilter -> Lude.Text) (\s a -> s {workflowId = a} :: WorkflowExecutionFilter)
{-# DEPRECATED wefWorkflowId "Use generic-lens or generic-optics with 'workflowId' instead." #-}

instance Lude.ToJSON WorkflowExecutionFilter where
  toJSON WorkflowExecutionFilter' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("workflowId" Lude..= workflowId)])
