{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionFilter
  ( WorkflowExecutionFilter (..)
  -- * Smart constructor
  , mkWorkflowExecutionFilter
  -- * Lenses
  , wefWorkflowId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.WorkflowId as Types

-- | Used to filter the workflow executions in visibility APIs by their @workflowId@ .
--
-- /See:/ 'mkWorkflowExecutionFilter' smart constructor.
newtype WorkflowExecutionFilter = WorkflowExecutionFilter'
  { workflowId :: Types.WorkflowId
    -- ^ The workflowId to pass of match the criteria of this filter.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionFilter' value with any optional fields omitted.
mkWorkflowExecutionFilter
    :: Types.WorkflowId -- ^ 'workflowId'
    -> WorkflowExecutionFilter
mkWorkflowExecutionFilter workflowId
  = WorkflowExecutionFilter'{workflowId}

-- | The workflowId to pass of match the criteria of this filter.
--
-- /Note:/ Consider using 'workflowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wefWorkflowId :: Lens.Lens' WorkflowExecutionFilter Types.WorkflowId
wefWorkflowId = Lens.field @"workflowId"
{-# INLINEABLE wefWorkflowId #-}
{-# DEPRECATED workflowId "Use generic-lens or generic-optics with 'workflowId' instead"  #-}

instance Core.FromJSON WorkflowExecutionFilter where
        toJSON WorkflowExecutionFilter{..}
          = Core.object
              (Core.catMaybes [Core.Just ("workflowId" Core..= workflowId)])
