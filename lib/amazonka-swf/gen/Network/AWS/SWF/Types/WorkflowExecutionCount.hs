{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SWF.Types.WorkflowExecutionCount
  ( WorkflowExecutionCount (..)
  -- * Smart constructor
  , mkWorkflowExecutionCount
  -- * Lenses
  , wecCount
  , wecTruncated
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the count of workflow executions returned from 'CountOpenWorkflowExecutions' or 'CountClosedWorkflowExecutions' 
--
-- /See:/ 'mkWorkflowExecutionCount' smart constructor.
data WorkflowExecutionCount = WorkflowExecutionCount'
  { count :: Core.Natural
    -- ^ The number of workflow executions.
  , truncated :: Core.Maybe Core.Bool
    -- ^ If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'WorkflowExecutionCount' value with any optional fields omitted.
mkWorkflowExecutionCount
    :: Core.Natural -- ^ 'count'
    -> WorkflowExecutionCount
mkWorkflowExecutionCount count
  = WorkflowExecutionCount'{count, truncated = Core.Nothing}

-- | The number of workflow executions.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecCount :: Lens.Lens' WorkflowExecutionCount Core.Natural
wecCount = Lens.field @"count"
{-# INLINEABLE wecCount #-}
{-# DEPRECATED count "Use generic-lens or generic-optics with 'count' instead"  #-}

-- | If set to true, indicates that the actual count was more than the maximum supported by this API and the count returned is the truncated value.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wecTruncated :: Lens.Lens' WorkflowExecutionCount (Core.Maybe Core.Bool)
wecTruncated = Lens.field @"truncated"
{-# INLINEABLE wecTruncated #-}
{-# DEPRECATED truncated "Use generic-lens or generic-optics with 'truncated' instead"  #-}

instance Core.FromJSON WorkflowExecutionCount where
        parseJSON
          = Core.withObject "WorkflowExecutionCount" Core.$
              \ x ->
                WorkflowExecutionCount' Core.<$>
                  (x Core..: "count") Core.<*> x Core..:? "truncated"
