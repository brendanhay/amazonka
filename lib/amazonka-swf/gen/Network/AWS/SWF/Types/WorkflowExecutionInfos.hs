{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.WorkflowExecutionInfos
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.WorkflowExecutionInfos
  ( WorkflowExecutionInfos (..),

    -- * Smart constructor
    mkWorkflowExecutionInfos,

    -- * Lenses
    weiExecutionInfos,
    weiNextPageToken,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.NextPageToken as Types
import qualified Network.AWS.SWF.Types.WorkflowExecutionInfo as Types

-- | Contains a paginated list of information about workflow executions.
--
-- /See:/ 'mkWorkflowExecutionInfos' smart constructor.
data WorkflowExecutionInfos = WorkflowExecutionInfos'
  { -- | The list of workflow information structures.
    executionInfos :: [Types.WorkflowExecutionInfo],
    -- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
    --
    -- The configured @maximumPageSize@ determines how many results can be returned in a single call.
    nextPageToken :: Core.Maybe Types.NextPageToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'WorkflowExecutionInfos' value with any optional fields omitted.
mkWorkflowExecutionInfos ::
  WorkflowExecutionInfos
mkWorkflowExecutionInfos =
  WorkflowExecutionInfos'
    { executionInfos = Core.mempty,
      nextPageToken = Core.Nothing
    }

-- | The list of workflow information structures.
--
-- /Note:/ Consider using 'executionInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiExecutionInfos :: Lens.Lens' WorkflowExecutionInfos [Types.WorkflowExecutionInfo]
weiExecutionInfos = Lens.field @"executionInfos"
{-# DEPRECATED weiExecutionInfos "Use generic-lens or generic-optics with 'executionInfos' instead." #-}

-- | If a @NextPageToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @nextPageToken@ . Keep all other arguments unchanged.
--
-- The configured @maximumPageSize@ determines how many results can be returned in a single call.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
weiNextPageToken :: Lens.Lens' WorkflowExecutionInfos (Core.Maybe Types.NextPageToken)
weiNextPageToken = Lens.field @"nextPageToken"
{-# DEPRECATED weiNextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead." #-}

instance Core.FromJSON WorkflowExecutionInfos where
  parseJSON =
    Core.withObject "WorkflowExecutionInfos" Core.$
      \x ->
        WorkflowExecutionInfos'
          Core.<$> (x Core..:? "executionInfos" Core..!= Core.mempty)
          Core.<*> (x Core..:? "nextPageToken")
