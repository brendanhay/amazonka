{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListPipelineExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of the most recent executions for a pipeline.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListPipelineExecutions
  ( -- * Creating a request
    ListPipelineExecutions (..),
    mkListPipelineExecutions,

    -- ** Request lenses
    lpePipelineName,
    lpeMaxResults,
    lpeNextToken,

    -- * Destructuring the response
    ListPipelineExecutionsResponse (..),
    mkListPipelineExecutionsResponse,

    -- ** Response lenses
    lperrsNextToken,
    lperrsPipelineExecutionSummaries,
    lperrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListPipelineExecutions@ action.
--
-- /See:/ 'mkListPipelineExecutions' smart constructor.
data ListPipelineExecutions = ListPipelineExecutions'
  { -- | The name of the pipeline for which you want to get execution summary information.
    pipelineName :: Types.PipelineName,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Pipeline history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token that was returned from the previous @ListPipelineExecutions@ call, which can be used to return the next set of pipeline executions in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelineExecutions' value with any optional fields omitted.
mkListPipelineExecutions ::
  -- | 'pipelineName'
  Types.PipelineName ->
  ListPipelineExecutions
mkListPipelineExecutions pipelineName =
  ListPipelineExecutions'
    { pipelineName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the pipeline for which you want to get execution summary information.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpePipelineName :: Lens.Lens' ListPipelineExecutions Types.PipelineName
lpePipelineName = Lens.field @"pipelineName"
{-# DEPRECATED lpePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Pipeline history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpeMaxResults :: Lens.Lens' ListPipelineExecutions (Core.Maybe Core.Natural)
lpeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lpeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token that was returned from the previous @ListPipelineExecutions@ call, which can be used to return the next set of pipeline executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpeNextToken :: Lens.Lens' ListPipelineExecutions (Core.Maybe Types.NextToken)
lpeNextToken = Lens.field @"nextToken"
{-# DEPRECATED lpeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListPipelineExecutions where
  toJSON ListPipelineExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListPipelineExecutions where
  type Rs ListPipelineExecutions = ListPipelineExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.ListPipelineExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPipelineExecutionsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "pipelineExecutionSummaries")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListPipelineExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"pipelineExecutionSummaries" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a @ListPipelineExecutions@ action.
--
-- /See:/ 'mkListPipelineExecutionsResponse' smart constructor.
data ListPipelineExecutionsResponse = ListPipelineExecutionsResponse'
  { -- | A token that can be used in the next @ListPipelineExecutions@ call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of executions in the history of a pipeline.
    pipelineExecutionSummaries :: Core.Maybe [Types.PipelineExecutionSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListPipelineExecutionsResponse' value with any optional fields omitted.
mkListPipelineExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListPipelineExecutionsResponse
mkListPipelineExecutionsResponse responseStatus =
  ListPipelineExecutionsResponse'
    { nextToken = Core.Nothing,
      pipelineExecutionSummaries = Core.Nothing,
      responseStatus
    }

-- | A token that can be used in the next @ListPipelineExecutions@ call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lperrsNextToken :: Lens.Lens' ListPipelineExecutionsResponse (Core.Maybe Types.NextToken)
lperrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lperrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of executions in the history of a pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lperrsPipelineExecutionSummaries :: Lens.Lens' ListPipelineExecutionsResponse (Core.Maybe [Types.PipelineExecutionSummary])
lperrsPipelineExecutionSummaries = Lens.field @"pipelineExecutionSummaries"
{-# DEPRECATED lperrsPipelineExecutionSummaries "Use generic-lens or generic-optics with 'pipelineExecutionSummaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lperrsResponseStatus :: Lens.Lens' ListPipelineExecutionsResponse Core.Int
lperrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lperrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
