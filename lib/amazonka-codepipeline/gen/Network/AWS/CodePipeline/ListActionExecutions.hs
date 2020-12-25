{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListActionExecutions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the action executions that have occurred in a pipeline.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListActionExecutions
  ( -- * Creating a request
    ListActionExecutions (..),
    mkListActionExecutions,

    -- ** Request lenses
    laePipelineName,
    laeFilter,
    laeMaxResults,
    laeNextToken,

    -- * Destructuring the response
    ListActionExecutionsResponse (..),
    mkListActionExecutionsResponse,

    -- ** Response lenses
    laerrsActionExecutionDetails,
    laerrsNextToken,
    laerrsResponseStatus,
  )
where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListActionExecutions' smart constructor.
data ListActionExecutions = ListActionExecutions'
  { -- | The name of the pipeline for which you want to list action execution history.
    pipelineName :: Types.PipelineName,
    -- | Input information used to filter action execution history.
    filter :: Core.Maybe Types.ActionExecutionFilter,
    -- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListActionExecutions' value with any optional fields omitted.
mkListActionExecutions ::
  -- | 'pipelineName'
  Types.PipelineName ->
  ListActionExecutions
mkListActionExecutions pipelineName =
  ListActionExecutions'
    { pipelineName,
      filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the pipeline for which you want to list action execution history.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laePipelineName :: Lens.Lens' ListActionExecutions Types.PipelineName
laePipelineName = Lens.field @"pipelineName"
{-# DEPRECATED laePipelineName "Use generic-lens or generic-optics with 'pipelineName' instead." #-}

-- | Input information used to filter action execution history.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laeFilter :: Lens.Lens' ListActionExecutions (Core.Maybe Types.ActionExecutionFilter)
laeFilter = Lens.field @"filter"
{-# DEPRECATED laeFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Action execution history is retained for up to 12 months, based on action execution start times. Default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laeMaxResults :: Lens.Lens' ListActionExecutions (Core.Maybe Core.Natural)
laeMaxResults = Lens.field @"maxResults"
{-# DEPRECATED laeMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token that was returned from the previous @ListActionExecutions@ call, which can be used to return the next set of action executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laeNextToken :: Lens.Lens' ListActionExecutions (Core.Maybe Types.NextToken)
laeNextToken = Lens.field @"nextToken"
{-# DEPRECATED laeNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListActionExecutions where
  toJSON ListActionExecutions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("pipelineName" Core..= pipelineName),
            ("filter" Core..=) Core.<$> filter,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListActionExecutions where
  type Rs ListActionExecutions = ListActionExecutionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodePipeline_20150709.ListActionExecutions")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionExecutionsResponse'
            Core.<$> (x Core..:? "actionExecutionDetails")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListActionExecutions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"actionExecutionDetails" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListActionExecutionsResponse' smart constructor.
data ListActionExecutionsResponse = ListActionExecutionsResponse'
  { -- | The details for a list of recent executions, such as action execution ID.
    actionExecutionDetails :: Core.Maybe [Types.ActionExecutionDetail],
    -- | If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListActionExecutionsResponse' value with any optional fields omitted.
mkListActionExecutionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListActionExecutionsResponse
mkListActionExecutionsResponse responseStatus =
  ListActionExecutionsResponse'
    { actionExecutionDetails =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The details for a list of recent executions, such as action execution ID.
--
-- /Note:/ Consider using 'actionExecutionDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laerrsActionExecutionDetails :: Lens.Lens' ListActionExecutionsResponse (Core.Maybe [Types.ActionExecutionDetail])
laerrsActionExecutionDetails = Lens.field @"actionExecutionDetails"
{-# DEPRECATED laerrsActionExecutionDetails "Use generic-lens or generic-optics with 'actionExecutionDetails' instead." #-}

-- | If the amount of returned information is significantly large, an identifier is also returned and can be used in a subsequent @ListActionExecutions@ call to return the next set of action executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laerrsNextToken :: Lens.Lens' ListActionExecutionsResponse (Core.Maybe Types.NextToken)
laerrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED laerrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laerrsResponseStatus :: Lens.Lens' ListActionExecutionsResponse Core.Int
laerrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED laerrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
