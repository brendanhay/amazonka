{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListPipelineExecutions (..)
    , mkListPipelineExecutions
    -- ** Request lenses
    , lpePipelineName
    , lpeMaxResults
    , lpeNextToken

    -- * Destructuring the response
    , ListPipelineExecutionsResponse (..)
    , mkListPipelineExecutionsResponse
    -- ** Response lenses
    , lperrsNextToken
    , lperrsPipelineExecutionSummaries
    , lperrsResponseStatus
    ) where

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
  { pipelineName :: Types.PipelineName
    -- ^ The name of the pipeline for which you want to get execution summary information.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Pipeline history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token that was returned from the previous @ListPipelineExecutions@ call, which can be used to return the next set of pipeline executions in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelineExecutions' value with any optional fields omitted.
mkListPipelineExecutions
    :: Types.PipelineName -- ^ 'pipelineName'
    -> ListPipelineExecutions
mkListPipelineExecutions pipelineName
  = ListPipelineExecutions'{pipelineName, maxResults = Core.Nothing,
                            nextToken = Core.Nothing}

-- | The name of the pipeline for which you want to get execution summary information.
--
-- /Note:/ Consider using 'pipelineName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpePipelineName :: Lens.Lens' ListPipelineExecutions Types.PipelineName
lpePipelineName = Lens.field @"pipelineName"
{-# INLINEABLE lpePipelineName #-}
{-# DEPRECATED pipelineName "Use generic-lens or generic-optics with 'pipelineName' instead"  #-}

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned nextToken value. Pipeline history is limited to the most recent 12 months, based on pipeline execution start times. Default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpeMaxResults :: Lens.Lens' ListPipelineExecutions (Core.Maybe Core.Natural)
lpeMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lpeMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token that was returned from the previous @ListPipelineExecutions@ call, which can be used to return the next set of pipeline executions in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpeNextToken :: Lens.Lens' ListPipelineExecutions (Core.Maybe Types.NextToken)
lpeNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpeNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPipelineExecutions where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPipelineExecutions where
        toHeaders ListPipelineExecutions{..}
          = Core.pure
              ("X-Amz-Target", "CodePipeline_20150709.ListPipelineExecutions")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPipelineExecutions where
        toJSON ListPipelineExecutions{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("pipelineName" Core..= pipelineName),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPipelineExecutions where
        type Rs ListPipelineExecutions = ListPipelineExecutionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPipelineExecutionsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*>
                     x Core..:? "pipelineExecutionSummaries"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPipelineExecutions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"pipelineExecutionSummaries" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the output of a @ListPipelineExecutions@ action.
--
-- /See:/ 'mkListPipelineExecutionsResponse' smart constructor.
data ListPipelineExecutionsResponse = ListPipelineExecutionsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ A token that can be used in the next @ListPipelineExecutions@ call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
  , pipelineExecutionSummaries :: Core.Maybe [Types.PipelineExecutionSummary]
    -- ^ A list of executions in the history of a pipeline.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPipelineExecutionsResponse' value with any optional fields omitted.
mkListPipelineExecutionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPipelineExecutionsResponse
mkListPipelineExecutionsResponse responseStatus
  = ListPipelineExecutionsResponse'{nextToken = Core.Nothing,
                                    pipelineExecutionSummaries = Core.Nothing, responseStatus}

-- | A token that can be used in the next @ListPipelineExecutions@ call. To view all items in the list, continue to call this operation with each subsequent token until no more nextToken values are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lperrsNextToken :: Lens.Lens' ListPipelineExecutionsResponse (Core.Maybe Types.NextToken)
lperrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lperrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A list of executions in the history of a pipeline.
--
-- /Note:/ Consider using 'pipelineExecutionSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lperrsPipelineExecutionSummaries :: Lens.Lens' ListPipelineExecutionsResponse (Core.Maybe [Types.PipelineExecutionSummary])
lperrsPipelineExecutionSummaries = Lens.field @"pipelineExecutionSummaries"
{-# INLINEABLE lperrsPipelineExecutionSummaries #-}
{-# DEPRECATED pipelineExecutionSummaries "Use generic-lens or generic-optics with 'pipelineExecutionSummaries' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lperrsResponseStatus :: Lens.Lens' ListPipelineExecutionsResponse Core.Int
lperrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lperrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
