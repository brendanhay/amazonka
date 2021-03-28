{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a summary of all of the pipelines associated with your account.
--
-- This operation returns paginated results.
module Network.AWS.CodePipeline.ListPipelines
    (
    -- * Creating a request
      ListPipelines (..)
    , mkListPipelines
    -- ** Request lenses
    , lpNextToken

    -- * Destructuring the response
    , ListPipelinesResponse (..)
    , mkListPipelinesResponse
    -- ** Response lenses
    , lprrsNextToken
    , lprrsPipelines
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.CodePipeline.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a @ListPipelines@ action.
--
-- /See:/ 'mkListPipelines' smart constructor.
newtype ListPipelines = ListPipelines'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ An identifier that was returned from the previous list pipelines call. It can be used to return the next set of pipelines in the list.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelines' value with any optional fields omitted.
mkListPipelines
    :: ListPipelines
mkListPipelines = ListPipelines'{nextToken = Core.Nothing}

-- | An identifier that was returned from the previous list pipelines call. It can be used to return the next set of pipelines in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpNextToken :: Lens.Lens' ListPipelines (Core.Maybe Types.NextToken)
lpNextToken = Lens.field @"nextToken"
{-# INLINEABLE lpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListPipelines where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListPipelines where
        toHeaders ListPipelines{..}
          = Core.pure ("X-Amz-Target", "CodePipeline_20150709.ListPipelines")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListPipelines where
        toJSON ListPipelines{..}
          = Core.object
              (Core.catMaybes [("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest ListPipelines where
        type Rs ListPipelines = ListPipelinesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "pipelines" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPipelines where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"pipelines" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the output of a @ListPipelines@ action.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
  , pipelines :: Core.Maybe [Types.PipelineSummary]
    -- ^ The list of pipelines.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListPipelinesResponse' value with any optional fields omitted.
mkListPipelinesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPipelinesResponse
mkListPipelinesResponse responseStatus
  = ListPipelinesResponse'{nextToken = Core.Nothing,
                           pipelines = Core.Nothing, responseStatus}

-- | If the amount of returned information is significantly large, an identifier is also returned. It can be used in a subsequent list pipelines call to return the next set of pipelines in the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextToken :: Lens.Lens' ListPipelinesResponse (Core.Maybe Types.NextToken)
lprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of pipelines.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPipelines :: Lens.Lens' ListPipelinesResponse (Core.Maybe [Types.PipelineSummary])
lprrsPipelines = Lens.field @"pipelines"
{-# INLINEABLE lprrsPipelines #-}
{-# DEPRECATED pipelines "Use generic-lens or generic-optics with 'pipelines' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsResponseStatus :: Lens.Lens' ListPipelinesResponse Core.Int
lprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
