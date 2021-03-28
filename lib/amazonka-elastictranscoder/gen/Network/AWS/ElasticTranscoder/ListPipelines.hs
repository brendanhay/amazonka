{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticTranscoder.ListPipelines
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The ListPipelines operation gets a list of the pipelines associated with the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.ElasticTranscoder.ListPipelines
    (
    -- * Creating a request
      ListPipelines (..)
    , mkListPipelines
    -- ** Request lenses
    , lpAscending
    , lpPageToken

    -- * Destructuring the response
    , ListPipelinesResponse (..)
    , mkListPipelinesResponse
    -- ** Response lenses
    , lprrsNextPageToken
    , lprrsPipelines
    , lprrsResponseStatus
    ) where

import qualified Network.AWS.ElasticTranscoder.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @ListPipelineRequest@ structure.
--
-- /See:/ 'mkListPipelines' smart constructor.
data ListPipelines = ListPipelines'
  { ascending :: Core.Maybe Types.Ascending
    -- ^ To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
  , pageToken :: Core.Maybe Types.PageToken
    -- ^ When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelines' value with any optional fields omitted.
mkListPipelines
    :: ListPipelines
mkListPipelines
  = ListPipelines'{ascending = Core.Nothing,
                   pageToken = Core.Nothing}

-- | To list pipelines in chronological order by the date and time that they were created, enter @true@ . To list pipelines in reverse chronological order, enter @false@ .
--
-- /Note:/ Consider using 'ascending' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpAscending :: Lens.Lens' ListPipelines (Core.Maybe Types.Ascending)
lpAscending = Lens.field @"ascending"
{-# INLINEABLE lpAscending #-}
{-# DEPRECATED ascending "Use generic-lens or generic-optics with 'ascending' instead"  #-}

-- | When Elastic Transcoder returns more than one page of results, use @pageToken@ in subsequent @GET@ requests to get each successive page of results. 
--
-- /Note:/ Consider using 'pageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lpPageToken :: Lens.Lens' ListPipelines (Core.Maybe Types.PageToken)
lpPageToken = Lens.field @"pageToken"
{-# INLINEABLE lpPageToken #-}
{-# DEPRECATED pageToken "Use generic-lens or generic-optics with 'pageToken' instead"  #-}

instance Core.ToQuery ListPipelines where
        toQuery ListPipelines{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Ascending") ascending
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PageToken") pageToken

instance Core.ToHeaders ListPipelines where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListPipelines where
        type Rs ListPipelines = ListPipelinesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2012-09-25/pipelines",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListPipelinesResponse' Core.<$>
                   (x Core..:? "NextPageToken") Core.<*> x Core..:? "Pipelines"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListPipelines where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextPageToken") =
            Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"pipelines" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"pageToken" Lens..~
                   rs Lens.^. Lens.field @"nextPageToken")

-- | A list of the pipelines associated with the current AWS account.
--
-- /See:/ 'mkListPipelinesResponse' smart constructor.
data ListPipelinesResponse = ListPipelinesResponse'
  { nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
  , pipelines :: Core.Maybe [Types.Pipeline]
    -- ^ An array of @Pipeline@ objects.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListPipelinesResponse' value with any optional fields omitted.
mkListPipelinesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListPipelinesResponse
mkListPipelinesResponse responseStatus
  = ListPipelinesResponse'{nextPageToken = Core.Nothing,
                           pipelines = Core.Nothing, responseStatus}

-- | A value that you use to access the second and subsequent pages of results, if any. When the pipelines fit on one page or when you've reached the last page of results, the value of @NextPageToken@ is @null@ .
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsNextPageToken :: Lens.Lens' ListPipelinesResponse (Core.Maybe Types.NextPageToken)
lprrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE lprrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | An array of @Pipeline@ objects.
--
-- /Note:/ Consider using 'pipelines' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lprrsPipelines :: Lens.Lens' ListPipelinesResponse (Core.Maybe [Types.Pipeline])
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
