{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawlerMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metrics about specified crawlers.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlerMetrics
    (
    -- * Creating a request
      GetCrawlerMetrics (..)
    , mkGetCrawlerMetrics
    -- ** Request lenses
    , gcmCrawlerNameList
    , gcmMaxResults
    , gcmNextToken

    -- * Destructuring the response
    , GetCrawlerMetricsResponse (..)
    , mkGetCrawlerMetricsResponse
    -- ** Response lenses
    , gcmrrsCrawlerMetricsList
    , gcmrrsNextToken
    , gcmrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCrawlerMetrics' smart constructor.
data GetCrawlerMetrics = GetCrawlerMetrics'
  { crawlerNameList :: Core.Maybe [Types.NameString]
    -- ^ A list of the names of crawlers about which to retrieve metrics.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum size of a list to return.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if this is a continuation call.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawlerMetrics' value with any optional fields omitted.
mkGetCrawlerMetrics
    :: GetCrawlerMetrics
mkGetCrawlerMetrics
  = GetCrawlerMetrics'{crawlerNameList = Core.Nothing,
                       maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | A list of the names of crawlers about which to retrieve metrics.
--
-- /Note:/ Consider using 'crawlerNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmCrawlerNameList :: Lens.Lens' GetCrawlerMetrics (Core.Maybe [Types.NameString])
gcmCrawlerNameList = Lens.field @"crawlerNameList"
{-# INLINEABLE gcmCrawlerNameList #-}
{-# DEPRECATED crawlerNameList "Use generic-lens or generic-optics with 'crawlerNameList' instead"  #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmMaxResults :: Lens.Lens' GetCrawlerMetrics (Core.Maybe Core.Natural)
gcmMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gcmMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmNextToken :: Lens.Lens' GetCrawlerMetrics (Core.Maybe Types.Token)
gcmNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcmNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetCrawlerMetrics where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCrawlerMetrics where
        toHeaders GetCrawlerMetrics{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetCrawlerMetrics") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCrawlerMetrics where
        toJSON GetCrawlerMetrics{..}
          = Core.object
              (Core.catMaybes
                 [("CrawlerNameList" Core..=) Core.<$> crawlerNameList,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetCrawlerMetrics where
        type Rs GetCrawlerMetrics = GetCrawlerMetricsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCrawlerMetricsResponse' Core.<$>
                   (x Core..:? "CrawlerMetricsList") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetCrawlerMetrics where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"crawlerMetricsList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkGetCrawlerMetricsResponse' smart constructor.
data GetCrawlerMetricsResponse = GetCrawlerMetricsResponse'
  { crawlerMetricsList :: Core.Maybe [Types.CrawlerMetrics]
    -- ^ A list of metrics for the specified crawler.
  , nextToken :: Core.Maybe Types.Token
    -- ^ A continuation token, if the returned list does not contain the last metric available.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawlerMetricsResponse' value with any optional fields omitted.
mkGetCrawlerMetricsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCrawlerMetricsResponse
mkGetCrawlerMetricsResponse responseStatus
  = GetCrawlerMetricsResponse'{crawlerMetricsList = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | A list of metrics for the specified crawler.
--
-- /Note:/ Consider using 'crawlerMetricsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsCrawlerMetricsList :: Lens.Lens' GetCrawlerMetricsResponse (Core.Maybe [Types.CrawlerMetrics])
gcmrrsCrawlerMetricsList = Lens.field @"crawlerMetricsList"
{-# INLINEABLE gcmrrsCrawlerMetricsList #-}
{-# DEPRECATED crawlerMetricsList "Use generic-lens or generic-optics with 'crawlerMetricsList' instead"  #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsNextToken :: Lens.Lens' GetCrawlerMetricsResponse (Core.Maybe Types.Token)
gcmrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcmrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsResponseStatus :: Lens.Lens' GetCrawlerMetricsResponse Core.Int
gcmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
