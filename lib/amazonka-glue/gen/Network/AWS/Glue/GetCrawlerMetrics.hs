{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    GetCrawlerMetrics (..),
    mkGetCrawlerMetrics,

    -- ** Request lenses
    gcmCrawlerNameList,
    gcmMaxResults,
    gcmNextToken,

    -- * Destructuring the response
    GetCrawlerMetricsResponse (..),
    mkGetCrawlerMetricsResponse,

    -- ** Response lenses
    gcmrrsCrawlerMetricsList,
    gcmrrsNextToken,
    gcmrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCrawlerMetrics' smart constructor.
data GetCrawlerMetrics = GetCrawlerMetrics'
  { -- | A list of the names of crawlers about which to retrieve metrics.
    crawlerNameList :: Core.Maybe [Types.NameString],
    -- | The maximum size of a list to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawlerMetrics' value with any optional fields omitted.
mkGetCrawlerMetrics ::
  GetCrawlerMetrics
mkGetCrawlerMetrics =
  GetCrawlerMetrics'
    { crawlerNameList = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A list of the names of crawlers about which to retrieve metrics.
--
-- /Note:/ Consider using 'crawlerNameList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmCrawlerNameList :: Lens.Lens' GetCrawlerMetrics (Core.Maybe [Types.NameString])
gcmCrawlerNameList = Lens.field @"crawlerNameList"
{-# DEPRECATED gcmCrawlerNameList "Use generic-lens or generic-optics with 'crawlerNameList' instead." #-}

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmMaxResults :: Lens.Lens' GetCrawlerMetrics (Core.Maybe Core.Natural)
gcmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gcmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmNextToken :: Lens.Lens' GetCrawlerMetrics (Core.Maybe Types.Token)
gcmNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetCrawlerMetrics where
  toJSON GetCrawlerMetrics {..} =
    Core.object
      ( Core.catMaybes
          [ ("CrawlerNameList" Core..=) Core.<$> crawlerNameList,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetCrawlerMetrics where
  type Rs GetCrawlerMetrics = GetCrawlerMetricsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetCrawlerMetrics")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlerMetricsResponse'
            Core.<$> (x Core..:? "CrawlerMetricsList")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetCrawlerMetrics where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"crawlerMetricsList" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetCrawlerMetricsResponse' smart constructor.
data GetCrawlerMetricsResponse = GetCrawlerMetricsResponse'
  { -- | A list of metrics for the specified crawler.
    crawlerMetricsList :: Core.Maybe [Types.CrawlerMetrics],
    -- | A continuation token, if the returned list does not contain the last metric available.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawlerMetricsResponse' value with any optional fields omitted.
mkGetCrawlerMetricsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCrawlerMetricsResponse
mkGetCrawlerMetricsResponse responseStatus =
  GetCrawlerMetricsResponse'
    { crawlerMetricsList = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of metrics for the specified crawler.
--
-- /Note:/ Consider using 'crawlerMetricsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsCrawlerMetricsList :: Lens.Lens' GetCrawlerMetricsResponse (Core.Maybe [Types.CrawlerMetrics])
gcmrrsCrawlerMetricsList = Lens.field @"crawlerMetricsList"
{-# DEPRECATED gcmrrsCrawlerMetricsList "Use generic-lens or generic-optics with 'crawlerMetricsList' instead." #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsNextToken :: Lens.Lens' GetCrawlerMetricsResponse (Core.Maybe Types.Token)
gcmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gcmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcmrrsResponseStatus :: Lens.Lens' GetCrawlerMetricsResponse Core.Int
gcmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
