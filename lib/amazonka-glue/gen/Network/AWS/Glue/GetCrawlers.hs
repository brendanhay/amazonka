{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for all crawlers defined in the customer account.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetCrawlers
  ( -- * Creating a request
    GetCrawlers (..),
    mkGetCrawlers,

    -- ** Request lenses
    gMaxResults,
    gNextToken,

    -- * Destructuring the response
    GetCrawlersResponse (..),
    mkGetCrawlersResponse,

    -- ** Response lenses
    grsCrawlers,
    grsNextToken,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCrawlers' smart constructor.
data GetCrawlers = GetCrawlers'
  { -- | The number of crawlers to return on each call.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawlers' value with any optional fields omitted.
mkGetCrawlers ::
  GetCrawlers
mkGetCrawlers =
  GetCrawlers' {maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The number of crawlers to return on each call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gMaxResults :: Lens.Lens' GetCrawlers (Core.Maybe Core.Natural)
gMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gNextToken :: Lens.Lens' GetCrawlers (Core.Maybe Types.NextToken)
gNextToken = Lens.field @"nextToken"
{-# DEPRECATED gNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetCrawlers where
  toJSON GetCrawlers {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetCrawlers where
  type Rs GetCrawlers = GetCrawlersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetCrawlers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlersResponse'
            Core.<$> (x Core..:? "Crawlers")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetCrawlers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"crawlers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetCrawlersResponse' smart constructor.
data GetCrawlersResponse = GetCrawlersResponse'
  { -- | A list of crawler metadata.
    crawlers :: Core.Maybe [Types.Crawler],
    -- | A continuation token, if the returned list has not reached the end of those defined in this customer account.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCrawlersResponse' value with any optional fields omitted.
mkGetCrawlersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCrawlersResponse
mkGetCrawlersResponse responseStatus =
  GetCrawlersResponse'
    { crawlers = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of crawler metadata.
--
-- /Note:/ Consider using 'crawlers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCrawlers :: Lens.Lens' GetCrawlersResponse (Core.Maybe [Types.Crawler])
grsCrawlers = Lens.field @"crawlers"
{-# DEPRECATED grsCrawlers "Use generic-lens or generic-optics with 'crawlers' instead." #-}

-- | A continuation token, if the returned list has not reached the end of those defined in this customer account.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsNextToken :: Lens.Lens' GetCrawlersResponse (Core.Maybe Types.NextToken)
grsNextToken = Lens.field @"nextToken"
{-# DEPRECATED grsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCrawlersResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
