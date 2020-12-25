{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.ListCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the names of all crawler resources in this AWS account, or the resources with the specified tag. This operation allows you to see which resources are available in your account, and their names.
--
-- This operation takes the optional @Tags@ field, which you can use as a filter on the response so that tagged resources can be retrieved as a group. If you choose to use tags filtering, only resources with the tag are retrieved.
module Network.AWS.Glue.ListCrawlers
  ( -- * Creating a request
    ListCrawlers (..),
    mkListCrawlers,

    -- ** Request lenses
    lcMaxResults,
    lcNextToken,
    lcTags,

    -- * Destructuring the response
    ListCrawlersResponse (..),
    mkListCrawlersResponse,

    -- ** Response lenses
    lcrrsCrawlerNames,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCrawlers' smart constructor.
data ListCrawlers = ListCrawlers'
  { -- | The maximum size of a list to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | A continuation token, if this is a continuation request.
    nextToken :: Core.Maybe Types.Token,
    -- | Specifies to return only these tagged resources.
    tags :: Core.Maybe (Core.HashMap Types.TagKey Types.TagValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCrawlers' value with any optional fields omitted.
mkListCrawlers ::
  ListCrawlers
mkListCrawlers =
  ListCrawlers'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      tags = Core.Nothing
    }

-- | The maximum size of a list to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListCrawlers (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A continuation token, if this is a continuation request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListCrawlers (Core.Maybe Types.Token)
lcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies to return only these tagged resources.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcTags :: Lens.Lens' ListCrawlers (Core.Maybe (Core.HashMap Types.TagKey Types.TagValue))
lcTags = Lens.field @"tags"
{-# DEPRECATED lcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON ListCrawlers where
  toJSON ListCrawlers {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest ListCrawlers where
  type Rs ListCrawlers = ListCrawlersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.ListCrawlers")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCrawlersResponse'
            Core.<$> (x Core..:? "CrawlerNames")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListCrawlersResponse' smart constructor.
data ListCrawlersResponse = ListCrawlersResponse'
  { -- | The names of all crawlers in the account, or the crawlers with the specified tags.
    crawlerNames :: Core.Maybe [Types.NameString],
    -- | A continuation token, if the returned list does not contain the last metric available.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCrawlersResponse' value with any optional fields omitted.
mkListCrawlersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCrawlersResponse
mkListCrawlersResponse responseStatus =
  ListCrawlersResponse'
    { crawlerNames = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The names of all crawlers in the account, or the crawlers with the specified tags.
--
-- /Note:/ Consider using 'crawlerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsCrawlerNames :: Lens.Lens' ListCrawlersResponse (Core.Maybe [Types.NameString])
lcrrsCrawlerNames = Lens.field @"crawlerNames"
{-# DEPRECATED lcrrsCrawlerNames "Use generic-lens or generic-optics with 'crawlerNames' instead." #-}

-- | A continuation token, if the returned list does not contain the last metric available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListCrawlersResponse (Core.Maybe Types.Token)
lcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListCrawlersResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
