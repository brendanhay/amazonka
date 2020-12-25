{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata for a specified crawler.
module Network.AWS.Glue.GetCrawler
  ( -- * Creating a request
    GetCrawler (..),
    mkGetCrawler,

    -- ** Request lenses
    gcgName,

    -- * Destructuring the response
    GetCrawlerResponse (..),
    mkGetCrawlerResponse,

    -- ** Response lenses
    gcrlrsCrawler,
    gcrlrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCrawler' smart constructor.
newtype GetCrawler = GetCrawler'
  { -- | The name of the crawler to retrieve metadata for.
    name :: Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawler' value with any optional fields omitted.
mkGetCrawler ::
  -- | 'name'
  Types.Name ->
  GetCrawler
mkGetCrawler name = GetCrawler' {name}

-- | The name of the crawler to retrieve metadata for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcgName :: Lens.Lens' GetCrawler Types.Name
gcgName = Lens.field @"name"
{-# DEPRECATED gcgName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON GetCrawler where
  toJSON GetCrawler {..} =
    Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetCrawler where
  type Rs GetCrawler = GetCrawlerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.GetCrawler")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCrawlerResponse'
            Core.<$> (x Core..:? "Crawler") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'
  { -- | The metadata for the specified crawler.
    crawler :: Core.Maybe Types.Crawler,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetCrawlerResponse' value with any optional fields omitted.
mkGetCrawlerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCrawlerResponse
mkGetCrawlerResponse responseStatus =
  GetCrawlerResponse' {crawler = Core.Nothing, responseStatus}

-- | The metadata for the specified crawler.
--
-- /Note:/ Consider using 'crawler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrlrsCrawler :: Lens.Lens' GetCrawlerResponse (Core.Maybe Types.Crawler)
gcrlrsCrawler = Lens.field @"crawler"
{-# DEPRECATED gcrlrsCrawler "Use generic-lens or generic-optics with 'crawler' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrlrsResponseStatus :: Lens.Lens' GetCrawlerResponse Core.Int
gcrlrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
