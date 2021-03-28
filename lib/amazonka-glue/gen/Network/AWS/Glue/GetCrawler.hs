{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCrawler (..)
    , mkGetCrawler
    -- ** Request lenses
    , gcgName

    -- * Destructuring the response
    , GetCrawlerResponse (..)
    , mkGetCrawlerResponse
    -- ** Response lenses
    , gcrlrsCrawler
    , gcrlrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCrawler' smart constructor.
newtype GetCrawler = GetCrawler'
  { name :: Types.Name
    -- ^ The name of the crawler to retrieve metadata for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCrawler' value with any optional fields omitted.
mkGetCrawler
    :: Types.Name -- ^ 'name'
    -> GetCrawler
mkGetCrawler name = GetCrawler'{name}

-- | The name of the crawler to retrieve metadata for.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcgName :: Lens.Lens' GetCrawler Types.Name
gcgName = Lens.field @"name"
{-# INLINEABLE gcgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery GetCrawler where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCrawler where
        toHeaders GetCrawler{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.GetCrawler") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCrawler where
        toJSON GetCrawler{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest GetCrawler where
        type Rs GetCrawler = GetCrawlerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCrawlerResponse' Core.<$>
                   (x Core..:? "Crawler") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCrawlerResponse' smart constructor.
data GetCrawlerResponse = GetCrawlerResponse'
  { crawler :: Core.Maybe Types.Crawler
    -- ^ The metadata for the specified crawler.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetCrawlerResponse' value with any optional fields omitted.
mkGetCrawlerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCrawlerResponse
mkGetCrawlerResponse responseStatus
  = GetCrawlerResponse'{crawler = Core.Nothing, responseStatus}

-- | The metadata for the specified crawler.
--
-- /Note:/ Consider using 'crawler' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrlrsCrawler :: Lens.Lens' GetCrawlerResponse (Core.Maybe Types.Crawler)
gcrlrsCrawler = Lens.field @"crawler"
{-# INLINEABLE gcrlrsCrawler #-}
{-# DEPRECATED crawler "Use generic-lens or generic-optics with 'crawler' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrlrsResponseStatus :: Lens.Lens' GetCrawlerResponse Core.Int
gcrlrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrlrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
