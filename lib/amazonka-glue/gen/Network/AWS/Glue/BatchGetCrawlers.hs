{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.BatchGetCrawlers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of resource metadata for a given list of crawler names. After calling the @ListCrawlers@ operation, you can call this operation to access the data to which you have been granted permissions. This operation supports all IAM permissions, including permission conditions that uses tags.
module Network.AWS.Glue.BatchGetCrawlers
    (
    -- * Creating a request
      BatchGetCrawlers (..)
    , mkBatchGetCrawlers
    -- ** Request lenses
    , bgcCrawlerNames

    -- * Destructuring the response
    , BatchGetCrawlersResponse (..)
    , mkBatchGetCrawlersResponse
    -- ** Response lenses
    , bgcrrsCrawlers
    , bgcrrsCrawlersNotFound
    , bgcrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkBatchGetCrawlers' smart constructor.
newtype BatchGetCrawlers = BatchGetCrawlers'
  { crawlerNames :: [Types.NameString]
    -- ^ A list of crawler names, which might be the names returned from the @ListCrawlers@ operation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'BatchGetCrawlers' value with any optional fields omitted.
mkBatchGetCrawlers
    :: BatchGetCrawlers
mkBatchGetCrawlers = BatchGetCrawlers'{crawlerNames = Core.mempty}

-- | A list of crawler names, which might be the names returned from the @ListCrawlers@ operation.
--
-- /Note:/ Consider using 'crawlerNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcCrawlerNames :: Lens.Lens' BatchGetCrawlers [Types.NameString]
bgcCrawlerNames = Lens.field @"crawlerNames"
{-# INLINEABLE bgcCrawlerNames #-}
{-# DEPRECATED crawlerNames "Use generic-lens or generic-optics with 'crawlerNames' instead"  #-}

instance Core.ToQuery BatchGetCrawlers where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchGetCrawlers where
        toHeaders BatchGetCrawlers{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.BatchGetCrawlers") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchGetCrawlers where
        toJSON BatchGetCrawlers{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CrawlerNames" Core..= crawlerNames)])

instance Core.AWSRequest BatchGetCrawlers where
        type Rs BatchGetCrawlers = BatchGetCrawlersResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchGetCrawlersResponse' Core.<$>
                   (x Core..:? "Crawlers") Core.<*> x Core..:? "CrawlersNotFound"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkBatchGetCrawlersResponse' smart constructor.
data BatchGetCrawlersResponse = BatchGetCrawlersResponse'
  { crawlers :: Core.Maybe [Types.Crawler]
    -- ^ A list of crawler definitions.
  , crawlersNotFound :: Core.Maybe [Types.NameString]
    -- ^ A list of names of crawlers that were not found.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'BatchGetCrawlersResponse' value with any optional fields omitted.
mkBatchGetCrawlersResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchGetCrawlersResponse
mkBatchGetCrawlersResponse responseStatus
  = BatchGetCrawlersResponse'{crawlers = Core.Nothing,
                              crawlersNotFound = Core.Nothing, responseStatus}

-- | A list of crawler definitions.
--
-- /Note:/ Consider using 'crawlers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrrsCrawlers :: Lens.Lens' BatchGetCrawlersResponse (Core.Maybe [Types.Crawler])
bgcrrsCrawlers = Lens.field @"crawlers"
{-# INLINEABLE bgcrrsCrawlers #-}
{-# DEPRECATED crawlers "Use generic-lens or generic-optics with 'crawlers' instead"  #-}

-- | A list of names of crawlers that were not found.
--
-- /Note:/ Consider using 'crawlersNotFound' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrrsCrawlersNotFound :: Lens.Lens' BatchGetCrawlersResponse (Core.Maybe [Types.NameString])
bgcrrsCrawlersNotFound = Lens.field @"crawlersNotFound"
{-# INLINEABLE bgcrrsCrawlersNotFound #-}
{-# DEPRECATED crawlersNotFound "Use generic-lens or generic-optics with 'crawlersNotFound' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgcrrsResponseStatus :: Lens.Lens' BatchGetCrawlersResponse Core.Int
bgcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE bgcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
