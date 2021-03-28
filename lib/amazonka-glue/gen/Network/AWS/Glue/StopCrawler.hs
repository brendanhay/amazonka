{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If the specified crawler is running, stops the crawl.
module Network.AWS.Glue.StopCrawler
    (
    -- * Creating a request
      StopCrawler (..)
    , mkStopCrawler
    -- ** Request lenses
    , scgName

    -- * Destructuring the response
    , StopCrawlerResponse (..)
    , mkStopCrawlerResponse
    -- ** Response lenses
    , srsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopCrawler' smart constructor.
newtype StopCrawler = StopCrawler'
  { name :: Types.Name
    -- ^ Name of the crawler to stop.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCrawler' value with any optional fields omitted.
mkStopCrawler
    :: Types.Name -- ^ 'name'
    -> StopCrawler
mkStopCrawler name = StopCrawler'{name}

-- | Name of the crawler to stop.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scgName :: Lens.Lens' StopCrawler Types.Name
scgName = Lens.field @"name"
{-# INLINEABLE scgName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery StopCrawler where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopCrawler where
        toHeaders StopCrawler{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StopCrawler") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopCrawler where
        toJSON StopCrawler{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest StopCrawler where
        type Rs StopCrawler = StopCrawlerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopCrawlerResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopCrawlerResponse' smart constructor.
newtype StopCrawlerResponse = StopCrawlerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCrawlerResponse' value with any optional fields omitted.
mkStopCrawlerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopCrawlerResponse
mkStopCrawlerResponse responseStatus
  = StopCrawlerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srsResponseStatus :: Lens.Lens' StopCrawlerResponse Core.Int
srsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE srsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
