{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteCrawler
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a specified crawler from the AWS Glue Data Catalog, unless the crawler state is @RUNNING@ .
module Network.AWS.Glue.DeleteCrawler
    (
    -- * Creating a request
      DeleteCrawler (..)
    , mkDeleteCrawler
    -- ** Request lenses
    , dcName

    -- * Destructuring the response
    , DeleteCrawlerResponse (..)
    , mkDeleteCrawlerResponse
    -- ** Response lenses
    , dcrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCrawler' smart constructor.
newtype DeleteCrawler = DeleteCrawler'
  { name :: Types.Name
    -- ^ The name of the crawler to remove.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCrawler' value with any optional fields omitted.
mkDeleteCrawler
    :: Types.Name -- ^ 'name'
    -> DeleteCrawler
mkDeleteCrawler name = DeleteCrawler'{name}

-- | The name of the crawler to remove.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DeleteCrawler Types.Name
dcName = Lens.field @"name"
{-# INLINEABLE dcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

instance Core.ToQuery DeleteCrawler where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCrawler where
        toHeaders DeleteCrawler{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.DeleteCrawler") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCrawler where
        toJSON DeleteCrawler{..}
          = Core.object (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.AWSRequest DeleteCrawler where
        type Rs DeleteCrawler = DeleteCrawlerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteCrawlerResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCrawlerResponse' smart constructor.
newtype DeleteCrawlerResponse = DeleteCrawlerResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCrawlerResponse' value with any optional fields omitted.
mkDeleteCrawlerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCrawlerResponse
mkDeleteCrawlerResponse responseStatus
  = DeleteCrawlerResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrfrsResponseStatus :: Lens.Lens' DeleteCrawlerResponse Core.Int
dcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
