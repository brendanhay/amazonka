{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetVpcLinks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the 'VpcLinks' collection under the caller's account in a selected region.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetVpcLinks
    (
    -- * Creating a request
      GetVpcLinks (..)
    , mkGetVpcLinks
    -- ** Request lenses
    , gvlLimit
    , gvlPosition

    -- * Destructuring the response
    , GetVpcLinksResponse (..)
    , mkGetVpcLinksResponse
    -- ** Response lenses
    , gvlrrsItems
    , gvlrrsPosition
    , gvlrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Gets the 'VpcLinks' collection under the caller's account in a selected region.
--
-- /See:/ 'mkGetVpcLinks' smart constructor.
data GetVpcLinks = GetVpcLinks'
  { limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVpcLinks' value with any optional fields omitted.
mkGetVpcLinks
    :: GetVpcLinks
mkGetVpcLinks
  = GetVpcLinks'{limit = Core.Nothing, position = Core.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlLimit :: Lens.Lens' GetVpcLinks (Core.Maybe Core.Int)
gvlLimit = Lens.field @"limit"
{-# INLINEABLE gvlLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlPosition :: Lens.Lens' GetVpcLinks (Core.Maybe Core.Text)
gvlPosition = Lens.field @"position"
{-# INLINEABLE gvlPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetVpcLinks where
        toQuery GetVpcLinks{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetVpcLinks where
        toHeaders GetVpcLinks{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetVpcLinks where
        type Rs GetVpcLinks = GetVpcLinksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/vpclinks",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetVpcLinksResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetVpcLinks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | The collection of VPC links under the caller's account in a region.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-with-private-integration.html Getting Started with Private Integrations> , <https://docs.aws.amazon.com/apigateway/latest/developerguide/set-up-private-integration.html Set up Private Integrations> 
--
-- /See:/ 'mkGetVpcLinksResponse' smart constructor.
data GetVpcLinksResponse = GetVpcLinksResponse'
  { items :: Core.Maybe [Types.VpcLink]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetVpcLinksResponse' value with any optional fields omitted.
mkGetVpcLinksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetVpcLinksResponse
mkGetVpcLinksResponse responseStatus
  = GetVpcLinksResponse'{items = Core.Nothing,
                         position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsItems :: Lens.Lens' GetVpcLinksResponse (Core.Maybe [Types.VpcLink])
gvlrrsItems = Lens.field @"items"
{-# INLINEABLE gvlrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsPosition :: Lens.Lens' GetVpcLinksResponse (Core.Maybe Core.Text)
gvlrrsPosition = Lens.field @"position"
{-# INLINEABLE gvlrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gvlrrsResponseStatus :: Lens.Lens' GetVpcLinksResponse Core.Int
gvlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gvlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
