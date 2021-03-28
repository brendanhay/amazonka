{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetBasePathMappings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of 'BasePathMapping' resources.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetBasePathMappings
    (
    -- * Creating a request
      GetBasePathMappings (..)
    , mkGetBasePathMappings
    -- ** Request lenses
    , gDomainName
    , gLimit
    , gPosition

    -- * Destructuring the response
    , GetBasePathMappingsResponse (..)
    , mkGetBasePathMappingsResponse
    -- ** Response lenses
    , gbpmrrsItems
    , gbpmrrsPosition
    , gbpmrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about a collection of 'BasePathMapping' resources.
--
-- /See:/ 'mkGetBasePathMappings' smart constructor.
data GetBasePathMappings = GetBasePathMappings'
  { domainName :: Core.Text
    -- ^ [Required] The domain name of a 'BasePathMapping' resource.
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBasePathMappings' value with any optional fields omitted.
mkGetBasePathMappings
    :: Core.Text -- ^ 'domainName'
    -> GetBasePathMappings
mkGetBasePathMappings domainName
  = GetBasePathMappings'{domainName, limit = Core.Nothing,
                         position = Core.Nothing}

-- | [Required] The domain name of a 'BasePathMapping' resource.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gDomainName :: Lens.Lens' GetBasePathMappings Core.Text
gDomainName = Lens.field @"domainName"
{-# INLINEABLE gDomainName #-}
{-# DEPRECATED domainName "Use generic-lens or generic-optics with 'domainName' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gLimit :: Lens.Lens' GetBasePathMappings (Core.Maybe Core.Int)
gLimit = Lens.field @"limit"
{-# INLINEABLE gLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gPosition :: Lens.Lens' GetBasePathMappings (Core.Maybe Core.Text)
gPosition = Lens.field @"position"
{-# INLINEABLE gPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetBasePathMappings where
        toQuery GetBasePathMappings{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetBasePathMappings where
        toHeaders GetBasePathMappings{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetBasePathMappings where
        type Rs GetBasePathMappings = GetBasePathMappingsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/domainnames/" Core.<> Core.toText domainName Core.<>
                             "/basepathmappings",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetBasePathMappingsResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetBasePathMappings where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of 'BasePathMapping' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names> 
--
-- /See:/ 'mkGetBasePathMappingsResponse' smart constructor.
data GetBasePathMappingsResponse = GetBasePathMappingsResponse'
  { items :: Core.Maybe [Types.BasePathMapping]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetBasePathMappingsResponse' value with any optional fields omitted.
mkGetBasePathMappingsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetBasePathMappingsResponse
mkGetBasePathMappingsResponse responseStatus
  = GetBasePathMappingsResponse'{items = Core.Nothing,
                                 position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmrrsItems :: Lens.Lens' GetBasePathMappingsResponse (Core.Maybe [Types.BasePathMapping])
gbpmrrsItems = Lens.field @"items"
{-# INLINEABLE gbpmrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmrrsPosition :: Lens.Lens' GetBasePathMappingsResponse (Core.Maybe Core.Text)
gbpmrrsPosition = Lens.field @"position"
{-# INLINEABLE gbpmrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gbpmrrsResponseStatus :: Lens.Lens' GetBasePathMappingsResponse Core.Int
gbpmrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gbpmrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
