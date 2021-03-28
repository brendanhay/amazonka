{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetDomainNames
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of 'DomainName' resources.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetDomainNames
    (
    -- * Creating a request
      GetDomainNames (..)
    , mkGetDomainNames
    -- ** Request lenses
    , gdnLimit
    , gdnPosition

    -- * Destructuring the response
    , GetDomainNamesResponse (..)
    , mkGetDomainNamesResponse
    -- ** Response lenses
    , gdnrrsItems
    , gdnrrsPosition
    , gdnrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe a collection of 'DomainName' resources.
--
-- /See:/ 'mkGetDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetDomainNames' value with any optional fields omitted.
mkGetDomainNames
    :: GetDomainNames
mkGetDomainNames
  = GetDomainNames'{limit = Core.Nothing, position = Core.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnLimit :: Lens.Lens' GetDomainNames (Core.Maybe Core.Int)
gdnLimit = Lens.field @"limit"
{-# INLINEABLE gdnLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnPosition :: Lens.Lens' GetDomainNames (Core.Maybe Core.Text)
gdnPosition = Lens.field @"position"
{-# INLINEABLE gdnPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetDomainNames where
        toQuery GetDomainNames{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetDomainNames where
        toHeaders GetDomainNames{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetDomainNames where
        type Rs GetDomainNames = GetDomainNamesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/domainnames",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetDomainNamesResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetDomainNames where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of 'DomainName' resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Client-Side Certificate> 
--
-- /See:/ 'mkGetDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { items :: Core.Maybe [Types.DomainName]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetDomainNamesResponse' value with any optional fields omitted.
mkGetDomainNamesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetDomainNamesResponse
mkGetDomainNamesResponse responseStatus
  = GetDomainNamesResponse'{items = Core.Nothing,
                            position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnrrsItems :: Lens.Lens' GetDomainNamesResponse (Core.Maybe [Types.DomainName])
gdnrrsItems = Lens.field @"items"
{-# INLINEABLE gdnrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnrrsPosition :: Lens.Lens' GetDomainNamesResponse (Core.Maybe Core.Text)
gdnrrsPosition = Lens.field @"position"
{-# INLINEABLE gdnrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gdnrrsResponseStatus :: Lens.Lens' GetDomainNamesResponse Core.Int
gdnrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gdnrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
