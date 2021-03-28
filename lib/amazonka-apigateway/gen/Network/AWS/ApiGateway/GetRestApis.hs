{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetRestApis
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the 'RestApis' resources for your collection.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetRestApis
    (
    -- * Creating a request
      GetRestApis (..)
    , mkGetRestApis
    -- ** Request lenses
    , graLimit
    , graPosition

    -- * Destructuring the response
    , GetRestApisResponse (..)
    , mkGetRestApisResponse
    -- ** Response lenses
    , grarrsItems
    , grarrsPosition
    , grarrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to list existing 'RestApis' defined for your collection.
--
-- /See:/ 'mkGetRestApis' smart constructor.
data GetRestApis = GetRestApis'
  { limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRestApis' value with any optional fields omitted.
mkGetRestApis
    :: GetRestApis
mkGetRestApis
  = GetRestApis'{limit = Core.Nothing, position = Core.Nothing}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
graLimit :: Lens.Lens' GetRestApis (Core.Maybe Core.Int)
graLimit = Lens.field @"limit"
{-# INLINEABLE graLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
graPosition :: Lens.Lens' GetRestApis (Core.Maybe Core.Text)
graPosition = Lens.field @"position"
{-# INLINEABLE graPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetRestApis where
        toQuery GetRestApis{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetRestApis where
        toHeaders GetRestApis{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetRestApis where
        type Rs GetRestApis = GetRestApisResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/restapis",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRestApisResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetRestApis where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Contains references to your APIs and links that guide you in how to interact with your collection. A collection offers a paginated view of your APIs.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-create-api.html Create an API> 
--
-- /See:/ 'mkGetRestApisResponse' smart constructor.
data GetRestApisResponse = GetRestApisResponse'
  { items :: Core.Maybe [Types.RestApi]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetRestApisResponse' value with any optional fields omitted.
mkGetRestApisResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRestApisResponse
mkGetRestApisResponse responseStatus
  = GetRestApisResponse'{items = Core.Nothing,
                         position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grarrsItems :: Lens.Lens' GetRestApisResponse (Core.Maybe [Types.RestApi])
grarrsItems = Lens.field @"items"
{-# INLINEABLE grarrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grarrsPosition :: Lens.Lens' GetRestApisResponse (Core.Maybe Core.Text)
grarrsPosition = Lens.field @"position"
{-# INLINEABLE grarrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grarrsResponseStatus :: Lens.Lens' GetRestApisResponse Core.Int
grarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
