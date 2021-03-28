{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetUsagePlans
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plans of the caller's account.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetUsagePlans
    (
    -- * Creating a request
      GetUsagePlans (..)
    , mkGetUsagePlans
    -- ** Request lenses
    , gupKeyId
    , gupLimit
    , gupPosition

    -- * Destructuring the response
    , GetUsagePlansResponse (..)
    , mkGetUsagePlansResponse
    -- ** Response lenses
    , guprrsItems
    , guprrsPosition
    , guprrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get all the usage plans of the caller's account.
--
-- /See:/ 'mkGetUsagePlans' smart constructor.
data GetUsagePlans = GetUsagePlans'
  { keyId :: Core.Maybe Core.Text
    -- ^ The identifier of the API key associated with the usage plans.
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsagePlans' value with any optional fields omitted.
mkGetUsagePlans
    :: GetUsagePlans
mkGetUsagePlans
  = GetUsagePlans'{keyId = Core.Nothing, limit = Core.Nothing,
                   position = Core.Nothing}

-- | The identifier of the API key associated with the usage plans.
--
-- /Note:/ Consider using 'keyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupKeyId :: Lens.Lens' GetUsagePlans (Core.Maybe Core.Text)
gupKeyId = Lens.field @"keyId"
{-# INLINEABLE gupKeyId #-}
{-# DEPRECATED keyId "Use generic-lens or generic-optics with 'keyId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupLimit :: Lens.Lens' GetUsagePlans (Core.Maybe Core.Int)
gupLimit = Lens.field @"limit"
{-# INLINEABLE gupLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupPosition :: Lens.Lens' GetUsagePlans (Core.Maybe Core.Text)
gupPosition = Lens.field @"position"
{-# INLINEABLE gupPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetUsagePlans where
        toQuery GetUsagePlans{..}
          = Core.maybe Core.mempty (Core.toQueryPair "keyId") keyId Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "limit") limit
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetUsagePlans where
        toHeaders GetUsagePlans{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetUsagePlans where
        type Rs GetUsagePlans = GetUsagePlansResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/usageplans",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUsagePlansResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetUsagePlans where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents a collection of usage plans for an AWS account.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> 
--
-- /See:/ 'mkGetUsagePlansResponse' smart constructor.
data GetUsagePlansResponse = GetUsagePlansResponse'
  { items :: Core.Maybe [Types.UsagePlan]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsagePlansResponse' value with any optional fields omitted.
mkGetUsagePlansResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetUsagePlansResponse
mkGetUsagePlansResponse responseStatus
  = GetUsagePlansResponse'{items = Core.Nothing,
                           position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsItems :: Lens.Lens' GetUsagePlansResponse (Core.Maybe [Types.UsagePlan])
guprrsItems = Lens.field @"items"
{-# INLINEABLE guprrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsPosition :: Lens.Lens' GetUsagePlansResponse (Core.Maybe Core.Text)
guprrsPosition = Lens.field @"position"
{-# INLINEABLE guprrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
guprrsResponseStatus :: Lens.Lens' GetUsagePlansResponse Core.Int
guprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE guprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
