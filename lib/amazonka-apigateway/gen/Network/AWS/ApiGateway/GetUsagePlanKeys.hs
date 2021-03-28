{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.GetUsagePlanKeys
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all the usage plan keys representing the API keys added to a specified usage plan.
--
-- This operation returns paginated results.
module Network.AWS.ApiGateway.GetUsagePlanKeys
    (
    -- * Creating a request
      GetUsagePlanKeys (..)
    , mkGetUsagePlanKeys
    -- ** Request lenses
    , gupkUsagePlanId
    , gupkLimit
    , gupkNameQuery
    , gupkPosition

    -- * Destructuring the response
    , GetUsagePlanKeysResponse (..)
    , mkGetUsagePlanKeysResponse
    -- ** Response lenses
    , gupkrrsItems
    , gupkrrsPosition
    , gupkrrsResponseStatus
    ) where

import qualified Network.AWS.ApiGateway.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The GET request to get all the usage plan keys representing the API keys added to a specified usage plan.
--
-- /See:/ 'mkGetUsagePlanKeys' smart constructor.
data GetUsagePlanKeys = GetUsagePlanKeys'
  { usagePlanId :: Core.Text
    -- ^ [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
  , limit :: Core.Maybe Core.Int
    -- ^ The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
  , nameQuery :: Core.Maybe Core.Text
    -- ^ A query parameter specifying the name of the to-be-returned usage plan keys.
  , position :: Core.Maybe Core.Text
    -- ^ The current pagination position in the paged result set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsagePlanKeys' value with any optional fields omitted.
mkGetUsagePlanKeys
    :: Core.Text -- ^ 'usagePlanId'
    -> GetUsagePlanKeys
mkGetUsagePlanKeys usagePlanId
  = GetUsagePlanKeys'{usagePlanId, limit = Core.Nothing,
                      nameQuery = Core.Nothing, position = Core.Nothing}

-- | [Required] The Id of the 'UsagePlan' resource representing the usage plan containing the to-be-retrieved 'UsagePlanKey' resource representing a plan customer.
--
-- /Note:/ Consider using 'usagePlanId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkUsagePlanId :: Lens.Lens' GetUsagePlanKeys Core.Text
gupkUsagePlanId = Lens.field @"usagePlanId"
{-# INLINEABLE gupkUsagePlanId #-}
{-# DEPRECATED usagePlanId "Use generic-lens or generic-optics with 'usagePlanId' instead"  #-}

-- | The maximum number of returned results per page. The default value is 25 and the maximum value is 500.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkLimit :: Lens.Lens' GetUsagePlanKeys (Core.Maybe Core.Int)
gupkLimit = Lens.field @"limit"
{-# INLINEABLE gupkLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A query parameter specifying the name of the to-be-returned usage plan keys.
--
-- /Note:/ Consider using 'nameQuery' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkNameQuery :: Lens.Lens' GetUsagePlanKeys (Core.Maybe Core.Text)
gupkNameQuery = Lens.field @"nameQuery"
{-# INLINEABLE gupkNameQuery #-}
{-# DEPRECATED nameQuery "Use generic-lens or generic-optics with 'nameQuery' instead"  #-}

-- | The current pagination position in the paged result set.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkPosition :: Lens.Lens' GetUsagePlanKeys (Core.Maybe Core.Text)
gupkPosition = Lens.field @"position"
{-# INLINEABLE gupkPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

instance Core.ToQuery GetUsagePlanKeys where
        toQuery GetUsagePlanKeys{..}
          = Core.maybe Core.mempty (Core.toQueryPair "limit") limit Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "name") nameQuery
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "position") position

instance Core.ToHeaders GetUsagePlanKeys where
        toHeaders GetUsagePlanKeys{..}
          = Core.pure ("Accept", "application/json")

instance Core.AWSRequest GetUsagePlanKeys where
        type Rs GetUsagePlanKeys = GetUsagePlanKeysResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/usageplans/" Core.<> Core.toText usagePlanId Core.<> "/keys",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUsagePlanKeysResponse' Core.<$>
                   (x Core..:? "item") Core.<*> x Core..:? "position" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetUsagePlanKeys where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"position") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"items" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"position" Lens..~ rs Lens.^. Lens.field @"position")

-- | Represents the collection of usage plan keys added to usage plans for the associated API keys and, possibly, other types of keys.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/api-gateway-api-usage-plans.html Create and Use Usage Plans> 
--
-- /See:/ 'mkGetUsagePlanKeysResponse' smart constructor.
data GetUsagePlanKeysResponse = GetUsagePlanKeysResponse'
  { items :: Core.Maybe [Types.UsagePlanKey]
    -- ^ The current page of elements from this collection.
  , position :: Core.Maybe Core.Text
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsagePlanKeysResponse' value with any optional fields omitted.
mkGetUsagePlanKeysResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetUsagePlanKeysResponse
mkGetUsagePlanKeysResponse responseStatus
  = GetUsagePlanKeysResponse'{items = Core.Nothing,
                              position = Core.Nothing, responseStatus}

-- | The current page of elements from this collection.
--
-- /Note:/ Consider using 'items' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkrrsItems :: Lens.Lens' GetUsagePlanKeysResponse (Core.Maybe [Types.UsagePlanKey])
gupkrrsItems = Lens.field @"items"
{-# INLINEABLE gupkrrsItems #-}
{-# DEPRECATED items "Use generic-lens or generic-optics with 'items' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'position' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkrrsPosition :: Lens.Lens' GetUsagePlanKeysResponse (Core.Maybe Core.Text)
gupkrrsPosition = Lens.field @"position"
{-# INLINEABLE gupkrrsPosition #-}
{-# DEPRECATED position "Use generic-lens or generic-optics with 'position' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gupkrrsResponseStatus :: Lens.Lens' GetUsagePlanKeysResponse Core.Int
gupkrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gupkrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
