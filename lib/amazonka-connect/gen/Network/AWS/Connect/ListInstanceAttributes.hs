{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.ListInstanceAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a paginated list of all attribute types for the given instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListInstanceAttributes
    (
    -- * Creating a request
      ListInstanceAttributes (..)
    , mkListInstanceAttributes
    -- ** Request lenses
    , liaInstanceId
    , liaMaxResults
    , liaNextToken

    -- * Destructuring the response
    , ListInstanceAttributesResponse (..)
    , mkListInstanceAttributesResponse
    -- ** Response lenses
    , liarrsAttributes
    , liarrsNextToken
    , liarrsResponseStatus
    ) where

import qualified Network.AWS.Connect.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListInstanceAttributes' smart constructor.
data ListInstanceAttributes = ListInstanceAttributes'
  { instanceId :: Types.InstanceId
    -- ^ The identifier of the Amazon Connect instance.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximimum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceAttributes' value with any optional fields omitted.
mkListInstanceAttributes
    :: Types.InstanceId -- ^ 'instanceId'
    -> ListInstanceAttributes
mkListInstanceAttributes instanceId
  = ListInstanceAttributes'{instanceId, maxResults = Core.Nothing,
                            nextToken = Core.Nothing}

-- | The identifier of the Amazon Connect instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaInstanceId :: Lens.Lens' ListInstanceAttributes Types.InstanceId
liaInstanceId = Lens.field @"instanceId"
{-# INLINEABLE liaInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximimum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaMaxResults :: Lens.Lens' ListInstanceAttributes (Core.Maybe Core.Natural)
liaMaxResults = Lens.field @"maxResults"
{-# INLINEABLE liaMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results. Use the value returned in the previous response in the next request to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liaNextToken :: Lens.Lens' ListInstanceAttributes (Core.Maybe Types.NextToken)
liaNextToken = Lens.field @"nextToken"
{-# INLINEABLE liaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListInstanceAttributes where
        toQuery ListInstanceAttributes{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListInstanceAttributes where
        toHeaders ListInstanceAttributes{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListInstanceAttributes where
        type Rs ListInstanceAttributes = ListInstanceAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/instance/" Core.<> Core.toText instanceId Core.<> "/attributes",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListInstanceAttributesResponse' Core.<$>
                   (x Core..:? "Attributes") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListInstanceAttributes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"attributes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListInstanceAttributesResponse' smart constructor.
data ListInstanceAttributesResponse = ListInstanceAttributesResponse'
  { attributes :: Core.Maybe [Types.Attribute]
    -- ^ The attribute types.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ If there are additional results, this is the token for the next set of results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListInstanceAttributesResponse' value with any optional fields omitted.
mkListInstanceAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListInstanceAttributesResponse
mkListInstanceAttributesResponse responseStatus
  = ListInstanceAttributesResponse'{attributes = Core.Nothing,
                                    nextToken = Core.Nothing, responseStatus}

-- | The attribute types.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarrsAttributes :: Lens.Lens' ListInstanceAttributesResponse (Core.Maybe [Types.Attribute])
liarrsAttributes = Lens.field @"attributes"
{-# INLINEABLE liarrsAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

-- | If there are additional results, this is the token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarrsNextToken :: Lens.Lens' ListInstanceAttributesResponse (Core.Maybe Types.NextToken)
liarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE liarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liarrsResponseStatus :: Lens.Lens' ListInstanceAttributesResponse Core.Int
liarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE liarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
