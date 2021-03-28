{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListCachePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of cache policies.
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListCachePolicies
    (
    -- * Creating a request
      ListCachePolicies (..)
    , mkListCachePolicies
    -- ** Request lenses
    , lcpMarker
    , lcpMaxItems
    , lcpType

    -- * Destructuring the response
    , ListCachePoliciesResponse (..)
    , mkListCachePoliciesResponse
    -- ** Response lenses
    , lcprrsCachePolicyList
    , lcprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListCachePolicies' smart constructor.
data ListCachePolicies = ListCachePolicies'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of cache policies that you want in the response.
  , type' :: Core.Maybe Types.CachePolicyType
    -- ^ A filter to return only the specified kinds of cache policies. Valid values are:
--
--
--     * @managed@ – Returns only the managed policies created by AWS.
--
--
--     * @custom@ – Returns only the custom policies created in your AWS account.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListCachePolicies' value with any optional fields omitted.
mkListCachePolicies
    :: ListCachePolicies
mkListCachePolicies
  = ListCachePolicies'{marker = Core.Nothing,
                       maxItems = Core.Nothing, type' = Core.Nothing}

-- | Use this field when paginating results to indicate where to begin in your list of cache policies. The response includes cache policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMarker :: Lens.Lens' ListCachePolicies (Core.Maybe Core.Text)
lcpMarker = Lens.field @"marker"
{-# INLINEABLE lcpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of cache policies that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpMaxItems :: Lens.Lens' ListCachePolicies (Core.Maybe Core.Text)
lcpMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lcpMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A filter to return only the specified kinds of cache policies. Valid values are:
--
--
--     * @managed@ – Returns only the managed policies created by AWS.
--
--
--     * @custom@ – Returns only the custom policies created in your AWS account.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcpType :: Lens.Lens' ListCachePolicies (Core.Maybe Types.CachePolicyType)
lcpType = Lens.field @"type'"
{-# INLINEABLE lcpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery ListCachePolicies where
        toQuery ListCachePolicies{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'

instance Core.ToHeaders ListCachePolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListCachePolicies where
        type Rs ListCachePolicies = ListCachePoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/cache-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListCachePoliciesResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListCachePoliciesResponse' smart constructor.
data ListCachePoliciesResponse = ListCachePoliciesResponse'
  { cachePolicyList :: Core.Maybe Types.CachePolicyList
    -- ^ A list of cache policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListCachePoliciesResponse' value with any optional fields omitted.
mkListCachePoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListCachePoliciesResponse
mkListCachePoliciesResponse responseStatus
  = ListCachePoliciesResponse'{cachePolicyList = Core.Nothing,
                               responseStatus}

-- | A list of cache policies.
--
-- /Note:/ Consider using 'cachePolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsCachePolicyList :: Lens.Lens' ListCachePoliciesResponse (Core.Maybe Types.CachePolicyList)
lcprrsCachePolicyList = Lens.field @"cachePolicyList"
{-# INLINEABLE lcprrsCachePolicyList #-}
{-# DEPRECATED cachePolicyList "Use generic-lens or generic-optics with 'cachePolicyList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcprrsResponseStatus :: Lens.Lens' ListCachePoliciesResponse Core.Int
lcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
