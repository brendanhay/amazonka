{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListOriginRequestPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of origin request policies.
--
-- You can optionally apply a filter to return only the managed policies created by AWS, or only the custom policies created in your AWS account.
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListOriginRequestPolicies
    (
    -- * Creating a request
      ListOriginRequestPolicies (..)
    , mkListOriginRequestPolicies
    -- ** Request lenses
    , lorpMarker
    , lorpMaxItems
    , lorpType

    -- * Destructuring the response
    , ListOriginRequestPoliciesResponse (..)
    , mkListOriginRequestPoliciesResponse
    -- ** Response lenses
    , lorprrsOriginRequestPolicyList
    , lorprrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListOriginRequestPolicies' smart constructor.
data ListOriginRequestPolicies = ListOriginRequestPolicies'
  { marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of origin request policies that you want in the response.
  , type' :: Core.Maybe Types.OriginRequestPolicyType
    -- ^ A filter to return only the specified kinds of origin request policies. Valid values are:
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

-- | Creates a 'ListOriginRequestPolicies' value with any optional fields omitted.
mkListOriginRequestPolicies
    :: ListOriginRequestPolicies
mkListOriginRequestPolicies
  = ListOriginRequestPolicies'{marker = Core.Nothing,
                               maxItems = Core.Nothing, type' = Core.Nothing}

-- | Use this field when paginating results to indicate where to begin in your list of origin request policies. The response includes origin request policies in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorpMarker :: Lens.Lens' ListOriginRequestPolicies (Core.Maybe Core.Text)
lorpMarker = Lens.field @"marker"
{-# INLINEABLE lorpMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of origin request policies that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorpMaxItems :: Lens.Lens' ListOriginRequestPolicies (Core.Maybe Core.Text)
lorpMaxItems = Lens.field @"maxItems"
{-# INLINEABLE lorpMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | A filter to return only the specified kinds of origin request policies. Valid values are:
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
lorpType :: Lens.Lens' ListOriginRequestPolicies (Core.Maybe Types.OriginRequestPolicyType)
lorpType = Lens.field @"type'"
{-# INLINEABLE lorpType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery ListOriginRequestPolicies where
        toQuery ListOriginRequestPolicies{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'

instance Core.ToHeaders ListOriginRequestPolicies where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListOriginRequestPolicies where
        type Rs ListOriginRequestPolicies =
             ListOriginRequestPoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2020-05-31/origin-request-policy",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListOriginRequestPoliciesResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListOriginRequestPoliciesResponse' smart constructor.
data ListOriginRequestPoliciesResponse = ListOriginRequestPoliciesResponse'
  { originRequestPolicyList :: Core.Maybe Types.OriginRequestPolicyList
    -- ^ A list of origin request policies.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListOriginRequestPoliciesResponse' value with any optional fields omitted.
mkListOriginRequestPoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListOriginRequestPoliciesResponse
mkListOriginRequestPoliciesResponse responseStatus
  = ListOriginRequestPoliciesResponse'{originRequestPolicyList =
                                         Core.Nothing,
                                       responseStatus}

-- | A list of origin request policies.
--
-- /Note:/ Consider using 'originRequestPolicyList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorprrsOriginRequestPolicyList :: Lens.Lens' ListOriginRequestPoliciesResponse (Core.Maybe Types.OriginRequestPolicyList)
lorprrsOriginRequestPolicyList = Lens.field @"originRequestPolicyList"
{-# INLINEABLE lorprrsOriginRequestPolicyList #-}
{-# DEPRECATED originRequestPolicyList "Use generic-lens or generic-optics with 'originRequestPolicyList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorprrsResponseStatus :: Lens.Lens' ListOriginRequestPoliciesResponse Core.Int
lorprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lorprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
