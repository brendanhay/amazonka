{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of distribution IDs for distributions that have a cache behavior that’s associated with the specified origin request policy.
--
-- You can optionally specify the maximum number of items to receive in the response. If the total number of items in the list exceeds the maximum that you specify, or the default maximum, the response is paginated. To get the next page of items, send a subsequent request that specifies the @NextMarker@ value from the current response as the @Marker@ value in the subsequent request.
module Network.AWS.CloudFront.ListDistributionsByOriginRequestPolicyId
    (
    -- * Creating a request
      ListDistributionsByOriginRequestPolicyId (..)
    , mkListDistributionsByOriginRequestPolicyId
    -- ** Request lenses
    , ldborpiOriginRequestPolicyId
    , ldborpiMarker
    , ldborpiMaxItems

    -- * Destructuring the response
    , ListDistributionsByOriginRequestPolicyIdResponse (..)
    , mkListDistributionsByOriginRequestPolicyIdResponse
    -- ** Response lenses
    , ldborpirrsDistributionIdList
    , ldborpirrsResponseStatus
    ) where

import qualified Network.AWS.CloudFront.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListDistributionsByOriginRequestPolicyId' smart constructor.
data ListDistributionsByOriginRequestPolicyId = ListDistributionsByOriginRequestPolicyId'
  { originRequestPolicyId :: Core.Text
    -- ^ The ID of the origin request policy whose associated distribution IDs you want to list.
  , marker :: Core.Maybe Core.Text
    -- ^ Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
  , maxItems :: Core.Maybe Core.Text
    -- ^ The maximum number of distribution IDs that you want in the response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByOriginRequestPolicyId' value with any optional fields omitted.
mkListDistributionsByOriginRequestPolicyId
    :: Core.Text -- ^ 'originRequestPolicyId'
    -> ListDistributionsByOriginRequestPolicyId
mkListDistributionsByOriginRequestPolicyId originRequestPolicyId
  = ListDistributionsByOriginRequestPolicyId'{originRequestPolicyId,
                                              marker = Core.Nothing, maxItems = Core.Nothing}

-- | The ID of the origin request policy whose associated distribution IDs you want to list.
--
-- /Note:/ Consider using 'originRequestPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpiOriginRequestPolicyId :: Lens.Lens' ListDistributionsByOriginRequestPolicyId Core.Text
ldborpiOriginRequestPolicyId = Lens.field @"originRequestPolicyId"
{-# INLINEABLE ldborpiOriginRequestPolicyId #-}
{-# DEPRECATED originRequestPolicyId "Use generic-lens or generic-optics with 'originRequestPolicyId' instead"  #-}

-- | Use this field when paginating results to indicate where to begin in your list of distribution IDs. The response includes distribution IDs in the list that occur after the marker. To get the next page of the list, set this field’s value to the value of @NextMarker@ from the current page’s response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpiMarker :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Core.Maybe Core.Text)
ldborpiMarker = Lens.field @"marker"
{-# INLINEABLE ldborpiMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of distribution IDs that you want in the response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpiMaxItems :: Lens.Lens' ListDistributionsByOriginRequestPolicyId (Core.Maybe Core.Text)
ldborpiMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ldborpiMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListDistributionsByOriginRequestPolicyId
         where
        toQuery ListDistributionsByOriginRequestPolicyId{..}
          = Core.maybe Core.mempty (Core.toQueryPair "Marker") marker Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListDistributionsByOriginRequestPolicyId
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListDistributionsByOriginRequestPolicyId
         where
        type Rs ListDistributionsByOriginRequestPolicyId =
             ListDistributionsByOriginRequestPolicyIdResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2020-05-31/distributionsByOriginRequestPolicyId/" Core.<>
                             Core.toText originRequestPolicyId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListDistributionsByOriginRequestPolicyIdResponse' Core.<$>
                   (Core.parseXML x) Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListDistributionsByOriginRequestPolicyIdResponse' smart constructor.
data ListDistributionsByOriginRequestPolicyIdResponse = ListDistributionsByOriginRequestPolicyIdResponse'
  { distributionIdList :: Core.Maybe Types.DistributionIdList
    -- ^ A list of distribution IDs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListDistributionsByOriginRequestPolicyIdResponse' value with any optional fields omitted.
mkListDistributionsByOriginRequestPolicyIdResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListDistributionsByOriginRequestPolicyIdResponse
mkListDistributionsByOriginRequestPolicyIdResponse responseStatus
  = ListDistributionsByOriginRequestPolicyIdResponse'{distributionIdList
                                                        = Core.Nothing,
                                                      responseStatus}

-- | A list of distribution IDs.
--
-- /Note:/ Consider using 'distributionIdList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpirrsDistributionIdList :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse (Core.Maybe Types.DistributionIdList)
ldborpirrsDistributionIdList = Lens.field @"distributionIdList"
{-# INLINEABLE ldborpirrsDistributionIdList #-}
{-# DEPRECATED distributionIdList "Use generic-lens or generic-optics with 'distributionIdList' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ldborpirrsResponseStatus :: Lens.Lens' ListDistributionsByOriginRequestPolicyIdResponse Core.Int
ldborpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ldborpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
