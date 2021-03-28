{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about all of the versions for a specified traffic policy.
--
-- Traffic policy versions are listed in numerical order by @VersionNumber@ .
module Network.AWS.Route53.ListTrafficPolicyVersions
    (
    -- * Creating a request
      ListTrafficPolicyVersions (..)
    , mkListTrafficPolicyVersions
    -- ** Request lenses
    , ltpvId
    , ltpvMaxItems
    , ltpvTrafficPolicyVersionMarker

    -- * Destructuring the response
    , ListTrafficPolicyVersionsResponse (..)
    , mkListTrafficPolicyVersionsResponse
    -- ** Response lenses
    , ltpvrrsTrafficPolicies
    , ltpvrrsIsTruncated
    , ltpvrrsTrafficPolicyVersionMarker
    , ltpvrrsMaxItems
    , ltpvrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains the information about the request to list your traffic policies.
--
-- /See:/ 'mkListTrafficPolicyVersions' smart constructor.
data ListTrafficPolicyVersions = ListTrafficPolicyVersions'
  { id :: Types.Id
    -- ^ Specify the value of @Id@ of the traffic policy for which you want to list all versions.
  , maxItems :: Core.Maybe Types.MaxItems
    -- ^ The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Route 53 will return if you submit another request.
  , trafficPolicyVersionMarker :: Core.Maybe Types.TrafficPolicyVersionMarker
    -- ^ For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyVersions' value with any optional fields omitted.
mkListTrafficPolicyVersions
    :: Types.Id -- ^ 'id'
    -> ListTrafficPolicyVersions
mkListTrafficPolicyVersions id
  = ListTrafficPolicyVersions'{id, maxItems = Core.Nothing,
                               trafficPolicyVersionMarker = Core.Nothing}

-- | Specify the value of @Id@ of the traffic policy for which you want to list all versions.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvId :: Lens.Lens' ListTrafficPolicyVersions Types.Id
ltpvId = Lens.field @"id"
{-# INLINEABLE ltpvId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvMaxItems :: Lens.Lens' ListTrafficPolicyVersions (Core.Maybe Types.MaxItems)
ltpvMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ltpvMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
--
-- /Note:/ Consider using 'trafficPolicyVersionMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvTrafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersions (Core.Maybe Types.TrafficPolicyVersionMarker)
ltpvTrafficPolicyVersionMarker = Lens.field @"trafficPolicyVersionMarker"
{-# INLINEABLE ltpvTrafficPolicyVersionMarker #-}
{-# DEPRECATED trafficPolicyVersionMarker "Use generic-lens or generic-optics with 'trafficPolicyVersionMarker' instead"  #-}

instance Core.ToQuery ListTrafficPolicyVersions where
        toQuery ListTrafficPolicyVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxitems") maxItems
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "trafficpolicyversion")
                trafficPolicyVersionMarker

instance Core.ToHeaders ListTrafficPolicyVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTrafficPolicyVersions where
        type Rs ListTrafficPolicyVersions =
             ListTrafficPolicyVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2013-04-01/trafficpolicies/" Core.<> Core.toText id Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListTrafficPolicyVersionsResponse' Core.<$>
                   (x Core..@ "TrafficPolicies" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "TrafficPolicy")
                     Core.<*> x Core..@ "IsTruncated"
                     Core.<*> x Core..@ "TrafficPolicyVersionMarker"
                     Core.<*> x Core..@ "MaxItems"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyVersionsResponse' smart constructor.
data ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse'
  { trafficPolicies :: [Types.TrafficPolicy]
    -- ^ A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
  , isTruncated :: Core.Bool
    -- ^ A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
  , trafficPolicyVersionMarker :: Types.TrafficPolicyVersionMarker
    -- ^ If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
  , maxItems :: Types.MaxItems
    -- ^ The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyVersionsResponse' value with any optional fields omitted.
mkListTrafficPolicyVersionsResponse
    :: Core.Bool -- ^ 'isTruncated'
    -> Types.TrafficPolicyVersionMarker -- ^ 'trafficPolicyVersionMarker'
    -> Types.MaxItems -- ^ 'maxItems'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTrafficPolicyVersionsResponse
mkListTrafficPolicyVersionsResponse isTruncated
  trafficPolicyVersionMarker maxItems responseStatus
  = ListTrafficPolicyVersionsResponse'{trafficPolicies = Core.mempty,
                                       isTruncated, trafficPolicyVersionMarker, maxItems,
                                       responseStatus}

-- | A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsTrafficPolicies :: Lens.Lens' ListTrafficPolicyVersionsResponse [Types.TrafficPolicy]
ltpvrrsTrafficPolicies = Lens.field @"trafficPolicies"
{-# INLINEABLE ltpvrrsTrafficPolicies #-}
{-# DEPRECATED trafficPolicies "Use generic-lens or generic-optics with 'trafficPolicies' instead"  #-}

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsIsTruncated :: Lens.Lens' ListTrafficPolicyVersionsResponse Core.Bool
ltpvrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE ltpvrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'trafficPolicyVersionMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsTrafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersionsResponse Types.TrafficPolicyVersionMarker
ltpvrrsTrafficPolicyVersionMarker = Lens.field @"trafficPolicyVersionMarker"
{-# INLINEABLE ltpvrrsTrafficPolicyVersionMarker #-}
{-# DEPRECATED trafficPolicyVersionMarker "Use generic-lens or generic-optics with 'trafficPolicyVersionMarker' instead"  #-}

-- | The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsMaxItems :: Lens.Lens' ListTrafficPolicyVersionsResponse Types.MaxItems
ltpvrrsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ltpvrrsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsResponseStatus :: Lens.Lens' ListTrafficPolicyVersionsResponse Core.Int
ltpvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltpvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
