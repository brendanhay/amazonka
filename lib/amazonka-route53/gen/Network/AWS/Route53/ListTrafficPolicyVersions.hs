{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListTrafficPolicyVersions (..),
    mkListTrafficPolicyVersions,

    -- ** Request lenses
    ltpvId,
    ltpvMaxItems,
    ltpvTrafficPolicyVersionMarker,

    -- * Destructuring the response
    ListTrafficPolicyVersionsResponse (..),
    mkListTrafficPolicyVersionsResponse,

    -- ** Response lenses
    ltpvrrsTrafficPolicies,
    ltpvrrsIsTruncated,
    ltpvrrsTrafficPolicyVersionMarker,
    ltpvrrsMaxItems,
    ltpvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains the information about the request to list your traffic policies.
--
-- /See:/ 'mkListTrafficPolicyVersions' smart constructor.
data ListTrafficPolicyVersions = ListTrafficPolicyVersions'
  { -- | Specify the value of @Id@ of the traffic policy for which you want to list all versions.
    id :: Types.Id,
    -- | The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Route 53 will return if you submit another request.
    maxItems :: Core.Maybe Types.MaxItems,
    -- | For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter.
    --
    -- If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
    trafficPolicyVersionMarker :: Core.Maybe Types.TrafficPolicyVersionMarker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyVersions' value with any optional fields omitted.
mkListTrafficPolicyVersions ::
  -- | 'id'
  Types.Id ->
  ListTrafficPolicyVersions
mkListTrafficPolicyVersions id =
  ListTrafficPolicyVersions'
    { id,
      maxItems = Core.Nothing,
      trafficPolicyVersionMarker = Core.Nothing
    }

-- | Specify the value of @Id@ of the traffic policy for which you want to list all versions.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvId :: Lens.Lens' ListTrafficPolicyVersions Types.Id
ltpvId = Lens.field @"id"
{-# DEPRECATED ltpvId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The maximum number of traffic policy versions that you want Amazon Route 53 to include in the response body for this request. If the specified traffic policy has more than @MaxItems@ versions, the value of @IsTruncated@ in the response is @true@ , and the value of the @TrafficPolicyVersionMarker@ element is the ID of the first version that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvMaxItems :: Lens.Lens' ListTrafficPolicyVersions (Core.Maybe Types.MaxItems)
ltpvMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ltpvMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | For your first request to @ListTrafficPolicyVersions@ , don't include the @TrafficPolicyVersionMarker@ parameter.
--
-- If you have more traffic policy versions than the value of @MaxItems@ , @ListTrafficPolicyVersions@ returns only the first group of @MaxItems@ versions. To get more traffic policy versions, submit another @ListTrafficPolicyVersions@ request. For the value of @TrafficPolicyVersionMarker@ , specify the value of @TrafficPolicyVersionMarker@ in the previous response.
--
-- /Note:/ Consider using 'trafficPolicyVersionMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvTrafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersions (Core.Maybe Types.TrafficPolicyVersionMarker)
ltpvTrafficPolicyVersionMarker = Lens.field @"trafficPolicyVersionMarker"
{-# DEPRECATED ltpvTrafficPolicyVersionMarker "Use generic-lens or generic-optics with 'trafficPolicyVersionMarker' instead." #-}

instance Core.AWSRequest ListTrafficPolicyVersions where
  type
    Rs ListTrafficPolicyVersions =
      ListTrafficPolicyVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2013-04-01/trafficpolicies/" Core.<> (Core.toText id)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxitems" Core.<$> maxItems
            Core.<> ( Core.toQueryValue "trafficpolicyversion"
                        Core.<$> trafficPolicyVersionMarker
                    ),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPolicyVersionsResponse'
            Core.<$> ( x Core..@? "TrafficPolicies" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "TrafficPolicy"
                     )
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "TrafficPolicyVersionMarker")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyVersionsResponse' smart constructor.
data ListTrafficPolicyVersionsResponse = ListTrafficPolicyVersionsResponse'
  { -- | A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
    trafficPolicies :: [Types.TrafficPolicy],
    -- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
    isTruncated :: Core.Bool,
    -- | If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter.
    --
    -- This element is present only if @IsTruncated@ is @true@ .
    trafficPolicyVersionMarker :: Types.TrafficPolicyVersionMarker,
    -- | The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
    maxItems :: Types.MaxItems,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyVersionsResponse' value with any optional fields omitted.
mkListTrafficPolicyVersionsResponse ::
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'trafficPolicyVersionMarker'
  Types.TrafficPolicyVersionMarker ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListTrafficPolicyVersionsResponse
mkListTrafficPolicyVersionsResponse
  isTruncated
  trafficPolicyVersionMarker
  maxItems
  responseStatus =
    ListTrafficPolicyVersionsResponse'
      { trafficPolicies = Core.mempty,
        isTruncated,
        trafficPolicyVersionMarker,
        maxItems,
        responseStatus
      }

-- | A list that contains one @TrafficPolicy@ element for each traffic policy version that is associated with the specified traffic policy.
--
-- /Note:/ Consider using 'trafficPolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsTrafficPolicies :: Lens.Lens' ListTrafficPolicyVersionsResponse [Types.TrafficPolicy]
ltpvrrsTrafficPolicies = Lens.field @"trafficPolicies"
{-# DEPRECATED ltpvrrsTrafficPolicies "Use generic-lens or generic-optics with 'trafficPolicies' instead." #-}

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicyVersions@ request and specifying the value of @NextMarker@ in the @marker@ parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsIsTruncated :: Lens.Lens' ListTrafficPolicyVersionsResponse Core.Bool
ltpvrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED ltpvrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | If @IsTruncated@ is @true@ , the value of @TrafficPolicyVersionMarker@ identifies the first traffic policy that Amazon Route 53 will return if you submit another request. Call @ListTrafficPolicyVersions@ again and specify the value of @TrafficPolicyVersionMarker@ in the @TrafficPolicyVersionMarker@ request parameter.
--
-- This element is present only if @IsTruncated@ is @true@ .
--
-- /Note:/ Consider using 'trafficPolicyVersionMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsTrafficPolicyVersionMarker :: Lens.Lens' ListTrafficPolicyVersionsResponse Types.TrafficPolicyVersionMarker
ltpvrrsTrafficPolicyVersionMarker = Lens.field @"trafficPolicyVersionMarker"
{-# DEPRECATED ltpvrrsTrafficPolicyVersionMarker "Use generic-lens or generic-optics with 'trafficPolicyVersionMarker' instead." #-}

-- | The value that you specified for the @maxitems@ parameter in the @ListTrafficPolicyVersions@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsMaxItems :: Lens.Lens' ListTrafficPolicyVersionsResponse Types.MaxItems
ltpvrrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ltpvrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpvrrsResponseStatus :: Lens.Lens' ListTrafficPolicyVersionsResponse Core.Int
ltpvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltpvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
