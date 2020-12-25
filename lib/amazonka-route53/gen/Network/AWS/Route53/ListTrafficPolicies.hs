{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the latest version for every traffic policy that is associated with the current AWS account. Policies are listed in the order that they were created in.
--
-- For information about how of deleting a traffic policy affects the response from @ListTrafficPolicies@ , see <https://docs.aws.amazon.com/Route53/latest/APIReference/API_DeleteTrafficPolicy.html DeleteTrafficPolicy> .
module Network.AWS.Route53.ListTrafficPolicies
  ( -- * Creating a request
    ListTrafficPolicies (..),
    mkListTrafficPolicies,

    -- ** Request lenses
    ltpMaxItems,
    ltpTrafficPolicyIdMarker,

    -- * Destructuring the response
    ListTrafficPoliciesResponse (..),
    mkListTrafficPoliciesResponse,

    -- ** Response lenses
    ltprrsTrafficPolicySummaries,
    ltprrsIsTruncated,
    ltprrsTrafficPolicyIdMarker,
    ltprrsMaxItems,
    ltprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains the information about the request to list the traffic policies that are associated with the current AWS account.
--
-- /See:/ 'mkListTrafficPolicies' smart constructor.
data ListTrafficPolicies = ListTrafficPolicies'
  { -- | (Optional) The maximum number of traffic policies that you want Amazon Route 53 to return in response to this request. If you have more than @MaxItems@ traffic policies, the value of @IsTruncated@ in the response is @true@ , and the value of @TrafficPolicyIdMarker@ is the ID of the first traffic policy that Route 53 will return if you submit another request.
    maxItems :: Core.Maybe Types.MaxItems,
    -- | (Conditional) For your first request to @ListTrafficPolicies@ , don't include the @TrafficPolicyIdMarker@ parameter.
    --
    -- If you have more traffic policies than the value of @MaxItems@ , @ListTrafficPolicies@ returns only the first @MaxItems@ traffic policies. To get the next group of policies, submit another request to @ListTrafficPolicies@ . For the value of @TrafficPolicyIdMarker@ , specify the value of @TrafficPolicyIdMarker@ that was returned in the previous response.
    trafficPolicyIdMarker :: Core.Maybe Types.TrafficPolicyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicies' value with any optional fields omitted.
mkListTrafficPolicies ::
  ListTrafficPolicies
mkListTrafficPolicies =
  ListTrafficPolicies'
    { maxItems = Core.Nothing,
      trafficPolicyIdMarker = Core.Nothing
    }

-- | (Optional) The maximum number of traffic policies that you want Amazon Route 53 to return in response to this request. If you have more than @MaxItems@ traffic policies, the value of @IsTruncated@ in the response is @true@ , and the value of @TrafficPolicyIdMarker@ is the ID of the first traffic policy that Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpMaxItems :: Lens.Lens' ListTrafficPolicies (Core.Maybe Types.MaxItems)
ltpMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ltpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | (Conditional) For your first request to @ListTrafficPolicies@ , don't include the @TrafficPolicyIdMarker@ parameter.
--
-- If you have more traffic policies than the value of @MaxItems@ , @ListTrafficPolicies@ returns only the first @MaxItems@ traffic policies. To get the next group of policies, submit another request to @ListTrafficPolicies@ . For the value of @TrafficPolicyIdMarker@ , specify the value of @TrafficPolicyIdMarker@ that was returned in the previous response.
--
-- /Note:/ Consider using 'trafficPolicyIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpTrafficPolicyIdMarker :: Lens.Lens' ListTrafficPolicies (Core.Maybe Types.TrafficPolicyId)
ltpTrafficPolicyIdMarker = Lens.field @"trafficPolicyIdMarker"
{-# DEPRECATED ltpTrafficPolicyIdMarker "Use generic-lens or generic-optics with 'trafficPolicyIdMarker' instead." #-}

instance Core.AWSRequest ListTrafficPolicies where
  type Rs ListTrafficPolicies = ListTrafficPoliciesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/trafficpolicies",
        Core._rqQuery =
          Core.toQueryValue "maxitems" Core.<$> maxItems
            Core.<> ( Core.toQueryValue "trafficpolicyid"
                        Core.<$> trafficPolicyIdMarker
                    ),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPoliciesResponse'
            Core.<$> ( x Core..@? "TrafficPolicySummaries" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "TrafficPolicySummary"
                     )
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "TrafficPolicyIdMarker")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPoliciesResponse' smart constructor.
data ListTrafficPoliciesResponse = ListTrafficPoliciesResponse'
  { -- | A list that contains one @TrafficPolicySummary@ element for each traffic policy that was created by the current AWS account.
    trafficPolicySummaries :: [Types.TrafficPolicySummary],
    -- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicies@ request and specifying the value of @TrafficPolicyIdMarker@ in the @TrafficPolicyIdMarker@ request parameter.
    isTruncated :: Core.Bool,
    -- | If the value of @IsTruncated@ is @true@ , @TrafficPolicyIdMarker@ is the ID of the first traffic policy in the next group of @MaxItems@ traffic policies.
    trafficPolicyIdMarker :: Types.TrafficPolicyIdMarker,
    -- | The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicies@ request that produced the current response.
    maxItems :: Types.MaxItems,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPoliciesResponse' value with any optional fields omitted.
mkListTrafficPoliciesResponse ::
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'trafficPolicyIdMarker'
  Types.TrafficPolicyIdMarker ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListTrafficPoliciesResponse
mkListTrafficPoliciesResponse
  isTruncated
  trafficPolicyIdMarker
  maxItems
  responseStatus =
    ListTrafficPoliciesResponse'
      { trafficPolicySummaries =
          Core.mempty,
        isTruncated,
        trafficPolicyIdMarker,
        maxItems,
        responseStatus
      }

-- | A list that contains one @TrafficPolicySummary@ element for each traffic policy that was created by the current AWS account.
--
-- /Note:/ Consider using 'trafficPolicySummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsTrafficPolicySummaries :: Lens.Lens' ListTrafficPoliciesResponse [Types.TrafficPolicySummary]
ltprrsTrafficPolicySummaries = Lens.field @"trafficPolicySummaries"
{-# DEPRECATED ltprrsTrafficPolicySummaries "Use generic-lens or generic-optics with 'trafficPolicySummaries' instead." #-}

-- | A flag that indicates whether there are more traffic policies to be listed. If the response was truncated, you can get the next group of traffic policies by submitting another @ListTrafficPolicies@ request and specifying the value of @TrafficPolicyIdMarker@ in the @TrafficPolicyIdMarker@ request parameter.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsIsTruncated :: Lens.Lens' ListTrafficPoliciesResponse Core.Bool
ltprrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED ltprrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | If the value of @IsTruncated@ is @true@ , @TrafficPolicyIdMarker@ is the ID of the first traffic policy in the next group of @MaxItems@ traffic policies.
--
-- /Note:/ Consider using 'trafficPolicyIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsTrafficPolicyIdMarker :: Lens.Lens' ListTrafficPoliciesResponse Types.TrafficPolicyIdMarker
ltprrsTrafficPolicyIdMarker = Lens.field @"trafficPolicyIdMarker"
{-# DEPRECATED ltprrsTrafficPolicyIdMarker "Use generic-lens or generic-optics with 'trafficPolicyIdMarker' instead." #-}

-- | The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicies@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsMaxItems :: Lens.Lens' ListTrafficPoliciesResponse Types.MaxItems
ltprrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ltprrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltprrsResponseStatus :: Lens.Lens' ListTrafficPoliciesResponse Core.Int
ltprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
