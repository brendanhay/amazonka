{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using a specify traffic policy version.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
module Network.AWS.Route53.ListTrafficPolicyInstancesByPolicy
  ( -- * Creating a request
    ListTrafficPolicyInstancesByPolicy (..),
    mkListTrafficPolicyInstancesByPolicy,

    -- ** Request lenses
    ltpibpTrafficPolicyId,
    ltpibpTrafficPolicyVersion,
    ltpibpHostedZoneIdMarker,
    ltpibpMaxItems,
    ltpibpTrafficPolicyInstanceNameMarker,
    ltpibpTrafficPolicyInstanceTypeMarker,

    -- * Destructuring the response
    ListTrafficPolicyInstancesByPolicyResponse (..),
    mkListTrafficPolicyInstancesByPolicyResponse,

    -- ** Response lenses
    ltpibprrsTrafficPolicyInstances,
    ltpibprrsIsTruncated,
    ltpibprrsMaxItems,
    ltpibprrsHostedZoneIdMarker,
    ltpibprrsTrafficPolicyInstanceNameMarker,
    ltpibprrsTrafficPolicyInstanceTypeMarker,
    ltpibprrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A complex type that contains the information about the request to list your traffic policy instances.
--
-- /See:/ 'mkListTrafficPolicyInstancesByPolicy' smart constructor.
data ListTrafficPolicyInstancesByPolicy = ListTrafficPolicyInstancesByPolicy'
  { -- | The ID of the traffic policy for which you want to list traffic policy instances.
    trafficPolicyId :: Types.TrafficPolicyId,
    -- | The version of the traffic policy for which you want to list traffic policy instances. The version must be associated with the traffic policy that is specified by @TrafficPolicyId@ .
    trafficPolicyVersion :: Core.Natural,
    -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
    --
    -- For the value of @hostedzoneid@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
    hostedZoneIdMarker :: Core.Maybe Types.HostedZoneIdMarker,
    -- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
    maxItems :: Core.Maybe Types.MaxItems,
    -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
    --
    -- For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
    trafficPolicyInstanceNameMarker :: Core.Maybe Types.TrafficPolicyInstanceNameMarker,
    -- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
    --
    -- For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
    -- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
    trafficPolicyInstanceTypeMarker :: Core.Maybe Types.RecordType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyInstancesByPolicy' value with any optional fields omitted.
mkListTrafficPolicyInstancesByPolicy ::
  -- | 'trafficPolicyId'
  Types.TrafficPolicyId ->
  -- | 'trafficPolicyVersion'
  Core.Natural ->
  ListTrafficPolicyInstancesByPolicy
mkListTrafficPolicyInstancesByPolicy
  trafficPolicyId
  trafficPolicyVersion =
    ListTrafficPolicyInstancesByPolicy'
      { trafficPolicyId,
        trafficPolicyVersion,
        hostedZoneIdMarker = Core.Nothing,
        maxItems = Core.Nothing,
        trafficPolicyInstanceNameMarker = Core.Nothing,
        trafficPolicyInstanceTypeMarker = Core.Nothing
      }

-- | The ID of the traffic policy for which you want to list traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyId :: Lens.Lens' ListTrafficPolicyInstancesByPolicy Types.TrafficPolicyId
ltpibpTrafficPolicyId = Lens.field @"trafficPolicyId"
{-# DEPRECATED ltpibpTrafficPolicyId "Use generic-lens or generic-optics with 'trafficPolicyId' instead." #-}

-- | The version of the traffic policy for which you want to list traffic policy instances. The version must be associated with the traffic policy that is specified by @TrafficPolicyId@ .
--
-- /Note:/ Consider using 'trafficPolicyVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyVersion :: Lens.Lens' ListTrafficPolicyInstancesByPolicy Core.Natural
ltpibpTrafficPolicyVersion = Lens.field @"trafficPolicyVersion"
{-# DEPRECATED ltpibpTrafficPolicyVersion "Use generic-lens or generic-optics with 'trafficPolicyVersion' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @hostedzoneid@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Core.Maybe Types.HostedZoneIdMarker)
ltpibpHostedZoneIdMarker = Lens.field @"hostedZoneIdMarker"
{-# DEPRECATED ltpibpHostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead." #-}

-- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Core.Maybe Types.MaxItems)
ltpibpMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ltpibpMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Core.Maybe Types.TrafficPolicyInstanceNameMarker)
ltpibpTrafficPolicyInstanceNameMarker = Lens.field @"trafficPolicyInstanceNameMarker"
{-# DEPRECATED ltpibpTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstancesByPolicy@ request.
--
-- For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the name of the first traffic policy instance that Amazon Route 53 will return if you submit another request.
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibpTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicy (Core.Maybe Types.RecordType)
ltpibpTrafficPolicyInstanceTypeMarker = Lens.field @"trafficPolicyInstanceTypeMarker"
{-# DEPRECATED ltpibpTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

instance Core.AWSRequest ListTrafficPolicyInstancesByPolicy where
  type
    Rs ListTrafficPolicyInstancesByPolicy =
      ListTrafficPolicyInstancesByPolicyResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath "/2013-04-01/trafficpolicyinstances/trafficpolicy",
        Core._rqQuery =
          Core.toQueryValue "id" trafficPolicyId
            Core.<> (Core.toQueryValue "version" trafficPolicyVersion)
            Core.<> (Core.toQueryValue "hostedzoneid" Core.<$> hostedZoneIdMarker)
            Core.<> (Core.toQueryValue "maxitems" Core.<$> maxItems)
            Core.<> ( Core.toQueryValue "trafficpolicyinstancename"
                        Core.<$> trafficPolicyInstanceNameMarker
                    )
            Core.<> ( Core.toQueryValue "trafficpolicyinstancetype"
                        Core.<$> trafficPolicyInstanceTypeMarker
                    ),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListTrafficPolicyInstancesByPolicyResponse'
            Core.<$> ( x Core..@? "TrafficPolicyInstances" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "TrafficPolicyInstance"
                     )
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (x Core..@? "HostedZoneIdMarker")
            Core.<*> (x Core..@? "TrafficPolicyInstanceNameMarker")
            Core.<*> (x Core..@? "TrafficPolicyInstanceTypeMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyInstancesByPolicyResponse' smart constructor.
data ListTrafficPolicyInstancesByPolicyResponse = ListTrafficPolicyInstancesByPolicyResponse'
  { -- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
    trafficPolicyInstances :: [Types.TrafficPolicyInstance],
    -- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ elements in the corresponding request parameters.
    isTruncated :: Core.Bool,
    -- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
    maxItems :: Types.MaxItems,
    -- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance in the next group of traffic policy instances.
    hostedZoneIdMarker :: Core.Maybe Types.ResourceId,
    -- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
    trafficPolicyInstanceNameMarker :: Core.Maybe Types.TrafficPolicyInstanceNameMarker,
    -- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
    trafficPolicyInstanceTypeMarker :: Core.Maybe Types.RecordType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyInstancesByPolicyResponse' value with any optional fields omitted.
mkListTrafficPolicyInstancesByPolicyResponse ::
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListTrafficPolicyInstancesByPolicyResponse
mkListTrafficPolicyInstancesByPolicyResponse
  isTruncated
  maxItems
  responseStatus =
    ListTrafficPolicyInstancesByPolicyResponse'
      { trafficPolicyInstances =
          Core.mempty,
        isTruncated,
        maxItems,
        hostedZoneIdMarker = Core.Nothing,
        trafficPolicyInstanceNameMarker = Core.Nothing,
        trafficPolicyInstanceTypeMarker = Core.Nothing,
        responseStatus
      }

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- /Note:/ Consider using 'trafficPolicyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsTrafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse [Types.TrafficPolicyInstance]
ltpibprrsTrafficPolicyInstances = Lens.field @"trafficPolicyInstances"
{-# DEPRECATED ltpibprrsTrafficPolicyInstances "Use generic-lens or generic-optics with 'trafficPolicyInstances' instead." #-}

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by calling @ListTrafficPolicyInstancesByPolicy@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ elements in the corresponding request parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsIsTruncated :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Core.Bool
ltpibprrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED ltpibprrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstancesByPolicy@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Types.MaxItems
ltpibprrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED ltpibprrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance in the next group of traffic policy instances.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Core.Maybe Types.ResourceId)
ltpibprrsHostedZoneIdMarker = Lens.field @"hostedZoneIdMarker"
{-# DEPRECATED ltpibprrsHostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Core.Maybe Types.TrafficPolicyInstanceNameMarker)
ltpibprrsTrafficPolicyInstanceNameMarker = Lens.field @"trafficPolicyInstanceNameMarker"
{-# DEPRECATED ltpibprrsTrafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead." #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse (Core.Maybe Types.RecordType)
ltpibprrsTrafficPolicyInstanceTypeMarker = Lens.field @"trafficPolicyInstanceTypeMarker"
{-# DEPRECATED ltpibprrsTrafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibprrsResponseStatus :: Lens.Lens' ListTrafficPolicyInstancesByPolicyResponse Core.Int
ltpibprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltpibprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
