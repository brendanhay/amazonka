{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created in a specified hosted zone.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
module Network.AWS.Route53.ListTrafficPolicyInstancesByHostedZone
    (
    -- * Creating a request
      ListTrafficPolicyInstancesByHostedZone (..)
    , mkListTrafficPolicyInstancesByHostedZone
    -- ** Request lenses
    , ltpibhzHostedZoneId
    , ltpibhzMaxItems
    , ltpibhzTrafficPolicyInstanceNameMarker
    , ltpibhzTrafficPolicyInstanceTypeMarker

    -- * Destructuring the response
    , ListTrafficPolicyInstancesByHostedZoneResponse (..)
    , mkListTrafficPolicyInstancesByHostedZoneResponse
    -- ** Response lenses
    , ltpibhzrrsTrafficPolicyInstances
    , ltpibhzrrsIsTruncated
    , ltpibhzrrsMaxItems
    , ltpibhzrrsTrafficPolicyInstanceNameMarker
    , ltpibhzrrsTrafficPolicyInstanceTypeMarker
    , ltpibhzrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request for the traffic policy instances that you created in a specified hosted zone.
--
-- /See:/ 'mkListTrafficPolicyInstancesByHostedZone' smart constructor.
data ListTrafficPolicyInstancesByHostedZone = ListTrafficPolicyInstancesByHostedZone'
  { hostedZoneId :: Types.ResourceId
    -- ^ The ID of the hosted zone that you want to list traffic policy instances for.
  , maxItems :: Core.Maybe Types.MaxItems
    -- ^ The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
  , trafficPolicyInstanceNameMarker :: Core.Maybe Types.TrafficPolicyInstanceNameMarker
    -- ^ If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
  , trafficPolicyInstanceTypeMarker :: Core.Maybe Types.RecordType
    -- ^ If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyInstancesByHostedZone' value with any optional fields omitted.
mkListTrafficPolicyInstancesByHostedZone
    :: Types.ResourceId -- ^ 'hostedZoneId'
    -> ListTrafficPolicyInstancesByHostedZone
mkListTrafficPolicyInstancesByHostedZone hostedZoneId
  = ListTrafficPolicyInstancesByHostedZone'{hostedZoneId,
                                            maxItems = Core.Nothing,
                                            trafficPolicyInstanceNameMarker = Core.Nothing,
                                            trafficPolicyInstanceTypeMarker = Core.Nothing}

-- | The ID of the hosted zone that you want to list traffic policy instances for.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzHostedZoneId :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone Types.ResourceId
ltpibhzHostedZoneId = Lens.field @"hostedZoneId"
{-# INLINEABLE ltpibhzHostedZoneId #-}
{-# DEPRECATED hostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead"  #-}

-- | The maximum number of traffic policy instances to be included in the response body for this request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance that Amazon Route 53 will return if you submit another request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Core.Maybe Types.MaxItems)
ltpibhzMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ltpibhzMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Core.Maybe Types.TrafficPolicyInstanceNameMarker)
ltpibhzTrafficPolicyInstanceNameMarker = Lens.field @"trafficPolicyInstanceNameMarker"
{-# INLINEABLE ltpibhzTrafficPolicyInstanceNameMarker #-}
{-# DEPRECATED trafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead"  #-}

-- | If the value of @IsTruncated@ in the previous response is true, you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZone (Core.Maybe Types.RecordType)
ltpibhzTrafficPolicyInstanceTypeMarker = Lens.field @"trafficPolicyInstanceTypeMarker"
{-# INLINEABLE ltpibhzTrafficPolicyInstanceTypeMarker #-}
{-# DEPRECATED trafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead"  #-}

instance Core.ToQuery ListTrafficPolicyInstancesByHostedZone where
        toQuery ListTrafficPolicyInstancesByHostedZone{..}
          = Core.toQueryPair "id" hostedZoneId Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxitems") maxItems
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "trafficpolicyinstancename")
                trafficPolicyInstanceNameMarker
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "trafficpolicyinstancetype")
                trafficPolicyInstanceTypeMarker

instance Core.ToHeaders ListTrafficPolicyInstancesByHostedZone
         where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTrafficPolicyInstancesByHostedZone
         where
        type Rs ListTrafficPolicyInstancesByHostedZone =
             ListTrafficPolicyInstancesByHostedZoneResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2013-04-01/trafficpolicyinstances/hostedzone",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListTrafficPolicyInstancesByHostedZoneResponse' Core.<$>
                   (x Core..@ "TrafficPolicyInstances" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "TrafficPolicyInstance")
                     Core.<*> x Core..@ "IsTruncated"
                     Core.<*> x Core..@ "MaxItems"
                     Core.<*> x Core..@? "TrafficPolicyInstanceNameMarker"
                     Core.<*> x Core..@? "TrafficPolicyInstanceTypeMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyInstancesByHostedZoneResponse' smart constructor.
data ListTrafficPolicyInstancesByHostedZoneResponse = ListTrafficPolicyInstancesByHostedZoneResponse'
  { trafficPolicyInstances :: [Types.TrafficPolicyInstance]
    -- ^ A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request. 
  , isTruncated :: Core.Bool
    -- ^ A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by submitting another @ListTrafficPolicyInstancesByHostedZone@ request and specifying the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
  , maxItems :: Types.MaxItems
    -- ^ The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicyInstancesByHostedZone@ request that produced the current response.
  , trafficPolicyInstanceNameMarker :: Core.Maybe Types.TrafficPolicyInstanceNameMarker
    -- ^ If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of traffic policy instances.
  , trafficPolicyInstanceTypeMarker :: Core.Maybe Types.RecordType
    -- ^ If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of traffic policy instances.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyInstancesByHostedZoneResponse' value with any optional fields omitted.
mkListTrafficPolicyInstancesByHostedZoneResponse
    :: Core.Bool -- ^ 'isTruncated'
    -> Types.MaxItems -- ^ 'maxItems'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTrafficPolicyInstancesByHostedZoneResponse
mkListTrafficPolicyInstancesByHostedZoneResponse isTruncated
  maxItems responseStatus
  = ListTrafficPolicyInstancesByHostedZoneResponse'{trafficPolicyInstances
                                                      = Core.mempty,
                                                    isTruncated, maxItems,
                                                    trafficPolicyInstanceNameMarker = Core.Nothing,
                                                    trafficPolicyInstanceTypeMarker = Core.Nothing,
                                                    responseStatus}

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request. 
--
-- /Note:/ Consider using 'trafficPolicyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrrsTrafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse [Types.TrafficPolicyInstance]
ltpibhzrrsTrafficPolicyInstances = Lens.field @"trafficPolicyInstances"
{-# INLINEABLE ltpibhzrrsTrafficPolicyInstances #-}
{-# DEPRECATED trafficPolicyInstances "Use generic-lens or generic-optics with 'trafficPolicyInstances' instead"  #-}

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get the next group of traffic policy instances by submitting another @ListTrafficPolicyInstancesByHostedZone@ request and specifying the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrrsIsTruncated :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Core.Bool
ltpibhzrrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE ltpibhzrrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The value that you specified for the @MaxItems@ parameter in the @ListTrafficPolicyInstancesByHostedZone@ request that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrrsMaxItems :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Types.MaxItems
ltpibhzrrsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ltpibhzrrsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrrsTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse (Core.Maybe Types.TrafficPolicyInstanceNameMarker)
ltpibhzrrsTrafficPolicyInstanceNameMarker = Lens.field @"trafficPolicyInstanceNameMarker"
{-# INLINEABLE ltpibhzrrsTrafficPolicyInstanceNameMarker #-}
{-# DEPRECATED trafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead"  #-}

-- | If @IsTruncated@ is true, @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance in the next group of traffic policy instances.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrrsTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse (Core.Maybe Types.RecordType)
ltpibhzrrsTrafficPolicyInstanceTypeMarker = Lens.field @"trafficPolicyInstanceTypeMarker"
{-# INLINEABLE ltpibhzrrsTrafficPolicyInstanceTypeMarker #-}
{-# DEPRECATED trafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpibhzrrsResponseStatus :: Lens.Lens' ListTrafficPolicyInstancesByHostedZoneResponse Core.Int
ltpibhzrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltpibhzrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
