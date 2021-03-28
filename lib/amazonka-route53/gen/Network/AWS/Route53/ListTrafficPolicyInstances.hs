{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTrafficPolicyInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the traffic policy instances that you created by using the current AWS account.
--
-- Route 53 returns a maximum of 100 items in each response. If you have a lot of traffic policy instances, you can use the @MaxItems@ parameter to list them in groups of up to 100.
module Network.AWS.Route53.ListTrafficPolicyInstances
    (
    -- * Creating a request
      ListTrafficPolicyInstances (..)
    , mkListTrafficPolicyInstances
    -- ** Request lenses
    , ltpiHostedZoneIdMarker
    , ltpiMaxItems
    , ltpiTrafficPolicyInstanceNameMarker
    , ltpiTrafficPolicyInstanceTypeMarker

    -- * Destructuring the response
    , ListTrafficPolicyInstancesResponse (..)
    , mkListTrafficPolicyInstancesResponse
    -- ** Response lenses
    , ltpirrsTrafficPolicyInstances
    , ltpirrsIsTruncated
    , ltpirrsMaxItems
    , ltpirrsHostedZoneIdMarker
    , ltpirrsTrafficPolicyInstanceNameMarker
    , ltpirrsTrafficPolicyInstanceTypeMarker
    , ltpirrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to get information about the traffic policy instances that you created by using the current AWS account.
--
-- /See:/ 'mkListTrafficPolicyInstances' smart constructor.
data ListTrafficPolicyInstances = ListTrafficPolicyInstances'
  { hostedZoneIdMarker :: Core.Maybe Types.HostedZoneIdMarker
    -- ^ If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @HostedZoneId@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
  , maxItems :: Core.Maybe Types.MaxItems
    -- ^ The maximum number of traffic policy instances that you want Amazon Route 53 to return in response to a @ListTrafficPolicyInstances@ request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
  , trafficPolicyInstanceNameMarker :: Core.Maybe Types.TrafficPolicyInstanceNameMarker
    -- ^ If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
  , trafficPolicyInstanceTypeMarker :: Core.Maybe Types.RecordType
    -- ^ If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyInstances' value with any optional fields omitted.
mkListTrafficPolicyInstances
    :: ListTrafficPolicyInstances
mkListTrafficPolicyInstances
  = ListTrafficPolicyInstances'{hostedZoneIdMarker = Core.Nothing,
                                maxItems = Core.Nothing,
                                trafficPolicyInstanceNameMarker = Core.Nothing,
                                trafficPolicyInstanceTypeMarker = Core.Nothing}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @HostedZoneId@ , specify the value of @HostedZoneIdMarker@ from the previous response, which is the hosted zone ID of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstances (Core.Maybe Types.HostedZoneIdMarker)
ltpiHostedZoneIdMarker = Lens.field @"hostedZoneIdMarker"
{-# INLINEABLE ltpiHostedZoneIdMarker #-}
{-# DEPRECATED hostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead"  #-}

-- | The maximum number of traffic policy instances that you want Amazon Route 53 to return in response to a @ListTrafficPolicyInstances@ request. If you have more than @MaxItems@ traffic policy instances, the value of the @IsTruncated@ element in the response is @true@ , and the values of @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ represent the first traffic policy instance in the next group of @MaxItems@ traffic policy instances.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiMaxItems :: Lens.Lens' ListTrafficPolicyInstances (Core.Maybe Types.MaxItems)
ltpiMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ltpiMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancename@ , specify the value of @TrafficPolicyInstanceNameMarker@ from the previous response, which is the name of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstances (Core.Maybe Types.TrafficPolicyInstanceNameMarker)
ltpiTrafficPolicyInstanceNameMarker = Lens.field @"trafficPolicyInstanceNameMarker"
{-# INLINEABLE ltpiTrafficPolicyInstanceNameMarker #-}
{-# DEPRECATED trafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead"  #-}

-- | If the value of @IsTruncated@ in the previous response was @true@ , you have more traffic policy instances. To get more traffic policy instances, submit another @ListTrafficPolicyInstances@ request. For the value of @trafficpolicyinstancetype@ , specify the value of @TrafficPolicyInstanceTypeMarker@ from the previous response, which is the type of the first traffic policy instance in the next group of traffic policy instances.
--
-- If the value of @IsTruncated@ in the previous response was @false@ , there are no more traffic policy instances to get.
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpiTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstances (Core.Maybe Types.RecordType)
ltpiTrafficPolicyInstanceTypeMarker = Lens.field @"trafficPolicyInstanceTypeMarker"
{-# INLINEABLE ltpiTrafficPolicyInstanceTypeMarker #-}
{-# DEPRECATED trafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead"  #-}

instance Core.ToQuery ListTrafficPolicyInstances where
        toQuery ListTrafficPolicyInstances{..}
          = Core.maybe Core.mempty (Core.toQueryPair "hostedzoneid")
              hostedZoneIdMarker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxitems") maxItems
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "trafficpolicyinstancename")
                trafficPolicyInstanceNameMarker
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "trafficpolicyinstancetype")
                trafficPolicyInstanceTypeMarker

instance Core.ToHeaders ListTrafficPolicyInstances where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListTrafficPolicyInstances where
        type Rs ListTrafficPolicyInstances =
             ListTrafficPolicyInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath = "/2013-04-01/trafficpolicyinstances",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXML
              (\ s h x ->
                 ListTrafficPolicyInstancesResponse' Core.<$>
                   (x Core..@ "TrafficPolicyInstances" Core..@! Core.mempty Core..<@>
                      Core.parseXMLList "TrafficPolicyInstance")
                     Core.<*> x Core..@ "IsTruncated"
                     Core.<*> x Core..@ "MaxItems"
                     Core.<*> x Core..@? "HostedZoneIdMarker"
                     Core.<*> x Core..@? "TrafficPolicyInstanceNameMarker"
                     Core.<*> x Core..@? "TrafficPolicyInstanceTypeMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A complex type that contains the response information for the request.
--
-- /See:/ 'mkListTrafficPolicyInstancesResponse' smart constructor.
data ListTrafficPolicyInstancesResponse = ListTrafficPolicyInstancesResponse'
  { trafficPolicyInstances :: [Types.TrafficPolicyInstance]
    -- ^ A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
  , isTruncated :: Core.Bool
    -- ^ A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get more traffic policy instances by calling @ListTrafficPolicyInstances@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
  , maxItems :: Types.MaxItems
    -- ^ The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstances@ that produced the current response.
  , hostedZoneIdMarker :: Core.Maybe Types.ResourceId
    -- ^ If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request. 
  , trafficPolicyInstanceNameMarker :: Core.Maybe Types.TrafficPolicyInstanceNameMarker
    -- ^ If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request. 
  , trafficPolicyInstanceTypeMarker :: Core.Maybe Types.RecordType
    -- ^ If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListTrafficPolicyInstancesResponse' value with any optional fields omitted.
mkListTrafficPolicyInstancesResponse
    :: Core.Bool -- ^ 'isTruncated'
    -> Types.MaxItems -- ^ 'maxItems'
    -> Core.Int -- ^ 'responseStatus'
    -> ListTrafficPolicyInstancesResponse
mkListTrafficPolicyInstancesResponse isTruncated maxItems
  responseStatus
  = ListTrafficPolicyInstancesResponse'{trafficPolicyInstances =
                                          Core.mempty,
                                        isTruncated, maxItems, hostedZoneIdMarker = Core.Nothing,
                                        trafficPolicyInstanceNameMarker = Core.Nothing,
                                        trafficPolicyInstanceTypeMarker = Core.Nothing,
                                        responseStatus}

-- | A list that contains one @TrafficPolicyInstance@ element for each traffic policy instance that matches the elements in the request.
--
-- /Note:/ Consider using 'trafficPolicyInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsTrafficPolicyInstances :: Lens.Lens' ListTrafficPolicyInstancesResponse [Types.TrafficPolicyInstance]
ltpirrsTrafficPolicyInstances = Lens.field @"trafficPolicyInstances"
{-# INLINEABLE ltpirrsTrafficPolicyInstances #-}
{-# DEPRECATED trafficPolicyInstances "Use generic-lens or generic-optics with 'trafficPolicyInstances' instead"  #-}

-- | A flag that indicates whether there are more traffic policy instances to be listed. If the response was truncated, you can get more traffic policy instances by calling @ListTrafficPolicyInstances@ again and specifying the values of the @HostedZoneIdMarker@ , @TrafficPolicyInstanceNameMarker@ , and @TrafficPolicyInstanceTypeMarker@ in the corresponding request parameters.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsIsTruncated :: Lens.Lens' ListTrafficPolicyInstancesResponse Core.Bool
ltpirrsIsTruncated = Lens.field @"isTruncated"
{-# INLINEABLE ltpirrsIsTruncated #-}
{-# DEPRECATED isTruncated "Use generic-lens or generic-optics with 'isTruncated' instead"  #-}

-- | The value that you specified for the @MaxItems@ parameter in the call to @ListTrafficPolicyInstances@ that produced the current response.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsMaxItems :: Lens.Lens' ListTrafficPolicyInstancesResponse Types.MaxItems
ltpirrsMaxItems = Lens.field @"maxItems"
{-# INLINEABLE ltpirrsMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

-- | If @IsTruncated@ is @true@ , @HostedZoneIdMarker@ is the ID of the hosted zone of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request. 
--
-- /Note:/ Consider using 'hostedZoneIdMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsHostedZoneIdMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Core.Maybe Types.ResourceId)
ltpirrsHostedZoneIdMarker = Lens.field @"hostedZoneIdMarker"
{-# INLINEABLE ltpirrsHostedZoneIdMarker #-}
{-# DEPRECATED hostedZoneIdMarker "Use generic-lens or generic-optics with 'hostedZoneIdMarker' instead"  #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceNameMarker@ is the name of the first traffic policy instance that Route 53 will return if you submit another @ListTrafficPolicyInstances@ request. 
--
-- /Note:/ Consider using 'trafficPolicyInstanceNameMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsTrafficPolicyInstanceNameMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Core.Maybe Types.TrafficPolicyInstanceNameMarker)
ltpirrsTrafficPolicyInstanceNameMarker = Lens.field @"trafficPolicyInstanceNameMarker"
{-# INLINEABLE ltpirrsTrafficPolicyInstanceNameMarker #-}
{-# DEPRECATED trafficPolicyInstanceNameMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceNameMarker' instead"  #-}

-- | If @IsTruncated@ is @true@ , @TrafficPolicyInstanceTypeMarker@ is the DNS type of the resource record sets that are associated with the first traffic policy instance that Amazon Route 53 will return if you submit another @ListTrafficPolicyInstances@ request. 
--
-- /Note:/ Consider using 'trafficPolicyInstanceTypeMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsTrafficPolicyInstanceTypeMarker :: Lens.Lens' ListTrafficPolicyInstancesResponse (Core.Maybe Types.RecordType)
ltpirrsTrafficPolicyInstanceTypeMarker = Lens.field @"trafficPolicyInstanceTypeMarker"
{-# INLINEABLE ltpirrsTrafficPolicyInstanceTypeMarker #-}
{-# DEPRECATED trafficPolicyInstanceTypeMarker "Use generic-lens or generic-optics with 'trafficPolicyInstanceTypeMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltpirrsResponseStatus :: Lens.Lens' ListTrafficPolicyInstancesResponse Core.Int
ltpirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltpirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
