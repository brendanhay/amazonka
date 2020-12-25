{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified Availability Zones from the set of Availability Zones for the specified load balancer in EC2-Classic or a default VPC.
--
-- For load balancers in a non-default VPC, use 'DetachLoadBalancerFromSubnets' .
-- There must be at least one Availability Zone registered with a load balancer at all times. After an Availability Zone is removed, all instances registered with the load balancer that are in the removed Availability Zone go into the @OutOfService@ state. Then, the load balancer attempts to equally balance the traffic among its remaining Availability Zones.
-- For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.DisableAvailabilityZonesForLoadBalancer
  ( -- * Creating a request
    DisableAvailabilityZonesForLoadBalancer (..),
    mkDisableAvailabilityZonesForLoadBalancer,

    -- ** Request lenses
    dazflbLoadBalancerName,
    dazflbAvailabilityZones,

    -- * Destructuring the response
    DisableAvailabilityZonesForLoadBalancerResponse (..),
    mkDisableAvailabilityZonesForLoadBalancerResponse,

    -- ** Response lenses
    dazflbrrsAvailabilityZones,
    dazflbrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkDisableAvailabilityZonesForLoadBalancer' smart constructor.
data DisableAvailabilityZonesForLoadBalancer = DisableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The Availability Zones.
    availabilityZones :: [Types.AvailabilityZone]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAvailabilityZonesForLoadBalancer' value with any optional fields omitted.
mkDisableAvailabilityZonesForLoadBalancer ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  DisableAvailabilityZonesForLoadBalancer
mkDisableAvailabilityZonesForLoadBalancer loadBalancerName =
  DisableAvailabilityZonesForLoadBalancer'
    { loadBalancerName,
      availabilityZones = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbLoadBalancerName :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer Types.AccessPointName
dazflbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED dazflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The Availability Zones.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbAvailabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancer [Types.AvailabilityZone]
dazflbAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dazflbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Core.AWSRequest DisableAvailabilityZonesForLoadBalancer where
  type
    Rs DisableAvailabilityZonesForLoadBalancer =
      DisableAvailabilityZonesForLoadBalancerResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "DisableAvailabilityZonesForLoadBalancer")
                Core.<> (Core.pure ("Version", "2012-06-01"))
                Core.<> (Core.toQueryValue "LoadBalancerName" loadBalancerName)
                Core.<> ( Core.toQueryValue
                            "AvailabilityZones"
                            (Core.toQueryList "member" availabilityZones)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "DisableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          DisableAvailabilityZonesForLoadBalancerResponse'
            Core.<$> ( x Core..@? "AvailabilityZones"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output for DisableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkDisableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data DisableAvailabilityZonesForLoadBalancerResponse = DisableAvailabilityZonesForLoadBalancerResponse'
  { -- | The remaining Availability Zones for the load balancer.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableAvailabilityZonesForLoadBalancerResponse' value with any optional fields omitted.
mkDisableAvailabilityZonesForLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DisableAvailabilityZonesForLoadBalancerResponse
mkDisableAvailabilityZonesForLoadBalancerResponse responseStatus =
  DisableAvailabilityZonesForLoadBalancerResponse'
    { availabilityZones =
        Core.Nothing,
      responseStatus
    }

-- | The remaining Availability Zones for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbrrsAvailabilityZones :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse (Core.Maybe [Types.AvailabilityZone])
dazflbrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED dazflbrrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dazflbrrsResponseStatus :: Lens.Lens' DisableAvailabilityZonesForLoadBalancerResponse Core.Int
dazflbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dazflbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
