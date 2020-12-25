{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified Availability Zones to the set of Availability Zones for the specified load balancer in EC2-Classic or a default VPC.
--
-- For load balancers in a non-default VPC, use 'AttachLoadBalancerToSubnets' .
-- The load balancer evenly distributes requests across all its registered Availability Zones that contain instances. For more information, see <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-az.html Add or Remove Availability Zones> in the /Classic Load Balancers Guide/ .
module Network.AWS.ELB.EnableAvailabilityZonesForLoadBalancer
  ( -- * Creating a request
    EnableAvailabilityZonesForLoadBalancer (..),
    mkEnableAvailabilityZonesForLoadBalancer,

    -- ** Request lenses
    eazflbLoadBalancerName,
    eazflbAvailabilityZones,

    -- * Destructuring the response
    EnableAvailabilityZonesForLoadBalancerResponse (..),
    mkEnableAvailabilityZonesForLoadBalancerResponse,

    -- ** Response lenses
    eazflbrrsAvailabilityZones,
    eazflbrrsResponseStatus,
  )
where

import qualified Network.AWS.ELB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkEnableAvailabilityZonesForLoadBalancer' smart constructor.
data EnableAvailabilityZonesForLoadBalancer = EnableAvailabilityZonesForLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Types.AccessPointName,
    -- | The Availability Zones. These must be in the same region as the load balancer.
    availabilityZones :: [Types.AvailabilityZone]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAvailabilityZonesForLoadBalancer' value with any optional fields omitted.
mkEnableAvailabilityZonesForLoadBalancer ::
  -- | 'loadBalancerName'
  Types.AccessPointName ->
  EnableAvailabilityZonesForLoadBalancer
mkEnableAvailabilityZonesForLoadBalancer loadBalancerName =
  EnableAvailabilityZonesForLoadBalancer'
    { loadBalancerName,
      availabilityZones = Core.mempty
    }

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbLoadBalancerName :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer Types.AccessPointName
eazflbLoadBalancerName = Lens.field @"loadBalancerName"
{-# DEPRECATED eazflbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The Availability Zones. These must be in the same region as the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbAvailabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancer [Types.AvailabilityZone]
eazflbAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED eazflbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

instance Core.AWSRequest EnableAvailabilityZonesForLoadBalancer where
  type
    Rs EnableAvailabilityZonesForLoadBalancer =
      EnableAvailabilityZonesForLoadBalancerResponse
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
            ( Core.pure ("Action", "EnableAvailabilityZonesForLoadBalancer")
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
      "EnableAvailabilityZonesForLoadBalancerResult"
      ( \s h x ->
          EnableAvailabilityZonesForLoadBalancerResponse'
            Core.<$> ( x Core..@? "AvailabilityZones"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Contains the output of EnableAvailabilityZonesForLoadBalancer.
--
-- /See:/ 'mkEnableAvailabilityZonesForLoadBalancerResponse' smart constructor.
data EnableAvailabilityZonesForLoadBalancerResponse = EnableAvailabilityZonesForLoadBalancerResponse'
  { -- | The updated list of Availability Zones for the load balancer.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableAvailabilityZonesForLoadBalancerResponse' value with any optional fields omitted.
mkEnableAvailabilityZonesForLoadBalancerResponse ::
  -- | 'responseStatus'
  Core.Int ->
  EnableAvailabilityZonesForLoadBalancerResponse
mkEnableAvailabilityZonesForLoadBalancerResponse responseStatus =
  EnableAvailabilityZonesForLoadBalancerResponse'
    { availabilityZones =
        Core.Nothing,
      responseStatus
    }

-- | The updated list of Availability Zones for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbrrsAvailabilityZones :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse (Core.Maybe [Types.AvailabilityZone])
eazflbrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED eazflbrrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eazflbrrsResponseStatus :: Lens.Lens' EnableAvailabilityZonesForLoadBalancerResponse Core.Int
eazflbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED eazflbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
