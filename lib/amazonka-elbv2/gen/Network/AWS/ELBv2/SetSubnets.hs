{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetSubnets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Availability Zones for the specified public subnets for the specified Application Load Balancer or Network Load Balancer. The specified subnets replace the previously enabled subnets.
--
-- When you specify subnets for a Network Load Balancer, you must include all subnets that were enabled previously, with their existing configurations, plus any additional subnets.
module Network.AWS.ELBv2.SetSubnets
  ( -- * Creating a request
    SetSubnets (..),
    mkSetSubnets,

    -- ** Request lenses
    ssLoadBalancerArn,
    ssIpAddressType,
    ssSubnetMappings,
    ssSubnets,

    -- * Destructuring the response
    SetSubnetsResponse (..),
    mkSetSubnetsResponse,

    -- ** Response lenses
    ssrrsAvailabilityZones,
    ssrrsIpAddressType,
    ssrrsResponseStatus,
  )
where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetSubnets' smart constructor.
data SetSubnets = SetSubnets'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Types.LoadBalancerArn,
    -- | [Network Load Balancers] The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener. Internal load balancers must use @ipv4@ .
    ipAddressType :: Core.Maybe Types.IpAddressType,
    -- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
    --
    -- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
    -- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
    -- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
    -- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
    subnetMappings :: Core.Maybe [Types.SubnetMapping],
    -- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
    --
    -- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
    -- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
    -- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
    -- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
    subnets :: Core.Maybe [Types.SubnetId]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSubnets' value with any optional fields omitted.
mkSetSubnets ::
  -- | 'loadBalancerArn'
  Types.LoadBalancerArn ->
  SetSubnets
mkSetSubnets loadBalancerArn =
  SetSubnets'
    { loadBalancerArn,
      ipAddressType = Core.Nothing,
      subnetMappings = Core.Nothing,
      subnets = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLoadBalancerArn :: Lens.Lens' SetSubnets Types.LoadBalancerArn
ssLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# DEPRECATED ssLoadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead." #-}

-- | [Network Load Balancers] The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener. Internal load balancers must use @ipv4@ .
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssIpAddressType :: Lens.Lens' SetSubnets (Core.Maybe Types.IpAddressType)
ssIpAddressType = Lens.field @"ipAddressType"
{-# DEPRECATED ssIpAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
--
-- /Note:/ Consider using 'subnetMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSubnetMappings :: Lens.Lens' SetSubnets (Core.Maybe [Types.SubnetMapping])
ssSubnetMappings = Lens.field @"subnetMappings"
{-# DEPRECATED ssSubnetMappings "Use generic-lens or generic-optics with 'subnetMappings' instead." #-}

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSubnets :: Lens.Lens' SetSubnets (Core.Maybe [Types.SubnetId])
ssSubnets = Lens.field @"subnets"
{-# DEPRECATED ssSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

instance Core.AWSRequest SetSubnets where
  type Rs SetSubnets = SetSubnetsResponse
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
            ( Core.pure ("Action", "SetSubnets")
                Core.<> (Core.pure ("Version", "2015-12-01"))
                Core.<> (Core.toQueryValue "LoadBalancerArn" loadBalancerArn)
                Core.<> (Core.toQueryValue "IpAddressType" Core.<$> ipAddressType)
                Core.<> ( Core.toQueryValue
                            "SubnetMappings"
                            (Core.toQueryList "member" Core.<$> subnetMappings)
                        )
                Core.<> ( Core.toQueryValue
                            "Subnets"
                            (Core.toQueryList "member" Core.<$> subnets)
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "SetSubnetsResult"
      ( \s h x ->
          SetSubnetsResponse'
            Core.<$> ( x Core..@? "AvailabilityZones"
                         Core..<@> Core.parseXMLList "member"
                     )
            Core.<*> (x Core..@? "IpAddressType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSetSubnetsResponse' smart constructor.
data SetSubnetsResponse = SetSubnetsResponse'
  { -- | Information about the subnets.
    availabilityZones :: Core.Maybe [Types.AvailabilityZone],
    -- | [Network Load Balancers] The IP address type.
    ipAddressType :: Core.Maybe Types.IpAddressType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetSubnetsResponse' value with any optional fields omitted.
mkSetSubnetsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SetSubnetsResponse
mkSetSubnetsResponse responseStatus =
  SetSubnetsResponse'
    { availabilityZones = Core.Nothing,
      ipAddressType = Core.Nothing,
      responseStatus
    }

-- | Information about the subnets.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsAvailabilityZones :: Lens.Lens' SetSubnetsResponse (Core.Maybe [Types.AvailabilityZone])
ssrrsAvailabilityZones = Lens.field @"availabilityZones"
{-# DEPRECATED ssrrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | [Network Load Balancers] The IP address type.
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsIpAddressType :: Lens.Lens' SetSubnetsResponse (Core.Maybe Types.IpAddressType)
ssrrsIpAddressType = Lens.field @"ipAddressType"
{-# DEPRECATED ssrrsIpAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrrsResponseStatus :: Lens.Lens' SetSubnetsResponse Core.Int
ssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
