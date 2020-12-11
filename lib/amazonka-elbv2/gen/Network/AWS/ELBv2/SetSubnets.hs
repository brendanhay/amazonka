{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ssSubnetMappings,
    ssSubnets,
    ssIPAddressType,
    ssLoadBalancerARN,

    -- * Destructuring the response
    SetSubnetsResponse (..),
    mkSetSubnetsResponse,

    -- ** Response lenses
    ssrsAvailabilityZones,
    ssrsIPAddressType,
    ssrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetSubnets' smart constructor.
data SetSubnets = SetSubnets'
  { subnetMappings ::
      Lude.Maybe [SubnetMapping],
    subnets :: Lude.Maybe [Lude.Text],
    ipAddressType :: Lude.Maybe IPAddressType,
    loadBalancerARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSubnets' with the minimum fields required to make a request.
--
-- * 'ipAddressType' - [Network Load Balancers] The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener. Internal load balancers must use @ipv4@ .
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'subnetMappings' - The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
-- * 'subnets' - The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
mkSetSubnets ::
  -- | 'loadBalancerARN'
  Lude.Text ->
  SetSubnets
mkSetSubnets pLoadBalancerARN_ =
  SetSubnets'
    { subnetMappings = Lude.Nothing,
      subnets = Lude.Nothing,
      ipAddressType = Lude.Nothing,
      loadBalancerARN = pLoadBalancerARN_
    }

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
--
-- /Note:/ Consider using 'subnetMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSubnetMappings :: Lens.Lens' SetSubnets (Lude.Maybe [SubnetMapping])
ssSubnetMappings = Lens.lens (subnetMappings :: SetSubnets -> Lude.Maybe [SubnetMapping]) (\s a -> s {subnetMappings = a} :: SetSubnets)
{-# DEPRECATED ssSubnetMappings "Use generic-lens or generic-optics with 'subnetMappings' instead." #-}

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssSubnets :: Lens.Lens' SetSubnets (Lude.Maybe [Lude.Text])
ssSubnets = Lens.lens (subnets :: SetSubnets -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: SetSubnets)
{-# DEPRECATED ssSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | [Network Load Balancers] The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener. Internal load balancers must use @ipv4@ .
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssIPAddressType :: Lens.Lens' SetSubnets (Lude.Maybe IPAddressType)
ssIPAddressType = Lens.lens (ipAddressType :: SetSubnets -> Lude.Maybe IPAddressType) (\s a -> s {ipAddressType = a} :: SetSubnets)
{-# DEPRECATED ssIPAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLoadBalancerARN :: Lens.Lens' SetSubnets Lude.Text
ssLoadBalancerARN = Lens.lens (loadBalancerARN :: SetSubnets -> Lude.Text) (\s a -> s {loadBalancerARN = a} :: SetSubnets)
{-# DEPRECATED ssLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

instance Lude.AWSRequest SetSubnets where
  type Rs SetSubnets = SetSubnetsResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "SetSubnetsResult"
      ( \s h x ->
          SetSubnetsResponse'
            Lude.<$> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (x Lude..@? "IpAddressType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetSubnets where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath SetSubnets where
  toPath = Lude.const "/"

instance Lude.ToQuery SetSubnets where
  toQuery SetSubnets' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("SetSubnets" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "SubnetMappings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> subnetMappings),
        "Subnets"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> subnets),
        "IpAddressType" Lude.=: ipAddressType,
        "LoadBalancerArn" Lude.=: loadBalancerARN
      ]

-- | /See:/ 'mkSetSubnetsResponse' smart constructor.
data SetSubnetsResponse = SetSubnetsResponse'
  { availabilityZones ::
      Lude.Maybe [AvailabilityZone],
    ipAddressType :: Lude.Maybe IPAddressType,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetSubnetsResponse' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - Information about the subnets.
-- * 'ipAddressType' - [Network Load Balancers] The IP address type.
-- * 'responseStatus' - The response status code.
mkSetSubnetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetSubnetsResponse
mkSetSubnetsResponse pResponseStatus_ =
  SetSubnetsResponse'
    { availabilityZones = Lude.Nothing,
      ipAddressType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the subnets.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsAvailabilityZones :: Lens.Lens' SetSubnetsResponse (Lude.Maybe [AvailabilityZone])
ssrsAvailabilityZones = Lens.lens (availabilityZones :: SetSubnetsResponse -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: SetSubnetsResponse)
{-# DEPRECATED ssrsAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | [Network Load Balancers] The IP address type.
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsIPAddressType :: Lens.Lens' SetSubnetsResponse (Lude.Maybe IPAddressType)
ssrsIPAddressType = Lens.lens (ipAddressType :: SetSubnetsResponse -> Lude.Maybe IPAddressType) (\s a -> s {ipAddressType = a} :: SetSubnetsResponse)
{-# DEPRECATED ssrsIPAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssrsResponseStatus :: Lens.Lens' SetSubnetsResponse Lude.Int
ssrsResponseStatus = Lens.lens (responseStatus :: SetSubnetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetSubnetsResponse)
{-# DEPRECATED ssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
