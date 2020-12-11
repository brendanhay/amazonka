{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.CreateLoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Application Load Balancer, Network Load Balancer, or Gateway Load Balancer.
--
-- For more information, see the following:
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html Application Load Balancers>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html Network Load Balancers>
--
--
--     * <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-load-balancers.html Gateway Load Balancers>
--
--
-- This operation is idempotent, which means that it completes at most one time. If you attempt to create multiple load balancers with the same settings, each call succeeds.
module Network.AWS.ELBv2.CreateLoadBalancer
  ( -- * Creating a request
    CreateLoadBalancer (..),
    mkCreateLoadBalancer,

    -- ** Request lenses
    clbSubnetMappings,
    clbSecurityGroups,
    clbSubnets,
    clbCustomerOwnedIPv4Pool,
    clbIPAddressType,
    clbScheme,
    clbType,
    clbTags,
    clbName,

    -- * Destructuring the response
    CreateLoadBalancerResponse (..),
    mkCreateLoadBalancerResponse,

    -- ** Response lenses
    clbrsLoadBalancers,
    clbrsResponseStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { subnetMappings ::
      Lude.Maybe [SubnetMapping],
    securityGroups :: Lude.Maybe [Lude.Text],
    subnets :: Lude.Maybe [Lude.Text],
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    ipAddressType :: Lude.Maybe IPAddressType,
    scheme :: Lude.Maybe LoadBalancerSchemeEnum,
    type' :: Lude.Maybe LoadBalancerTypeEnum,
    tags :: Lude.Maybe (Lude.NonEmpty Tag),
    name :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateLoadBalancer' with the minimum fields required to make a request.
--
-- * 'customerOwnedIPv4Pool' - [Application Load Balancers on Outposts] The ID of the customer-owned address pool (CoIP pool).
-- * 'ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
-- * 'name' - The name of the load balancer.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, must not begin or end with a hyphen, and must not begin with "internal-".
-- * 'scheme' - The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
-- The default is an Internet-facing load balancer.
-- You cannot specify a scheme for a Gateway Load Balancer.
-- * 'securityGroups' - [Application Load Balancers] The IDs of the security groups for the load balancer.
-- * 'subnetMappings' - The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- * 'subnets' - The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones.
-- * 'tags' - The tags to assign to the load balancer.
-- * 'type'' - The type of load balancer. The default is @application@ .
mkCreateLoadBalancer ::
  -- | 'name'
  Lude.Text ->
  CreateLoadBalancer
mkCreateLoadBalancer pName_ =
  CreateLoadBalancer'
    { subnetMappings = Lude.Nothing,
      securityGroups = Lude.Nothing,
      subnets = Lude.Nothing,
      customerOwnedIPv4Pool = Lude.Nothing,
      ipAddressType = Lude.Nothing,
      scheme = Lude.Nothing,
      type' = Lude.Nothing,
      tags = Lude.Nothing,
      name = pName_
    }

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones. You cannot specify Elastic IP addresses for your subnets.
--
-- /Note:/ Consider using 'subnetMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSubnetMappings :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [SubnetMapping])
clbSubnetMappings = Lens.lens (subnetMappings :: CreateLoadBalancer -> Lude.Maybe [SubnetMapping]) (\s a -> s {subnetMappings = a} :: CreateLoadBalancer)
{-# DEPRECATED clbSubnetMappings "Use generic-lens or generic-optics with 'subnetMappings' instead." #-}

-- | [Application Load Balancers] The IDs of the security groups for the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSecurityGroups :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Lude.Text])
clbSecurityGroups = Lens.lens (securityGroups :: CreateLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: CreateLoadBalancer)
{-# DEPRECATED clbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSubnets :: Lens.Lens' CreateLoadBalancer (Lude.Maybe [Lude.Text])
clbSubnets = Lens.lens (subnets :: CreateLoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {subnets = a} :: CreateLoadBalancer)
{-# DEPRECATED clbSubnets "Use generic-lens or generic-optics with 'subnets' instead." #-}

-- | [Application Load Balancers on Outposts] The ID of the customer-owned address pool (CoIP pool).
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCustomerOwnedIPv4Pool :: Lens.Lens' CreateLoadBalancer (Lude.Maybe Lude.Text)
clbCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: CreateLoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: CreateLoadBalancer)
{-# DEPRECATED clbCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbIPAddressType :: Lens.Lens' CreateLoadBalancer (Lude.Maybe IPAddressType)
clbIPAddressType = Lens.lens (ipAddressType :: CreateLoadBalancer -> Lude.Maybe IPAddressType) (\s a -> s {ipAddressType = a} :: CreateLoadBalancer)
{-# DEPRECATED clbIPAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
-- The default is an Internet-facing load balancer.
-- You cannot specify a scheme for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbScheme :: Lens.Lens' CreateLoadBalancer (Lude.Maybe LoadBalancerSchemeEnum)
clbScheme = Lens.lens (scheme :: CreateLoadBalancer -> Lude.Maybe LoadBalancerSchemeEnum) (\s a -> s {scheme = a} :: CreateLoadBalancer)
{-# DEPRECATED clbScheme "Use generic-lens or generic-optics with 'scheme' instead." #-}

-- | The type of load balancer. The default is @application@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbType :: Lens.Lens' CreateLoadBalancer (Lude.Maybe LoadBalancerTypeEnum)
clbType = Lens.lens (type' :: CreateLoadBalancer -> Lude.Maybe LoadBalancerTypeEnum) (\s a -> s {type' = a} :: CreateLoadBalancer)
{-# DEPRECATED clbType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The tags to assign to the load balancer.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbTags :: Lens.Lens' CreateLoadBalancer (Lude.Maybe (Lude.NonEmpty Tag))
clbTags = Lens.lens (tags :: CreateLoadBalancer -> Lude.Maybe (Lude.NonEmpty Tag)) (\s a -> s {tags = a} :: CreateLoadBalancer)
{-# DEPRECATED clbTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The name of the load balancer.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, must not begin or end with a hyphen, and must not begin with "internal-".
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbName :: Lens.Lens' CreateLoadBalancer Lude.Text
clbName = Lens.lens (name :: CreateLoadBalancer -> Lude.Text) (\s a -> s {name = a} :: CreateLoadBalancer)
{-# DEPRECATED clbName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest CreateLoadBalancer where
  type Rs CreateLoadBalancer = CreateLoadBalancerResponse
  request = Req.postQuery eLBv2Service
  response =
    Res.receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            Lude.<$> ( x Lude..@? "LoadBalancers" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "member")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateLoadBalancer where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath CreateLoadBalancer where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateLoadBalancer where
  toQuery CreateLoadBalancer' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("CreateLoadBalancer" :: Lude.ByteString),
        "Version" Lude.=: ("2015-12-01" :: Lude.ByteString),
        "SubnetMappings"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> subnetMappings),
        "SecurityGroups"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> securityGroups),
        "Subnets"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> subnets),
        "CustomerOwnedIpv4Pool" Lude.=: customerOwnedIPv4Pool,
        "IpAddressType" Lude.=: ipAddressType,
        "Scheme" Lude.=: scheme,
        "Type" Lude.=: type',
        "Tags"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> tags),
        "Name" Lude.=: name
      ]

-- | /See:/ 'mkCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { loadBalancers ::
      Lude.Maybe [LoadBalancer],
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

-- | Creates a value of 'CreateLoadBalancerResponse' with the minimum fields required to make a request.
--
-- * 'loadBalancers' - Information about the load balancer.
-- * 'responseStatus' - The response status code.
mkCreateLoadBalancerResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateLoadBalancerResponse
mkCreateLoadBalancerResponse pResponseStatus_ =
  CreateLoadBalancerResponse'
    { loadBalancers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the load balancer.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrsLoadBalancers :: Lens.Lens' CreateLoadBalancerResponse (Lude.Maybe [LoadBalancer])
clbrsLoadBalancers = Lens.lens (loadBalancers :: CreateLoadBalancerResponse -> Lude.Maybe [LoadBalancer]) (\s a -> s {loadBalancers = a} :: CreateLoadBalancerResponse)
{-# DEPRECATED clbrsLoadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrsResponseStatus :: Lens.Lens' CreateLoadBalancerResponse Lude.Int
clbrsResponseStatus = Lens.lens (responseStatus :: CreateLoadBalancerResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateLoadBalancerResponse)
{-# DEPRECATED clbrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
