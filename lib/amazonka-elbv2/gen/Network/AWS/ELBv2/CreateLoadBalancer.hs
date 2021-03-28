{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      CreateLoadBalancer (..)
    , mkCreateLoadBalancer
    -- ** Request lenses
    , clbName
    , clbCustomerOwnedIpv4Pool
    , clbIpAddressType
    , clbScheme
    , clbSecurityGroups
    , clbSubnetMappings
    , clbSubnets
    , clbTags
    , clbType

    -- * Destructuring the response
    , CreateLoadBalancerResponse (..)
    , mkCreateLoadBalancerResponse
    -- ** Response lenses
    , clbrrsLoadBalancers
    , clbrrsResponseStatus
    ) where

import qualified Network.AWS.ELBv2.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { name :: Types.LoadBalancerName
    -- ^ The name of the load balancer.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, must not begin or end with a hyphen, and must not begin with "internal-".
  , customerOwnedIpv4Pool :: Core.Maybe Types.CustomerOwnedIpv4Pool
    -- ^ [Application Load Balancers on Outposts] The ID of the customer-owned address pool (CoIP pool).
  , ipAddressType :: Core.Maybe Types.IpAddressType
    -- ^ The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
  , scheme :: Core.Maybe Types.LoadBalancerSchemeEnum
    -- ^ The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
-- The default is an Internet-facing load balancer.
-- You cannot specify a scheme for a Gateway Load Balancer.
  , securityGroups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ [Application Load Balancers] The IDs of the security groups for the load balancer.
  , subnetMappings :: Core.Maybe [Types.SubnetMapping]
    -- ^ The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones. You cannot specify Elastic IP addresses for your subnets.
  , subnets :: Core.Maybe [Types.SubnetId]
    -- ^ The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones.
  , tags :: Core.Maybe (Core.NonEmpty Types.Tag)
    -- ^ The tags to assign to the load balancer.
  , type' :: Core.Maybe Types.LoadBalancerTypeEnum
    -- ^ The type of load balancer. The default is @application@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateLoadBalancer' value with any optional fields omitted.
mkCreateLoadBalancer
    :: Types.LoadBalancerName -- ^ 'name'
    -> CreateLoadBalancer
mkCreateLoadBalancer name
  = CreateLoadBalancer'{name, customerOwnedIpv4Pool = Core.Nothing,
                        ipAddressType = Core.Nothing, scheme = Core.Nothing,
                        securityGroups = Core.Nothing, subnetMappings = Core.Nothing,
                        subnets = Core.Nothing, tags = Core.Nothing, type' = Core.Nothing}

-- | The name of the load balancer.
--
-- This name must be unique per region per account, can have a maximum of 32 characters, must contain only alphanumeric characters or hyphens, must not begin or end with a hyphen, and must not begin with "internal-".
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbName :: Lens.Lens' CreateLoadBalancer Types.LoadBalancerName
clbName = Lens.field @"name"
{-# INLINEABLE clbName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | [Application Load Balancers on Outposts] The ID of the customer-owned address pool (CoIP pool).
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbCustomerOwnedIpv4Pool :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.CustomerOwnedIpv4Pool)
clbCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# INLINEABLE clbCustomerOwnedIpv4Pool #-}
{-# DEPRECATED customerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead"  #-}

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@ .
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbIpAddressType :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.IpAddressType)
clbIpAddressType = Lens.field @"ipAddressType"
{-# INLINEABLE clbIpAddressType #-}
{-# DEPRECATED ipAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead"  #-}

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
-- The default is an Internet-facing load balancer.
-- You cannot specify a scheme for a Gateway Load Balancer.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbScheme :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.LoadBalancerSchemeEnum)
clbScheme = Lens.field @"scheme"
{-# INLINEABLE clbScheme #-}
{-# DEPRECATED scheme "Use generic-lens or generic-optics with 'scheme' instead"  #-}

-- | [Application Load Balancers] The IDs of the security groups for the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSecurityGroups :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.SecurityGroupId])
clbSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE clbSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones. You cannot specify Elastic IP addresses for your subnets.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones. You can specify one Elastic IP address per subnet if you need static IP addresses for your internet-facing load balancer. For internal load balancers, you can specify one private IP address per subnet from the IPv4 range of the subnet. For internet-facing load balancer, you can specify one IPv6 address per subnet.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones. You cannot specify Elastic IP addresses for your subnets.
--
-- /Note:/ Consider using 'subnetMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSubnetMappings :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.SubnetMapping])
clbSubnetMappings = Lens.field @"subnetMappings"
{-# INLINEABLE clbSubnetMappings #-}
{-# DEPRECATED subnetMappings "Use generic-lens or generic-optics with 'subnetMappings' instead"  #-}

-- | The IDs of the public subnets. You can specify only one subnet per Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two Availability Zones.
-- [Application Load Balancers on Outposts] You must specify one Outpost subnet.
-- [Application Load Balancers on Local Zones] You can specify subnets from one or more Local Zones.
-- [Network Load Balancers] You can specify subnets from one or more Availability Zones.
-- [Gateway Load Balancers] You can specify subnets from one or more Availability Zones.
--
-- /Note:/ Consider using 'subnets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbSubnets :: Lens.Lens' CreateLoadBalancer (Core.Maybe [Types.SubnetId])
clbSubnets = Lens.field @"subnets"
{-# INLINEABLE clbSubnets #-}
{-# DEPRECATED subnets "Use generic-lens or generic-optics with 'subnets' instead"  #-}

-- | The tags to assign to the load balancer.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbTags :: Lens.Lens' CreateLoadBalancer (Core.Maybe (Core.NonEmpty Types.Tag))
clbTags = Lens.field @"tags"
{-# INLINEABLE clbTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | The type of load balancer. The default is @application@ .
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbType :: Lens.Lens' CreateLoadBalancer (Core.Maybe Types.LoadBalancerTypeEnum)
clbType = Lens.field @"type'"
{-# INLINEABLE clbType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.ToQuery CreateLoadBalancer where
        toQuery CreateLoadBalancer{..}
          = Core.toQueryPair "Action" ("CreateLoadBalancer" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2015-12-01" :: Core.Text)
              Core.<> Core.toQueryPair "Name" name
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "CustomerOwnedIpv4Pool")
                customerOwnedIpv4Pool
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "IpAddressType")
                ipAddressType
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Scheme") scheme
              Core.<>
              Core.toQueryPair "SecurityGroups"
                (Core.maybe Core.mempty (Core.toQueryList "member") securityGroups)
              Core.<>
              Core.toQueryPair "SubnetMappings"
                (Core.maybe Core.mempty (Core.toQueryList "member") subnetMappings)
              Core.<>
              Core.toQueryPair "Subnets"
                (Core.maybe Core.mempty (Core.toQueryList "member") subnets)
              Core.<>
              Core.toQueryPair "Tags"
                (Core.maybe Core.mempty (Core.toQueryList "member") tags)
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Type") type'

instance Core.ToHeaders CreateLoadBalancer where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest CreateLoadBalancer where
        type Rs CreateLoadBalancer = CreateLoadBalancerResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "CreateLoadBalancerResult"
              (\ s h x ->
                 CreateLoadBalancerResponse' Core.<$>
                   (x Core..@? "LoadBalancers" Core..<@> Core.parseXMLList "member")
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { loadBalancers :: Core.Maybe [Types.LoadBalancer]
    -- ^ Information about the load balancer.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateLoadBalancerResponse' value with any optional fields omitted.
mkCreateLoadBalancerResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateLoadBalancerResponse
mkCreateLoadBalancerResponse responseStatus
  = CreateLoadBalancerResponse'{loadBalancers = Core.Nothing,
                                responseStatus}

-- | Information about the load balancer.
--
-- /Note:/ Consider using 'loadBalancers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrrsLoadBalancers :: Lens.Lens' CreateLoadBalancerResponse (Core.Maybe [Types.LoadBalancer])
clbrrsLoadBalancers = Lens.field @"loadBalancers"
{-# INLINEABLE clbrrsLoadBalancers #-}
{-# DEPRECATED loadBalancers "Use generic-lens or generic-optics with 'loadBalancers' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
clbrrsResponseStatus :: Lens.Lens' CreateLoadBalancerResponse Core.Int
clbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE clbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
