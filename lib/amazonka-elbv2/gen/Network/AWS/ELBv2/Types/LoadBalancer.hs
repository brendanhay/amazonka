{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.LoadBalancer
  ( LoadBalancer (..)
  -- * Smart constructor
  , mkLoadBalancer
  -- * Lenses
  , lbAvailabilityZones
  , lbCanonicalHostedZoneId
  , lbCreatedTime
  , lbCustomerOwnedIpv4Pool
  , lbDNSName
  , lbIpAddressType
  , lbLoadBalancerArn
  , lbLoadBalancerName
  , lbScheme
  , lbSecurityGroups
  , lbState
  , lbType
  , lbVpcId
  ) where

import qualified Network.AWS.ELBv2.Types.AvailabilityZone as Types
import qualified Network.AWS.ELBv2.Types.CanonicalHostedZoneId as Types
import qualified Network.AWS.ELBv2.Types.CustomerOwnedIpv4Pool as Types
import qualified Network.AWS.ELBv2.Types.DNSName as Types
import qualified Network.AWS.ELBv2.Types.IpAddressType as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerArn as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerName as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerState as Types
import qualified Network.AWS.ELBv2.Types.LoadBalancerTypeEnum as Types
import qualified Network.AWS.ELBv2.Types.SecurityGroupId as Types
import qualified Network.AWS.ELBv2.Types.VpcId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a load balancer.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { availabilityZones :: Core.Maybe [Types.AvailabilityZone]
    -- ^ The subnets for the load balancer.
  , canonicalHostedZoneId :: Core.Maybe Types.CanonicalHostedZoneId
    -- ^ The ID of the Amazon Route 53 hosted zone associated with the load balancer.
  , createdTime :: Core.Maybe Core.UTCTime
    -- ^ The date and time the load balancer was created.
  , customerOwnedIpv4Pool :: Core.Maybe Types.CustomerOwnedIpv4Pool
    -- ^ [Application Load Balancers on Outposts] The ID of the customer-owned address pool.
  , dNSName :: Core.Maybe Types.DNSName
    -- ^ The public DNS name of the load balancer.
  , ipAddressType :: Core.Maybe Types.IpAddressType
    -- ^ The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
  , loadBalancerArn :: Core.Maybe Types.LoadBalancerArn
    -- ^ The Amazon Resource Name (ARN) of the load balancer.
  , loadBalancerName :: Core.Maybe Types.LoadBalancerName
    -- ^ The name of the load balancer.
  , scheme :: Core.Maybe Types.LoadBalancerSchemeEnum
    -- ^ The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
  , securityGroups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the security groups for the load balancer.
  , state :: Core.Maybe Types.LoadBalancerState
    -- ^ The state of the load balancer.
  , type' :: Core.Maybe Types.LoadBalancerTypeEnum
    -- ^ The type of load balancer.
  , vpcId :: Core.Maybe Types.VpcId
    -- ^ The ID of the VPC for the load balancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'LoadBalancer' value with any optional fields omitted.
mkLoadBalancer
    :: LoadBalancer
mkLoadBalancer
  = LoadBalancer'{availabilityZones = Core.Nothing,
                  canonicalHostedZoneId = Core.Nothing, createdTime = Core.Nothing,
                  customerOwnedIpv4Pool = Core.Nothing, dNSName = Core.Nothing,
                  ipAddressType = Core.Nothing, loadBalancerArn = Core.Nothing,
                  loadBalancerName = Core.Nothing, scheme = Core.Nothing,
                  securityGroups = Core.Nothing, state = Core.Nothing,
                  type' = Core.Nothing, vpcId = Core.Nothing}

-- | The subnets for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbAvailabilityZones :: Lens.Lens' LoadBalancer (Core.Maybe [Types.AvailabilityZone])
lbAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE lbAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

-- | The ID of the Amazon Route 53 hosted zone associated with the load balancer.
--
-- /Note:/ Consider using 'canonicalHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCanonicalHostedZoneId :: Lens.Lens' LoadBalancer (Core.Maybe Types.CanonicalHostedZoneId)
lbCanonicalHostedZoneId = Lens.field @"canonicalHostedZoneId"
{-# INLINEABLE lbCanonicalHostedZoneId #-}
{-# DEPRECATED canonicalHostedZoneId "Use generic-lens or generic-optics with 'canonicalHostedZoneId' instead"  #-}

-- | The date and time the load balancer was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCreatedTime :: Lens.Lens' LoadBalancer (Core.Maybe Core.UTCTime)
lbCreatedTime = Lens.field @"createdTime"
{-# INLINEABLE lbCreatedTime #-}
{-# DEPRECATED createdTime "Use generic-lens or generic-optics with 'createdTime' instead"  #-}

-- | [Application Load Balancers on Outposts] The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCustomerOwnedIpv4Pool :: Lens.Lens' LoadBalancer (Core.Maybe Types.CustomerOwnedIpv4Pool)
lbCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# INLINEABLE lbCustomerOwnedIpv4Pool #-}
{-# DEPRECATED customerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead"  #-}

-- | The public DNS name of the load balancer.
--
-- /Note:/ Consider using 'dNSName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbDNSName :: Lens.Lens' LoadBalancer (Core.Maybe Types.DNSName)
lbDNSName = Lens.field @"dNSName"
{-# INLINEABLE lbDNSName #-}
{-# DEPRECATED dNSName "Use generic-lens or generic-optics with 'dNSName' instead"  #-}

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbIpAddressType :: Lens.Lens' LoadBalancer (Core.Maybe Types.IpAddressType)
lbIpAddressType = Lens.field @"ipAddressType"
{-# INLINEABLE lbIpAddressType #-}
{-# DEPRECATED ipAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead"  #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLoadBalancerArn :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerArn)
lbLoadBalancerArn = Lens.field @"loadBalancerArn"
{-# INLINEABLE lbLoadBalancerArn #-}
{-# DEPRECATED loadBalancerArn "Use generic-lens or generic-optics with 'loadBalancerArn' instead"  #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLoadBalancerName :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerName)
lbLoadBalancerName = Lens.field @"loadBalancerName"
{-# INLINEABLE lbLoadBalancerName #-}
{-# DEPRECATED loadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead"  #-}

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbScheme :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerSchemeEnum)
lbScheme = Lens.field @"scheme"
{-# INLINEABLE lbScheme #-}
{-# DEPRECATED scheme "Use generic-lens or generic-optics with 'scheme' instead"  #-}

-- | The IDs of the security groups for the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbSecurityGroups :: Lens.Lens' LoadBalancer (Core.Maybe [Types.SecurityGroupId])
lbSecurityGroups = Lens.field @"securityGroups"
{-# INLINEABLE lbSecurityGroups #-}
{-# DEPRECATED securityGroups "Use generic-lens or generic-optics with 'securityGroups' instead"  #-}

-- | The state of the load balancer.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbState :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerState)
lbState = Lens.field @"state"
{-# INLINEABLE lbState #-}
{-# DEPRECATED state "Use generic-lens or generic-optics with 'state' instead"  #-}

-- | The type of load balancer.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbType :: Lens.Lens' LoadBalancer (Core.Maybe Types.LoadBalancerTypeEnum)
lbType = Lens.field @"type'"
{-# INLINEABLE lbType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | The ID of the VPC for the load balancer.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbVpcId :: Lens.Lens' LoadBalancer (Core.Maybe Types.VpcId)
lbVpcId = Lens.field @"vpcId"
{-# INLINEABLE lbVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML LoadBalancer where
        parseXML x
          = LoadBalancer' Core.<$>
              (x Core..@? "AvailabilityZones" Core..<@>
                 Core.parseXMLList "member")
                Core.<*> x Core..@? "CanonicalHostedZoneId"
                Core.<*> x Core..@? "CreatedTime"
                Core.<*> x Core..@? "CustomerOwnedIpv4Pool"
                Core.<*> x Core..@? "DNSName"
                Core.<*> x Core..@? "IpAddressType"
                Core.<*> x Core..@? "LoadBalancerArn"
                Core.<*> x Core..@? "LoadBalancerName"
                Core.<*> x Core..@? "Scheme"
                Core.<*>
                x Core..@? "SecurityGroups" Core..<@> Core.parseXMLList "member"
                Core.<*> x Core..@? "State"
                Core.<*> x Core..@? "Type"
                Core.<*> x Core..@? "VpcId"
