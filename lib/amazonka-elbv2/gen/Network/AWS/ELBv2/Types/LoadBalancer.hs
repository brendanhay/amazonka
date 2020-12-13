{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancer
  ( LoadBalancer (..),

    -- * Smart constructor
    mkLoadBalancer,

    -- * Lenses
    lbState,
    lbSecurityGroups,
    lbLoadBalancerName,
    lbCreatedTime,
    lbVPCId,
    lbCanonicalHostedZoneId,
    lbAvailabilityZones,
    lbCustomerOwnedIPv4Pool,
    lbLoadBalancerARN,
    lbIPAddressType,
    lbScheme,
    lbType,
    lbDNSName,
  )
where

import Network.AWS.ELBv2.Types.AvailabilityZone
import Network.AWS.ELBv2.Types.IPAddressType
import Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
import Network.AWS.ELBv2.Types.LoadBalancerState
import Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a load balancer.
--
-- /See:/ 'mkLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The state of the load balancer.
    state :: Lude.Maybe LoadBalancerState,
    -- | The IDs of the security groups for the load balancer.
    securityGroups :: Lude.Maybe [Lude.Text],
    -- | The name of the load balancer.
    loadBalancerName :: Lude.Maybe Lude.Text,
    -- | The date and time the load balancer was created.
    createdTime :: Lude.Maybe Lude.DateTime,
    -- | The ID of the VPC for the load balancer.
    vpcId :: Lude.Maybe Lude.Text,
    -- | The ID of the Amazon Route 53 hosted zone associated with the load balancer.
    canonicalHostedZoneId :: Lude.Maybe Lude.Text,
    -- | The subnets for the load balancer.
    availabilityZones :: Lude.Maybe [AvailabilityZone],
    -- | [Application Load Balancers on Outposts] The ID of the customer-owned address pool.
    customerOwnedIPv4Pool :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerARN :: Lude.Maybe Lude.Text,
    -- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
    ipAddressType :: Lude.Maybe IPAddressType,
    -- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
    --
    -- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
    scheme :: Lude.Maybe LoadBalancerSchemeEnum,
    -- | The type of load balancer.
    type' :: Lude.Maybe LoadBalancerTypeEnum,
    -- | The public DNS name of the load balancer.
    dnsName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancer' with the minimum fields required to make a request.
--
-- * 'state' - The state of the load balancer.
-- * 'securityGroups' - The IDs of the security groups for the load balancer.
-- * 'loadBalancerName' - The name of the load balancer.
-- * 'createdTime' - The date and time the load balancer was created.
-- * 'vpcId' - The ID of the VPC for the load balancer.
-- * 'canonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load balancer.
-- * 'availabilityZones' - The subnets for the load balancer.
-- * 'customerOwnedIPv4Pool' - [Application Load Balancers on Outposts] The ID of the customer-owned address pool.
-- * 'loadBalancerARN' - The Amazon Resource Name (ARN) of the load balancer.
-- * 'ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
-- * 'scheme' - The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
-- * 'type'' - The type of load balancer.
-- * 'dnsName' - The public DNS name of the load balancer.
mkLoadBalancer ::
  LoadBalancer
mkLoadBalancer =
  LoadBalancer'
    { state = Lude.Nothing,
      securityGroups = Lude.Nothing,
      loadBalancerName = Lude.Nothing,
      createdTime = Lude.Nothing,
      vpcId = Lude.Nothing,
      canonicalHostedZoneId = Lude.Nothing,
      availabilityZones = Lude.Nothing,
      customerOwnedIPv4Pool = Lude.Nothing,
      loadBalancerARN = Lude.Nothing,
      ipAddressType = Lude.Nothing,
      scheme = Lude.Nothing,
      type' = Lude.Nothing,
      dnsName = Lude.Nothing
    }

-- | The state of the load balancer.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbState :: Lens.Lens' LoadBalancer (Lude.Maybe LoadBalancerState)
lbState = Lens.lens (state :: LoadBalancer -> Lude.Maybe LoadBalancerState) (\s a -> s {state = a} :: LoadBalancer)
{-# DEPRECATED lbState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The IDs of the security groups for the load balancer.
--
-- /Note:/ Consider using 'securityGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbSecurityGroups :: Lens.Lens' LoadBalancer (Lude.Maybe [Lude.Text])
lbSecurityGroups = Lens.lens (securityGroups :: LoadBalancer -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroups = a} :: LoadBalancer)
{-# DEPRECATED lbSecurityGroups "Use generic-lens or generic-optics with 'securityGroups' instead." #-}

-- | The name of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLoadBalancerName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbLoadBalancerName = Lens.lens (loadBalancerName :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerName = a} :: LoadBalancer)
{-# DEPRECATED lbLoadBalancerName "Use generic-lens or generic-optics with 'loadBalancerName' instead." #-}

-- | The date and time the load balancer was created.
--
-- /Note:/ Consider using 'createdTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCreatedTime :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.DateTime)
lbCreatedTime = Lens.lens (createdTime :: LoadBalancer -> Lude.Maybe Lude.DateTime) (\s a -> s {createdTime = a} :: LoadBalancer)
{-# DEPRECATED lbCreatedTime "Use generic-lens or generic-optics with 'createdTime' instead." #-}

-- | The ID of the VPC for the load balancer.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbVPCId :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbVPCId = Lens.lens (vpcId :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {vpcId = a} :: LoadBalancer)
{-# DEPRECATED lbVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

-- | The ID of the Amazon Route 53 hosted zone associated with the load balancer.
--
-- /Note:/ Consider using 'canonicalHostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCanonicalHostedZoneId :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbCanonicalHostedZoneId = Lens.lens (canonicalHostedZoneId :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {canonicalHostedZoneId = a} :: LoadBalancer)
{-# DEPRECATED lbCanonicalHostedZoneId "Use generic-lens or generic-optics with 'canonicalHostedZoneId' instead." #-}

-- | The subnets for the load balancer.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbAvailabilityZones :: Lens.Lens' LoadBalancer (Lude.Maybe [AvailabilityZone])
lbAvailabilityZones = Lens.lens (availabilityZones :: LoadBalancer -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: LoadBalancer)
{-# DEPRECATED lbAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | [Application Load Balancers on Outposts] The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIPv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbCustomerOwnedIPv4Pool :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbCustomerOwnedIPv4Pool = Lens.lens (customerOwnedIPv4Pool :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {customerOwnedIPv4Pool = a} :: LoadBalancer)
{-# DEPRECATED lbCustomerOwnedIPv4Pool "Use generic-lens or generic-optics with 'customerOwnedIPv4Pool' instead." #-}

-- | The Amazon Resource Name (ARN) of the load balancer.
--
-- /Note:/ Consider using 'loadBalancerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbLoadBalancerARN :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbLoadBalancerARN = Lens.lens (loadBalancerARN :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {loadBalancerARN = a} :: LoadBalancer)
{-# DEPRECATED lbLoadBalancerARN "Use generic-lens or generic-optics with 'loadBalancerARN' instead." #-}

-- | The type of IP addresses used by the subnets for your load balancer. The possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for IPv4 and IPv6 addresses).
--
-- /Note:/ Consider using 'ipAddressType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbIPAddressType :: Lens.Lens' LoadBalancer (Lude.Maybe IPAddressType)
lbIPAddressType = Lens.lens (ipAddressType :: LoadBalancer -> Lude.Maybe IPAddressType) (\s a -> s {ipAddressType = a} :: LoadBalancer)
{-# DEPRECATED lbIPAddressType "Use generic-lens or generic-optics with 'ipAddressType' instead." #-}

-- | The nodes of an Internet-facing load balancer have public IP addresses. The DNS name of an Internet-facing load balancer is publicly resolvable to the public IP addresses of the nodes. Therefore, Internet-facing load balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses. The DNS name of an internal load balancer is publicly resolvable to the private IP addresses of the nodes. Therefore, internal load balancers can route requests only from clients with access to the VPC for the load balancer.
--
-- /Note:/ Consider using 'scheme' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbScheme :: Lens.Lens' LoadBalancer (Lude.Maybe LoadBalancerSchemeEnum)
lbScheme = Lens.lens (scheme :: LoadBalancer -> Lude.Maybe LoadBalancerSchemeEnum) (\s a -> s {scheme = a} :: LoadBalancer)
{-# DEPRECATED lbScheme "Use generic-lens or generic-optics with 'scheme' instead." #-}

-- | The type of load balancer.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbType :: Lens.Lens' LoadBalancer (Lude.Maybe LoadBalancerTypeEnum)
lbType = Lens.lens (type' :: LoadBalancer -> Lude.Maybe LoadBalancerTypeEnum) (\s a -> s {type' = a} :: LoadBalancer)
{-# DEPRECATED lbType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The public DNS name of the load balancer.
--
-- /Note:/ Consider using 'dnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbDNSName :: Lens.Lens' LoadBalancer (Lude.Maybe Lude.Text)
lbDNSName = Lens.lens (dnsName :: LoadBalancer -> Lude.Maybe Lude.Text) (\s a -> s {dnsName = a} :: LoadBalancer)
{-# DEPRECATED lbDNSName "Use generic-lens or generic-optics with 'dnsName' instead." #-}

instance Lude.FromXML LoadBalancer where
  parseXML x =
    LoadBalancer'
      Lude.<$> (x Lude..@? "State")
      Lude.<*> ( x Lude..@? "SecurityGroups" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "LoadBalancerName")
      Lude.<*> (x Lude..@? "CreatedTime")
      Lude.<*> (x Lude..@? "VpcId")
      Lude.<*> (x Lude..@? "CanonicalHostedZoneId")
      Lude.<*> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "CustomerOwnedIpv4Pool")
      Lude.<*> (x Lude..@? "LoadBalancerArn")
      Lude.<*> (x Lude..@? "IpAddressType")
      Lude.<*> (x Lude..@? "Scheme")
      Lude.<*> (x Lude..@? "Type")
      Lude.<*> (x Lude..@? "DNSName")
