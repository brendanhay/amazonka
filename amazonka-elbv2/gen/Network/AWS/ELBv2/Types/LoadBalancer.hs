{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.LoadBalancer where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types.AvailabilityZone
import Network.AWS.ELBv2.Types.IpAddressType
import Network.AWS.ELBv2.Types.LoadBalancerSchemeEnum
import Network.AWS.ELBv2.Types.LoadBalancerState
import Network.AWS.ELBv2.Types.LoadBalancerTypeEnum
import qualified Network.AWS.Lens as Lens

-- | Information about a load balancer.
--
-- /See:/ 'newLoadBalancer' smart constructor.
data LoadBalancer = LoadBalancer'
  { -- | The type of IP addresses used by the subnets for your load balancer. The
    -- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
    -- IPv4 and IPv6 addresses).
    ipAddressType :: Core.Maybe IpAddressType,
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Maybe Core.Text,
    -- | [Application Load Balancers on Outposts] The ID of the customer-owned
    -- address pool.
    customerOwnedIpv4Pool :: Core.Maybe Core.Text,
    -- | The subnets for the load balancer.
    availabilityZones :: Core.Maybe [AvailabilityZone],
    -- | The nodes of an Internet-facing load balancer have public IP addresses.
    -- The DNS name of an Internet-facing load balancer is publicly resolvable
    -- to the public IP addresses of the nodes. Therefore, Internet-facing load
    -- balancers can route requests from clients over the internet.
    --
    -- The nodes of an internal load balancer have only private IP addresses.
    -- The DNS name of an internal load balancer is publicly resolvable to the
    -- private IP addresses of the nodes. Therefore, internal load balancers
    -- can route requests only from clients with access to the VPC for the load
    -- balancer.
    scheme :: Core.Maybe LoadBalancerSchemeEnum,
    -- | The date and time the load balancer was created.
    createdTime :: Core.Maybe Core.ISO8601,
    -- | The IDs of the security groups for the load balancer.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The state of the load balancer.
    state :: Core.Maybe LoadBalancerState,
    -- | The public DNS name of the load balancer.
    dNSName :: Core.Maybe Core.Text,
    -- | The type of load balancer.
    type' :: Core.Maybe LoadBalancerTypeEnum,
    -- | The ID of the Amazon Route 53 hosted zone associated with the load
    -- balancer.
    canonicalHostedZoneId :: Core.Maybe Core.Text,
    -- | The ID of the VPC for the load balancer.
    vpcId :: Core.Maybe Core.Text,
    -- | The name of the load balancer.
    loadBalancerName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'loadBalancer_ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
--
-- 'loadBalancerArn', 'loadBalancer_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'customerOwnedIpv4Pool', 'loadBalancer_customerOwnedIpv4Pool' - [Application Load Balancers on Outposts] The ID of the customer-owned
-- address pool.
--
-- 'availabilityZones', 'loadBalancer_availabilityZones' - The subnets for the load balancer.
--
-- 'scheme', 'loadBalancer_scheme' - The nodes of an Internet-facing load balancer have public IP addresses.
-- The DNS name of an Internet-facing load balancer is publicly resolvable
-- to the public IP addresses of the nodes. Therefore, Internet-facing load
-- balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses.
-- The DNS name of an internal load balancer is publicly resolvable to the
-- private IP addresses of the nodes. Therefore, internal load balancers
-- can route requests only from clients with access to the VPC for the load
-- balancer.
--
-- 'createdTime', 'loadBalancer_createdTime' - The date and time the load balancer was created.
--
-- 'securityGroups', 'loadBalancer_securityGroups' - The IDs of the security groups for the load balancer.
--
-- 'state', 'loadBalancer_state' - The state of the load balancer.
--
-- 'dNSName', 'loadBalancer_dNSName' - The public DNS name of the load balancer.
--
-- 'type'', 'loadBalancer_type' - The type of load balancer.
--
-- 'canonicalHostedZoneId', 'loadBalancer_canonicalHostedZoneId' - The ID of the Amazon Route 53 hosted zone associated with the load
-- balancer.
--
-- 'vpcId', 'loadBalancer_vpcId' - The ID of the VPC for the load balancer.
--
-- 'loadBalancerName', 'loadBalancer_loadBalancerName' - The name of the load balancer.
newLoadBalancer ::
  LoadBalancer
newLoadBalancer =
  LoadBalancer'
    { ipAddressType = Core.Nothing,
      loadBalancerArn = Core.Nothing,
      customerOwnedIpv4Pool = Core.Nothing,
      availabilityZones = Core.Nothing,
      scheme = Core.Nothing,
      createdTime = Core.Nothing,
      securityGroups = Core.Nothing,
      state = Core.Nothing,
      dNSName = Core.Nothing,
      type' = Core.Nothing,
      canonicalHostedZoneId = Core.Nothing,
      vpcId = Core.Nothing,
      loadBalancerName = Core.Nothing
    }

-- | The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
loadBalancer_ipAddressType :: Lens.Lens' LoadBalancer (Core.Maybe IpAddressType)
loadBalancer_ipAddressType = Lens.lens (\LoadBalancer' {ipAddressType} -> ipAddressType) (\s@LoadBalancer' {} a -> s {ipAddressType = a} :: LoadBalancer)

-- | The Amazon Resource Name (ARN) of the load balancer.
loadBalancer_loadBalancerArn :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_loadBalancerArn = Lens.lens (\LoadBalancer' {loadBalancerArn} -> loadBalancerArn) (\s@LoadBalancer' {} a -> s {loadBalancerArn = a} :: LoadBalancer)

-- | [Application Load Balancers on Outposts] The ID of the customer-owned
-- address pool.
loadBalancer_customerOwnedIpv4Pool :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_customerOwnedIpv4Pool = Lens.lens (\LoadBalancer' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@LoadBalancer' {} a -> s {customerOwnedIpv4Pool = a} :: LoadBalancer)

-- | The subnets for the load balancer.
loadBalancer_availabilityZones :: Lens.Lens' LoadBalancer (Core.Maybe [AvailabilityZone])
loadBalancer_availabilityZones = Lens.lens (\LoadBalancer' {availabilityZones} -> availabilityZones) (\s@LoadBalancer' {} a -> s {availabilityZones = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The nodes of an Internet-facing load balancer have public IP addresses.
-- The DNS name of an Internet-facing load balancer is publicly resolvable
-- to the public IP addresses of the nodes. Therefore, Internet-facing load
-- balancers can route requests from clients over the internet.
--
-- The nodes of an internal load balancer have only private IP addresses.
-- The DNS name of an internal load balancer is publicly resolvable to the
-- private IP addresses of the nodes. Therefore, internal load balancers
-- can route requests only from clients with access to the VPC for the load
-- balancer.
loadBalancer_scheme :: Lens.Lens' LoadBalancer (Core.Maybe LoadBalancerSchemeEnum)
loadBalancer_scheme = Lens.lens (\LoadBalancer' {scheme} -> scheme) (\s@LoadBalancer' {} a -> s {scheme = a} :: LoadBalancer)

-- | The date and time the load balancer was created.
loadBalancer_createdTime :: Lens.Lens' LoadBalancer (Core.Maybe Core.UTCTime)
loadBalancer_createdTime = Lens.lens (\LoadBalancer' {createdTime} -> createdTime) (\s@LoadBalancer' {} a -> s {createdTime = a} :: LoadBalancer) Core.. Lens.mapping Core._Time

-- | The IDs of the security groups for the load balancer.
loadBalancer_securityGroups :: Lens.Lens' LoadBalancer (Core.Maybe [Core.Text])
loadBalancer_securityGroups = Lens.lens (\LoadBalancer' {securityGroups} -> securityGroups) (\s@LoadBalancer' {} a -> s {securityGroups = a} :: LoadBalancer) Core.. Lens.mapping Lens._Coerce

-- | The state of the load balancer.
loadBalancer_state :: Lens.Lens' LoadBalancer (Core.Maybe LoadBalancerState)
loadBalancer_state = Lens.lens (\LoadBalancer' {state} -> state) (\s@LoadBalancer' {} a -> s {state = a} :: LoadBalancer)

-- | The public DNS name of the load balancer.
loadBalancer_dNSName :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_dNSName = Lens.lens (\LoadBalancer' {dNSName} -> dNSName) (\s@LoadBalancer' {} a -> s {dNSName = a} :: LoadBalancer)

-- | The type of load balancer.
loadBalancer_type :: Lens.Lens' LoadBalancer (Core.Maybe LoadBalancerTypeEnum)
loadBalancer_type = Lens.lens (\LoadBalancer' {type'} -> type') (\s@LoadBalancer' {} a -> s {type' = a} :: LoadBalancer)

-- | The ID of the Amazon Route 53 hosted zone associated with the load
-- balancer.
loadBalancer_canonicalHostedZoneId :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_canonicalHostedZoneId = Lens.lens (\LoadBalancer' {canonicalHostedZoneId} -> canonicalHostedZoneId) (\s@LoadBalancer' {} a -> s {canonicalHostedZoneId = a} :: LoadBalancer)

-- | The ID of the VPC for the load balancer.
loadBalancer_vpcId :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_vpcId = Lens.lens (\LoadBalancer' {vpcId} -> vpcId) (\s@LoadBalancer' {} a -> s {vpcId = a} :: LoadBalancer)

-- | The name of the load balancer.
loadBalancer_loadBalancerName :: Lens.Lens' LoadBalancer (Core.Maybe Core.Text)
loadBalancer_loadBalancerName = Lens.lens (\LoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancer' {} a -> s {loadBalancerName = a} :: LoadBalancer)

instance Core.FromXML LoadBalancer where
  parseXML x =
    LoadBalancer'
      Core.<$> (x Core..@? "IpAddressType")
      Core.<*> (x Core..@? "LoadBalancerArn")
      Core.<*> (x Core..@? "CustomerOwnedIpv4Pool")
      Core.<*> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "Scheme")
      Core.<*> (x Core..@? "CreatedTime")
      Core.<*> ( x Core..@? "SecurityGroups" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "member")
               )
      Core.<*> (x Core..@? "State")
      Core.<*> (x Core..@? "DNSName")
      Core.<*> (x Core..@? "Type")
      Core.<*> (x Core..@? "CanonicalHostedZoneId")
      Core.<*> (x Core..@? "VpcId")
      Core.<*> (x Core..@? "LoadBalancerName")

instance Core.Hashable LoadBalancer

instance Core.NFData LoadBalancer
