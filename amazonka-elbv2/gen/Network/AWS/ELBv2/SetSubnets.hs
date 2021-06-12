{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.SetSubnets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the Availability Zones for the specified public subnets for the
-- specified Application Load Balancer or Network Load Balancer. The
-- specified subnets replace the previously enabled subnets.
--
-- When you specify subnets for a Network Load Balancer, you must include
-- all subnets that were enabled previously, with their existing
-- configurations, plus any additional subnets.
module Network.AWS.ELBv2.SetSubnets
  ( -- * Creating a Request
    SetSubnets (..),
    newSetSubnets,

    -- * Request Lenses
    setSubnets_ipAddressType,
    setSubnets_subnetMappings,
    setSubnets_subnets,
    setSubnets_loadBalancerArn,

    -- * Destructuring the Response
    SetSubnetsResponse (..),
    newSetSubnetsResponse,

    -- * Response Lenses
    setSubnetsResponse_ipAddressType,
    setSubnetsResponse_availabilityZones,
    setSubnetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetSubnets' smart constructor.
data SetSubnets = SetSubnets'
  { -- | [Network Load Balancers] The type of IP addresses used by the subnets
    -- for your load balancer. The possible values are @ipv4@ (for IPv4
    -- addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t
    -- specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
    -- Internal load balancers must use @ipv4@.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | The IDs of the public subnets. You can specify only one subnet per
    -- Availability Zone. You must specify either subnets or subnet mappings.
    --
    -- [Application Load Balancers] You must specify subnets from at least two
    -- Availability Zones. You cannot specify Elastic IP addresses for your
    -- subnets.
    --
    -- [Application Load Balancers on Outposts] You must specify one Outpost
    -- subnet.
    --
    -- [Application Load Balancers on Local Zones] You can specify subnets from
    -- one or more Local Zones.
    --
    -- [Network Load Balancers] You can specify subnets from one or more
    -- Availability Zones. You can specify one Elastic IP address per subnet if
    -- you need static IP addresses for your internet-facing load balancer. For
    -- internal load balancers, you can specify one private IP address per
    -- subnet from the IPv4 range of the subnet. For internet-facing load
    -- balancer, you can specify one IPv6 address per subnet.
    subnetMappings :: Core.Maybe [SubnetMapping],
    -- | The IDs of the public subnets. You can specify only one subnet per
    -- Availability Zone. You must specify either subnets or subnet mappings.
    --
    -- [Application Load Balancers] You must specify subnets from at least two
    -- Availability Zones.
    --
    -- [Application Load Balancers on Outposts] You must specify one Outpost
    -- subnet.
    --
    -- [Application Load Balancers on Local Zones] You can specify subnets from
    -- one or more Local Zones.
    --
    -- [Network Load Balancers] You can specify subnets from one or more
    -- Availability Zones.
    subnets :: Core.Maybe [Core.Text],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'setSubnets_ipAddressType' - [Network Load Balancers] The type of IP addresses used by the subnets
-- for your load balancer. The possible values are @ipv4@ (for IPv4
-- addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t
-- specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
-- Internal load balancers must use @ipv4@.
--
-- 'subnetMappings', 'setSubnets_subnetMappings' - The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two
-- Availability Zones. You cannot specify Elastic IP addresses for your
-- subnets.
--
-- [Application Load Balancers on Outposts] You must specify one Outpost
-- subnet.
--
-- [Application Load Balancers on Local Zones] You can specify subnets from
-- one or more Local Zones.
--
-- [Network Load Balancers] You can specify subnets from one or more
-- Availability Zones. You can specify one Elastic IP address per subnet if
-- you need static IP addresses for your internet-facing load balancer. For
-- internal load balancers, you can specify one private IP address per
-- subnet from the IPv4 range of the subnet. For internet-facing load
-- balancer, you can specify one IPv6 address per subnet.
--
-- 'subnets', 'setSubnets_subnets' - The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two
-- Availability Zones.
--
-- [Application Load Balancers on Outposts] You must specify one Outpost
-- subnet.
--
-- [Application Load Balancers on Local Zones] You can specify subnets from
-- one or more Local Zones.
--
-- [Network Load Balancers] You can specify subnets from one or more
-- Availability Zones.
--
-- 'loadBalancerArn', 'setSubnets_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
newSetSubnets ::
  -- | 'loadBalancerArn'
  Core.Text ->
  SetSubnets
newSetSubnets pLoadBalancerArn_ =
  SetSubnets'
    { ipAddressType = Core.Nothing,
      subnetMappings = Core.Nothing,
      subnets = Core.Nothing,
      loadBalancerArn = pLoadBalancerArn_
    }

-- | [Network Load Balancers] The type of IP addresses used by the subnets
-- for your load balancer. The possible values are @ipv4@ (for IPv4
-- addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t
-- specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
-- Internal load balancers must use @ipv4@.
setSubnets_ipAddressType :: Lens.Lens' SetSubnets (Core.Maybe IpAddressType)
setSubnets_ipAddressType = Lens.lens (\SetSubnets' {ipAddressType} -> ipAddressType) (\s@SetSubnets' {} a -> s {ipAddressType = a} :: SetSubnets)

-- | The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two
-- Availability Zones. You cannot specify Elastic IP addresses for your
-- subnets.
--
-- [Application Load Balancers on Outposts] You must specify one Outpost
-- subnet.
--
-- [Application Load Balancers on Local Zones] You can specify subnets from
-- one or more Local Zones.
--
-- [Network Load Balancers] You can specify subnets from one or more
-- Availability Zones. You can specify one Elastic IP address per subnet if
-- you need static IP addresses for your internet-facing load balancer. For
-- internal load balancers, you can specify one private IP address per
-- subnet from the IPv4 range of the subnet. For internet-facing load
-- balancer, you can specify one IPv6 address per subnet.
setSubnets_subnetMappings :: Lens.Lens' SetSubnets (Core.Maybe [SubnetMapping])
setSubnets_subnetMappings = Lens.lens (\SetSubnets' {subnetMappings} -> subnetMappings) (\s@SetSubnets' {} a -> s {subnetMappings = a} :: SetSubnets) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings.
--
-- [Application Load Balancers] You must specify subnets from at least two
-- Availability Zones.
--
-- [Application Load Balancers on Outposts] You must specify one Outpost
-- subnet.
--
-- [Application Load Balancers on Local Zones] You can specify subnets from
-- one or more Local Zones.
--
-- [Network Load Balancers] You can specify subnets from one or more
-- Availability Zones.
setSubnets_subnets :: Lens.Lens' SetSubnets (Core.Maybe [Core.Text])
setSubnets_subnets = Lens.lens (\SetSubnets' {subnets} -> subnets) (\s@SetSubnets' {} a -> s {subnets = a} :: SetSubnets) Core.. Lens.mapping Lens._Coerce

-- | The Amazon Resource Name (ARN) of the load balancer.
setSubnets_loadBalancerArn :: Lens.Lens' SetSubnets Core.Text
setSubnets_loadBalancerArn = Lens.lens (\SetSubnets' {loadBalancerArn} -> loadBalancerArn) (\s@SetSubnets' {} a -> s {loadBalancerArn = a} :: SetSubnets)

instance Core.AWSRequest SetSubnets where
  type AWSResponse SetSubnets = SetSubnetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "SetSubnetsResult"
      ( \s h x ->
          SetSubnetsResponse'
            Core.<$> (x Core..@? "IpAddressType")
            Core.<*> ( x Core..@? "AvailabilityZones" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetSubnets

instance Core.NFData SetSubnets

instance Core.ToHeaders SetSubnets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath SetSubnets where
  toPath = Core.const "/"

instance Core.ToQuery SetSubnets where
  toQuery SetSubnets' {..} =
    Core.mconcat
      [ "Action" Core.=: ("SetSubnets" :: Core.ByteString),
        "Version" Core.=: ("2015-12-01" :: Core.ByteString),
        "IpAddressType" Core.=: ipAddressType,
        "SubnetMappings"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> subnetMappings),
        "Subnets"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> subnets),
        "LoadBalancerArn" Core.=: loadBalancerArn
      ]

-- | /See:/ 'newSetSubnetsResponse' smart constructor.
data SetSubnetsResponse = SetSubnetsResponse'
  { -- | [Network Load Balancers] The IP address type.
    ipAddressType :: Core.Maybe IpAddressType,
    -- | Information about the subnets.
    availabilityZones :: Core.Maybe [AvailabilityZone],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'setSubnetsResponse_ipAddressType' - [Network Load Balancers] The IP address type.
--
-- 'availabilityZones', 'setSubnetsResponse_availabilityZones' - Information about the subnets.
--
-- 'httpStatus', 'setSubnetsResponse_httpStatus' - The response's http status code.
newSetSubnetsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetSubnetsResponse
newSetSubnetsResponse pHttpStatus_ =
  SetSubnetsResponse'
    { ipAddressType = Core.Nothing,
      availabilityZones = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | [Network Load Balancers] The IP address type.
setSubnetsResponse_ipAddressType :: Lens.Lens' SetSubnetsResponse (Core.Maybe IpAddressType)
setSubnetsResponse_ipAddressType = Lens.lens (\SetSubnetsResponse' {ipAddressType} -> ipAddressType) (\s@SetSubnetsResponse' {} a -> s {ipAddressType = a} :: SetSubnetsResponse)

-- | Information about the subnets.
setSubnetsResponse_availabilityZones :: Lens.Lens' SetSubnetsResponse (Core.Maybe [AvailabilityZone])
setSubnetsResponse_availabilityZones = Lens.lens (\SetSubnetsResponse' {availabilityZones} -> availabilityZones) (\s@SetSubnetsResponse' {} a -> s {availabilityZones = a} :: SetSubnetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
setSubnetsResponse_httpStatus :: Lens.Lens' SetSubnetsResponse Core.Int
setSubnetsResponse_httpStatus = Lens.lens (\SetSubnetsResponse' {httpStatus} -> httpStatus) (\s@SetSubnetsResponse' {} a -> s {httpStatus = a} :: SetSubnetsResponse)

instance Core.NFData SetSubnetsResponse
