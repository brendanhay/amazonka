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
-- Module      : Amazonka.ELBV2.SetSubnets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.ELBV2.SetSubnets
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
    setSubnetsResponse_availabilityZones,
    setSubnetsResponse_ipAddressType,
    setSubnetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetSubnets' smart constructor.
data SetSubnets = SetSubnets'
  { -- | [Network Load Balancers] The type of IP addresses used by the subnets
    -- for your load balancer. The possible values are @ipv4@ (for IPv4
    -- addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t
    -- specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
    -- .
    ipAddressType :: Prelude.Maybe IpAddressType,
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
    subnetMappings :: Prelude.Maybe [SubnetMapping],
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
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- .
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
  Prelude.Text ->
  SetSubnets
newSetSubnets pLoadBalancerArn_ =
  SetSubnets'
    { ipAddressType = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      subnets = Prelude.Nothing,
      loadBalancerArn = pLoadBalancerArn_
    }

-- | [Network Load Balancers] The type of IP addresses used by the subnets
-- for your load balancer. The possible values are @ipv4@ (for IPv4
-- addresses) and @dualstack@ (for IPv4 and IPv6 addresses). You can’t
-- specify @dualstack@ for a load balancer with a UDP or TCP_UDP listener.
-- .
setSubnets_ipAddressType :: Lens.Lens' SetSubnets (Prelude.Maybe IpAddressType)
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
setSubnets_subnetMappings :: Lens.Lens' SetSubnets (Prelude.Maybe [SubnetMapping])
setSubnets_subnetMappings = Lens.lens (\SetSubnets' {subnetMappings} -> subnetMappings) (\s@SetSubnets' {} a -> s {subnetMappings = a} :: SetSubnets) Prelude.. Lens.mapping Lens.coerced

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
setSubnets_subnets :: Lens.Lens' SetSubnets (Prelude.Maybe [Prelude.Text])
setSubnets_subnets = Lens.lens (\SetSubnets' {subnets} -> subnets) (\s@SetSubnets' {} a -> s {subnets = a} :: SetSubnets) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the load balancer.
setSubnets_loadBalancerArn :: Lens.Lens' SetSubnets Prelude.Text
setSubnets_loadBalancerArn = Lens.lens (\SetSubnets' {loadBalancerArn} -> loadBalancerArn) (\s@SetSubnets' {} a -> s {loadBalancerArn = a} :: SetSubnets)

instance Core.AWSRequest SetSubnets where
  type AWSResponse SetSubnets = SetSubnetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetSubnetsResult"
      ( \s h x ->
          SetSubnetsResponse'
            Prelude.<$> ( x
                            Data..@? "AvailabilityZones"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (x Data..@? "IpAddressType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetSubnets where
  hashWithSalt _salt SetSubnets' {..} =
    _salt
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` subnetMappings
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` loadBalancerArn

instance Prelude.NFData SetSubnets where
  rnf SetSubnets' {..} =
    Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf subnetMappings
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf loadBalancerArn

instance Data.ToHeaders SetSubnets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetSubnets where
  toPath = Prelude.const "/"

instance Data.ToQuery SetSubnets where
  toQuery SetSubnets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetSubnets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "IpAddressType" Data.=: ipAddressType,
        "SubnetMappings"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> subnetMappings
            ),
        "Subnets"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> subnets),
        "LoadBalancerArn" Data.=: loadBalancerArn
      ]

-- | /See:/ 'newSetSubnetsResponse' smart constructor.
data SetSubnetsResponse = SetSubnetsResponse'
  { -- | Information about the subnets.
    availabilityZones :: Prelude.Maybe [AvailabilityZone],
    -- | [Network Load Balancers] The IP address type.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availabilityZones', 'setSubnetsResponse_availabilityZones' - Information about the subnets.
--
-- 'ipAddressType', 'setSubnetsResponse_ipAddressType' - [Network Load Balancers] The IP address type.
--
-- 'httpStatus', 'setSubnetsResponse_httpStatus' - The response's http status code.
newSetSubnetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetSubnetsResponse
newSetSubnetsResponse pHttpStatus_ =
  SetSubnetsResponse'
    { availabilityZones =
        Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the subnets.
setSubnetsResponse_availabilityZones :: Lens.Lens' SetSubnetsResponse (Prelude.Maybe [AvailabilityZone])
setSubnetsResponse_availabilityZones = Lens.lens (\SetSubnetsResponse' {availabilityZones} -> availabilityZones) (\s@SetSubnetsResponse' {} a -> s {availabilityZones = a} :: SetSubnetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | [Network Load Balancers] The IP address type.
setSubnetsResponse_ipAddressType :: Lens.Lens' SetSubnetsResponse (Prelude.Maybe IpAddressType)
setSubnetsResponse_ipAddressType = Lens.lens (\SetSubnetsResponse' {ipAddressType} -> ipAddressType) (\s@SetSubnetsResponse' {} a -> s {ipAddressType = a} :: SetSubnetsResponse)

-- | The response's http status code.
setSubnetsResponse_httpStatus :: Lens.Lens' SetSubnetsResponse Prelude.Int
setSubnetsResponse_httpStatus = Lens.lens (\SetSubnetsResponse' {httpStatus} -> httpStatus) (\s@SetSubnetsResponse' {} a -> s {httpStatus = a} :: SetSubnetsResponse)

instance Prelude.NFData SetSubnetsResponse where
  rnf SetSubnetsResponse' {..} =
    Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf httpStatus
