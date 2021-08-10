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
-- Module      : Network.AWS.ELBv2.CreateLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Application Load Balancer, Network Load Balancer, or Gateway
-- Load Balancer.
--
-- For more information, see the following:
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/application/application-load-balancers.html Application Load Balancers>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/network-load-balancers.html Network Load Balancers>
--
-- -   <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/gateway-load-balancers.html Gateway Load Balancers>
--
-- This operation is idempotent, which means that it completes at most one
-- time. If you attempt to create multiple load balancers with the same
-- settings, each call succeeds.
module Network.AWS.ELBv2.CreateLoadBalancer
  ( -- * Creating a Request
    CreateLoadBalancer (..),
    newCreateLoadBalancer,

    -- * Request Lenses
    createLoadBalancer_ipAddressType,
    createLoadBalancer_customerOwnedIpv4Pool,
    createLoadBalancer_subnetMappings,
    createLoadBalancer_scheme,
    createLoadBalancer_securityGroups,
    createLoadBalancer_tags,
    createLoadBalancer_type,
    createLoadBalancer_subnets,
    createLoadBalancer_name,

    -- * Destructuring the Response
    CreateLoadBalancerResponse (..),
    newCreateLoadBalancerResponse,

    -- * Response Lenses
    createLoadBalancerResponse_loadBalancers,
    createLoadBalancerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | The type of IP addresses used by the subnets for your load balancer. The
    -- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
    -- IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | [Application Load Balancers on Outposts] The ID of the customer-owned
    -- address pool (CoIP pool).
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
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
    --
    -- [Gateway Load Balancers] You can specify subnets from one or more
    -- Availability Zones. You cannot specify Elastic IP addresses for your
    -- subnets.
    subnetMappings :: Prelude.Maybe [SubnetMapping],
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
    --
    -- The default is an Internet-facing load balancer.
    --
    -- You cannot specify a scheme for a Gateway Load Balancer.
    scheme :: Prelude.Maybe LoadBalancerSchemeEnum,
    -- | [Application Load Balancers] The IDs of the security groups for the load
    -- balancer.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The tags to assign to the load balancer.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The type of load balancer. The default is @application@.
    type' :: Prelude.Maybe LoadBalancerTypeEnum,
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
    --
    -- [Gateway Load Balancers] You can specify subnets from one or more
    -- Availability Zones.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The name of the load balancer.
    --
    -- This name must be unique per region per account, can have a maximum of
    -- 32 characters, must contain only alphanumeric characters or hyphens,
    -- must not begin or end with a hyphen, and must not begin with
    -- \"internal-\".
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressType', 'createLoadBalancer_ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@.
--
-- 'customerOwnedIpv4Pool', 'createLoadBalancer_customerOwnedIpv4Pool' - [Application Load Balancers on Outposts] The ID of the customer-owned
-- address pool (CoIP pool).
--
-- 'subnetMappings', 'createLoadBalancer_subnetMappings' - The IDs of the public subnets. You can specify only one subnet per
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
-- [Gateway Load Balancers] You can specify subnets from one or more
-- Availability Zones. You cannot specify Elastic IP addresses for your
-- subnets.
--
-- 'scheme', 'createLoadBalancer_scheme' - The nodes of an Internet-facing load balancer have public IP addresses.
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
-- The default is an Internet-facing load balancer.
--
-- You cannot specify a scheme for a Gateway Load Balancer.
--
-- 'securityGroups', 'createLoadBalancer_securityGroups' - [Application Load Balancers] The IDs of the security groups for the load
-- balancer.
--
-- 'tags', 'createLoadBalancer_tags' - The tags to assign to the load balancer.
--
-- 'type'', 'createLoadBalancer_type' - The type of load balancer. The default is @application@.
--
-- 'subnets', 'createLoadBalancer_subnets' - The IDs of the public subnets. You can specify only one subnet per
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
-- [Gateway Load Balancers] You can specify subnets from one or more
-- Availability Zones.
--
-- 'name', 'createLoadBalancer_name' - The name of the load balancer.
--
-- This name must be unique per region per account, can have a maximum of
-- 32 characters, must contain only alphanumeric characters or hyphens,
-- must not begin or end with a hyphen, and must not begin with
-- \"internal-\".
newCreateLoadBalancer ::
  -- | 'name'
  Prelude.Text ->
  CreateLoadBalancer
newCreateLoadBalancer pName_ =
  CreateLoadBalancer'
    { ipAddressType =
        Prelude.Nothing,
      customerOwnedIpv4Pool = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      scheme = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      subnets = Prelude.Nothing,
      name = pName_
    }

-- | The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses). Internal load balancers must use @ipv4@.
createLoadBalancer_ipAddressType :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe IpAddressType)
createLoadBalancer_ipAddressType = Lens.lens (\CreateLoadBalancer' {ipAddressType} -> ipAddressType) (\s@CreateLoadBalancer' {} a -> s {ipAddressType = a} :: CreateLoadBalancer)

-- | [Application Load Balancers on Outposts] The ID of the customer-owned
-- address pool (CoIP pool).
createLoadBalancer_customerOwnedIpv4Pool :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_customerOwnedIpv4Pool = Lens.lens (\CreateLoadBalancer' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@CreateLoadBalancer' {} a -> s {customerOwnedIpv4Pool = a} :: CreateLoadBalancer)

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
--
-- [Gateway Load Balancers] You can specify subnets from one or more
-- Availability Zones. You cannot specify Elastic IP addresses for your
-- subnets.
createLoadBalancer_subnetMappings :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [SubnetMapping])
createLoadBalancer_subnetMappings = Lens.lens (\CreateLoadBalancer' {subnetMappings} -> subnetMappings) (\s@CreateLoadBalancer' {} a -> s {subnetMappings = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens._Coerce

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
--
-- The default is an Internet-facing load balancer.
--
-- You cannot specify a scheme for a Gateway Load Balancer.
createLoadBalancer_scheme :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe LoadBalancerSchemeEnum)
createLoadBalancer_scheme = Lens.lens (\CreateLoadBalancer' {scheme} -> scheme) (\s@CreateLoadBalancer' {} a -> s {scheme = a} :: CreateLoadBalancer)

-- | [Application Load Balancers] The IDs of the security groups for the load
-- balancer.
createLoadBalancer_securityGroups :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [Prelude.Text])
createLoadBalancer_securityGroups = Lens.lens (\CreateLoadBalancer' {securityGroups} -> securityGroups) (\s@CreateLoadBalancer' {} a -> s {securityGroups = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens._Coerce

-- | The tags to assign to the load balancer.
createLoadBalancer_tags :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe (Prelude.NonEmpty Tag))
createLoadBalancer_tags = Lens.lens (\CreateLoadBalancer' {tags} -> tags) (\s@CreateLoadBalancer' {} a -> s {tags = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens._Coerce

-- | The type of load balancer. The default is @application@.
createLoadBalancer_type :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe LoadBalancerTypeEnum)
createLoadBalancer_type = Lens.lens (\CreateLoadBalancer' {type'} -> type') (\s@CreateLoadBalancer' {} a -> s {type' = a} :: CreateLoadBalancer)

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
--
-- [Gateway Load Balancers] You can specify subnets from one or more
-- Availability Zones.
createLoadBalancer_subnets :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe [Prelude.Text])
createLoadBalancer_subnets = Lens.lens (\CreateLoadBalancer' {subnets} -> subnets) (\s@CreateLoadBalancer' {} a -> s {subnets = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the load balancer.
--
-- This name must be unique per region per account, can have a maximum of
-- 32 characters, must contain only alphanumeric characters or hyphens,
-- must not begin or end with a hyphen, and must not begin with
-- \"internal-\".
createLoadBalancer_name :: Lens.Lens' CreateLoadBalancer Prelude.Text
createLoadBalancer_name = Lens.lens (\CreateLoadBalancer' {name} -> name) (\s@CreateLoadBalancer' {} a -> s {name = a} :: CreateLoadBalancer)

instance Core.AWSRequest CreateLoadBalancer where
  type
    AWSResponse CreateLoadBalancer =
      CreateLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            Prelude.<$> ( x Core..@? "LoadBalancers" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoadBalancer

instance Prelude.NFData CreateLoadBalancer

instance Core.ToHeaders CreateLoadBalancer where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateLoadBalancer where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLoadBalancer where
  toQuery CreateLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateLoadBalancer" :: Prelude.ByteString),
        "Version"
          Core.=: ("2015-12-01" :: Prelude.ByteString),
        "IpAddressType" Core.=: ipAddressType,
        "CustomerOwnedIpv4Pool"
          Core.=: customerOwnedIpv4Pool,
        "SubnetMappings"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> subnetMappings
            ),
        "Scheme" Core.=: scheme,
        "SecurityGroups"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> securityGroups
            ),
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "Type" Core.=: type',
        "Subnets"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> subnets),
        "Name" Core.=: name
      ]

-- | /See:/ 'newCreateLoadBalancerResponse' smart constructor.
data CreateLoadBalancerResponse = CreateLoadBalancerResponse'
  { -- | Information about the load balancer.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancers', 'createLoadBalancerResponse_loadBalancers' - Information about the load balancer.
--
-- 'httpStatus', 'createLoadBalancerResponse_httpStatus' - The response's http status code.
newCreateLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLoadBalancerResponse
newCreateLoadBalancerResponse pHttpStatus_ =
  CreateLoadBalancerResponse'
    { loadBalancers =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the load balancer.
createLoadBalancerResponse_loadBalancers :: Lens.Lens' CreateLoadBalancerResponse (Prelude.Maybe [LoadBalancer])
createLoadBalancerResponse_loadBalancers = Lens.lens (\CreateLoadBalancerResponse' {loadBalancers} -> loadBalancers) (\s@CreateLoadBalancerResponse' {} a -> s {loadBalancers = a} :: CreateLoadBalancerResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
createLoadBalancerResponse_httpStatus :: Lens.Lens' CreateLoadBalancerResponse Prelude.Int
createLoadBalancerResponse_httpStatus = Lens.lens (\CreateLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerResponse)

instance Prelude.NFData CreateLoadBalancerResponse
