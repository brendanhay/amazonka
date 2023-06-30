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
-- Module      : Amazonka.ELBV2.CreateLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.ELBV2.CreateLoadBalancer
  ( -- * Creating a Request
    CreateLoadBalancer (..),
    newCreateLoadBalancer,

    -- * Request Lenses
    createLoadBalancer_customerOwnedIpv4Pool,
    createLoadBalancer_ipAddressType,
    createLoadBalancer_scheme,
    createLoadBalancer_securityGroups,
    createLoadBalancer_subnetMappings,
    createLoadBalancer_subnets,
    createLoadBalancer_tags,
    createLoadBalancer_type,
    createLoadBalancer_name,

    -- * Destructuring the Response
    CreateLoadBalancerResponse (..),
    newCreateLoadBalancerResponse,

    -- * Response Lenses
    createLoadBalancerResponse_loadBalancers,
    createLoadBalancerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLoadBalancer' smart constructor.
data CreateLoadBalancer = CreateLoadBalancer'
  { -- | [Application Load Balancers on Outposts] The ID of the customer-owned
    -- address pool (CoIP pool).
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The type of IP addresses used by the subnets for your load balancer. The
    -- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
    -- IPv4 and IPv6 addresses).
    ipAddressType :: Prelude.Maybe IpAddressType,
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
    -- | The IDs of the public subnets. You can specify only one subnet per
    -- Availability Zone. You must specify either subnets or subnet mappings,
    -- but not both.
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
    -- | The IDs of the public subnets. You can specify only one subnet per
    -- Availability Zone. You must specify either subnets or subnet mappings,
    -- but not both. To specify an Elastic IP address, specify subnet mappings
    -- instead of subnets.
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
    -- | The tags to assign to the load balancer.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The type of load balancer. The default is @application@.
    type' :: Prelude.Maybe LoadBalancerTypeEnum,
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
-- 'customerOwnedIpv4Pool', 'createLoadBalancer_customerOwnedIpv4Pool' - [Application Load Balancers on Outposts] The ID of the customer-owned
-- address pool (CoIP pool).
--
-- 'ipAddressType', 'createLoadBalancer_ipAddressType' - The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
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
-- 'subnetMappings', 'createLoadBalancer_subnetMappings' - The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings,
-- but not both.
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
-- 'subnets', 'createLoadBalancer_subnets' - The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings,
-- but not both. To specify an Elastic IP address, specify subnet mappings
-- instead of subnets.
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
-- 'tags', 'createLoadBalancer_tags' - The tags to assign to the load balancer.
--
-- 'type'', 'createLoadBalancer_type' - The type of load balancer. The default is @application@.
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
    { customerOwnedIpv4Pool =
        Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      scheme = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnetMappings = Prelude.Nothing,
      subnets = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing,
      name = pName_
    }

-- | [Application Load Balancers on Outposts] The ID of the customer-owned
-- address pool (CoIP pool).
createLoadBalancer_customerOwnedIpv4Pool :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe Prelude.Text)
createLoadBalancer_customerOwnedIpv4Pool = Lens.lens (\CreateLoadBalancer' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@CreateLoadBalancer' {} a -> s {customerOwnedIpv4Pool = a} :: CreateLoadBalancer)

-- | The type of IP addresses used by the subnets for your load balancer. The
-- possible values are @ipv4@ (for IPv4 addresses) and @dualstack@ (for
-- IPv4 and IPv6 addresses).
createLoadBalancer_ipAddressType :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe IpAddressType)
createLoadBalancer_ipAddressType = Lens.lens (\CreateLoadBalancer' {ipAddressType} -> ipAddressType) (\s@CreateLoadBalancer' {} a -> s {ipAddressType = a} :: CreateLoadBalancer)

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
createLoadBalancer_securityGroups = Lens.lens (\CreateLoadBalancer' {securityGroups} -> securityGroups) (\s@CreateLoadBalancer' {} a -> s {securityGroups = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings,
-- but not both.
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
createLoadBalancer_subnetMappings = Lens.lens (\CreateLoadBalancer' {subnetMappings} -> subnetMappings) (\s@CreateLoadBalancer' {} a -> s {subnetMappings = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the public subnets. You can specify only one subnet per
-- Availability Zone. You must specify either subnets or subnet mappings,
-- but not both. To specify an Elastic IP address, specify subnet mappings
-- instead of subnets.
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
createLoadBalancer_subnets = Lens.lens (\CreateLoadBalancer' {subnets} -> subnets) (\s@CreateLoadBalancer' {} a -> s {subnets = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The tags to assign to the load balancer.
createLoadBalancer_tags :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe (Prelude.NonEmpty Tag))
createLoadBalancer_tags = Lens.lens (\CreateLoadBalancer' {tags} -> tags) (\s@CreateLoadBalancer' {} a -> s {tags = a} :: CreateLoadBalancer) Prelude.. Lens.mapping Lens.coerced

-- | The type of load balancer. The default is @application@.
createLoadBalancer_type :: Lens.Lens' CreateLoadBalancer (Prelude.Maybe LoadBalancerTypeEnum)
createLoadBalancer_type = Lens.lens (\CreateLoadBalancer' {type'} -> type') (\s@CreateLoadBalancer' {} a -> s {type' = a} :: CreateLoadBalancer)

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
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateLoadBalancerResult"
      ( \s h x ->
          CreateLoadBalancerResponse'
            Prelude.<$> ( x
                            Data..@? "LoadBalancers"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLoadBalancer where
  hashWithSalt _salt CreateLoadBalancer' {..} =
    _salt
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` scheme
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` subnetMappings
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateLoadBalancer where
  rnf CreateLoadBalancer' {..} =
    Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf scheme
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnetMappings
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateLoadBalancer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateLoadBalancer where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLoadBalancer where
  toQuery CreateLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateLoadBalancer" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "CustomerOwnedIpv4Pool"
          Data.=: customerOwnedIpv4Pool,
        "IpAddressType" Data.=: ipAddressType,
        "Scheme" Data.=: scheme,
        "SecurityGroups"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> securityGroups
            ),
        "SubnetMappings"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> subnetMappings
            ),
        "Subnets"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> subnets),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "Type" Data.=: type',
        "Name" Data.=: name
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
createLoadBalancerResponse_loadBalancers = Lens.lens (\CreateLoadBalancerResponse' {loadBalancers} -> loadBalancers) (\s@CreateLoadBalancerResponse' {} a -> s {loadBalancers = a} :: CreateLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createLoadBalancerResponse_httpStatus :: Lens.Lens' CreateLoadBalancerResponse Prelude.Int
createLoadBalancerResponse_httpStatus = Lens.lens (\CreateLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@CreateLoadBalancerResponse' {} a -> s {httpStatus = a} :: CreateLoadBalancerResponse)

instance Prelude.NFData CreateLoadBalancerResponse where
  rnf CreateLoadBalancerResponse' {..} =
    Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf httpStatus
