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
-- Module      : Amazonka.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your subnets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeSubnets
  ( -- * Creating a Request
    DescribeSubnets (..),
    newDescribeSubnets,

    -- * Request Lenses
    describeSubnets_dryRun,
    describeSubnets_filters,
    describeSubnets_maxResults,
    describeSubnets_nextToken,
    describeSubnets_subnetIds,

    -- * Destructuring the Response
    DescribeSubnetsResponse (..),
    newDescribeSubnetsResponse,

    -- * Response Lenses
    describeSubnetsResponse_nextToken,
    describeSubnetsResponse_subnets,
    describeSubnetsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSubnets' smart constructor.
data DescribeSubnets = DescribeSubnets'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @availability-zone@ - The Availability Zone for the subnet. You can
    --     also use @availabilityZone@ as the filter name.
    --
    -- -   @availability-zone-id@ - The ID of the Availability Zone for the
    --     subnet. You can also use @availabilityZoneId@ as the filter name.
    --
    -- -   @available-ip-address-count@ - The number of IPv4 addresses in the
    --     subnet that are available.
    --
    -- -   @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you
    --     specify must exactly match the subnet\'s CIDR block for information
    --     to be returned for the subnet. You can also use @cidr@ or
    --     @cidrBlock@ as the filter names.
    --
    -- -   @customer-owned-ipv4-pool@ - The customer-owned IPv4 address pool
    --     associated with the subnet.
    --
    -- -   @default-for-az@ - Indicates whether this is the default subnet for
    --     the Availability Zone (@true@ | @false@). You can also use
    --     @defaultForAz@ as the filter name.
    --
    -- -   @enable-dns64@ - Indicates whether DNS queries made to the
    --     Amazon-provided DNS Resolver in this subnet should return synthetic
    --     IPv6 addresses for IPv4-only destinations.
    --
    -- -   @enable-lni-at-device-index@ - Indicates the device position for
    --     local network interfaces in this subnet. For example, @1@ indicates
    --     local network interfaces in this subnet are the secondary network
    --     interface (eth1).
    --
    -- -   @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block
    --     associated with the subnet.
    --
    -- -   @ipv6-cidr-block-association.association-id@ - An association ID for
    --     an IPv6 CIDR block associated with the subnet.
    --
    -- -   @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR
    --     block associated with the subnet.
    --
    -- -   @ipv6-native@ - Indicates whether this is an IPv6 only subnet
    --     (@true@ | @false@).
    --
    -- -   @map-customer-owned-ip-on-launch@ - Indicates whether a network
    --     interface created in this subnet (including a network interface
    --     created by RunInstances) receives a customer-owned IPv4 address.
    --
    -- -   @map-public-ip-on-launch@ - Indicates whether instances launched in
    --     this subnet receive a public IPv4 address.
    --
    -- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     subnet.
    --
    -- -   @private-dns-name-options-on-launch.hostname-type@ - The type of
    --     hostname to assign to instances in the subnet at launch. For
    --     IPv4-only and dual-stack (IPv4 and IPv6) subnets, an instance DNS
    --     name can be based on the instance IPv4 address (ip-name) or the
    --     instance ID (resource-name). For IPv6 only subnets, an instance DNS
    --     name must be based on the instance ID (resource-name).
    --
    -- -   @private-dns-name-options-on-launch.enable-resource-name-dns-a-record@
    --     - Indicates whether to respond to DNS queries for instance hostnames
    --     with DNS A records.
    --
    -- -   @private-dns-name-options-on-launch.enable-resource-name-dns-aaaa-record@
    --     - Indicates whether to respond to DNS queries for instance hostnames
    --     with DNS AAAA records.
    --
    -- -   @state@ - The state of the subnet (@pending@ | @available@).
    --
    -- -   @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.
    --
    -- -   @subnet-id@ - The ID of the subnet.
    --
    -- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
    --     resource. Use the tag key in the filter name and the tag value as
    --     the filter value. For example, to find all resources that have a tag
    --     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
    --     the filter name and @TeamA@ for the filter value.
    --
    -- -   @tag-key@ - The key of a tag assigned to the resource. Use this
    --     filter to find all resources assigned a tag with a specific key,
    --     regardless of the tag value.
    --
    -- -   @vpc-id@ - The ID of the VPC for the subnet.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more subnet IDs.
    --
    -- Default: Describes all your subnets.
    subnetIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSubnets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeSubnets_filters' - One or more filters.
--
-- -   @availability-zone@ - The Availability Zone for the subnet. You can
--     also use @availabilityZone@ as the filter name.
--
-- -   @availability-zone-id@ - The ID of the Availability Zone for the
--     subnet. You can also use @availabilityZoneId@ as the filter name.
--
-- -   @available-ip-address-count@ - The number of IPv4 addresses in the
--     subnet that are available.
--
-- -   @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you
--     specify must exactly match the subnet\'s CIDR block for information
--     to be returned for the subnet. You can also use @cidr@ or
--     @cidrBlock@ as the filter names.
--
-- -   @customer-owned-ipv4-pool@ - The customer-owned IPv4 address pool
--     associated with the subnet.
--
-- -   @default-for-az@ - Indicates whether this is the default subnet for
--     the Availability Zone (@true@ | @false@). You can also use
--     @defaultForAz@ as the filter name.
--
-- -   @enable-dns64@ - Indicates whether DNS queries made to the
--     Amazon-provided DNS Resolver in this subnet should return synthetic
--     IPv6 addresses for IPv4-only destinations.
--
-- -   @enable-lni-at-device-index@ - Indicates the device position for
--     local network interfaces in this subnet. For example, @1@ indicates
--     local network interfaces in this subnet are the secondary network
--     interface (eth1).
--
-- -   @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block
--     associated with the subnet.
--
-- -   @ipv6-cidr-block-association.association-id@ - An association ID for
--     an IPv6 CIDR block associated with the subnet.
--
-- -   @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR
--     block associated with the subnet.
--
-- -   @ipv6-native@ - Indicates whether this is an IPv6 only subnet
--     (@true@ | @false@).
--
-- -   @map-customer-owned-ip-on-launch@ - Indicates whether a network
--     interface created in this subnet (including a network interface
--     created by RunInstances) receives a customer-owned IPv4 address.
--
-- -   @map-public-ip-on-launch@ - Indicates whether instances launched in
--     this subnet receive a public IPv4 address.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     subnet.
--
-- -   @private-dns-name-options-on-launch.hostname-type@ - The type of
--     hostname to assign to instances in the subnet at launch. For
--     IPv4-only and dual-stack (IPv4 and IPv6) subnets, an instance DNS
--     name can be based on the instance IPv4 address (ip-name) or the
--     instance ID (resource-name). For IPv6 only subnets, an instance DNS
--     name must be based on the instance ID (resource-name).
--
-- -   @private-dns-name-options-on-launch.enable-resource-name-dns-a-record@
--     - Indicates whether to respond to DNS queries for instance hostnames
--     with DNS A records.
--
-- -   @private-dns-name-options-on-launch.enable-resource-name-dns-aaaa-record@
--     - Indicates whether to respond to DNS queries for instance hostnames
--     with DNS AAAA records.
--
-- -   @state@ - The state of the subnet (@pending@ | @available@).
--
-- -   @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC for the subnet.
--
-- 'maxResults', 'describeSubnets_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'nextToken', 'describeSubnets_nextToken' - The token for the next page of results.
--
-- 'subnetIds', 'describeSubnets_subnetIds' - One or more subnet IDs.
--
-- Default: Describes all your subnets.
newDescribeSubnets ::
  DescribeSubnets
newDescribeSubnets =
  DescribeSubnets'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      subnetIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSubnets_dryRun :: Lens.Lens' DescribeSubnets (Prelude.Maybe Prelude.Bool)
describeSubnets_dryRun = Lens.lens (\DescribeSubnets' {dryRun} -> dryRun) (\s@DescribeSubnets' {} a -> s {dryRun = a} :: DescribeSubnets)

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone for the subnet. You can
--     also use @availabilityZone@ as the filter name.
--
-- -   @availability-zone-id@ - The ID of the Availability Zone for the
--     subnet. You can also use @availabilityZoneId@ as the filter name.
--
-- -   @available-ip-address-count@ - The number of IPv4 addresses in the
--     subnet that are available.
--
-- -   @cidr-block@ - The IPv4 CIDR block of the subnet. The CIDR block you
--     specify must exactly match the subnet\'s CIDR block for information
--     to be returned for the subnet. You can also use @cidr@ or
--     @cidrBlock@ as the filter names.
--
-- -   @customer-owned-ipv4-pool@ - The customer-owned IPv4 address pool
--     associated with the subnet.
--
-- -   @default-for-az@ - Indicates whether this is the default subnet for
--     the Availability Zone (@true@ | @false@). You can also use
--     @defaultForAz@ as the filter name.
--
-- -   @enable-dns64@ - Indicates whether DNS queries made to the
--     Amazon-provided DNS Resolver in this subnet should return synthetic
--     IPv6 addresses for IPv4-only destinations.
--
-- -   @enable-lni-at-device-index@ - Indicates the device position for
--     local network interfaces in this subnet. For example, @1@ indicates
--     local network interfaces in this subnet are the secondary network
--     interface (eth1).
--
-- -   @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block
--     associated with the subnet.
--
-- -   @ipv6-cidr-block-association.association-id@ - An association ID for
--     an IPv6 CIDR block associated with the subnet.
--
-- -   @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR
--     block associated with the subnet.
--
-- -   @ipv6-native@ - Indicates whether this is an IPv6 only subnet
--     (@true@ | @false@).
--
-- -   @map-customer-owned-ip-on-launch@ - Indicates whether a network
--     interface created in this subnet (including a network interface
--     created by RunInstances) receives a customer-owned IPv4 address.
--
-- -   @map-public-ip-on-launch@ - Indicates whether instances launched in
--     this subnet receive a public IPv4 address.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     subnet.
--
-- -   @private-dns-name-options-on-launch.hostname-type@ - The type of
--     hostname to assign to instances in the subnet at launch. For
--     IPv4-only and dual-stack (IPv4 and IPv6) subnets, an instance DNS
--     name can be based on the instance IPv4 address (ip-name) or the
--     instance ID (resource-name). For IPv6 only subnets, an instance DNS
--     name must be based on the instance ID (resource-name).
--
-- -   @private-dns-name-options-on-launch.enable-resource-name-dns-a-record@
--     - Indicates whether to respond to DNS queries for instance hostnames
--     with DNS A records.
--
-- -   @private-dns-name-options-on-launch.enable-resource-name-dns-aaaa-record@
--     - Indicates whether to respond to DNS queries for instance hostnames
--     with DNS AAAA records.
--
-- -   @state@ - The state of the subnet (@pending@ | @available@).
--
-- -   @subnet-arn@ - The Amazon Resource Name (ARN) of the subnet.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @tag@:\<key> - The key\/value combination of a tag assigned to the
--     resource. Use the tag key in the filter name and the tag value as
--     the filter value. For example, to find all resources that have a tag
--     with the key @Owner@ and the value @TeamA@, specify @tag:Owner@ for
--     the filter name and @TeamA@ for the filter value.
--
-- -   @tag-key@ - The key of a tag assigned to the resource. Use this
--     filter to find all resources assigned a tag with a specific key,
--     regardless of the tag value.
--
-- -   @vpc-id@ - The ID of the VPC for the subnet.
describeSubnets_filters :: Lens.Lens' DescribeSubnets (Prelude.Maybe [Filter])
describeSubnets_filters = Lens.lens (\DescribeSubnets' {filters} -> filters) (\s@DescribeSubnets' {} a -> s {filters = a} :: DescribeSubnets) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeSubnets_maxResults :: Lens.Lens' DescribeSubnets (Prelude.Maybe Prelude.Natural)
describeSubnets_maxResults = Lens.lens (\DescribeSubnets' {maxResults} -> maxResults) (\s@DescribeSubnets' {} a -> s {maxResults = a} :: DescribeSubnets)

-- | The token for the next page of results.
describeSubnets_nextToken :: Lens.Lens' DescribeSubnets (Prelude.Maybe Prelude.Text)
describeSubnets_nextToken = Lens.lens (\DescribeSubnets' {nextToken} -> nextToken) (\s@DescribeSubnets' {} a -> s {nextToken = a} :: DescribeSubnets)

-- | One or more subnet IDs.
--
-- Default: Describes all your subnets.
describeSubnets_subnetIds :: Lens.Lens' DescribeSubnets (Prelude.Maybe [Prelude.Text])
describeSubnets_subnetIds = Lens.lens (\DescribeSubnets' {subnetIds} -> subnetIds) (\s@DescribeSubnets' {} a -> s {subnetIds = a} :: DescribeSubnets) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeSubnets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSubnetsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSubnetsResponse_subnets
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSubnets_nextToken
          Lens..~ rs
          Lens.^? describeSubnetsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSubnets where
  type
    AWSResponse DescribeSubnets =
      DescribeSubnetsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSubnetsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "subnetSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSubnets where
  hashWithSalt _salt DescribeSubnets' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` subnetIds

instance Prelude.NFData DescribeSubnets where
  rnf DescribeSubnets' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subnetIds

instance Data.ToHeaders DescribeSubnets where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSubnets where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSubnets where
  toQuery DescribeSubnets' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSubnets" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "SubnetId" Prelude.<$> subnetIds)
      ]

-- | /See:/ 'newDescribeSubnetsResponse' smart constructor.
data DescribeSubnetsResponse = DescribeSubnetsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more subnets.
    subnets :: Prelude.Maybe [Subnet],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSubnetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSubnetsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'subnets', 'describeSubnetsResponse_subnets' - Information about one or more subnets.
--
-- 'httpStatus', 'describeSubnetsResponse_httpStatus' - The response's http status code.
newDescribeSubnetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSubnetsResponse
newDescribeSubnetsResponse pHttpStatus_ =
  DescribeSubnetsResponse'
    { nextToken =
        Prelude.Nothing,
      subnets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeSubnetsResponse_nextToken :: Lens.Lens' DescribeSubnetsResponse (Prelude.Maybe Prelude.Text)
describeSubnetsResponse_nextToken = Lens.lens (\DescribeSubnetsResponse' {nextToken} -> nextToken) (\s@DescribeSubnetsResponse' {} a -> s {nextToken = a} :: DescribeSubnetsResponse)

-- | Information about one or more subnets.
describeSubnetsResponse_subnets :: Lens.Lens' DescribeSubnetsResponse (Prelude.Maybe [Subnet])
describeSubnetsResponse_subnets = Lens.lens (\DescribeSubnetsResponse' {subnets} -> subnets) (\s@DescribeSubnetsResponse' {} a -> s {subnets = a} :: DescribeSubnetsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSubnetsResponse_httpStatus :: Lens.Lens' DescribeSubnetsResponse Prelude.Int
describeSubnetsResponse_httpStatus = Lens.lens (\DescribeSubnetsResponse' {httpStatus} -> httpStatus) (\s@DescribeSubnetsResponse' {} a -> s {httpStatus = a} :: DescribeSubnetsResponse)

instance Prelude.NFData DescribeSubnetsResponse where
  rnf DescribeSubnetsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf httpStatus
