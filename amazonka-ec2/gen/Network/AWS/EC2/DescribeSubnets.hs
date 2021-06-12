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
-- Module      : Network.AWS.EC2.DescribeSubnets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your subnets.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html Your VPC and Subnets>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeSubnets
  ( -- * Creating a Request
    DescribeSubnets (..),
    newDescribeSubnets,

    -- * Request Lenses
    describeSubnets_nextToken,
    describeSubnets_dryRun,
    describeSubnets_maxResults,
    describeSubnets_subnetIds,
    describeSubnets_filters,

    -- * Destructuring the Response
    DescribeSubnetsResponse (..),
    newDescribeSubnetsResponse,

    -- * Response Lenses
    describeSubnetsResponse_nextToken,
    describeSubnetsResponse_subnets,
    describeSubnetsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeSubnets' smart constructor.
data DescribeSubnets = DescribeSubnets'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more subnet IDs.
    --
    -- Default: Describes all your subnets.
    subnetIds :: Core.Maybe [Core.Text],
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
    -- -   @default-for-az@ - Indicates whether this is the default subnet for
    --     the Availability Zone. You can also use @defaultForAz@ as the filter
    --     name.
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
    -- -   @owner-id@ - The ID of the AWS account that owns the subnet.
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeSubnets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSubnets_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeSubnets_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeSubnets_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'subnetIds', 'describeSubnets_subnetIds' - One or more subnet IDs.
--
-- Default: Describes all your subnets.
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
-- -   @default-for-az@ - Indicates whether this is the default subnet for
--     the Availability Zone. You can also use @defaultForAz@ as the filter
--     name.
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
-- -   @owner-id@ - The ID of the AWS account that owns the subnet.
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
newDescribeSubnets ::
  DescribeSubnets
newDescribeSubnets =
  DescribeSubnets'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      subnetIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeSubnets_nextToken :: Lens.Lens' DescribeSubnets (Core.Maybe Core.Text)
describeSubnets_nextToken = Lens.lens (\DescribeSubnets' {nextToken} -> nextToken) (\s@DescribeSubnets' {} a -> s {nextToken = a} :: DescribeSubnets)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSubnets_dryRun :: Lens.Lens' DescribeSubnets (Core.Maybe Core.Bool)
describeSubnets_dryRun = Lens.lens (\DescribeSubnets' {dryRun} -> dryRun) (\s@DescribeSubnets' {} a -> s {dryRun = a} :: DescribeSubnets)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeSubnets_maxResults :: Lens.Lens' DescribeSubnets (Core.Maybe Core.Natural)
describeSubnets_maxResults = Lens.lens (\DescribeSubnets' {maxResults} -> maxResults) (\s@DescribeSubnets' {} a -> s {maxResults = a} :: DescribeSubnets)

-- | One or more subnet IDs.
--
-- Default: Describes all your subnets.
describeSubnets_subnetIds :: Lens.Lens' DescribeSubnets (Core.Maybe [Core.Text])
describeSubnets_subnetIds = Lens.lens (\DescribeSubnets' {subnetIds} -> subnetIds) (\s@DescribeSubnets' {} a -> s {subnetIds = a} :: DescribeSubnets) Core.. Lens.mapping Lens._Coerce

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
-- -   @default-for-az@ - Indicates whether this is the default subnet for
--     the Availability Zone. You can also use @defaultForAz@ as the filter
--     name.
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
-- -   @owner-id@ - The ID of the AWS account that owns the subnet.
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
describeSubnets_filters :: Lens.Lens' DescribeSubnets (Core.Maybe [Filter])
describeSubnets_filters = Lens.lens (\DescribeSubnets' {filters} -> filters) (\s@DescribeSubnets' {} a -> s {filters = a} :: DescribeSubnets) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeSubnets where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSubnetsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSubnetsResponse_subnets Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeSubnets_nextToken
          Lens..~ rs
          Lens.^? describeSubnetsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeSubnets where
  type
    AWSResponse DescribeSubnets =
      DescribeSubnetsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSubnetsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "subnetSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeSubnets

instance Core.NFData DescribeSubnets

instance Core.ToHeaders DescribeSubnets where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSubnets where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSubnets where
  toQuery DescribeSubnets' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSubnets" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "SubnetId" Core.<$> subnetIds),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeSubnetsResponse' smart constructor.
data DescribeSubnetsResponse = DescribeSubnetsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more subnets.
    subnets :: Core.Maybe [Subnet],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeSubnetsResponse
newDescribeSubnetsResponse pHttpStatus_ =
  DescribeSubnetsResponse'
    { nextToken = Core.Nothing,
      subnets = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeSubnetsResponse_nextToken :: Lens.Lens' DescribeSubnetsResponse (Core.Maybe Core.Text)
describeSubnetsResponse_nextToken = Lens.lens (\DescribeSubnetsResponse' {nextToken} -> nextToken) (\s@DescribeSubnetsResponse' {} a -> s {nextToken = a} :: DescribeSubnetsResponse)

-- | Information about one or more subnets.
describeSubnetsResponse_subnets :: Lens.Lens' DescribeSubnetsResponse (Core.Maybe [Subnet])
describeSubnetsResponse_subnets = Lens.lens (\DescribeSubnetsResponse' {subnets} -> subnets) (\s@DescribeSubnetsResponse' {} a -> s {subnets = a} :: DescribeSubnetsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeSubnetsResponse_httpStatus :: Lens.Lens' DescribeSubnetsResponse Core.Int
describeSubnetsResponse_httpStatus = Lens.lens (\DescribeSubnetsResponse' {httpStatus} -> httpStatus) (\s@DescribeSubnetsResponse' {} a -> s {httpStatus = a} :: DescribeSubnetsResponse)

instance Core.NFData DescribeSubnetsResponse
