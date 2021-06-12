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
-- Module      : Network.AWS.EC2.DescribeVpcs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeVpcs
  ( -- * Creating a Request
    DescribeVpcs (..),
    newDescribeVpcs,

    -- * Request Lenses
    describeVpcs_nextToken,
    describeVpcs_dryRun,
    describeVpcs_maxResults,
    describeVpcs_filters,
    describeVpcs_vpcIds,

    -- * Destructuring the Response
    DescribeVpcsResponse (..),
    newDescribeVpcsResponse,

    -- * Response Lenses
    describeVpcsResponse_nextToken,
    describeVpcsResponse_vpcs,
    describeVpcsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeVpcs' smart constructor.
data DescribeVpcs = DescribeVpcs'
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
    -- | One or more filters.
    --
    -- -   @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you
    --     specify must exactly match the VPC\'s CIDR block for information to
    --     be returned for the VPC. Must contain the slash followed by one or
    --     two digits (for example, @\/28@).
    --
    -- -   @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated
    --     with the VPC.
    --
    -- -   @cidr-block-association.association-id@ - The association ID for an
    --     IPv4 CIDR block associated with the VPC.
    --
    -- -   @cidr-block-association.state@ - The state of an IPv4 CIDR block
    --     associated with the VPC.
    --
    -- -   @dhcp-options-id@ - The ID of a set of DHCP options.
    --
    -- -   @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block
    --     associated with the VPC.
    --
    -- -   @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address
    --     pool from which the IPv6 CIDR block is allocated.
    --
    -- -   @ipv6-cidr-block-association.association-id@ - The association ID
    --     for an IPv6 CIDR block associated with the VPC.
    --
    -- -   @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR
    --     block associated with the VPC.
    --
    -- -   @isDefault@ - Indicates whether the VPC is the default VPC.
    --
    -- -   @owner-id@ - The ID of the AWS account that owns the VPC.
    --
    -- -   @state@ - The state of the VPC (@pending@ | @available@).
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
    -- -   @vpc-id@ - The ID of the VPC.
    filters :: Core.Maybe [Filter],
    -- | One or more VPC IDs.
    --
    -- Default: Describes all your VPCs.
    vpcIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcs_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeVpcs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeVpcs_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeVpcs_filters' - One or more filters.
--
-- -   @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you
--     specify must exactly match the VPC\'s CIDR block for information to
--     be returned for the VPC. Must contain the slash followed by one or
--     two digits (for example, @\/28@).
--
-- -   @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated
--     with the VPC.
--
-- -   @cidr-block-association.association-id@ - The association ID for an
--     IPv4 CIDR block associated with the VPC.
--
-- -   @cidr-block-association.state@ - The state of an IPv4 CIDR block
--     associated with the VPC.
--
-- -   @dhcp-options-id@ - The ID of a set of DHCP options.
--
-- -   @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block
--     associated with the VPC.
--
-- -   @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address
--     pool from which the IPv6 CIDR block is allocated.
--
-- -   @ipv6-cidr-block-association.association-id@ - The association ID
--     for an IPv6 CIDR block associated with the VPC.
--
-- -   @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR
--     block associated with the VPC.
--
-- -   @isDefault@ - Indicates whether the VPC is the default VPC.
--
-- -   @owner-id@ - The ID of the AWS account that owns the VPC.
--
-- -   @state@ - The state of the VPC (@pending@ | @available@).
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
-- -   @vpc-id@ - The ID of the VPC.
--
-- 'vpcIds', 'describeVpcs_vpcIds' - One or more VPC IDs.
--
-- Default: Describes all your VPCs.
newDescribeVpcs ::
  DescribeVpcs
newDescribeVpcs =
  DescribeVpcs'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing,
      vpcIds = Core.Nothing
    }

-- | The token for the next page of results.
describeVpcs_nextToken :: Lens.Lens' DescribeVpcs (Core.Maybe Core.Text)
describeVpcs_nextToken = Lens.lens (\DescribeVpcs' {nextToken} -> nextToken) (\s@DescribeVpcs' {} a -> s {nextToken = a} :: DescribeVpcs)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcs_dryRun :: Lens.Lens' DescribeVpcs (Core.Maybe Core.Bool)
describeVpcs_dryRun = Lens.lens (\DescribeVpcs' {dryRun} -> dryRun) (\s@DescribeVpcs' {} a -> s {dryRun = a} :: DescribeVpcs)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeVpcs_maxResults :: Lens.Lens' DescribeVpcs (Core.Maybe Core.Natural)
describeVpcs_maxResults = Lens.lens (\DescribeVpcs' {maxResults} -> maxResults) (\s@DescribeVpcs' {} a -> s {maxResults = a} :: DescribeVpcs)

-- | One or more filters.
--
-- -   @cidr@ - The primary IPv4 CIDR block of the VPC. The CIDR block you
--     specify must exactly match the VPC\'s CIDR block for information to
--     be returned for the VPC. Must contain the slash followed by one or
--     two digits (for example, @\/28@).
--
-- -   @cidr-block-association.cidr-block@ - An IPv4 CIDR block associated
--     with the VPC.
--
-- -   @cidr-block-association.association-id@ - The association ID for an
--     IPv4 CIDR block associated with the VPC.
--
-- -   @cidr-block-association.state@ - The state of an IPv4 CIDR block
--     associated with the VPC.
--
-- -   @dhcp-options-id@ - The ID of a set of DHCP options.
--
-- -   @ipv6-cidr-block-association.ipv6-cidr-block@ - An IPv6 CIDR block
--     associated with the VPC.
--
-- -   @ipv6-cidr-block-association.ipv6-pool@ - The ID of the IPv6 address
--     pool from which the IPv6 CIDR block is allocated.
--
-- -   @ipv6-cidr-block-association.association-id@ - The association ID
--     for an IPv6 CIDR block associated with the VPC.
--
-- -   @ipv6-cidr-block-association.state@ - The state of an IPv6 CIDR
--     block associated with the VPC.
--
-- -   @isDefault@ - Indicates whether the VPC is the default VPC.
--
-- -   @owner-id@ - The ID of the AWS account that owns the VPC.
--
-- -   @state@ - The state of the VPC (@pending@ | @available@).
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
-- -   @vpc-id@ - The ID of the VPC.
describeVpcs_filters :: Lens.Lens' DescribeVpcs (Core.Maybe [Filter])
describeVpcs_filters = Lens.lens (\DescribeVpcs' {filters} -> filters) (\s@DescribeVpcs' {} a -> s {filters = a} :: DescribeVpcs) Core.. Lens.mapping Lens._Coerce

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
describeVpcs_vpcIds :: Lens.Lens' DescribeVpcs (Core.Maybe [Core.Text])
describeVpcs_vpcIds = Lens.lens (\DescribeVpcs' {vpcIds} -> vpcIds) (\s@DescribeVpcs' {} a -> s {vpcIds = a} :: DescribeVpcs) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeVpcs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcsResponse_vpcs Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVpcs_nextToken
          Lens..~ rs
          Lens.^? describeVpcsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest DescribeVpcs where
  type AWSResponse DescribeVpcs = DescribeVpcsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "vpcSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpcs

instance Core.NFData DescribeVpcs

instance Core.ToHeaders DescribeVpcs where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeVpcs where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpcs where
  toQuery DescribeVpcs' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeVpcs" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        Core.toQuery
          (Core.toQueryList "VpcId" Core.<$> vpcIds)
      ]

-- | /See:/ 'newDescribeVpcsResponse' smart constructor.
data DescribeVpcsResponse = DescribeVpcsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more VPCs.
    vpcs :: Core.Maybe [Vpc],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'vpcs', 'describeVpcsResponse_vpcs' - Information about one or more VPCs.
--
-- 'httpStatus', 'describeVpcsResponse_httpStatus' - The response's http status code.
newDescribeVpcsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcsResponse
newDescribeVpcsResponse pHttpStatus_ =
  DescribeVpcsResponse'
    { nextToken = Core.Nothing,
      vpcs = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeVpcsResponse_nextToken :: Lens.Lens' DescribeVpcsResponse (Core.Maybe Core.Text)
describeVpcsResponse_nextToken = Lens.lens (\DescribeVpcsResponse' {nextToken} -> nextToken) (\s@DescribeVpcsResponse' {} a -> s {nextToken = a} :: DescribeVpcsResponse)

-- | Information about one or more VPCs.
describeVpcsResponse_vpcs :: Lens.Lens' DescribeVpcsResponse (Core.Maybe [Vpc])
describeVpcsResponse_vpcs = Lens.lens (\DescribeVpcsResponse' {vpcs} -> vpcs) (\s@DescribeVpcsResponse' {} a -> s {vpcs = a} :: DescribeVpcsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcsResponse_httpStatus :: Lens.Lens' DescribeVpcsResponse Core.Int
describeVpcsResponse_httpStatus = Lens.lens (\DescribeVpcsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcsResponse' {} a -> s {httpStatus = a} :: DescribeVpcsResponse)

instance Core.NFData DescribeVpcsResponse
