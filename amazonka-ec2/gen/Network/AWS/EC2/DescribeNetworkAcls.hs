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
-- Module      : Network.AWS.EC2.DescribeNetworkAcls
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your network ACLs.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_ACLs.html Network ACLs>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNetworkAcls
  ( -- * Creating a Request
    DescribeNetworkAcls (..),
    newDescribeNetworkAcls,

    -- * Request Lenses
    describeNetworkAcls_nextToken,
    describeNetworkAcls_dryRun,
    describeNetworkAcls_maxResults,
    describeNetworkAcls_networkAclIds,
    describeNetworkAcls_filters,

    -- * Destructuring the Response
    DescribeNetworkAclsResponse (..),
    newDescribeNetworkAclsResponse,

    -- * Response Lenses
    describeNetworkAclsResponse_nextToken,
    describeNetworkAclsResponse_networkAcls,
    describeNetworkAclsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNetworkAcls' smart constructor.
data DescribeNetworkAcls = DescribeNetworkAcls'
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
    -- | One or more network ACL IDs.
    --
    -- Default: Describes all your network ACLs.
    networkAclIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @association.association-id@ - The ID of an association ID for the
    --     ACL.
    --
    -- -   @association.network-acl-id@ - The ID of the network ACL involved in
    --     the association.
    --
    -- -   @association.subnet-id@ - The ID of the subnet involved in the
    --     association.
    --
    -- -   @default@ - Indicates whether the ACL is the default network ACL for
    --     the VPC.
    --
    -- -   @entry.cidr@ - The IPv4 CIDR range specified in the entry.
    --
    -- -   @entry.icmp.code@ - The ICMP code specified in the entry, if any.
    --
    -- -   @entry.icmp.type@ - The ICMP type specified in the entry, if any.
    --
    -- -   @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.
    --
    -- -   @entry.port-range.from@ - The start of the port range specified in
    --     the entry.
    --
    -- -   @entry.port-range.to@ - The end of the port range specified in the
    --     entry.
    --
    -- -   @entry.protocol@ - The protocol specified in the entry (@tcp@ |
    --     @udp@ | @icmp@ or a protocol number).
    --
    -- -   @entry.rule-action@ - Allows or denies the matching traffic (@allow@
    --     | @deny@).
    --
    -- -   @entry.rule-number@ - The number of an entry (in other words, rule)
    --     in the set of ACL entries.
    --
    -- -   @network-acl-id@ - The ID of the network ACL.
    --
    -- -   @owner-id@ - The ID of the AWS account that owns the network ACL.
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
    -- -   @vpc-id@ - The ID of the VPC for the network ACL.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNetworkAcls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkAcls_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeNetworkAcls_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeNetworkAcls_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'networkAclIds', 'describeNetworkAcls_networkAclIds' - One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
--
-- 'filters', 'describeNetworkAcls_filters' - One or more filters.
--
-- -   @association.association-id@ - The ID of an association ID for the
--     ACL.
--
-- -   @association.network-acl-id@ - The ID of the network ACL involved in
--     the association.
--
-- -   @association.subnet-id@ - The ID of the subnet involved in the
--     association.
--
-- -   @default@ - Indicates whether the ACL is the default network ACL for
--     the VPC.
--
-- -   @entry.cidr@ - The IPv4 CIDR range specified in the entry.
--
-- -   @entry.icmp.code@ - The ICMP code specified in the entry, if any.
--
-- -   @entry.icmp.type@ - The ICMP type specified in the entry, if any.
--
-- -   @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.
--
-- -   @entry.port-range.from@ - The start of the port range specified in
--     the entry.
--
-- -   @entry.port-range.to@ - The end of the port range specified in the
--     entry.
--
-- -   @entry.protocol@ - The protocol specified in the entry (@tcp@ |
--     @udp@ | @icmp@ or a protocol number).
--
-- -   @entry.rule-action@ - Allows or denies the matching traffic (@allow@
--     | @deny@).
--
-- -   @entry.rule-number@ - The number of an entry (in other words, rule)
--     in the set of ACL entries.
--
-- -   @network-acl-id@ - The ID of the network ACL.
--
-- -   @owner-id@ - The ID of the AWS account that owns the network ACL.
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
-- -   @vpc-id@ - The ID of the VPC for the network ACL.
newDescribeNetworkAcls ::
  DescribeNetworkAcls
newDescribeNetworkAcls =
  DescribeNetworkAcls'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      networkAclIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeNetworkAcls_nextToken :: Lens.Lens' DescribeNetworkAcls (Core.Maybe Core.Text)
describeNetworkAcls_nextToken = Lens.lens (\DescribeNetworkAcls' {nextToken} -> nextToken) (\s@DescribeNetworkAcls' {} a -> s {nextToken = a} :: DescribeNetworkAcls)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNetworkAcls_dryRun :: Lens.Lens' DescribeNetworkAcls (Core.Maybe Core.Bool)
describeNetworkAcls_dryRun = Lens.lens (\DescribeNetworkAcls' {dryRun} -> dryRun) (\s@DescribeNetworkAcls' {} a -> s {dryRun = a} :: DescribeNetworkAcls)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNetworkAcls_maxResults :: Lens.Lens' DescribeNetworkAcls (Core.Maybe Core.Natural)
describeNetworkAcls_maxResults = Lens.lens (\DescribeNetworkAcls' {maxResults} -> maxResults) (\s@DescribeNetworkAcls' {} a -> s {maxResults = a} :: DescribeNetworkAcls)

-- | One or more network ACL IDs.
--
-- Default: Describes all your network ACLs.
describeNetworkAcls_networkAclIds :: Lens.Lens' DescribeNetworkAcls (Core.Maybe [Core.Text])
describeNetworkAcls_networkAclIds = Lens.lens (\DescribeNetworkAcls' {networkAclIds} -> networkAclIds) (\s@DescribeNetworkAcls' {} a -> s {networkAclIds = a} :: DescribeNetworkAcls) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @association.association-id@ - The ID of an association ID for the
--     ACL.
--
-- -   @association.network-acl-id@ - The ID of the network ACL involved in
--     the association.
--
-- -   @association.subnet-id@ - The ID of the subnet involved in the
--     association.
--
-- -   @default@ - Indicates whether the ACL is the default network ACL for
--     the VPC.
--
-- -   @entry.cidr@ - The IPv4 CIDR range specified in the entry.
--
-- -   @entry.icmp.code@ - The ICMP code specified in the entry, if any.
--
-- -   @entry.icmp.type@ - The ICMP type specified in the entry, if any.
--
-- -   @entry.ipv6-cidr@ - The IPv6 CIDR range specified in the entry.
--
-- -   @entry.port-range.from@ - The start of the port range specified in
--     the entry.
--
-- -   @entry.port-range.to@ - The end of the port range specified in the
--     entry.
--
-- -   @entry.protocol@ - The protocol specified in the entry (@tcp@ |
--     @udp@ | @icmp@ or a protocol number).
--
-- -   @entry.rule-action@ - Allows or denies the matching traffic (@allow@
--     | @deny@).
--
-- -   @entry.rule-number@ - The number of an entry (in other words, rule)
--     in the set of ACL entries.
--
-- -   @network-acl-id@ - The ID of the network ACL.
--
-- -   @owner-id@ - The ID of the AWS account that owns the network ACL.
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
-- -   @vpc-id@ - The ID of the VPC for the network ACL.
describeNetworkAcls_filters :: Lens.Lens' DescribeNetworkAcls (Core.Maybe [Filter])
describeNetworkAcls_filters = Lens.lens (\DescribeNetworkAcls' {filters} -> filters) (\s@DescribeNetworkAcls' {} a -> s {filters = a} :: DescribeNetworkAcls) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeNetworkAcls where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNetworkAclsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNetworkAclsResponse_networkAcls
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNetworkAcls_nextToken
          Lens..~ rs
          Lens.^? describeNetworkAclsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeNetworkAcls where
  type
    AWSResponse DescribeNetworkAcls =
      DescribeNetworkAclsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNetworkAclsResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "networkAclSet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeNetworkAcls

instance Core.NFData DescribeNetworkAcls

instance Core.ToHeaders DescribeNetworkAcls where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeNetworkAcls where
  toPath = Core.const "/"

instance Core.ToQuery DescribeNetworkAcls where
  toQuery DescribeNetworkAcls' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeNetworkAcls" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "NetworkAclId"
              Core.<$> networkAclIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeNetworkAclsResponse' smart constructor.
data DescribeNetworkAclsResponse = DescribeNetworkAclsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more network ACLs.
    networkAcls :: Core.Maybe [NetworkAcl],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNetworkAclsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNetworkAclsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'networkAcls', 'describeNetworkAclsResponse_networkAcls' - Information about one or more network ACLs.
--
-- 'httpStatus', 'describeNetworkAclsResponse_httpStatus' - The response's http status code.
newDescribeNetworkAclsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNetworkAclsResponse
newDescribeNetworkAclsResponse pHttpStatus_ =
  DescribeNetworkAclsResponse'
    { nextToken =
        Core.Nothing,
      networkAcls = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNetworkAclsResponse_nextToken :: Lens.Lens' DescribeNetworkAclsResponse (Core.Maybe Core.Text)
describeNetworkAclsResponse_nextToken = Lens.lens (\DescribeNetworkAclsResponse' {nextToken} -> nextToken) (\s@DescribeNetworkAclsResponse' {} a -> s {nextToken = a} :: DescribeNetworkAclsResponse)

-- | Information about one or more network ACLs.
describeNetworkAclsResponse_networkAcls :: Lens.Lens' DescribeNetworkAclsResponse (Core.Maybe [NetworkAcl])
describeNetworkAclsResponse_networkAcls = Lens.lens (\DescribeNetworkAclsResponse' {networkAcls} -> networkAcls) (\s@DescribeNetworkAclsResponse' {} a -> s {networkAcls = a} :: DescribeNetworkAclsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNetworkAclsResponse_httpStatus :: Lens.Lens' DescribeNetworkAclsResponse Core.Int
describeNetworkAclsResponse_httpStatus = Lens.lens (\DescribeNetworkAclsResponse' {httpStatus} -> httpStatus) (\s@DescribeNetworkAclsResponse' {} a -> s {httpStatus = a} :: DescribeNetworkAclsResponse)

instance Core.NFData DescribeNetworkAclsResponse
