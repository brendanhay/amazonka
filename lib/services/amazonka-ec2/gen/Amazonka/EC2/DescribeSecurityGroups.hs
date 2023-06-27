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
-- Module      : Amazonka.EC2.DescribeSecurityGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified security groups or all of your security groups.
--
-- A security group is for use with instances either in the EC2-Classic
-- platform or in a specific VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-network-security.html Amazon EC2 security groups>
-- in the /Amazon Elastic Compute Cloud User Guide/ and
-- <https://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_SecurityGroups.html Security groups for your VPC>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- We are retiring EC2-Classic. We recommend that you migrate from
-- EC2-Classic to a VPC. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-migrate.html Migrate from EC2-Classic to a VPC>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeSecurityGroups
  ( -- * Creating a Request
    DescribeSecurityGroups (..),
    newDescribeSecurityGroups,

    -- * Request Lenses
    describeSecurityGroups_dryRun,
    describeSecurityGroups_filters,
    describeSecurityGroups_groupIds,
    describeSecurityGroups_groupNames,
    describeSecurityGroups_maxResults,
    describeSecurityGroups_nextToken,

    -- * Destructuring the Response
    DescribeSecurityGroupsResponse (..),
    newDescribeSecurityGroupsResponse,

    -- * Response Lenses
    describeSecurityGroupsResponse_nextToken,
    describeSecurityGroupsResponse_securityGroups,
    describeSecurityGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeSecurityGroups' smart constructor.
data DescribeSecurityGroups = DescribeSecurityGroups'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The filters. If using multiple filters for rules, the results include
    -- security groups for which any combination of rules - not necessarily a
    -- single rule - match all filters.
    --
    -- -   @description@ - The description of the security group.
    --
    -- -   @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound
    --     security group rule.
    --
    -- -   @egress.ip-permission.from-port@ - For an outbound rule, the start
    --     of port range for the TCP and UDP protocols, or an ICMP type number.
    --
    -- -   @egress.ip-permission.group-id@ - The ID of a security group that
    --     has been referenced in an outbound security group rule.
    --
    -- -   @egress.ip-permission.group-name@ - The name of a security group
    --     that is referenced in an outbound security group rule.
    --
    -- -   @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an
    --     outbound security group rule.
    --
    -- -   @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to
    --     which a security group rule allows outbound access.
    --
    -- -   @egress.ip-permission.protocol@ - The IP protocol for an outbound
    --     security group rule (@tcp@ | @udp@ | @icmp@, a protocol number, or
    --     -1 for all protocols).
    --
    -- -   @egress.ip-permission.to-port@ - For an outbound rule, the end of
    --     port range for the TCP and UDP protocols, or an ICMP code.
    --
    -- -   @egress.ip-permission.user-id@ - The ID of an Amazon Web Services
    --     account that has been referenced in an outbound security group rule.
    --
    -- -   @group-id@ - The ID of the security group.
    --
    -- -   @group-name@ - The name of the security group.
    --
    -- -   @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security
    --     group rule.
    --
    -- -   @ip-permission.from-port@ - For an inbound rule, the start of port
    --     range for the TCP and UDP protocols, or an ICMP type number.
    --
    -- -   @ip-permission.group-id@ - The ID of a security group that has been
    --     referenced in an inbound security group rule.
    --
    -- -   @ip-permission.group-name@ - The name of a security group that is
    --     referenced in an inbound security group rule.
    --
    -- -   @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound
    --     security group rule.
    --
    -- -   @ip-permission.prefix-list-id@ - The ID of a prefix list from which
    --     a security group rule allows inbound access.
    --
    -- -   @ip-permission.protocol@ - The IP protocol for an inbound security
    --     group rule (@tcp@ | @udp@ | @icmp@, a protocol number, or -1 for all
    --     protocols).
    --
    -- -   @ip-permission.to-port@ - For an inbound rule, the end of port range
    --     for the TCP and UDP protocols, or an ICMP code.
    --
    -- -   @ip-permission.user-id@ - The ID of an Amazon Web Services account
    --     that has been referenced in an inbound security group rule.
    --
    -- -   @owner-id@ - The Amazon Web Services account ID of the owner of the
    --     security group.
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
    -- -   @vpc-id@ - The ID of the VPC specified when the security group was
    --     created.
    filters :: Prelude.Maybe [Filter],
    -- | The IDs of the security groups. Required for security groups in a
    -- nondefault VPC.
    --
    -- Default: Describes all of your security groups.
    groupIds :: Prelude.Maybe [Prelude.Text],
    -- | [EC2-Classic and default VPC only] The names of the security groups. You
    -- can specify either the security group name or the security group ID. For
    -- security groups in a nondefault VPC, use the @group-name@ filter to
    -- describe security groups by name.
    --
    -- Default: Describes all of your security groups.
    groupNames :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. This value can be between 5 and 1000. If this parameter is not
    -- specified, then all items are returned. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeSecurityGroups_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeSecurityGroups_filters' - The filters. If using multiple filters for rules, the results include
-- security groups for which any combination of rules - not necessarily a
-- single rule - match all filters.
--
-- -   @description@ - The description of the security group.
--
-- -   @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound
--     security group rule.
--
-- -   @egress.ip-permission.from-port@ - For an outbound rule, the start
--     of port range for the TCP and UDP protocols, or an ICMP type number.
--
-- -   @egress.ip-permission.group-id@ - The ID of a security group that
--     has been referenced in an outbound security group rule.
--
-- -   @egress.ip-permission.group-name@ - The name of a security group
--     that is referenced in an outbound security group rule.
--
-- -   @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an
--     outbound security group rule.
--
-- -   @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to
--     which a security group rule allows outbound access.
--
-- -   @egress.ip-permission.protocol@ - The IP protocol for an outbound
--     security group rule (@tcp@ | @udp@ | @icmp@, a protocol number, or
--     -1 for all protocols).
--
-- -   @egress.ip-permission.to-port@ - For an outbound rule, the end of
--     port range for the TCP and UDP protocols, or an ICMP code.
--
-- -   @egress.ip-permission.user-id@ - The ID of an Amazon Web Services
--     account that has been referenced in an outbound security group rule.
--
-- -   @group-id@ - The ID of the security group.
--
-- -   @group-name@ - The name of the security group.
--
-- -   @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security
--     group rule.
--
-- -   @ip-permission.from-port@ - For an inbound rule, the start of port
--     range for the TCP and UDP protocols, or an ICMP type number.
--
-- -   @ip-permission.group-id@ - The ID of a security group that has been
--     referenced in an inbound security group rule.
--
-- -   @ip-permission.group-name@ - The name of a security group that is
--     referenced in an inbound security group rule.
--
-- -   @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound
--     security group rule.
--
-- -   @ip-permission.prefix-list-id@ - The ID of a prefix list from which
--     a security group rule allows inbound access.
--
-- -   @ip-permission.protocol@ - The IP protocol for an inbound security
--     group rule (@tcp@ | @udp@ | @icmp@, a protocol number, or -1 for all
--     protocols).
--
-- -   @ip-permission.to-port@ - For an inbound rule, the end of port range
--     for the TCP and UDP protocols, or an ICMP code.
--
-- -   @ip-permission.user-id@ - The ID of an Amazon Web Services account
--     that has been referenced in an inbound security group rule.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the owner of the
--     security group.
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
-- -   @vpc-id@ - The ID of the VPC specified when the security group was
--     created.
--
-- 'groupIds', 'describeSecurityGroups_groupIds' - The IDs of the security groups. Required for security groups in a
-- nondefault VPC.
--
-- Default: Describes all of your security groups.
--
-- 'groupNames', 'describeSecurityGroups_groupNames' - [EC2-Classic and default VPC only] The names of the security groups. You
-- can specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, use the @group-name@ filter to
-- describe security groups by name.
--
-- Default: Describes all of your security groups.
--
-- 'maxResults', 'describeSecurityGroups_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. This value can be between 5 and 1000. If this parameter is not
-- specified, then all items are returned. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeSecurityGroups_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
newDescribeSecurityGroups ::
  DescribeSecurityGroups
newDescribeSecurityGroups =
  DescribeSecurityGroups'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      groupIds = Prelude.Nothing,
      groupNames = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeSecurityGroups_dryRun :: Lens.Lens' DescribeSecurityGroups (Prelude.Maybe Prelude.Bool)
describeSecurityGroups_dryRun = Lens.lens (\DescribeSecurityGroups' {dryRun} -> dryRun) (\s@DescribeSecurityGroups' {} a -> s {dryRun = a} :: DescribeSecurityGroups)

-- | The filters. If using multiple filters for rules, the results include
-- security groups for which any combination of rules - not necessarily a
-- single rule - match all filters.
--
-- -   @description@ - The description of the security group.
--
-- -   @egress.ip-permission.cidr@ - An IPv4 CIDR block for an outbound
--     security group rule.
--
-- -   @egress.ip-permission.from-port@ - For an outbound rule, the start
--     of port range for the TCP and UDP protocols, or an ICMP type number.
--
-- -   @egress.ip-permission.group-id@ - The ID of a security group that
--     has been referenced in an outbound security group rule.
--
-- -   @egress.ip-permission.group-name@ - The name of a security group
--     that is referenced in an outbound security group rule.
--
-- -   @egress.ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an
--     outbound security group rule.
--
-- -   @egress.ip-permission.prefix-list-id@ - The ID of a prefix list to
--     which a security group rule allows outbound access.
--
-- -   @egress.ip-permission.protocol@ - The IP protocol for an outbound
--     security group rule (@tcp@ | @udp@ | @icmp@, a protocol number, or
--     -1 for all protocols).
--
-- -   @egress.ip-permission.to-port@ - For an outbound rule, the end of
--     port range for the TCP and UDP protocols, or an ICMP code.
--
-- -   @egress.ip-permission.user-id@ - The ID of an Amazon Web Services
--     account that has been referenced in an outbound security group rule.
--
-- -   @group-id@ - The ID of the security group.
--
-- -   @group-name@ - The name of the security group.
--
-- -   @ip-permission.cidr@ - An IPv4 CIDR block for an inbound security
--     group rule.
--
-- -   @ip-permission.from-port@ - For an inbound rule, the start of port
--     range for the TCP and UDP protocols, or an ICMP type number.
--
-- -   @ip-permission.group-id@ - The ID of a security group that has been
--     referenced in an inbound security group rule.
--
-- -   @ip-permission.group-name@ - The name of a security group that is
--     referenced in an inbound security group rule.
--
-- -   @ip-permission.ipv6-cidr@ - An IPv6 CIDR block for an inbound
--     security group rule.
--
-- -   @ip-permission.prefix-list-id@ - The ID of a prefix list from which
--     a security group rule allows inbound access.
--
-- -   @ip-permission.protocol@ - The IP protocol for an inbound security
--     group rule (@tcp@ | @udp@ | @icmp@, a protocol number, or -1 for all
--     protocols).
--
-- -   @ip-permission.to-port@ - For an inbound rule, the end of port range
--     for the TCP and UDP protocols, or an ICMP code.
--
-- -   @ip-permission.user-id@ - The ID of an Amazon Web Services account
--     that has been referenced in an inbound security group rule.
--
-- -   @owner-id@ - The Amazon Web Services account ID of the owner of the
--     security group.
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
-- -   @vpc-id@ - The ID of the VPC specified when the security group was
--     created.
describeSecurityGroups_filters :: Lens.Lens' DescribeSecurityGroups (Prelude.Maybe [Filter])
describeSecurityGroups_filters = Lens.lens (\DescribeSecurityGroups' {filters} -> filters) (\s@DescribeSecurityGroups' {} a -> s {filters = a} :: DescribeSecurityGroups) Prelude.. Lens.mapping Lens.coerced

-- | The IDs of the security groups. Required for security groups in a
-- nondefault VPC.
--
-- Default: Describes all of your security groups.
describeSecurityGroups_groupIds :: Lens.Lens' DescribeSecurityGroups (Prelude.Maybe [Prelude.Text])
describeSecurityGroups_groupIds = Lens.lens (\DescribeSecurityGroups' {groupIds} -> groupIds) (\s@DescribeSecurityGroups' {} a -> s {groupIds = a} :: DescribeSecurityGroups) Prelude.. Lens.mapping Lens.coerced

-- | [EC2-Classic and default VPC only] The names of the security groups. You
-- can specify either the security group name or the security group ID. For
-- security groups in a nondefault VPC, use the @group-name@ filter to
-- describe security groups by name.
--
-- Default: Describes all of your security groups.
describeSecurityGroups_groupNames :: Lens.Lens' DescribeSecurityGroups (Prelude.Maybe [Prelude.Text])
describeSecurityGroups_groupNames = Lens.lens (\DescribeSecurityGroups' {groupNames} -> groupNames) (\s@DescribeSecurityGroups' {} a -> s {groupNames = a} :: DescribeSecurityGroups) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. This value can be between 5 and 1000. If this parameter is not
-- specified, then all items are returned. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeSecurityGroups_maxResults :: Lens.Lens' DescribeSecurityGroups (Prelude.Maybe Prelude.Natural)
describeSecurityGroups_maxResults = Lens.lens (\DescribeSecurityGroups' {maxResults} -> maxResults) (\s@DescribeSecurityGroups' {} a -> s {maxResults = a} :: DescribeSecurityGroups)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeSecurityGroups_nextToken :: Lens.Lens' DescribeSecurityGroups (Prelude.Maybe Prelude.Text)
describeSecurityGroups_nextToken = Lens.lens (\DescribeSecurityGroups' {nextToken} -> nextToken) (\s@DescribeSecurityGroups' {} a -> s {nextToken = a} :: DescribeSecurityGroups)

instance Core.AWSPager DescribeSecurityGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeSecurityGroupsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeSecurityGroupsResponse_securityGroups
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeSecurityGroups_nextToken
          Lens..~ rs
          Lens.^? describeSecurityGroupsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeSecurityGroups where
  type
    AWSResponse DescribeSecurityGroups =
      DescribeSecurityGroupsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeSecurityGroupsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "securityGroupInfo"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeSecurityGroups where
  hashWithSalt _salt DescribeSecurityGroups' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` groupIds
      `Prelude.hashWithSalt` groupNames
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeSecurityGroups where
  rnf DescribeSecurityGroups' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf groupIds
      `Prelude.seq` Prelude.rnf groupNames
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders DescribeSecurityGroups where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSecurityGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSecurityGroups where
  toQuery DescribeSecurityGroups' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSecurityGroups" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        Data.toQuery
          (Data.toQueryList "GroupId" Prelude.<$> groupIds),
        Data.toQuery
          ( Data.toQueryList "GroupName"
              Prelude.<$> groupNames
          ),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newDescribeSecurityGroupsResponse' smart constructor.
data DescribeSecurityGroupsResponse = DescribeSecurityGroupsResponse'
  { -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the security groups.
    securityGroups :: Prelude.Maybe [SecurityGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSecurityGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeSecurityGroupsResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'securityGroups', 'describeSecurityGroupsResponse_securityGroups' - Information about the security groups.
--
-- 'httpStatus', 'describeSecurityGroupsResponse_httpStatus' - The response's http status code.
newDescribeSecurityGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSecurityGroupsResponse
newDescribeSecurityGroupsResponse pHttpStatus_ =
  DescribeSecurityGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeSecurityGroupsResponse_nextToken :: Lens.Lens' DescribeSecurityGroupsResponse (Prelude.Maybe Prelude.Text)
describeSecurityGroupsResponse_nextToken = Lens.lens (\DescribeSecurityGroupsResponse' {nextToken} -> nextToken) (\s@DescribeSecurityGroupsResponse' {} a -> s {nextToken = a} :: DescribeSecurityGroupsResponse)

-- | Information about the security groups.
describeSecurityGroupsResponse_securityGroups :: Lens.Lens' DescribeSecurityGroupsResponse (Prelude.Maybe [SecurityGroup])
describeSecurityGroupsResponse_securityGroups = Lens.lens (\DescribeSecurityGroupsResponse' {securityGroups} -> securityGroups) (\s@DescribeSecurityGroupsResponse' {} a -> s {securityGroups = a} :: DescribeSecurityGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeSecurityGroupsResponse_httpStatus :: Lens.Lens' DescribeSecurityGroupsResponse Prelude.Int
describeSecurityGroupsResponse_httpStatus = Lens.lens (\DescribeSecurityGroupsResponse' {httpStatus} -> httpStatus) (\s@DescribeSecurityGroupsResponse' {} a -> s {httpStatus = a} :: DescribeSecurityGroupsResponse)

instance
  Prelude.NFData
    DescribeSecurityGroupsResponse
  where
  rnf DescribeSecurityGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf httpStatus
