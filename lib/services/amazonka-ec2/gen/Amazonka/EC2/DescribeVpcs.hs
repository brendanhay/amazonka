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
-- Module      : Amazonka.EC2.DescribeVpcs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPCs.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcs
  ( -- * Creating a Request
    DescribeVpcs (..),
    newDescribeVpcs,

    -- * Request Lenses
    describeVpcs_dryRun,
    describeVpcs_filters,
    describeVpcs_maxResults,
    describeVpcs_nextToken,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcs' smart constructor.
data DescribeVpcs = DescribeVpcs'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
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
    -- -   @is-default@ - Indicates whether the VPC is the default VPC.
    --
    -- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
    --     VPC.
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
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more VPC IDs.
    --
    -- Default: Describes all your VPCs.
    vpcIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcs_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
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
-- -   @is-default@ - Indicates whether the VPC is the default VPC.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     VPC.
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
-- 'maxResults', 'describeVpcs_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeVpcs_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
--
-- 'vpcIds', 'describeVpcs_vpcIds' - One or more VPC IDs.
--
-- Default: Describes all your VPCs.
newDescribeVpcs ::
  DescribeVpcs
newDescribeVpcs =
  DescribeVpcs'
    { dryRun = Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcs_dryRun :: Lens.Lens' DescribeVpcs (Prelude.Maybe Prelude.Bool)
describeVpcs_dryRun = Lens.lens (\DescribeVpcs' {dryRun} -> dryRun) (\s@DescribeVpcs' {} a -> s {dryRun = a} :: DescribeVpcs)

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
-- -   @is-default@ - Indicates whether the VPC is the default VPC.
--
-- -   @owner-id@ - The ID of the Amazon Web Services account that owns the
--     VPC.
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
describeVpcs_filters :: Lens.Lens' DescribeVpcs (Prelude.Maybe [Filter])
describeVpcs_filters = Lens.lens (\DescribeVpcs' {filters} -> filters) (\s@DescribeVpcs' {} a -> s {filters = a} :: DescribeVpcs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeVpcs_maxResults :: Lens.Lens' DescribeVpcs (Prelude.Maybe Prelude.Natural)
describeVpcs_maxResults = Lens.lens (\DescribeVpcs' {maxResults} -> maxResults) (\s@DescribeVpcs' {} a -> s {maxResults = a} :: DescribeVpcs)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeVpcs_nextToken :: Lens.Lens' DescribeVpcs (Prelude.Maybe Prelude.Text)
describeVpcs_nextToken = Lens.lens (\DescribeVpcs' {nextToken} -> nextToken) (\s@DescribeVpcs' {} a -> s {nextToken = a} :: DescribeVpcs)

-- | One or more VPC IDs.
--
-- Default: Describes all your VPCs.
describeVpcs_vpcIds :: Lens.Lens' DescribeVpcs (Prelude.Maybe [Prelude.Text])
describeVpcs_vpcIds = Lens.lens (\DescribeVpcs' {vpcIds} -> vpcIds) (\s@DescribeVpcs' {} a -> s {vpcIds = a} :: DescribeVpcs) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeVpcs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcsResponse_vpcs
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVpcs_nextToken
          Lens..~ rs
          Lens.^? describeVpcsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest DescribeVpcs where
  type AWSResponse DescribeVpcs = DescribeVpcsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "vpcSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeVpcs where
  hashWithSalt _salt DescribeVpcs' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcIds

instance Prelude.NFData DescribeVpcs where
  rnf DescribeVpcs' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcIds

instance Data.ToHeaders DescribeVpcs where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVpcs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpcs where
  toQuery DescribeVpcs' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeVpcs" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          (Data.toQueryList "VpcId" Prelude.<$> vpcIds)
      ]

-- | /See:/ 'newDescribeVpcsResponse' smart constructor.
data DescribeVpcsResponse = DescribeVpcsResponse'
  { -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about one or more VPCs.
    vpcs :: Prelude.Maybe [Vpc],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcsResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'vpcs', 'describeVpcsResponse_vpcs' - Information about one or more VPCs.
--
-- 'httpStatus', 'describeVpcsResponse_httpStatus' - The response's http status code.
newDescribeVpcsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcsResponse
newDescribeVpcsResponse pHttpStatus_ =
  DescribeVpcsResponse'
    { nextToken = Prelude.Nothing,
      vpcs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeVpcsResponse_nextToken :: Lens.Lens' DescribeVpcsResponse (Prelude.Maybe Prelude.Text)
describeVpcsResponse_nextToken = Lens.lens (\DescribeVpcsResponse' {nextToken} -> nextToken) (\s@DescribeVpcsResponse' {} a -> s {nextToken = a} :: DescribeVpcsResponse)

-- | Information about one or more VPCs.
describeVpcsResponse_vpcs :: Lens.Lens' DescribeVpcsResponse (Prelude.Maybe [Vpc])
describeVpcsResponse_vpcs = Lens.lens (\DescribeVpcsResponse' {vpcs} -> vpcs) (\s@DescribeVpcsResponse' {} a -> s {vpcs = a} :: DescribeVpcsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpcsResponse_httpStatus :: Lens.Lens' DescribeVpcsResponse Prelude.Int
describeVpcsResponse_httpStatus = Lens.lens (\DescribeVpcsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcsResponse' {} a -> s {httpStatus = a} :: DescribeVpcsResponse)

instance Prelude.NFData DescribeVpcsResponse where
  rnf DescribeVpcsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcs
      `Prelude.seq` Prelude.rnf httpStatus
