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
-- Module      : Amazonka.EC2.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your VPC peering connections.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeVpcPeeringConnections
  ( -- * Creating a Request
    DescribeVpcPeeringConnections (..),
    newDescribeVpcPeeringConnections,

    -- * Request Lenses
    describeVpcPeeringConnections_dryRun,
    describeVpcPeeringConnections_filters,
    describeVpcPeeringConnections_maxResults,
    describeVpcPeeringConnections_nextToken,
    describeVpcPeeringConnections_vpcPeeringConnectionIds,

    -- * Destructuring the Response
    DescribeVpcPeeringConnectionsResponse (..),
    newDescribeVpcPeeringConnectionsResponse,

    -- * Response Lenses
    describeVpcPeeringConnectionsResponse_nextToken,
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeVpcPeeringConnections' smart constructor.
data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more filters.
    --
    -- -   @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter
    --     VPC.
    --
    -- -   @accepter-vpc-info.owner-id@ - The ID of the Amazon Web Services
    --     account that owns the accepter VPC.
    --
    -- -   @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.
    --
    -- -   @expiration-time@ - The expiration date and time for the VPC peering
    --     connection.
    --
    -- -   @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the
    --     requester\'s VPC.
    --
    -- -   @requester-vpc-info.owner-id@ - The ID of the Amazon Web Services
    --     account that owns the requester VPC.
    --
    -- -   @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
    --
    -- -   @status-code@ - The status of the VPC peering connection
    --     (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ |
    --     @active@ | @deleting@ | @deleted@ | @rejected@).
    --
    -- -   @status-message@ - A message that provides more information about
    --     the status of the VPC peering connection, if applicable.
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
    -- -   @vpc-peering-connection-id@ - The ID of the VPC peering connection.
    filters :: Prelude.Maybe [Filter],
    -- | The maximum number of items to return for this request. To get the next
    -- page of items, make another request with the token returned in the
    -- output. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned from a previous paginated request. Pagination
    -- continues from the end of the items returned by the previous request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | One or more VPC peering connection IDs.
    --
    -- Default: Describes all your VPC peering connections.
    vpcPeeringConnectionIds :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeVpcPeeringConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'filters', 'describeVpcPeeringConnections_filters' - One or more filters.
--
-- -   @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter
--     VPC.
--
-- -   @accepter-vpc-info.owner-id@ - The ID of the Amazon Web Services
--     account that owns the accepter VPC.
--
-- -   @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.
--
-- -   @expiration-time@ - The expiration date and time for the VPC peering
--     connection.
--
-- -   @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the
--     requester\'s VPC.
--
-- -   @requester-vpc-info.owner-id@ - The ID of the Amazon Web Services
--     account that owns the requester VPC.
--
-- -   @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
--
-- -   @status-code@ - The status of the VPC peering connection
--     (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ |
--     @active@ | @deleting@ | @deleted@ | @rejected@).
--
-- -   @status-message@ - A message that provides more information about
--     the status of the VPC peering connection, if applicable.
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
-- -   @vpc-peering-connection-id@ - The ID of the VPC peering connection.
--
-- 'maxResults', 'describeVpcPeeringConnections_maxResults' - The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
--
-- 'nextToken', 'describeVpcPeeringConnections_nextToken' - The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
--
-- 'vpcPeeringConnectionIds', 'describeVpcPeeringConnections_vpcPeeringConnectionIds' - One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
newDescribeVpcPeeringConnections ::
  DescribeVpcPeeringConnections
newDescribeVpcPeeringConnections =
  DescribeVpcPeeringConnections'
    { dryRun =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      vpcPeeringConnectionIds = Prelude.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeVpcPeeringConnections_dryRun :: Lens.Lens' DescribeVpcPeeringConnections (Prelude.Maybe Prelude.Bool)
describeVpcPeeringConnections_dryRun = Lens.lens (\DescribeVpcPeeringConnections' {dryRun} -> dryRun) (\s@DescribeVpcPeeringConnections' {} a -> s {dryRun = a} :: DescribeVpcPeeringConnections)

-- | One or more filters.
--
-- -   @accepter-vpc-info.cidr-block@ - The IPv4 CIDR block of the accepter
--     VPC.
--
-- -   @accepter-vpc-info.owner-id@ - The ID of the Amazon Web Services
--     account that owns the accepter VPC.
--
-- -   @accepter-vpc-info.vpc-id@ - The ID of the accepter VPC.
--
-- -   @expiration-time@ - The expiration date and time for the VPC peering
--     connection.
--
-- -   @requester-vpc-info.cidr-block@ - The IPv4 CIDR block of the
--     requester\'s VPC.
--
-- -   @requester-vpc-info.owner-id@ - The ID of the Amazon Web Services
--     account that owns the requester VPC.
--
-- -   @requester-vpc-info.vpc-id@ - The ID of the requester VPC.
--
-- -   @status-code@ - The status of the VPC peering connection
--     (@pending-acceptance@ | @failed@ | @expired@ | @provisioning@ |
--     @active@ | @deleting@ | @deleted@ | @rejected@).
--
-- -   @status-message@ - A message that provides more information about
--     the status of the VPC peering connection, if applicable.
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
-- -   @vpc-peering-connection-id@ - The ID of the VPC peering connection.
describeVpcPeeringConnections_filters :: Lens.Lens' DescribeVpcPeeringConnections (Prelude.Maybe [Filter])
describeVpcPeeringConnections_filters = Lens.lens (\DescribeVpcPeeringConnections' {filters} -> filters) (\s@DescribeVpcPeeringConnections' {} a -> s {filters = a} :: DescribeVpcPeeringConnections) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return for this request. To get the next
-- page of items, make another request with the token returned in the
-- output. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Query-Requests.html#api-pagination Pagination>.
describeVpcPeeringConnections_maxResults :: Lens.Lens' DescribeVpcPeeringConnections (Prelude.Maybe Prelude.Natural)
describeVpcPeeringConnections_maxResults = Lens.lens (\DescribeVpcPeeringConnections' {maxResults} -> maxResults) (\s@DescribeVpcPeeringConnections' {} a -> s {maxResults = a} :: DescribeVpcPeeringConnections)

-- | The token returned from a previous paginated request. Pagination
-- continues from the end of the items returned by the previous request.
describeVpcPeeringConnections_nextToken :: Lens.Lens' DescribeVpcPeeringConnections (Prelude.Maybe Prelude.Text)
describeVpcPeeringConnections_nextToken = Lens.lens (\DescribeVpcPeeringConnections' {nextToken} -> nextToken) (\s@DescribeVpcPeeringConnections' {} a -> s {nextToken = a} :: DescribeVpcPeeringConnections)

-- | One or more VPC peering connection IDs.
--
-- Default: Describes all your VPC peering connections.
describeVpcPeeringConnections_vpcPeeringConnectionIds :: Lens.Lens' DescribeVpcPeeringConnections (Prelude.Maybe [Prelude.Text])
describeVpcPeeringConnections_vpcPeeringConnectionIds = Lens.lens (\DescribeVpcPeeringConnections' {vpcPeeringConnectionIds} -> vpcPeeringConnectionIds) (\s@DescribeVpcPeeringConnections' {} a -> s {vpcPeeringConnectionIds = a} :: DescribeVpcPeeringConnections) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSPager DescribeVpcPeeringConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVpcPeeringConnectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVpcPeeringConnectionsResponse_vpcPeeringConnections
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeVpcPeeringConnections_nextToken
          Lens..~ rs
          Lens.^? describeVpcPeeringConnectionsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeVpcPeeringConnections
  where
  type
    AWSResponse DescribeVpcPeeringConnections =
      DescribeVpcPeeringConnectionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeVpcPeeringConnectionsResponse'
            Prelude.<$> (x Data..@? "nextToken")
            Prelude.<*> ( x
                            Data..@? "vpcPeeringConnectionSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcPeeringConnections
  where
  hashWithSalt _salt DescribeVpcPeeringConnections' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vpcPeeringConnectionIds

instance Prelude.NFData DescribeVpcPeeringConnections where
  rnf DescribeVpcPeeringConnections' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionIds

instance Data.ToHeaders DescribeVpcPeeringConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeVpcPeeringConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeVpcPeeringConnections where
  toQuery DescribeVpcPeeringConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "DescribeVpcPeeringConnections" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        Data.toQuery
          (Data.toQueryList "Filter" Prelude.<$> filters),
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        Data.toQuery
          ( Data.toQueryList "VpcPeeringConnectionId"
              Prelude.<$> vpcPeeringConnectionIds
          )
      ]

-- | /See:/ 'newDescribeVpcPeeringConnectionsResponse' smart constructor.
data DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse'
  { -- | The token to include in another request to get the next page of items.
    -- This value is @null@ when there are no more items to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC peering connections.
    vpcPeeringConnections :: Prelude.Maybe [VpcPeeringConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeVpcPeeringConnectionsResponse_nextToken' - The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
--
-- 'vpcPeeringConnections', 'describeVpcPeeringConnectionsResponse_vpcPeeringConnections' - Information about the VPC peering connections.
--
-- 'httpStatus', 'describeVpcPeeringConnectionsResponse_httpStatus' - The response's http status code.
newDescribeVpcPeeringConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeVpcPeeringConnectionsResponse
newDescribeVpcPeeringConnectionsResponse pHttpStatus_ =
  DescribeVpcPeeringConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      vpcPeeringConnections =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to include in another request to get the next page of items.
-- This value is @null@ when there are no more items to return.
describeVpcPeeringConnectionsResponse_nextToken :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Prelude.Maybe Prelude.Text)
describeVpcPeeringConnectionsResponse_nextToken = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {nextToken = a} :: DescribeVpcPeeringConnectionsResponse)

-- | Information about the VPC peering connections.
describeVpcPeeringConnectionsResponse_vpcPeeringConnections :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Prelude.Maybe [VpcPeeringConnection])
describeVpcPeeringConnectionsResponse_vpcPeeringConnections = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {vpcPeeringConnections} -> vpcPeeringConnections) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {vpcPeeringConnections = a} :: DescribeVpcPeeringConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpcPeeringConnectionsResponse_httpStatus :: Lens.Lens' DescribeVpcPeeringConnectionsResponse Prelude.Int
describeVpcPeeringConnectionsResponse_httpStatus = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcPeeringConnectionsResponse)

instance
  Prelude.NFData
    DescribeVpcPeeringConnectionsResponse
  where
  rnf DescribeVpcPeeringConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcPeeringConnections
      `Prelude.seq` Prelude.rnf httpStatus
