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
-- Module      : Network.AWS.EC2.DescribeNatGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your NAT gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeNatGateways
  ( -- * Creating a Request
    DescribeNatGateways (..),
    newDescribeNatGateways,

    -- * Request Lenses
    describeNatGateways_nextToken,
    describeNatGateways_dryRun,
    describeNatGateways_maxResults,
    describeNatGateways_natGatewayIds,
    describeNatGateways_filter,

    -- * Destructuring the Response
    DescribeNatGatewaysResponse (..),
    newDescribeNatGatewaysResponse,

    -- * Response Lenses
    describeNatGatewaysResponse_nextToken,
    describeNatGatewaysResponse_natGateways,
    describeNatGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNatGateways' smart constructor.
data DescribeNatGateways = DescribeNatGateways'
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
    -- | One or more NAT gateway IDs.
    natGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @nat-gateway-id@ - The ID of the NAT gateway.
    --
    -- -   @state@ - The state of the NAT gateway (@pending@ | @failed@ |
    --     @available@ | @deleting@ | @deleted@).
    --
    -- -   @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
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
    -- -   @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
    filter' :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNatGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNatGateways_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeNatGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeNatGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'natGatewayIds', 'describeNatGateways_natGatewayIds' - One or more NAT gateway IDs.
--
-- 'filter'', 'describeNatGateways_filter' - One or more filters.
--
-- -   @nat-gateway-id@ - The ID of the NAT gateway.
--
-- -   @state@ - The state of the NAT gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
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
-- -   @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
newDescribeNatGateways ::
  DescribeNatGateways
newDescribeNatGateways =
  DescribeNatGateways'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      natGatewayIds = Core.Nothing,
      filter' = Core.Nothing
    }

-- | The token for the next page of results.
describeNatGateways_nextToken :: Lens.Lens' DescribeNatGateways (Core.Maybe Core.Text)
describeNatGateways_nextToken = Lens.lens (\DescribeNatGateways' {nextToken} -> nextToken) (\s@DescribeNatGateways' {} a -> s {nextToken = a} :: DescribeNatGateways)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeNatGateways_dryRun :: Lens.Lens' DescribeNatGateways (Core.Maybe Core.Bool)
describeNatGateways_dryRun = Lens.lens (\DescribeNatGateways' {dryRun} -> dryRun) (\s@DescribeNatGateways' {} a -> s {dryRun = a} :: DescribeNatGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeNatGateways_maxResults :: Lens.Lens' DescribeNatGateways (Core.Maybe Core.Natural)
describeNatGateways_maxResults = Lens.lens (\DescribeNatGateways' {maxResults} -> maxResults) (\s@DescribeNatGateways' {} a -> s {maxResults = a} :: DescribeNatGateways)

-- | One or more NAT gateway IDs.
describeNatGateways_natGatewayIds :: Lens.Lens' DescribeNatGateways (Core.Maybe [Core.Text])
describeNatGateways_natGatewayIds = Lens.lens (\DescribeNatGateways' {natGatewayIds} -> natGatewayIds) (\s@DescribeNatGateways' {} a -> s {natGatewayIds = a} :: DescribeNatGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @nat-gateway-id@ - The ID of the NAT gateway.
--
-- -   @state@ - The state of the NAT gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @subnet-id@ - The ID of the subnet in which the NAT gateway resides.
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
-- -   @vpc-id@ - The ID of the VPC in which the NAT gateway resides.
describeNatGateways_filter :: Lens.Lens' DescribeNatGateways (Core.Maybe [Filter])
describeNatGateways_filter = Lens.lens (\DescribeNatGateways' {filter'} -> filter') (\s@DescribeNatGateways' {} a -> s {filter' = a} :: DescribeNatGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeNatGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeNatGatewaysResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeNatGatewaysResponse_natGateways
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeNatGateways_nextToken
          Lens..~ rs
          Lens.^? describeNatGatewaysResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeNatGateways where
  type
    AWSResponse DescribeNatGateways =
      DescribeNatGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeNatGatewaysResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "natGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeNatGateways

instance Core.NFData DescribeNatGateways

instance Core.ToHeaders DescribeNatGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeNatGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeNatGateways where
  toQuery DescribeNatGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeNatGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "NatGatewayId"
              Core.<$> natGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filter')
      ]

-- | /See:/ 'newDescribeNatGatewaysResponse' smart constructor.
data DescribeNatGatewaysResponse = DescribeNatGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the NAT gateways.
    natGateways :: Core.Maybe [NatGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNatGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeNatGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'natGateways', 'describeNatGatewaysResponse_natGateways' - Information about the NAT gateways.
--
-- 'httpStatus', 'describeNatGatewaysResponse_httpStatus' - The response's http status code.
newDescribeNatGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNatGatewaysResponse
newDescribeNatGatewaysResponse pHttpStatus_ =
  DescribeNatGatewaysResponse'
    { nextToken =
        Core.Nothing,
      natGateways = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeNatGatewaysResponse_nextToken :: Lens.Lens' DescribeNatGatewaysResponse (Core.Maybe Core.Text)
describeNatGatewaysResponse_nextToken = Lens.lens (\DescribeNatGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeNatGatewaysResponse' {} a -> s {nextToken = a} :: DescribeNatGatewaysResponse)

-- | Information about the NAT gateways.
describeNatGatewaysResponse_natGateways :: Lens.Lens' DescribeNatGatewaysResponse (Core.Maybe [NatGateway])
describeNatGatewaysResponse_natGateways = Lens.lens (\DescribeNatGatewaysResponse' {natGateways} -> natGateways) (\s@DescribeNatGatewaysResponse' {} a -> s {natGateways = a} :: DescribeNatGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeNatGatewaysResponse_httpStatus :: Lens.Lens' DescribeNatGatewaysResponse Core.Int
describeNatGatewaysResponse_httpStatus = Lens.lens (\DescribeNatGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeNatGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeNatGatewaysResponse)

instance Core.NFData DescribeNatGatewaysResponse
