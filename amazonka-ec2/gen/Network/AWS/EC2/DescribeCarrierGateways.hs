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
-- Module      : Network.AWS.EC2.DescribeCarrierGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your carrier gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeCarrierGateways
  ( -- * Creating a Request
    DescribeCarrierGateways (..),
    newDescribeCarrierGateways,

    -- * Request Lenses
    describeCarrierGateways_nextToken,
    describeCarrierGateways_dryRun,
    describeCarrierGateways_maxResults,
    describeCarrierGateways_carrierGatewayIds,
    describeCarrierGateways_filters,

    -- * Destructuring the Response
    DescribeCarrierGatewaysResponse (..),
    newDescribeCarrierGatewaysResponse,

    -- * Response Lenses
    describeCarrierGatewaysResponse_carrierGateways,
    describeCarrierGatewaysResponse_nextToken,
    describeCarrierGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeCarrierGateways' smart constructor.
data DescribeCarrierGateways = DescribeCarrierGateways'
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
    -- | One or more carrier gateway IDs.
    carrierGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @carrier-gateway-id@ - The ID of the carrier gateway.
    --
    -- -   @state@ - The state of the carrier gateway (@pending@ | @failed@ |
    --     @available@ | @deleting@ | @deleted@).
    --
    -- -   @owner-id@ - The AWS account ID of the owner of the carrier gateway.
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
    -- -   @vpc-id@ - The ID of the VPC associated with the carrier gateway.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCarrierGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeCarrierGateways_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeCarrierGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeCarrierGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'carrierGatewayIds', 'describeCarrierGateways_carrierGatewayIds' - One or more carrier gateway IDs.
--
-- 'filters', 'describeCarrierGateways_filters' - One or more filters.
--
-- -   @carrier-gateway-id@ - The ID of the carrier gateway.
--
-- -   @state@ - The state of the carrier gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @owner-id@ - The AWS account ID of the owner of the carrier gateway.
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
-- -   @vpc-id@ - The ID of the VPC associated with the carrier gateway.
newDescribeCarrierGateways ::
  DescribeCarrierGateways
newDescribeCarrierGateways =
  DescribeCarrierGateways'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      carrierGatewayIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeCarrierGateways_nextToken :: Lens.Lens' DescribeCarrierGateways (Core.Maybe Core.Text)
describeCarrierGateways_nextToken = Lens.lens (\DescribeCarrierGateways' {nextToken} -> nextToken) (\s@DescribeCarrierGateways' {} a -> s {nextToken = a} :: DescribeCarrierGateways)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeCarrierGateways_dryRun :: Lens.Lens' DescribeCarrierGateways (Core.Maybe Core.Bool)
describeCarrierGateways_dryRun = Lens.lens (\DescribeCarrierGateways' {dryRun} -> dryRun) (\s@DescribeCarrierGateways' {} a -> s {dryRun = a} :: DescribeCarrierGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeCarrierGateways_maxResults :: Lens.Lens' DescribeCarrierGateways (Core.Maybe Core.Natural)
describeCarrierGateways_maxResults = Lens.lens (\DescribeCarrierGateways' {maxResults} -> maxResults) (\s@DescribeCarrierGateways' {} a -> s {maxResults = a} :: DescribeCarrierGateways)

-- | One or more carrier gateway IDs.
describeCarrierGateways_carrierGatewayIds :: Lens.Lens' DescribeCarrierGateways (Core.Maybe [Core.Text])
describeCarrierGateways_carrierGatewayIds = Lens.lens (\DescribeCarrierGateways' {carrierGatewayIds} -> carrierGatewayIds) (\s@DescribeCarrierGateways' {} a -> s {carrierGatewayIds = a} :: DescribeCarrierGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @carrier-gateway-id@ - The ID of the carrier gateway.
--
-- -   @state@ - The state of the carrier gateway (@pending@ | @failed@ |
--     @available@ | @deleting@ | @deleted@).
--
-- -   @owner-id@ - The AWS account ID of the owner of the carrier gateway.
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
-- -   @vpc-id@ - The ID of the VPC associated with the carrier gateway.
describeCarrierGateways_filters :: Lens.Lens' DescribeCarrierGateways (Core.Maybe [Filter])
describeCarrierGateways_filters = Lens.lens (\DescribeCarrierGateways' {filters} -> filters) (\s@DescribeCarrierGateways' {} a -> s {filters = a} :: DescribeCarrierGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeCarrierGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeCarrierGatewaysResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeCarrierGatewaysResponse_carrierGateways
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeCarrierGateways_nextToken
          Lens..~ rs
          Lens.^? describeCarrierGatewaysResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeCarrierGateways where
  type
    AWSResponse DescribeCarrierGateways =
      DescribeCarrierGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeCarrierGatewaysResponse'
            Core.<$> ( x Core..@? "carrierGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (x Core..@? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeCarrierGateways

instance Core.NFData DescribeCarrierGateways

instance Core.ToHeaders DescribeCarrierGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeCarrierGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCarrierGateways where
  toQuery DescribeCarrierGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeCarrierGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "CarrierGatewayId"
              Core.<$> carrierGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeCarrierGatewaysResponse' smart constructor.
data DescribeCarrierGatewaysResponse = DescribeCarrierGatewaysResponse'
  { -- | Information about the carrier gateway.
    carrierGateways :: Core.Maybe [CarrierGateway],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeCarrierGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGateways', 'describeCarrierGatewaysResponse_carrierGateways' - Information about the carrier gateway.
--
-- 'nextToken', 'describeCarrierGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeCarrierGatewaysResponse_httpStatus' - The response's http status code.
newDescribeCarrierGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeCarrierGatewaysResponse
newDescribeCarrierGatewaysResponse pHttpStatus_ =
  DescribeCarrierGatewaysResponse'
    { carrierGateways =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the carrier gateway.
describeCarrierGatewaysResponse_carrierGateways :: Lens.Lens' DescribeCarrierGatewaysResponse (Core.Maybe [CarrierGateway])
describeCarrierGatewaysResponse_carrierGateways = Lens.lens (\DescribeCarrierGatewaysResponse' {carrierGateways} -> carrierGateways) (\s@DescribeCarrierGatewaysResponse' {} a -> s {carrierGateways = a} :: DescribeCarrierGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeCarrierGatewaysResponse_nextToken :: Lens.Lens' DescribeCarrierGatewaysResponse (Core.Maybe Core.Text)
describeCarrierGatewaysResponse_nextToken = Lens.lens (\DescribeCarrierGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeCarrierGatewaysResponse' {} a -> s {nextToken = a} :: DescribeCarrierGatewaysResponse)

-- | The response's http status code.
describeCarrierGatewaysResponse_httpStatus :: Lens.Lens' DescribeCarrierGatewaysResponse Core.Int
describeCarrierGatewaysResponse_httpStatus = Lens.lens (\DescribeCarrierGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeCarrierGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeCarrierGatewaysResponse)

instance Core.NFData DescribeCarrierGatewaysResponse
