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
-- Module      : Network.AWS.EC2.DescribeInternetGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInternetGateways
  ( -- * Creating a Request
    DescribeInternetGateways (..),
    newDescribeInternetGateways,

    -- * Request Lenses
    describeInternetGateways_nextToken,
    describeInternetGateways_dryRun,
    describeInternetGateways_maxResults,
    describeInternetGateways_internetGatewayIds,
    describeInternetGateways_filters,

    -- * Destructuring the Response
    DescribeInternetGatewaysResponse (..),
    newDescribeInternetGatewaysResponse,

    -- * Response Lenses
    describeInternetGatewaysResponse_nextToken,
    describeInternetGatewaysResponse_internetGateways,
    describeInternetGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInternetGateways' smart constructor.
data DescribeInternetGateways = DescribeInternetGateways'
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
    -- | One or more internet gateway IDs.
    --
    -- Default: Describes all your internet gateways.
    internetGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    --
    -- -   @attachment.state@ - The current state of the attachment between the
    --     gateway and the VPC (@available@). Present only if a VPC is
    --     attached.
    --
    -- -   @attachment.vpc-id@ - The ID of an attached VPC.
    --
    -- -   @internet-gateway-id@ - The ID of the Internet gateway.
    --
    -- -   @owner-id@ - The ID of the AWS account that owns the internet
    --     gateway.
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
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInternetGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInternetGateways_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeInternetGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeInternetGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'internetGatewayIds', 'describeInternetGateways_internetGatewayIds' - One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
--
-- 'filters', 'describeInternetGateways_filters' - One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@available@). Present only if a VPC is
--     attached.
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @internet-gateway-id@ - The ID of the Internet gateway.
--
-- -   @owner-id@ - The ID of the AWS account that owns the internet
--     gateway.
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
newDescribeInternetGateways ::
  DescribeInternetGateways
newDescribeInternetGateways =
  DescribeInternetGateways'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      internetGatewayIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeInternetGateways_nextToken :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Text)
describeInternetGateways_nextToken = Lens.lens (\DescribeInternetGateways' {nextToken} -> nextToken) (\s@DescribeInternetGateways' {} a -> s {nextToken = a} :: DescribeInternetGateways)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInternetGateways_dryRun :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Bool)
describeInternetGateways_dryRun = Lens.lens (\DescribeInternetGateways' {dryRun} -> dryRun) (\s@DescribeInternetGateways' {} a -> s {dryRun = a} :: DescribeInternetGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeInternetGateways_maxResults :: Lens.Lens' DescribeInternetGateways (Core.Maybe Core.Natural)
describeInternetGateways_maxResults = Lens.lens (\DescribeInternetGateways' {maxResults} -> maxResults) (\s@DescribeInternetGateways' {} a -> s {maxResults = a} :: DescribeInternetGateways)

-- | One or more internet gateway IDs.
--
-- Default: Describes all your internet gateways.
describeInternetGateways_internetGatewayIds :: Lens.Lens' DescribeInternetGateways (Core.Maybe [Core.Text])
describeInternetGateways_internetGatewayIds = Lens.lens (\DescribeInternetGateways' {internetGatewayIds} -> internetGatewayIds) (\s@DescribeInternetGateways' {} a -> s {internetGatewayIds = a} :: DescribeInternetGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
--
-- -   @attachment.state@ - The current state of the attachment between the
--     gateway and the VPC (@available@). Present only if a VPC is
--     attached.
--
-- -   @attachment.vpc-id@ - The ID of an attached VPC.
--
-- -   @internet-gateway-id@ - The ID of the Internet gateway.
--
-- -   @owner-id@ - The ID of the AWS account that owns the internet
--     gateway.
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
describeInternetGateways_filters :: Lens.Lens' DescribeInternetGateways (Core.Maybe [Filter])
describeInternetGateways_filters = Lens.lens (\DescribeInternetGateways' {filters} -> filters) (\s@DescribeInternetGateways' {} a -> s {filters = a} :: DescribeInternetGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeInternetGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeInternetGatewaysResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeInternetGatewaysResponse_internetGateways
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeInternetGateways_nextToken
          Lens..~ rs
          Lens.^? describeInternetGatewaysResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeInternetGateways where
  type
    AWSResponse DescribeInternetGateways =
      DescribeInternetGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInternetGatewaysResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "internetGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeInternetGateways

instance Core.NFData DescribeInternetGateways

instance Core.ToHeaders DescribeInternetGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeInternetGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeInternetGateways where
  toQuery DescribeInternetGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeInternetGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "InternetGatewayId"
              Core.<$> internetGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeInternetGatewaysResponse' smart constructor.
data DescribeInternetGatewaysResponse = DescribeInternetGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about one or more internet gateways.
    internetGateways :: Core.Maybe [InternetGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInternetGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInternetGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'internetGateways', 'describeInternetGatewaysResponse_internetGateways' - Information about one or more internet gateways.
--
-- 'httpStatus', 'describeInternetGatewaysResponse_httpStatus' - The response's http status code.
newDescribeInternetGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInternetGatewaysResponse
newDescribeInternetGatewaysResponse pHttpStatus_ =
  DescribeInternetGatewaysResponse'
    { nextToken =
        Core.Nothing,
      internetGateways = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeInternetGatewaysResponse_nextToken :: Lens.Lens' DescribeInternetGatewaysResponse (Core.Maybe Core.Text)
describeInternetGatewaysResponse_nextToken = Lens.lens (\DescribeInternetGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeInternetGatewaysResponse' {} a -> s {nextToken = a} :: DescribeInternetGatewaysResponse)

-- | Information about one or more internet gateways.
describeInternetGatewaysResponse_internetGateways :: Lens.Lens' DescribeInternetGatewaysResponse (Core.Maybe [InternetGateway])
describeInternetGatewaysResponse_internetGateways = Lens.lens (\DescribeInternetGatewaysResponse' {internetGateways} -> internetGateways) (\s@DescribeInternetGatewaysResponse' {} a -> s {internetGateways = a} :: DescribeInternetGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeInternetGatewaysResponse_httpStatus :: Lens.Lens' DescribeInternetGatewaysResponse Core.Int
describeInternetGatewaysResponse_httpStatus = Lens.lens (\DescribeInternetGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeInternetGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeInternetGatewaysResponse)

instance Core.NFData DescribeInternetGatewaysResponse
