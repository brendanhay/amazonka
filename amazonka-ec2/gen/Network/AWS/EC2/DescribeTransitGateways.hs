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
-- Module      : Network.AWS.EC2.DescribeTransitGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more transit gateways. By default, all transit gateways
-- are described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTransitGateways
  ( -- * Creating a Request
    DescribeTransitGateways (..),
    newDescribeTransitGateways,

    -- * Request Lenses
    describeTransitGateways_nextToken,
    describeTransitGateways_dryRun,
    describeTransitGateways_maxResults,
    describeTransitGateways_transitGatewayIds,
    describeTransitGateways_filters,

    -- * Destructuring the Response
    DescribeTransitGatewaysResponse (..),
    newDescribeTransitGatewaysResponse,

    -- * Response Lenses
    describeTransitGatewaysResponse_nextToken,
    describeTransitGatewaysResponse_transitGateways,
    describeTransitGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeTransitGateways' smart constructor.
data DescribeTransitGateways = DescribeTransitGateways'
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
    -- | The IDs of the transit gateways.
    transitGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters. The possible values are:
    --
    -- -   @options.propagation-default-route-table-id@ - The ID of the default
    --     propagation route table.
    --
    -- -   @options.amazon-side-asn@ - The private ASN for the Amazon side of a
    --     BGP session.
    --
    -- -   @options.association-default-route-table-id@ - The ID of the default
    --     association route table.
    --
    -- -   @options.auto-accept-shared-attachments@ - Indicates whether there
    --     is automatic acceptance of attachment requests (@enable@ |
    --     @disable@).
    --
    -- -   @options.default-route-table-association@ - Indicates whether
    --     resource attachments are automatically associated with the default
    --     association route table (@enable@ | @disable@).
    --
    -- -   @options.default-route-table-propagation@ - Indicates whether
    --     resource attachments automatically propagate routes to the default
    --     propagation route table (@enable@ | @disable@).
    --
    -- -   @options.dns-support@ - Indicates whether DNS support is enabled
    --     (@enable@ | @disable@).
    --
    -- -   @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath
    --     Protocol support is enabled (@enable@ | @disable@).
    --
    -- -   @owner-id@ - The ID of the AWS account that owns the transit
    --     gateway.
    --
    -- -   @state@ - The state of the transit gateway (@available@ | @deleted@
    --     | @deleting@ | @modifying@ | @pending@).
    --
    -- -   @transit-gateway-id@ - The ID of the transit gateway.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransitGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGateways_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeTransitGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeTransitGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'transitGatewayIds', 'describeTransitGateways_transitGatewayIds' - The IDs of the transit gateways.
--
-- 'filters', 'describeTransitGateways_filters' - One or more filters. The possible values are:
--
-- -   @options.propagation-default-route-table-id@ - The ID of the default
--     propagation route table.
--
-- -   @options.amazon-side-asn@ - The private ASN for the Amazon side of a
--     BGP session.
--
-- -   @options.association-default-route-table-id@ - The ID of the default
--     association route table.
--
-- -   @options.auto-accept-shared-attachments@ - Indicates whether there
--     is automatic acceptance of attachment requests (@enable@ |
--     @disable@).
--
-- -   @options.default-route-table-association@ - Indicates whether
--     resource attachments are automatically associated with the default
--     association route table (@enable@ | @disable@).
--
-- -   @options.default-route-table-propagation@ - Indicates whether
--     resource attachments automatically propagate routes to the default
--     propagation route table (@enable@ | @disable@).
--
-- -   @options.dns-support@ - Indicates whether DNS support is enabled
--     (@enable@ | @disable@).
--
-- -   @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath
--     Protocol support is enabled (@enable@ | @disable@).
--
-- -   @owner-id@ - The ID of the AWS account that owns the transit
--     gateway.
--
-- -   @state@ - The state of the transit gateway (@available@ | @deleted@
--     | @deleting@ | @modifying@ | @pending@).
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
newDescribeTransitGateways ::
  DescribeTransitGateways
newDescribeTransitGateways =
  DescribeTransitGateways'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      transitGatewayIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeTransitGateways_nextToken :: Lens.Lens' DescribeTransitGateways (Core.Maybe Core.Text)
describeTransitGateways_nextToken = Lens.lens (\DescribeTransitGateways' {nextToken} -> nextToken) (\s@DescribeTransitGateways' {} a -> s {nextToken = a} :: DescribeTransitGateways)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeTransitGateways_dryRun :: Lens.Lens' DescribeTransitGateways (Core.Maybe Core.Bool)
describeTransitGateways_dryRun = Lens.lens (\DescribeTransitGateways' {dryRun} -> dryRun) (\s@DescribeTransitGateways' {} a -> s {dryRun = a} :: DescribeTransitGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeTransitGateways_maxResults :: Lens.Lens' DescribeTransitGateways (Core.Maybe Core.Natural)
describeTransitGateways_maxResults = Lens.lens (\DescribeTransitGateways' {maxResults} -> maxResults) (\s@DescribeTransitGateways' {} a -> s {maxResults = a} :: DescribeTransitGateways)

-- | The IDs of the transit gateways.
describeTransitGateways_transitGatewayIds :: Lens.Lens' DescribeTransitGateways (Core.Maybe [Core.Text])
describeTransitGateways_transitGatewayIds = Lens.lens (\DescribeTransitGateways' {transitGatewayIds} -> transitGatewayIds) (\s@DescribeTransitGateways' {} a -> s {transitGatewayIds = a} :: DescribeTransitGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. The possible values are:
--
-- -   @options.propagation-default-route-table-id@ - The ID of the default
--     propagation route table.
--
-- -   @options.amazon-side-asn@ - The private ASN for the Amazon side of a
--     BGP session.
--
-- -   @options.association-default-route-table-id@ - The ID of the default
--     association route table.
--
-- -   @options.auto-accept-shared-attachments@ - Indicates whether there
--     is automatic acceptance of attachment requests (@enable@ |
--     @disable@).
--
-- -   @options.default-route-table-association@ - Indicates whether
--     resource attachments are automatically associated with the default
--     association route table (@enable@ | @disable@).
--
-- -   @options.default-route-table-propagation@ - Indicates whether
--     resource attachments automatically propagate routes to the default
--     propagation route table (@enable@ | @disable@).
--
-- -   @options.dns-support@ - Indicates whether DNS support is enabled
--     (@enable@ | @disable@).
--
-- -   @options.vpn-ecmp-support@ - Indicates whether Equal Cost Multipath
--     Protocol support is enabled (@enable@ | @disable@).
--
-- -   @owner-id@ - The ID of the AWS account that owns the transit
--     gateway.
--
-- -   @state@ - The state of the transit gateway (@available@ | @deleted@
--     | @deleting@ | @modifying@ | @pending@).
--
-- -   @transit-gateway-id@ - The ID of the transit gateway.
describeTransitGateways_filters :: Lens.Lens' DescribeTransitGateways (Core.Maybe [Filter])
describeTransitGateways_filters = Lens.lens (\DescribeTransitGateways' {filters} -> filters) (\s@DescribeTransitGateways' {} a -> s {filters = a} :: DescribeTransitGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeTransitGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewaysResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeTransitGatewaysResponse_transitGateways
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeTransitGateways_nextToken
          Lens..~ rs
          Lens.^? describeTransitGatewaysResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeTransitGateways where
  type
    AWSResponse DescribeTransitGateways =
      DescribeTransitGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeTransitGatewaysResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "transitGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeTransitGateways

instance Core.NFData DescribeTransitGateways

instance Core.ToHeaders DescribeTransitGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeTransitGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeTransitGateways where
  toQuery DescribeTransitGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeTransitGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "TransitGatewayIds"
              Core.<$> transitGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeTransitGatewaysResponse' smart constructor.
data DescribeTransitGatewaysResponse = DescribeTransitGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the transit gateways.
    transitGateways :: Core.Maybe [TransitGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeTransitGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeTransitGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGateways', 'describeTransitGatewaysResponse_transitGateways' - Information about the transit gateways.
--
-- 'httpStatus', 'describeTransitGatewaysResponse_httpStatus' - The response's http status code.
newDescribeTransitGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeTransitGatewaysResponse
newDescribeTransitGatewaysResponse pHttpStatus_ =
  DescribeTransitGatewaysResponse'
    { nextToken =
        Core.Nothing,
      transitGateways = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeTransitGatewaysResponse_nextToken :: Lens.Lens' DescribeTransitGatewaysResponse (Core.Maybe Core.Text)
describeTransitGatewaysResponse_nextToken = Lens.lens (\DescribeTransitGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeTransitGatewaysResponse' {} a -> s {nextToken = a} :: DescribeTransitGatewaysResponse)

-- | Information about the transit gateways.
describeTransitGatewaysResponse_transitGateways :: Lens.Lens' DescribeTransitGatewaysResponse (Core.Maybe [TransitGateway])
describeTransitGatewaysResponse_transitGateways = Lens.lens (\DescribeTransitGatewaysResponse' {transitGateways} -> transitGateways) (\s@DescribeTransitGatewaysResponse' {} a -> s {transitGateways = a} :: DescribeTransitGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeTransitGatewaysResponse_httpStatus :: Lens.Lens' DescribeTransitGatewaysResponse Core.Int
describeTransitGatewaysResponse_httpStatus = Lens.lens (\DescribeTransitGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeTransitGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeTransitGatewaysResponse)

instance Core.NFData DescribeTransitGatewaysResponse
