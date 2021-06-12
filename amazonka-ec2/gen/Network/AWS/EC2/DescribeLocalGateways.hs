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
-- Module      : Network.AWS.EC2.DescribeLocalGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more local gateways. By default, all local gateways are
-- described. Alternatively, you can filter the results.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeLocalGateways
  ( -- * Creating a Request
    DescribeLocalGateways (..),
    newDescribeLocalGateways,

    -- * Request Lenses
    describeLocalGateways_nextToken,
    describeLocalGateways_dryRun,
    describeLocalGateways_maxResults,
    describeLocalGateways_localGatewayIds,
    describeLocalGateways_filters,

    -- * Destructuring the Response
    DescribeLocalGatewaysResponse (..),
    newDescribeLocalGatewaysResponse,

    -- * Response Lenses
    describeLocalGatewaysResponse_nextToken,
    describeLocalGatewaysResponse_localGateways,
    describeLocalGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLocalGateways' smart constructor.
data DescribeLocalGateways = DescribeLocalGateways'
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
    -- -   @local-gateway-id@ - The ID of a local gateway.
    --
    -- -   @local-gateway-route-table-id@ - The ID of the local gateway route
    --     table.
    --
    -- -   @local-gateway-route-table-virtual-interface-group-association-id@ -
    --     The ID of the association.
    --
    -- -   @local-gateway-route-table-virtual-interface-group-id@ - The ID of
    --     the virtual interface group.
    --
    -- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
    --
    -- -   @state@ - The state of the association.
    localGatewayIds :: Core.Maybe [Core.Text],
    -- | One or more filters.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLocalGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGateways_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeLocalGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeLocalGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'localGatewayIds', 'describeLocalGateways_localGatewayIds' - One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-virtual-interface-group-association-id@ -
--     The ID of the association.
--
-- -   @local-gateway-route-table-virtual-interface-group-id@ - The ID of
--     the virtual interface group.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @state@ - The state of the association.
--
-- 'filters', 'describeLocalGateways_filters' - One or more filters.
newDescribeLocalGateways ::
  DescribeLocalGateways
newDescribeLocalGateways =
  DescribeLocalGateways'
    { nextToken = Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      localGatewayIds = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeLocalGateways_nextToken :: Lens.Lens' DescribeLocalGateways (Core.Maybe Core.Text)
describeLocalGateways_nextToken = Lens.lens (\DescribeLocalGateways' {nextToken} -> nextToken) (\s@DescribeLocalGateways' {} a -> s {nextToken = a} :: DescribeLocalGateways)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeLocalGateways_dryRun :: Lens.Lens' DescribeLocalGateways (Core.Maybe Core.Bool)
describeLocalGateways_dryRun = Lens.lens (\DescribeLocalGateways' {dryRun} -> dryRun) (\s@DescribeLocalGateways' {} a -> s {dryRun = a} :: DescribeLocalGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeLocalGateways_maxResults :: Lens.Lens' DescribeLocalGateways (Core.Maybe Core.Natural)
describeLocalGateways_maxResults = Lens.lens (\DescribeLocalGateways' {maxResults} -> maxResults) (\s@DescribeLocalGateways' {} a -> s {maxResults = a} :: DescribeLocalGateways)

-- | One or more filters.
--
-- -   @local-gateway-id@ - The ID of a local gateway.
--
-- -   @local-gateway-route-table-id@ - The ID of the local gateway route
--     table.
--
-- -   @local-gateway-route-table-virtual-interface-group-association-id@ -
--     The ID of the association.
--
-- -   @local-gateway-route-table-virtual-interface-group-id@ - The ID of
--     the virtual interface group.
--
-- -   @outpost-arn@ - The Amazon Resource Name (ARN) of the Outpost.
--
-- -   @state@ - The state of the association.
describeLocalGateways_localGatewayIds :: Lens.Lens' DescribeLocalGateways (Core.Maybe [Core.Text])
describeLocalGateways_localGatewayIds = Lens.lens (\DescribeLocalGateways' {localGatewayIds} -> localGatewayIds) (\s@DescribeLocalGateways' {} a -> s {localGatewayIds = a} :: DescribeLocalGateways) Core.. Lens.mapping Lens._Coerce

-- | One or more filters.
describeLocalGateways_filters :: Lens.Lens' DescribeLocalGateways (Core.Maybe [Filter])
describeLocalGateways_filters = Lens.lens (\DescribeLocalGateways' {filters} -> filters) (\s@DescribeLocalGateways' {} a -> s {filters = a} :: DescribeLocalGateways) Core.. Lens.mapping Lens._Coerce

instance Core.AWSPager DescribeLocalGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewaysResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeLocalGatewaysResponse_localGateways
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeLocalGateways_nextToken
          Lens..~ rs
          Lens.^? describeLocalGatewaysResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest DescribeLocalGateways where
  type
    AWSResponse DescribeLocalGateways =
      DescribeLocalGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeLocalGatewaysResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "localGatewaySet" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLocalGateways

instance Core.NFData DescribeLocalGateways

instance Core.ToHeaders DescribeLocalGateways where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLocalGateways where
  toPath = Core.const "/"

instance Core.ToQuery DescribeLocalGateways where
  toQuery DescribeLocalGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeLocalGateways" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "LocalGatewayId"
              Core.<$> localGatewayIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeLocalGatewaysResponse' smart constructor.
data DescribeLocalGatewaysResponse = DescribeLocalGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the local gateways.
    localGateways :: Core.Maybe [LocalGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLocalGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeLocalGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'localGateways', 'describeLocalGatewaysResponse_localGateways' - Information about the local gateways.
--
-- 'httpStatus', 'describeLocalGatewaysResponse_httpStatus' - The response's http status code.
newDescribeLocalGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLocalGatewaysResponse
newDescribeLocalGatewaysResponse pHttpStatus_ =
  DescribeLocalGatewaysResponse'
    { nextToken =
        Core.Nothing,
      localGateways = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeLocalGatewaysResponse_nextToken :: Lens.Lens' DescribeLocalGatewaysResponse (Core.Maybe Core.Text)
describeLocalGatewaysResponse_nextToken = Lens.lens (\DescribeLocalGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeLocalGatewaysResponse' {} a -> s {nextToken = a} :: DescribeLocalGatewaysResponse)

-- | Information about the local gateways.
describeLocalGatewaysResponse_localGateways :: Lens.Lens' DescribeLocalGatewaysResponse (Core.Maybe [LocalGateway])
describeLocalGatewaysResponse_localGateways = Lens.lens (\DescribeLocalGatewaysResponse' {localGateways} -> localGateways) (\s@DescribeLocalGatewaysResponse' {} a -> s {localGateways = a} :: DescribeLocalGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeLocalGatewaysResponse_httpStatus :: Lens.Lens' DescribeLocalGatewaysResponse Core.Int
describeLocalGatewaysResponse_httpStatus = Lens.lens (\DescribeLocalGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeLocalGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeLocalGatewaysResponse)

instance Core.NFData DescribeLocalGatewaysResponse
