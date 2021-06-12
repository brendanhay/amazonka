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
-- Module      : Network.AWS.EC2.DescribeEgressOnlyInternetGateways
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your egress-only internet gateways.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeEgressOnlyInternetGateways
  ( -- * Creating a Request
    DescribeEgressOnlyInternetGateways (..),
    newDescribeEgressOnlyInternetGateways,

    -- * Request Lenses
    describeEgressOnlyInternetGateways_nextToken,
    describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds,
    describeEgressOnlyInternetGateways_dryRun,
    describeEgressOnlyInternetGateways_maxResults,
    describeEgressOnlyInternetGateways_filters,

    -- * Destructuring the Response
    DescribeEgressOnlyInternetGatewaysResponse (..),
    newDescribeEgressOnlyInternetGatewaysResponse,

    -- * Response Lenses
    describeEgressOnlyInternetGatewaysResponse_nextToken,
    describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways,
    describeEgressOnlyInternetGatewaysResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEgressOnlyInternetGateways' smart constructor.
data DescribeEgressOnlyInternetGateways = DescribeEgressOnlyInternetGateways'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | One or more egress-only internet gateway IDs.
    egressOnlyInternetGatewayIds :: Core.Maybe [Core.Text],
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
-- Create a value of 'DescribeEgressOnlyInternetGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEgressOnlyInternetGateways_nextToken' - The token for the next page of results.
--
-- 'egressOnlyInternetGatewayIds', 'describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds' - One or more egress-only internet gateway IDs.
--
-- 'dryRun', 'describeEgressOnlyInternetGateways_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeEgressOnlyInternetGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'describeEgressOnlyInternetGateways_filters' - One or more filters.
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
newDescribeEgressOnlyInternetGateways ::
  DescribeEgressOnlyInternetGateways
newDescribeEgressOnlyInternetGateways =
  DescribeEgressOnlyInternetGateways'
    { nextToken =
        Core.Nothing,
      egressOnlyInternetGatewayIds =
        Core.Nothing,
      dryRun = Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
describeEgressOnlyInternetGateways_nextToken :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe Core.Text)
describeEgressOnlyInternetGateways_nextToken = Lens.lens (\DescribeEgressOnlyInternetGateways' {nextToken} -> nextToken) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {nextToken = a} :: DescribeEgressOnlyInternetGateways)

-- | One or more egress-only internet gateway IDs.
describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe [Core.Text])
describeEgressOnlyInternetGateways_egressOnlyInternetGatewayIds = Lens.lens (\DescribeEgressOnlyInternetGateways' {egressOnlyInternetGatewayIds} -> egressOnlyInternetGatewayIds) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {egressOnlyInternetGatewayIds = a} :: DescribeEgressOnlyInternetGateways) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeEgressOnlyInternetGateways_dryRun :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe Core.Bool)
describeEgressOnlyInternetGateways_dryRun = Lens.lens (\DescribeEgressOnlyInternetGateways' {dryRun} -> dryRun) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {dryRun = a} :: DescribeEgressOnlyInternetGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
describeEgressOnlyInternetGateways_maxResults :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe Core.Natural)
describeEgressOnlyInternetGateways_maxResults = Lens.lens (\DescribeEgressOnlyInternetGateways' {maxResults} -> maxResults) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {maxResults = a} :: DescribeEgressOnlyInternetGateways)

-- | One or more filters.
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
describeEgressOnlyInternetGateways_filters :: Lens.Lens' DescribeEgressOnlyInternetGateways (Core.Maybe [Filter])
describeEgressOnlyInternetGateways_filters = Lens.lens (\DescribeEgressOnlyInternetGateways' {filters} -> filters) (\s@DescribeEgressOnlyInternetGateways' {} a -> s {filters = a} :: DescribeEgressOnlyInternetGateways) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    DescribeEgressOnlyInternetGateways
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeEgressOnlyInternetGatewaysResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeEgressOnlyInternetGateways_nextToken
          Lens..~ rs
          Lens.^? describeEgressOnlyInternetGatewaysResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeEgressOnlyInternetGateways
  where
  type
    AWSResponse DescribeEgressOnlyInternetGateways =
      DescribeEgressOnlyInternetGatewaysResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeEgressOnlyInternetGatewaysResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "egressOnlyInternetGatewaySet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeEgressOnlyInternetGateways

instance
  Core.NFData
    DescribeEgressOnlyInternetGateways

instance
  Core.ToHeaders
    DescribeEgressOnlyInternetGateways
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeEgressOnlyInternetGateways
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeEgressOnlyInternetGateways
  where
  toQuery DescribeEgressOnlyInternetGateways' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeEgressOnlyInternetGateways" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        Core.toQuery
          ( Core.toQueryList "EgressOnlyInternetGatewayId"
              Core.<$> egressOnlyInternetGatewayIds
          ),
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters)
      ]

-- | /See:/ 'newDescribeEgressOnlyInternetGatewaysResponse' smart constructor.
data DescribeEgressOnlyInternetGatewaysResponse = DescribeEgressOnlyInternetGatewaysResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the egress-only internet gateways.
    egressOnlyInternetGateways :: Core.Maybe [EgressOnlyInternetGateway],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEgressOnlyInternetGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeEgressOnlyInternetGatewaysResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'egressOnlyInternetGateways', 'describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways' - Information about the egress-only internet gateways.
--
-- 'httpStatus', 'describeEgressOnlyInternetGatewaysResponse_httpStatus' - The response's http status code.
newDescribeEgressOnlyInternetGatewaysResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEgressOnlyInternetGatewaysResponse
newDescribeEgressOnlyInternetGatewaysResponse
  pHttpStatus_ =
    DescribeEgressOnlyInternetGatewaysResponse'
      { nextToken =
          Core.Nothing,
        egressOnlyInternetGateways =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeEgressOnlyInternetGatewaysResponse_nextToken :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Core.Maybe Core.Text)
describeEgressOnlyInternetGatewaysResponse_nextToken = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {nextToken = a} :: DescribeEgressOnlyInternetGatewaysResponse)

-- | Information about the egress-only internet gateways.
describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse (Core.Maybe [EgressOnlyInternetGateway])
describeEgressOnlyInternetGatewaysResponse_egressOnlyInternetGateways = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {egressOnlyInternetGateways} -> egressOnlyInternetGateways) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {egressOnlyInternetGateways = a} :: DescribeEgressOnlyInternetGatewaysResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEgressOnlyInternetGatewaysResponse_httpStatus :: Lens.Lens' DescribeEgressOnlyInternetGatewaysResponse Core.Int
describeEgressOnlyInternetGatewaysResponse_httpStatus = Lens.lens (\DescribeEgressOnlyInternetGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeEgressOnlyInternetGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeEgressOnlyInternetGatewaysResponse)

instance
  Core.NFData
    DescribeEgressOnlyInternetGatewaysResponse
