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
-- Module      : Network.AWS.EC2.DescribeClientVpnTargetNetworks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the target networks associated with the specified Client VPN
-- endpoint.
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeClientVpnTargetNetworks
  ( -- * Creating a Request
    DescribeClientVpnTargetNetworks (..),
    newDescribeClientVpnTargetNetworks,

    -- * Request Lenses
    describeClientVpnTargetNetworks_nextToken,
    describeClientVpnTargetNetworks_dryRun,
    describeClientVpnTargetNetworks_maxResults,
    describeClientVpnTargetNetworks_associationIds,
    describeClientVpnTargetNetworks_filters,
    describeClientVpnTargetNetworks_clientVpnEndpointId,

    -- * Destructuring the Response
    DescribeClientVpnTargetNetworksResponse (..),
    newDescribeClientVpnTargetNetworksResponse,

    -- * Response Lenses
    describeClientVpnTargetNetworksResponse_nextToken,
    describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks,
    describeClientVpnTargetNetworksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeClientVpnTargetNetworks' smart constructor.
data DescribeClientVpnTargetNetworks = DescribeClientVpnTargetNetworks'
  { -- | The token to retrieve the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results can be seen by sending another request with
    -- the nextToken value.
    maxResults :: Core.Maybe Core.Natural,
    -- | The IDs of the target network associations.
    associationIds :: Core.Maybe [Core.Text],
    -- | One or more filters. Filter names and values are case-sensitive.
    --
    -- -   @association-id@ - The ID of the association.
    --
    -- -   @target-network-id@ - The ID of the subnet specified as the target
    --     network.
    --
    -- -   @vpc-id@ - The ID of the VPC in which the target network is located.
    filters :: Core.Maybe [Filter],
    -- | The ID of the Client VPN endpoint.
    clientVpnEndpointId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnTargetNetworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnTargetNetworks_nextToken' - The token to retrieve the next page of results.
--
-- 'dryRun', 'describeClientVpnTargetNetworks_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeClientVpnTargetNetworks_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
--
-- 'associationIds', 'describeClientVpnTargetNetworks_associationIds' - The IDs of the target network associations.
--
-- 'filters', 'describeClientVpnTargetNetworks_filters' - One or more filters. Filter names and values are case-sensitive.
--
-- -   @association-id@ - The ID of the association.
--
-- -   @target-network-id@ - The ID of the subnet specified as the target
--     network.
--
-- -   @vpc-id@ - The ID of the VPC in which the target network is located.
--
-- 'clientVpnEndpointId', 'describeClientVpnTargetNetworks_clientVpnEndpointId' - The ID of the Client VPN endpoint.
newDescribeClientVpnTargetNetworks ::
  -- | 'clientVpnEndpointId'
  Core.Text ->
  DescribeClientVpnTargetNetworks
newDescribeClientVpnTargetNetworks
  pClientVpnEndpointId_ =
    DescribeClientVpnTargetNetworks'
      { nextToken =
          Core.Nothing,
        dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        associationIds = Core.Nothing,
        filters = Core.Nothing,
        clientVpnEndpointId =
          pClientVpnEndpointId_
      }

-- | The token to retrieve the next page of results.
describeClientVpnTargetNetworks_nextToken :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe Core.Text)
describeClientVpnTargetNetworks_nextToken = Lens.lens (\DescribeClientVpnTargetNetworks' {nextToken} -> nextToken) (\s@DescribeClientVpnTargetNetworks' {} a -> s {nextToken = a} :: DescribeClientVpnTargetNetworks)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeClientVpnTargetNetworks_dryRun :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe Core.Bool)
describeClientVpnTargetNetworks_dryRun = Lens.lens (\DescribeClientVpnTargetNetworks' {dryRun} -> dryRun) (\s@DescribeClientVpnTargetNetworks' {} a -> s {dryRun = a} :: DescribeClientVpnTargetNetworks)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the nextToken value.
describeClientVpnTargetNetworks_maxResults :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe Core.Natural)
describeClientVpnTargetNetworks_maxResults = Lens.lens (\DescribeClientVpnTargetNetworks' {maxResults} -> maxResults) (\s@DescribeClientVpnTargetNetworks' {} a -> s {maxResults = a} :: DescribeClientVpnTargetNetworks)

-- | The IDs of the target network associations.
describeClientVpnTargetNetworks_associationIds :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe [Core.Text])
describeClientVpnTargetNetworks_associationIds = Lens.lens (\DescribeClientVpnTargetNetworks' {associationIds} -> associationIds) (\s@DescribeClientVpnTargetNetworks' {} a -> s {associationIds = a} :: DescribeClientVpnTargetNetworks) Core.. Lens.mapping Lens._Coerce

-- | One or more filters. Filter names and values are case-sensitive.
--
-- -   @association-id@ - The ID of the association.
--
-- -   @target-network-id@ - The ID of the subnet specified as the target
--     network.
--
-- -   @vpc-id@ - The ID of the VPC in which the target network is located.
describeClientVpnTargetNetworks_filters :: Lens.Lens' DescribeClientVpnTargetNetworks (Core.Maybe [Filter])
describeClientVpnTargetNetworks_filters = Lens.lens (\DescribeClientVpnTargetNetworks' {filters} -> filters) (\s@DescribeClientVpnTargetNetworks' {} a -> s {filters = a} :: DescribeClientVpnTargetNetworks) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Client VPN endpoint.
describeClientVpnTargetNetworks_clientVpnEndpointId :: Lens.Lens' DescribeClientVpnTargetNetworks Core.Text
describeClientVpnTargetNetworks_clientVpnEndpointId = Lens.lens (\DescribeClientVpnTargetNetworks' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@DescribeClientVpnTargetNetworks' {} a -> s {clientVpnEndpointId = a} :: DescribeClientVpnTargetNetworks)

instance
  Core.AWSPager
    DescribeClientVpnTargetNetworks
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeClientVpnTargetNetworksResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeClientVpnTargetNetworks_nextToken
          Lens..~ rs
          Lens.^? describeClientVpnTargetNetworksResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeClientVpnTargetNetworks
  where
  type
    AWSResponse DescribeClientVpnTargetNetworks =
      DescribeClientVpnTargetNetworksResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeClientVpnTargetNetworksResponse'
            Core.<$> (x Core..@? "nextToken")
            Core.<*> ( x Core..@? "clientVpnTargetNetworks"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeClientVpnTargetNetworks

instance Core.NFData DescribeClientVpnTargetNetworks

instance
  Core.ToHeaders
    DescribeClientVpnTargetNetworks
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeClientVpnTargetNetworks where
  toPath = Core.const "/"

instance Core.ToQuery DescribeClientVpnTargetNetworks where
  toQuery DescribeClientVpnTargetNetworks' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DescribeClientVpnTargetNetworks" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          ( Core.toQueryList "AssociationIds"
              Core.<$> associationIds
          ),
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "ClientVpnEndpointId" Core.=: clientVpnEndpointId
      ]

-- | /See:/ 'newDescribeClientVpnTargetNetworksResponse' smart constructor.
data DescribeClientVpnTargetNetworksResponse = DescribeClientVpnTargetNetworksResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the associated target networks.
    clientVpnTargetNetworks :: Core.Maybe [TargetNetwork],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeClientVpnTargetNetworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeClientVpnTargetNetworksResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'clientVpnTargetNetworks', 'describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks' - Information about the associated target networks.
--
-- 'httpStatus', 'describeClientVpnTargetNetworksResponse_httpStatus' - The response's http status code.
newDescribeClientVpnTargetNetworksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeClientVpnTargetNetworksResponse
newDescribeClientVpnTargetNetworksResponse
  pHttpStatus_ =
    DescribeClientVpnTargetNetworksResponse'
      { nextToken =
          Core.Nothing,
        clientVpnTargetNetworks =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeClientVpnTargetNetworksResponse_nextToken :: Lens.Lens' DescribeClientVpnTargetNetworksResponse (Core.Maybe Core.Text)
describeClientVpnTargetNetworksResponse_nextToken = Lens.lens (\DescribeClientVpnTargetNetworksResponse' {nextToken} -> nextToken) (\s@DescribeClientVpnTargetNetworksResponse' {} a -> s {nextToken = a} :: DescribeClientVpnTargetNetworksResponse)

-- | Information about the associated target networks.
describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks :: Lens.Lens' DescribeClientVpnTargetNetworksResponse (Core.Maybe [TargetNetwork])
describeClientVpnTargetNetworksResponse_clientVpnTargetNetworks = Lens.lens (\DescribeClientVpnTargetNetworksResponse' {clientVpnTargetNetworks} -> clientVpnTargetNetworks) (\s@DescribeClientVpnTargetNetworksResponse' {} a -> s {clientVpnTargetNetworks = a} :: DescribeClientVpnTargetNetworksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeClientVpnTargetNetworksResponse_httpStatus :: Lens.Lens' DescribeClientVpnTargetNetworksResponse Core.Int
describeClientVpnTargetNetworksResponse_httpStatus = Lens.lens (\DescribeClientVpnTargetNetworksResponse' {httpStatus} -> httpStatus) (\s@DescribeClientVpnTargetNetworksResponse' {} a -> s {httpStatus = a} :: DescribeClientVpnTargetNetworksResponse)

instance
  Core.NFData
    DescribeClientVpnTargetNetworksResponse
