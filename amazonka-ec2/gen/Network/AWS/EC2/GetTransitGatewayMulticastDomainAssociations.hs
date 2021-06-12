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
-- Module      : Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the associations for the transit gateway
-- multicast domain.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayMulticastDomainAssociations
  ( -- * Creating a Request
    GetTransitGatewayMulticastDomainAssociations (..),
    newGetTransitGatewayMulticastDomainAssociations,

    -- * Request Lenses
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    getTransitGatewayMulticastDomainAssociations_maxResults,
    getTransitGatewayMulticastDomainAssociations_filters,

    -- * Destructuring the Response
    GetTransitGatewayMulticastDomainAssociationsResponse (..),
    newGetTransitGatewayMulticastDomainAssociationsResponse,

    -- * Response Lenses
    getTransitGatewayMulticastDomainAssociationsResponse_nextToken,
    getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations,
    getTransitGatewayMulticastDomainAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { -- | The token for the next page of results.
    nextToken :: Core.Maybe Core.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Core.Maybe Core.Natural,
    -- | One or more filters. The possible values are:
    --
    -- -   @resource-id@ - The ID of the resource.
    --
    -- -   @resource-type@ - The type of resource. The valid value is: @vpc@.
    --
    -- -   @state@ - The state of the subnet association. Valid values are
    --     @associated@ | @associating@ | @disassociated@ | @disassociating@.
    --
    -- -   @subnet-id@ - The ID of the subnet.
    --
    -- -   @transit-gateway-attachment-id@ - The id of the transit gateway
    --     attachment.
    filters :: Core.Maybe [Filter]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTransitGatewayMulticastDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayMulticastDomainAssociations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getTransitGatewayMulticastDomainAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'transitGatewayMulticastDomainId', 'getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'maxResults', 'getTransitGatewayMulticastDomainAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'getTransitGatewayMulticastDomainAssociations_filters' - One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The type of resource. The valid value is: @vpc@.
--
-- -   @state@ - The state of the subnet association. Valid values are
--     @associated@ | @associating@ | @disassociated@ | @disassociating@.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @transit-gateway-attachment-id@ - The id of the transit gateway
--     attachment.
newGetTransitGatewayMulticastDomainAssociations ::
  GetTransitGatewayMulticastDomainAssociations
newGetTransitGatewayMulticastDomainAssociations =
  GetTransitGatewayMulticastDomainAssociations'
    { nextToken =
        Core.Nothing,
      dryRun = Core.Nothing,
      transitGatewayMulticastDomainId =
        Core.Nothing,
      maxResults = Core.Nothing,
      filters = Core.Nothing
    }

-- | The token for the next page of results.
getTransitGatewayMulticastDomainAssociations_nextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
getTransitGatewayMulticastDomainAssociations_nextToken = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {nextToken} -> nextToken) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Bool)
getTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | The ID of the transit gateway multicast domain.
getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Text)
getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayMulticastDomainAssociations_maxResults :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe Core.Natural)
getTransitGatewayMulticastDomainAssociations_maxResults = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {maxResults} -> maxResults) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {maxResults = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | One or more filters. The possible values are:
--
-- -   @resource-id@ - The ID of the resource.
--
-- -   @resource-type@ - The type of resource. The valid value is: @vpc@.
--
-- -   @state@ - The state of the subnet association. Valid values are
--     @associated@ | @associating@ | @disassociated@ | @disassociating@.
--
-- -   @subnet-id@ - The ID of the subnet.
--
-- -   @transit-gateway-attachment-id@ - The id of the transit gateway
--     attachment.
getTransitGatewayMulticastDomainAssociations_filters :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Core.Maybe [Filter])
getTransitGatewayMulticastDomainAssociations_filters = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {filters} -> filters) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {filters = a} :: GetTransitGatewayMulticastDomainAssociations) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSPager
    GetTransitGatewayMulticastDomainAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTransitGatewayMulticastDomainAssociations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayMulticastDomainAssociations
  where
  type
    AWSResponse
      GetTransitGatewayMulticastDomainAssociations =
      GetTransitGatewayMulticastDomainAssociationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayMulticastDomainAssociationsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "multicastDomainAssociations"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetTransitGatewayMulticastDomainAssociations

instance
  Core.NFData
    GetTransitGatewayMulticastDomainAssociations

instance
  Core.ToHeaders
    GetTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetTransitGatewayMulticastDomainAssociations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetTransitGatewayMulticastDomainAssociations
  where
  toQuery
    GetTransitGatewayMulticastDomainAssociations' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "GetTransitGatewayMulticastDomainAssociations" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "NextToken" Core.=: nextToken,
          "DryRun" Core.=: dryRun,
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          "MaxResults" Core.=: maxResults,
          Core.toQuery
            (Core.toQueryList "Filter" Core.<$> filters)
        ]

-- | /See:/ 'newGetTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the multicast domain associations.
    multicastDomainAssociations :: Core.Maybe [TransitGatewayMulticastDomainAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTransitGatewayMulticastDomainAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayMulticastDomainAssociationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'multicastDomainAssociations', 'getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations' - Information about the multicast domain associations.
--
-- 'httpStatus', 'getTransitGatewayMulticastDomainAssociationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayMulticastDomainAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTransitGatewayMulticastDomainAssociationsResponse
newGetTransitGatewayMulticastDomainAssociationsResponse
  pHttpStatus_ =
    GetTransitGatewayMulticastDomainAssociationsResponse'
      { nextToken =
          Core.Nothing,
        multicastDomainAssociations =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayMulticastDomainAssociationsResponse_nextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe Core.Text)
getTransitGatewayMulticastDomainAssociationsResponse_nextToken = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)

-- | Information about the multicast domain associations.
getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Core.Maybe [TransitGatewayMulticastDomainAssociation])
getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {multicastDomainAssociations} -> multicastDomainAssociations) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {multicastDomainAssociations = a} :: GetTransitGatewayMulticastDomainAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse Core.Int
getTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)

instance
  Core.NFData
    GetTransitGatewayMulticastDomainAssociationsResponse
