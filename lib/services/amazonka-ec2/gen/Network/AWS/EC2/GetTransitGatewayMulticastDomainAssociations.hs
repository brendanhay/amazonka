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
    getTransitGatewayMulticastDomainAssociations_filters,
    getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId,
    getTransitGatewayMulticastDomainAssociations_nextToken,
    getTransitGatewayMulticastDomainAssociations_dryRun,
    getTransitGatewayMulticastDomainAssociations_maxResults,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayMulticastDomainAssociations' smart constructor.
data GetTransitGatewayMulticastDomainAssociations = GetTransitGatewayMulticastDomainAssociations'
  { -- | One or more filters. The possible values are:
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
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayMulticastDomainAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
--
-- 'transitGatewayMulticastDomainId', 'getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'nextToken', 'getTransitGatewayMulticastDomainAssociations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getTransitGatewayMulticastDomainAssociations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getTransitGatewayMulticastDomainAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
newGetTransitGatewayMulticastDomainAssociations ::
  GetTransitGatewayMulticastDomainAssociations
newGetTransitGatewayMulticastDomainAssociations =
  GetTransitGatewayMulticastDomainAssociations'
    { filters =
        Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

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
getTransitGatewayMulticastDomainAssociations_filters :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe [Filter])
getTransitGatewayMulticastDomainAssociations_filters = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {filters} -> filters) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {filters = a} :: GetTransitGatewayMulticastDomainAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the transit gateway multicast domain.
getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
getTransitGatewayMulticastDomainAssociations_transitGatewayMulticastDomainId = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {transitGatewayMulticastDomainId = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | The token for the next page of results.
getTransitGatewayMulticastDomainAssociations_nextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Text)
getTransitGatewayMulticastDomainAssociations_nextToken = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {nextToken} -> nextToken) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayMulticastDomainAssociations_dryRun :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Bool)
getTransitGatewayMulticastDomainAssociations_dryRun = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {dryRun} -> dryRun) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {dryRun = a} :: GetTransitGatewayMulticastDomainAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayMulticastDomainAssociations_maxResults :: Lens.Lens' GetTransitGatewayMulticastDomainAssociations (Prelude.Maybe Prelude.Natural)
getTransitGatewayMulticastDomainAssociations_maxResults = Lens.lens (\GetTransitGatewayMulticastDomainAssociations' {maxResults} -> maxResults) (\s@GetTransitGatewayMulticastDomainAssociations' {} a -> s {maxResults = a} :: GetTransitGatewayMulticastDomainAssociations)

instance
  Core.AWSPager
    GetTransitGatewayMulticastDomainAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayMulticastDomainAssociations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayMulticastDomainAssociationsResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "multicastDomainAssociations"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayMulticastDomainAssociations

instance
  Prelude.NFData
    GetTransitGatewayMulticastDomainAssociations

instance
  Core.ToHeaders
    GetTransitGatewayMulticastDomainAssociations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetTransitGatewayMulticastDomainAssociations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetTransitGatewayMulticastDomainAssociations
  where
  toQuery
    GetTransitGatewayMulticastDomainAssociations' {..} =
      Prelude.mconcat
        [ "Action"
            Core.=: ( "GetTransitGatewayMulticastDomainAssociations" ::
                        Prelude.ByteString
                    ),
          "Version"
            Core.=: ("2016-11-15" :: Prelude.ByteString),
          Core.toQuery
            (Core.toQueryList "Filter" Prelude.<$> filters),
          "TransitGatewayMulticastDomainId"
            Core.=: transitGatewayMulticastDomainId,
          "NextToken" Core.=: nextToken,
          "DryRun" Core.=: dryRun,
          "MaxResults" Core.=: maxResults
        ]

-- | /See:/ 'newGetTransitGatewayMulticastDomainAssociationsResponse' smart constructor.
data GetTransitGatewayMulticastDomainAssociationsResponse = GetTransitGatewayMulticastDomainAssociationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the multicast domain associations.
    multicastDomainAssociations :: Prelude.Maybe [TransitGatewayMulticastDomainAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTransitGatewayMulticastDomainAssociationsResponse
newGetTransitGatewayMulticastDomainAssociationsResponse
  pHttpStatus_ =
    GetTransitGatewayMulticastDomainAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        multicastDomainAssociations =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayMulticastDomainAssociationsResponse_nextToken :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayMulticastDomainAssociationsResponse_nextToken = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)

-- | Information about the multicast domain associations.
getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse (Prelude.Maybe [TransitGatewayMulticastDomainAssociation])
getTransitGatewayMulticastDomainAssociationsResponse_multicastDomainAssociations = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {multicastDomainAssociations} -> multicastDomainAssociations) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {multicastDomainAssociations = a} :: GetTransitGatewayMulticastDomainAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTransitGatewayMulticastDomainAssociationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayMulticastDomainAssociationsResponse Prelude.Int
getTransitGatewayMulticastDomainAssociationsResponse_httpStatus = Lens.lens (\GetTransitGatewayMulticastDomainAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayMulticastDomainAssociationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayMulticastDomainAssociationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayMulticastDomainAssociationsResponse
