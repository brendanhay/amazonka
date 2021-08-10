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
-- Module      : Network.AWS.EC2.GetTransitGatewayPrefixListReferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the prefix list references in a specified transit
-- gateway route table.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayPrefixListReferences
  ( -- * Creating a Request
    GetTransitGatewayPrefixListReferences (..),
    newGetTransitGatewayPrefixListReferences,

    -- * Request Lenses
    getTransitGatewayPrefixListReferences_nextToken,
    getTransitGatewayPrefixListReferences_dryRun,
    getTransitGatewayPrefixListReferences_maxResults,
    getTransitGatewayPrefixListReferences_filters,
    getTransitGatewayPrefixListReferences_transitGatewayRouteTableId,

    -- * Destructuring the Response
    GetTransitGatewayPrefixListReferencesResponse (..),
    newGetTransitGatewayPrefixListReferencesResponse,

    -- * Response Lenses
    getTransitGatewayPrefixListReferencesResponse_nextToken,
    getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences,
    getTransitGatewayPrefixListReferencesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayPrefixListReferences' smart constructor.
data GetTransitGatewayPrefixListReferences = GetTransitGatewayPrefixListReferences'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | One or more filters. The possible values are:
    --
    -- -   @attachment.resource-id@ - The ID of the resource for the
    --     attachment.
    --
    -- -   @attachment.resource-type@ - The type of resource for the
    --     attachment. Valid values are @vpc@ | @vpn@ |
    --     @direct-connect-gateway@ | @peering@.
    --
    -- -   @attachment.transit-gateway-attachment-id@ - The ID of the
    --     attachment.
    --
    -- -   @is-blackhole@ - Whether traffic matching the route is blocked
    --     (@true@ | @false@).
    --
    -- -   @prefix-list-id@ - The ID of the prefix list.
    --
    -- -   @prefix-list-owner-id@ - The ID of the owner of the prefix list.
    --
    -- -   @state@ - The state of the prefix list reference (@pending@ |
    --     @available@ | @modifying@ | @deleting@).
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayPrefixListReferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayPrefixListReferences_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getTransitGatewayPrefixListReferences_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getTransitGatewayPrefixListReferences_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'getTransitGatewayPrefixListReferences_filters' - One or more filters. The possible values are:
--
-- -   @attachment.resource-id@ - The ID of the resource for the
--     attachment.
--
-- -   @attachment.resource-type@ - The type of resource for the
--     attachment. Valid values are @vpc@ | @vpn@ |
--     @direct-connect-gateway@ | @peering@.
--
-- -   @attachment.transit-gateway-attachment-id@ - The ID of the
--     attachment.
--
-- -   @is-blackhole@ - Whether traffic matching the route is blocked
--     (@true@ | @false@).
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @prefix-list-owner-id@ - The ID of the owner of the prefix list.
--
-- -   @state@ - The state of the prefix list reference (@pending@ |
--     @available@ | @modifying@ | @deleting@).
--
-- 'transitGatewayRouteTableId', 'getTransitGatewayPrefixListReferences_transitGatewayRouteTableId' - The ID of the transit gateway route table.
newGetTransitGatewayPrefixListReferences ::
  -- | 'transitGatewayRouteTableId'
  Prelude.Text ->
  GetTransitGatewayPrefixListReferences
newGetTransitGatewayPrefixListReferences
  pTransitGatewayRouteTableId_ =
    GetTransitGatewayPrefixListReferences'
      { nextToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        filters = Prelude.Nothing,
        transitGatewayRouteTableId =
          pTransitGatewayRouteTableId_
      }

-- | The token for the next page of results.
getTransitGatewayPrefixListReferences_nextToken :: Lens.Lens' GetTransitGatewayPrefixListReferences (Prelude.Maybe Prelude.Text)
getTransitGatewayPrefixListReferences_nextToken = Lens.lens (\GetTransitGatewayPrefixListReferences' {nextToken} -> nextToken) (\s@GetTransitGatewayPrefixListReferences' {} a -> s {nextToken = a} :: GetTransitGatewayPrefixListReferences)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayPrefixListReferences_dryRun :: Lens.Lens' GetTransitGatewayPrefixListReferences (Prelude.Maybe Prelude.Bool)
getTransitGatewayPrefixListReferences_dryRun = Lens.lens (\GetTransitGatewayPrefixListReferences' {dryRun} -> dryRun) (\s@GetTransitGatewayPrefixListReferences' {} a -> s {dryRun = a} :: GetTransitGatewayPrefixListReferences)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayPrefixListReferences_maxResults :: Lens.Lens' GetTransitGatewayPrefixListReferences (Prelude.Maybe Prelude.Natural)
getTransitGatewayPrefixListReferences_maxResults = Lens.lens (\GetTransitGatewayPrefixListReferences' {maxResults} -> maxResults) (\s@GetTransitGatewayPrefixListReferences' {} a -> s {maxResults = a} :: GetTransitGatewayPrefixListReferences)

-- | One or more filters. The possible values are:
--
-- -   @attachment.resource-id@ - The ID of the resource for the
--     attachment.
--
-- -   @attachment.resource-type@ - The type of resource for the
--     attachment. Valid values are @vpc@ | @vpn@ |
--     @direct-connect-gateway@ | @peering@.
--
-- -   @attachment.transit-gateway-attachment-id@ - The ID of the
--     attachment.
--
-- -   @is-blackhole@ - Whether traffic matching the route is blocked
--     (@true@ | @false@).
--
-- -   @prefix-list-id@ - The ID of the prefix list.
--
-- -   @prefix-list-owner-id@ - The ID of the owner of the prefix list.
--
-- -   @state@ - The state of the prefix list reference (@pending@ |
--     @available@ | @modifying@ | @deleting@).
getTransitGatewayPrefixListReferences_filters :: Lens.Lens' GetTransitGatewayPrefixListReferences (Prelude.Maybe [Filter])
getTransitGatewayPrefixListReferences_filters = Lens.lens (\GetTransitGatewayPrefixListReferences' {filters} -> filters) (\s@GetTransitGatewayPrefixListReferences' {} a -> s {filters = a} :: GetTransitGatewayPrefixListReferences) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the transit gateway route table.
getTransitGatewayPrefixListReferences_transitGatewayRouteTableId :: Lens.Lens' GetTransitGatewayPrefixListReferences Prelude.Text
getTransitGatewayPrefixListReferences_transitGatewayRouteTableId = Lens.lens (\GetTransitGatewayPrefixListReferences' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@GetTransitGatewayPrefixListReferences' {} a -> s {transitGatewayRouteTableId = a} :: GetTransitGatewayPrefixListReferences)

instance
  Core.AWSPager
    GetTransitGatewayPrefixListReferences
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayPrefixListReferencesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayPrefixListReferences_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayPrefixListReferencesResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayPrefixListReferences
  where
  type
    AWSResponse
      GetTransitGatewayPrefixListReferences =
      GetTransitGatewayPrefixListReferencesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayPrefixListReferencesResponse'
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayPrefixListReferenceSet"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayPrefixListReferences

instance
  Prelude.NFData
    GetTransitGatewayPrefixListReferences

instance
  Core.ToHeaders
    GetTransitGatewayPrefixListReferences
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetTransitGatewayPrefixListReferences
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetTransitGatewayPrefixListReferences
  where
  toQuery GetTransitGatewayPrefixListReferences' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetTransitGatewayPrefixListReferences" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "TransitGatewayRouteTableId"
          Core.=: transitGatewayRouteTableId
      ]

-- | /See:/ 'newGetTransitGatewayPrefixListReferencesResponse' smart constructor.
data GetTransitGatewayPrefixListReferencesResponse = GetTransitGatewayPrefixListReferencesResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the prefix list references.
    transitGatewayPrefixListReferences :: Prelude.Maybe [TransitGatewayPrefixListReference],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayPrefixListReferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayPrefixListReferencesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayPrefixListReferences', 'getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences' - Information about the prefix list references.
--
-- 'httpStatus', 'getTransitGatewayPrefixListReferencesResponse_httpStatus' - The response's http status code.
newGetTransitGatewayPrefixListReferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayPrefixListReferencesResponse
newGetTransitGatewayPrefixListReferencesResponse
  pHttpStatus_ =
    GetTransitGatewayPrefixListReferencesResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayPrefixListReferences =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayPrefixListReferencesResponse_nextToken :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayPrefixListReferencesResponse_nextToken = Lens.lens (\GetTransitGatewayPrefixListReferencesResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayPrefixListReferencesResponse' {} a -> s {nextToken = a} :: GetTransitGatewayPrefixListReferencesResponse)

-- | Information about the prefix list references.
getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse (Prelude.Maybe [TransitGatewayPrefixListReference])
getTransitGatewayPrefixListReferencesResponse_transitGatewayPrefixListReferences = Lens.lens (\GetTransitGatewayPrefixListReferencesResponse' {transitGatewayPrefixListReferences} -> transitGatewayPrefixListReferences) (\s@GetTransitGatewayPrefixListReferencesResponse' {} a -> s {transitGatewayPrefixListReferences = a} :: GetTransitGatewayPrefixListReferencesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTransitGatewayPrefixListReferencesResponse_httpStatus :: Lens.Lens' GetTransitGatewayPrefixListReferencesResponse Prelude.Int
getTransitGatewayPrefixListReferencesResponse_httpStatus = Lens.lens (\GetTransitGatewayPrefixListReferencesResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayPrefixListReferencesResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayPrefixListReferencesResponse)

instance
  Prelude.NFData
    GetTransitGatewayPrefixListReferencesResponse
