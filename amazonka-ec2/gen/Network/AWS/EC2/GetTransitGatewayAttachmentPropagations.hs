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
-- Module      : Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the route tables to which the specified resource attachment
-- propagates routes.
--
-- This operation returns paginated results.
module Network.AWS.EC2.GetTransitGatewayAttachmentPropagations
  ( -- * Creating a Request
    GetTransitGatewayAttachmentPropagations (..),
    newGetTransitGatewayAttachmentPropagations,

    -- * Request Lenses
    getTransitGatewayAttachmentPropagations_nextToken,
    getTransitGatewayAttachmentPropagations_dryRun,
    getTransitGatewayAttachmentPropagations_maxResults,
    getTransitGatewayAttachmentPropagations_filters,
    getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId,

    -- * Destructuring the Response
    GetTransitGatewayAttachmentPropagationsResponse (..),
    newGetTransitGatewayAttachmentPropagationsResponse,

    -- * Response Lenses
    getTransitGatewayAttachmentPropagationsResponse_nextToken,
    getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations,
    getTransitGatewayAttachmentPropagationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayAttachmentPropagations' smart constructor.
data GetTransitGatewayAttachmentPropagations = GetTransitGatewayAttachmentPropagations'
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
    -- | One or more filters. The possible values are:
    --
    -- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
    --     route table.
    filters :: Core.Maybe [Filter],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTransitGatewayAttachmentPropagations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayAttachmentPropagations_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'getTransitGatewayAttachmentPropagations_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'getTransitGatewayAttachmentPropagations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'filters', 'getTransitGatewayAttachmentPropagations_filters' - One or more filters. The possible values are:
--
-- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
--     route table.
--
-- 'transitGatewayAttachmentId', 'getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId' - The ID of the attachment.
newGetTransitGatewayAttachmentPropagations ::
  -- | 'transitGatewayAttachmentId'
  Core.Text ->
  GetTransitGatewayAttachmentPropagations
newGetTransitGatewayAttachmentPropagations
  pTransitGatewayAttachmentId_ =
    GetTransitGatewayAttachmentPropagations'
      { nextToken =
          Core.Nothing,
        dryRun = Core.Nothing,
        maxResults = Core.Nothing,
        filters = Core.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | The token for the next page of results.
getTransitGatewayAttachmentPropagations_nextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Text)
getTransitGatewayAttachmentPropagations_nextToken = Lens.lens (\GetTransitGatewayAttachmentPropagations' {nextToken} -> nextToken) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {nextToken = a} :: GetTransitGatewayAttachmentPropagations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayAttachmentPropagations_dryRun :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Bool)
getTransitGatewayAttachmentPropagations_dryRun = Lens.lens (\GetTransitGatewayAttachmentPropagations' {dryRun} -> dryRun) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {dryRun = a} :: GetTransitGatewayAttachmentPropagations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayAttachmentPropagations_maxResults :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe Core.Natural)
getTransitGatewayAttachmentPropagations_maxResults = Lens.lens (\GetTransitGatewayAttachmentPropagations' {maxResults} -> maxResults) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {maxResults = a} :: GetTransitGatewayAttachmentPropagations)

-- | One or more filters. The possible values are:
--
-- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
--     route table.
getTransitGatewayAttachmentPropagations_filters :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Core.Maybe [Filter])
getTransitGatewayAttachmentPropagations_filters = Lens.lens (\GetTransitGatewayAttachmentPropagations' {filters} -> filters) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {filters = a} :: GetTransitGatewayAttachmentPropagations) Core.. Lens.mapping Lens._Coerce

-- | The ID of the attachment.
getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId :: Lens.Lens' GetTransitGatewayAttachmentPropagations Core.Text
getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId = Lens.lens (\GetTransitGatewayAttachmentPropagations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {transitGatewayAttachmentId = a} :: GetTransitGatewayAttachmentPropagations)

instance
  Core.AWSPager
    GetTransitGatewayAttachmentPropagations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayAttachmentPropagationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getTransitGatewayAttachmentPropagations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayAttachmentPropagationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayAttachmentPropagations
  where
  type
    AWSResponse
      GetTransitGatewayAttachmentPropagations =
      GetTransitGatewayAttachmentPropagationsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetTransitGatewayAttachmentPropagationsResponse'
            Core.<$> (x Core..@? "nextToken")
              Core.<*> ( x Core..@? "transitGatewayAttachmentPropagations"
                           Core..!@ Core.mempty
                           Core.>>= Core.may (Core.parseXMLList "item")
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetTransitGatewayAttachmentPropagations

instance
  Core.NFData
    GetTransitGatewayAttachmentPropagations

instance
  Core.ToHeaders
    GetTransitGatewayAttachmentPropagations
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    GetTransitGatewayAttachmentPropagations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetTransitGatewayAttachmentPropagations
  where
  toQuery GetTransitGatewayAttachmentPropagations' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "GetTransitGatewayAttachmentPropagations" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Core.<$> filters),
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newGetTransitGatewayAttachmentPropagationsResponse' smart constructor.
data GetTransitGatewayAttachmentPropagationsResponse = GetTransitGatewayAttachmentPropagationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the propagation route tables.
    transitGatewayAttachmentPropagations :: Core.Maybe [TransitGatewayAttachmentPropagation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTransitGatewayAttachmentPropagationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayAttachmentPropagationsResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'transitGatewayAttachmentPropagations', 'getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations' - Information about the propagation route tables.
--
-- 'httpStatus', 'getTransitGatewayAttachmentPropagationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayAttachmentPropagationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTransitGatewayAttachmentPropagationsResponse
newGetTransitGatewayAttachmentPropagationsResponse
  pHttpStatus_ =
    GetTransitGatewayAttachmentPropagationsResponse'
      { nextToken =
          Core.Nothing,
        transitGatewayAttachmentPropagations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayAttachmentPropagationsResponse_nextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Core.Maybe Core.Text)
getTransitGatewayAttachmentPropagationsResponse_nextToken = Lens.lens (\GetTransitGatewayAttachmentPropagationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayAttachmentPropagationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayAttachmentPropagationsResponse)

-- | Information about the propagation route tables.
getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Core.Maybe [TransitGatewayAttachmentPropagation])
getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations = Lens.lens (\GetTransitGatewayAttachmentPropagationsResponse' {transitGatewayAttachmentPropagations} -> transitGatewayAttachmentPropagations) (\s@GetTransitGatewayAttachmentPropagationsResponse' {} a -> s {transitGatewayAttachmentPropagations = a} :: GetTransitGatewayAttachmentPropagationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTransitGatewayAttachmentPropagationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse Core.Int
getTransitGatewayAttachmentPropagationsResponse_httpStatus = Lens.lens (\GetTransitGatewayAttachmentPropagationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayAttachmentPropagationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayAttachmentPropagationsResponse)

instance
  Core.NFData
    GetTransitGatewayAttachmentPropagationsResponse
