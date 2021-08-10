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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTransitGatewayAttachmentPropagations' smart constructor.
data GetTransitGatewayAttachmentPropagations = GetTransitGatewayAttachmentPropagations'
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
    -- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
    --     route table.
    filters :: Prelude.Maybe [Filter],
    -- | The ID of the attachment.
    transitGatewayAttachmentId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetTransitGatewayAttachmentPropagations
newGetTransitGatewayAttachmentPropagations
  pTransitGatewayAttachmentId_ =
    GetTransitGatewayAttachmentPropagations'
      { nextToken =
          Prelude.Nothing,
        dryRun = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        filters = Prelude.Nothing,
        transitGatewayAttachmentId =
          pTransitGatewayAttachmentId_
      }

-- | The token for the next page of results.
getTransitGatewayAttachmentPropagations_nextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Prelude.Maybe Prelude.Text)
getTransitGatewayAttachmentPropagations_nextToken = Lens.lens (\GetTransitGatewayAttachmentPropagations' {nextToken} -> nextToken) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {nextToken = a} :: GetTransitGatewayAttachmentPropagations)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
getTransitGatewayAttachmentPropagations_dryRun :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Prelude.Maybe Prelude.Bool)
getTransitGatewayAttachmentPropagations_dryRun = Lens.lens (\GetTransitGatewayAttachmentPropagations' {dryRun} -> dryRun) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {dryRun = a} :: GetTransitGatewayAttachmentPropagations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
getTransitGatewayAttachmentPropagations_maxResults :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Prelude.Maybe Prelude.Natural)
getTransitGatewayAttachmentPropagations_maxResults = Lens.lens (\GetTransitGatewayAttachmentPropagations' {maxResults} -> maxResults) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {maxResults = a} :: GetTransitGatewayAttachmentPropagations)

-- | One or more filters. The possible values are:
--
-- -   @transit-gateway-route-table-id@ - The ID of the transit gateway
--     route table.
getTransitGatewayAttachmentPropagations_filters :: Lens.Lens' GetTransitGatewayAttachmentPropagations (Prelude.Maybe [Filter])
getTransitGatewayAttachmentPropagations_filters = Lens.lens (\GetTransitGatewayAttachmentPropagations' {filters} -> filters) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {filters = a} :: GetTransitGatewayAttachmentPropagations) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the attachment.
getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId :: Lens.Lens' GetTransitGatewayAttachmentPropagations Prelude.Text
getTransitGatewayAttachmentPropagations_transitGatewayAttachmentId = Lens.lens (\GetTransitGatewayAttachmentPropagations' {transitGatewayAttachmentId} -> transitGatewayAttachmentId) (\s@GetTransitGatewayAttachmentPropagations' {} a -> s {transitGatewayAttachmentId = a} :: GetTransitGatewayAttachmentPropagations)

instance
  Core.AWSPager
    GetTransitGatewayAttachmentPropagations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayAttachmentPropagationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayAttachmentPropagations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayAttachmentPropagationsResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..@? "nextToken")
              Prelude.<*> ( x Core..@? "transitGatewayAttachmentPropagations"
                              Core..!@ Prelude.mempty
                              Prelude.>>= Core.may (Core.parseXMLList "item")
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayAttachmentPropagations

instance
  Prelude.NFData
    GetTransitGatewayAttachmentPropagations

instance
  Core.ToHeaders
    GetTransitGatewayAttachmentPropagations
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    GetTransitGatewayAttachmentPropagations
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    GetTransitGatewayAttachmentPropagations
  where
  toQuery GetTransitGatewayAttachmentPropagations' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "GetTransitGatewayAttachmentPropagations" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "TransitGatewayAttachmentId"
          Core.=: transitGatewayAttachmentId
      ]

-- | /See:/ 'newGetTransitGatewayAttachmentPropagationsResponse' smart constructor.
data GetTransitGatewayAttachmentPropagationsResponse = GetTransitGatewayAttachmentPropagationsResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the propagation route tables.
    transitGatewayAttachmentPropagations :: Prelude.Maybe [TransitGatewayAttachmentPropagation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetTransitGatewayAttachmentPropagationsResponse
newGetTransitGatewayAttachmentPropagationsResponse
  pHttpStatus_ =
    GetTransitGatewayAttachmentPropagationsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayAttachmentPropagations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
getTransitGatewayAttachmentPropagationsResponse_nextToken :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayAttachmentPropagationsResponse_nextToken = Lens.lens (\GetTransitGatewayAttachmentPropagationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayAttachmentPropagationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayAttachmentPropagationsResponse)

-- | Information about the propagation route tables.
getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse (Prelude.Maybe [TransitGatewayAttachmentPropagation])
getTransitGatewayAttachmentPropagationsResponse_transitGatewayAttachmentPropagations = Lens.lens (\GetTransitGatewayAttachmentPropagationsResponse' {transitGatewayAttachmentPropagations} -> transitGatewayAttachmentPropagations) (\s@GetTransitGatewayAttachmentPropagationsResponse' {} a -> s {transitGatewayAttachmentPropagations = a} :: GetTransitGatewayAttachmentPropagationsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getTransitGatewayAttachmentPropagationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayAttachmentPropagationsResponse Prelude.Int
getTransitGatewayAttachmentPropagationsResponse_httpStatus = Lens.lens (\GetTransitGatewayAttachmentPropagationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayAttachmentPropagationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayAttachmentPropagationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayAttachmentPropagationsResponse
