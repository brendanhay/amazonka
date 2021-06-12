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
-- Module      : Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the associations between your Direct Connect gateways and virtual
-- private gateways and transit gateways. You must specify one of the
-- following:
--
-- -   A Direct Connect gateway
--
--     The response contains all virtual private gateways and transit
--     gateways associated with the Direct Connect gateway.
--
-- -   A virtual private gateway
--
--     The response contains the Direct Connect gateway.
--
-- -   A transit gateway
--
--     The response contains the Direct Connect gateway.
--
-- -   A Direct Connect gateway and a virtual private gateway
--
--     The response contains the association between the Direct Connect
--     gateway and virtual private gateway.
--
-- -   A Direct Connect gateway and a transit gateway
--
--     The response contains the association between the Direct Connect
--     gateway and transit gateway.
--
-- This operation returns paginated results.
module Network.AWS.DirectConnect.DescribeDirectConnectGatewayAssociations
  ( -- * Creating a Request
    DescribeDirectConnectGatewayAssociations (..),
    newDescribeDirectConnectGatewayAssociations,

    -- * Request Lenses
    describeDirectConnectGatewayAssociations_nextToken,
    describeDirectConnectGatewayAssociations_virtualGatewayId,
    describeDirectConnectGatewayAssociations_maxResults,
    describeDirectConnectGatewayAssociations_associatedGatewayId,
    describeDirectConnectGatewayAssociations_associationId,
    describeDirectConnectGatewayAssociations_directConnectGatewayId,

    -- * Destructuring the Response
    DescribeDirectConnectGatewayAssociationsResponse (..),
    newDescribeDirectConnectGatewayAssociationsResponse,

    -- * Response Lenses
    describeDirectConnectGatewayAssociationsResponse_nextToken,
    describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations,
    describeDirectConnectGatewayAssociationsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeDirectConnectGatewayAssociations' smart constructor.
data DescribeDirectConnectGatewayAssociations = DescribeDirectConnectGatewayAssociations'
  { -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the virtual private gateway or transit gateway.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Core.Maybe Core.Int,
    -- | The ID of the associated gateway.
    associatedGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectConnectGatewayAssociations_nextToken' - The token provided in the previous call to retrieve the next page.
--
-- 'virtualGatewayId', 'describeDirectConnectGatewayAssociations_virtualGatewayId' - The ID of the virtual private gateway or transit gateway.
--
-- 'maxResults', 'describeDirectConnectGatewayAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'associatedGatewayId', 'describeDirectConnectGatewayAssociations_associatedGatewayId' - The ID of the associated gateway.
--
-- 'associationId', 'describeDirectConnectGatewayAssociations_associationId' - The ID of the Direct Connect gateway association.
--
-- 'directConnectGatewayId', 'describeDirectConnectGatewayAssociations_directConnectGatewayId' - The ID of the Direct Connect gateway.
newDescribeDirectConnectGatewayAssociations ::
  DescribeDirectConnectGatewayAssociations
newDescribeDirectConnectGatewayAssociations =
  DescribeDirectConnectGatewayAssociations'
    { nextToken =
        Core.Nothing,
      virtualGatewayId = Core.Nothing,
      maxResults = Core.Nothing,
      associatedGatewayId =
        Core.Nothing,
      associationId = Core.Nothing,
      directConnectGatewayId =
        Core.Nothing
    }

-- | The token provided in the previous call to retrieve the next page.
describeDirectConnectGatewayAssociations_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Core.Maybe Core.Text)
describeDirectConnectGatewayAssociations_nextToken = Lens.lens (\DescribeDirectConnectGatewayAssociations' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the virtual private gateway or transit gateway.
describeDirectConnectGatewayAssociations_virtualGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Core.Maybe Core.Text)
describeDirectConnectGatewayAssociations_virtualGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {virtualGatewayId} -> virtualGatewayId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {virtualGatewayId = a} :: DescribeDirectConnectGatewayAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
describeDirectConnectGatewayAssociations_maxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Core.Maybe Core.Int)
describeDirectConnectGatewayAssociations_maxResults = Lens.lens (\DescribeDirectConnectGatewayAssociations' {maxResults} -> maxResults) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {maxResults = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the associated gateway.
describeDirectConnectGatewayAssociations_associatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Core.Maybe Core.Text)
describeDirectConnectGatewayAssociations_associatedGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {associatedGatewayId} -> associatedGatewayId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {associatedGatewayId = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the Direct Connect gateway association.
describeDirectConnectGatewayAssociations_associationId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Core.Maybe Core.Text)
describeDirectConnectGatewayAssociations_associationId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {associationId} -> associationId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {associationId = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the Direct Connect gateway.
describeDirectConnectGatewayAssociations_directConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Core.Maybe Core.Text)
describeDirectConnectGatewayAssociations_directConnectGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {directConnectGatewayId} -> directConnectGatewayId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAssociations)

instance
  Core.AWSPager
    DescribeDirectConnectGatewayAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewayAssociationsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeDirectConnectGatewayAssociations_nextToken
          Lens..~ rs
            Lens.^? describeDirectConnectGatewayAssociationsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    DescribeDirectConnectGatewayAssociations
  where
  type
    AWSResponse
      DescribeDirectConnectGatewayAssociations =
      DescribeDirectConnectGatewayAssociationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationsResponse'
            Core.<$> (x Core..?> "nextToken")
              Core.<*> ( x Core..?> "directConnectGatewayAssociations"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeDirectConnectGatewayAssociations

instance
  Core.NFData
    DescribeDirectConnectGatewayAssociations

instance
  Core.ToHeaders
    DescribeDirectConnectGatewayAssociations
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeDirectConnectGatewayAssociations" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeDirectConnectGatewayAssociations
  where
  toJSON DescribeDirectConnectGatewayAssociations' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("virtualGatewayId" Core..=)
              Core.<$> virtualGatewayId,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("associatedGatewayId" Core..=)
              Core.<$> associatedGatewayId,
            ("associationId" Core..=) Core.<$> associationId,
            ("directConnectGatewayId" Core..=)
              Core.<$> directConnectGatewayId
          ]
      )

instance
  Core.ToPath
    DescribeDirectConnectGatewayAssociations
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeDirectConnectGatewayAssociations
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeDirectConnectGatewayAssociationsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationsResponse = DescribeDirectConnectGatewayAssociationsResponse'
  { -- | The token to retrieve the next page.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the associations.
    directConnectGatewayAssociations :: Core.Maybe [DirectConnectGatewayAssociation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectConnectGatewayAssociationsResponse_nextToken' - The token to retrieve the next page.
--
-- 'directConnectGatewayAssociations', 'describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations' - Information about the associations.
--
-- 'httpStatus', 'describeDirectConnectGatewayAssociationsResponse_httpStatus' - The response's http status code.
newDescribeDirectConnectGatewayAssociationsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDirectConnectGatewayAssociationsResponse
newDescribeDirectConnectGatewayAssociationsResponse
  pHttpStatus_ =
    DescribeDirectConnectGatewayAssociationsResponse'
      { nextToken =
          Core.Nothing,
        directConnectGatewayAssociations =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to retrieve the next page.
describeDirectConnectGatewayAssociationsResponse_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Core.Maybe Core.Text)
describeDirectConnectGatewayAssociationsResponse_nextToken = Lens.lens (\DescribeDirectConnectGatewayAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAssociationsResponse' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationsResponse)

-- | Information about the associations.
describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Core.Maybe [DirectConnectGatewayAssociation])
describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations = Lens.lens (\DescribeDirectConnectGatewayAssociationsResponse' {directConnectGatewayAssociations} -> directConnectGatewayAssociations) (\s@DescribeDirectConnectGatewayAssociationsResponse' {} a -> s {directConnectGatewayAssociations = a} :: DescribeDirectConnectGatewayAssociationsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDirectConnectGatewayAssociationsResponse_httpStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse Core.Int
describeDirectConnectGatewayAssociationsResponse_httpStatus = Lens.lens (\DescribeDirectConnectGatewayAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectConnectGatewayAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeDirectConnectGatewayAssociationsResponse)

instance
  Core.NFData
    DescribeDirectConnectGatewayAssociationsResponse
