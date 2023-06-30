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
-- Module      : Amazonka.DirectConnect.DescribeDirectConnectGatewayAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.DirectConnect.DescribeDirectConnectGatewayAssociations
  ( -- * Creating a Request
    DescribeDirectConnectGatewayAssociations (..),
    newDescribeDirectConnectGatewayAssociations,

    -- * Request Lenses
    describeDirectConnectGatewayAssociations_associatedGatewayId,
    describeDirectConnectGatewayAssociations_associationId,
    describeDirectConnectGatewayAssociations_directConnectGatewayId,
    describeDirectConnectGatewayAssociations_maxResults,
    describeDirectConnectGatewayAssociations_nextToken,
    describeDirectConnectGatewayAssociations_virtualGatewayId,

    -- * Destructuring the Response
    DescribeDirectConnectGatewayAssociationsResponse (..),
    newDescribeDirectConnectGatewayAssociationsResponse,

    -- * Response Lenses
    describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations,
    describeDirectConnectGatewayAssociationsResponse_nextToken,
    describeDirectConnectGatewayAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDirectConnectGatewayAssociations' smart constructor.
data DescribeDirectConnectGatewayAssociations = DescribeDirectConnectGatewayAssociations'
  { -- | The ID of the associated gateway.
    associatedGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual private gateway or transit gateway.
    virtualGatewayId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedGatewayId', 'describeDirectConnectGatewayAssociations_associatedGatewayId' - The ID of the associated gateway.
--
-- 'associationId', 'describeDirectConnectGatewayAssociations_associationId' - The ID of the Direct Connect gateway association.
--
-- 'directConnectGatewayId', 'describeDirectConnectGatewayAssociations_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'maxResults', 'describeDirectConnectGatewayAssociations_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
--
-- 'nextToken', 'describeDirectConnectGatewayAssociations_nextToken' - The token provided in the previous call to retrieve the next page.
--
-- 'virtualGatewayId', 'describeDirectConnectGatewayAssociations_virtualGatewayId' - The ID of the virtual private gateway or transit gateway.
newDescribeDirectConnectGatewayAssociations ::
  DescribeDirectConnectGatewayAssociations
newDescribeDirectConnectGatewayAssociations =
  DescribeDirectConnectGatewayAssociations'
    { associatedGatewayId =
        Prelude.Nothing,
      associationId = Prelude.Nothing,
      directConnectGatewayId =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      virtualGatewayId =
        Prelude.Nothing
    }

-- | The ID of the associated gateway.
describeDirectConnectGatewayAssociations_associatedGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociations_associatedGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {associatedGatewayId} -> associatedGatewayId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {associatedGatewayId = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the Direct Connect gateway association.
describeDirectConnectGatewayAssociations_associationId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociations_associationId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {associationId} -> associationId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {associationId = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the Direct Connect gateway.
describeDirectConnectGatewayAssociations_directConnectGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociations_directConnectGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {directConnectGatewayId} -> directConnectGatewayId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGatewayAssociations)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
describeDirectConnectGatewayAssociations_maxResults :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Prelude.Maybe Prelude.Int)
describeDirectConnectGatewayAssociations_maxResults = Lens.lens (\DescribeDirectConnectGatewayAssociations' {maxResults} -> maxResults) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {maxResults = a} :: DescribeDirectConnectGatewayAssociations)

-- | The token provided in the previous call to retrieve the next page.
describeDirectConnectGatewayAssociations_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociations_nextToken = Lens.lens (\DescribeDirectConnectGatewayAssociations' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociations)

-- | The ID of the virtual private gateway or transit gateway.
describeDirectConnectGatewayAssociations_virtualGatewayId :: Lens.Lens' DescribeDirectConnectGatewayAssociations (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociations_virtualGatewayId = Lens.lens (\DescribeDirectConnectGatewayAssociations' {virtualGatewayId} -> virtualGatewayId) (\s@DescribeDirectConnectGatewayAssociations' {} a -> s {virtualGatewayId = a} :: DescribeDirectConnectGatewayAssociations)

instance
  Core.AWSPager
    DescribeDirectConnectGatewayAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewayAssociationsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& describeDirectConnectGatewayAssociations_nextToken
          Lens..~ rs
          Lens.^? describeDirectConnectGatewayAssociationsResponse_nextToken
          Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDirectConnectGatewayAssociations
  where
  type
    AWSResponse
      DescribeDirectConnectGatewayAssociations =
      DescribeDirectConnectGatewayAssociationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewayAssociationsResponse'
            Prelude.<$> ( x
                            Data..?> "directConnectGatewayAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDirectConnectGatewayAssociations
  where
  hashWithSalt
    _salt
    DescribeDirectConnectGatewayAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` associatedGatewayId
        `Prelude.hashWithSalt` associationId
        `Prelude.hashWithSalt` directConnectGatewayId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` virtualGatewayId

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAssociations
  where
  rnf DescribeDirectConnectGatewayAssociations' {..} =
    Prelude.rnf associatedGatewayId
      `Prelude.seq` Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf virtualGatewayId

instance
  Data.ToHeaders
    DescribeDirectConnectGatewayAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OvertureService.DescribeDirectConnectGatewayAssociations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeDirectConnectGatewayAssociations
  where
  toJSON DescribeDirectConnectGatewayAssociations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associatedGatewayId" Data..=)
              Prelude.<$> associatedGatewayId,
            ("associationId" Data..=) Prelude.<$> associationId,
            ("directConnectGatewayId" Data..=)
              Prelude.<$> directConnectGatewayId,
            ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken,
            ("virtualGatewayId" Data..=)
              Prelude.<$> virtualGatewayId
          ]
      )

instance
  Data.ToPath
    DescribeDirectConnectGatewayAssociations
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeDirectConnectGatewayAssociations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDirectConnectGatewayAssociationsResponse' smart constructor.
data DescribeDirectConnectGatewayAssociationsResponse = DescribeDirectConnectGatewayAssociationsResponse'
  { -- | Information about the associations.
    directConnectGatewayAssociations :: Prelude.Maybe [DirectConnectGatewayAssociation],
    -- | The token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewayAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociations', 'describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations' - Information about the associations.
--
-- 'nextToken', 'describeDirectConnectGatewayAssociationsResponse_nextToken' - The token to retrieve the next page.
--
-- 'httpStatus', 'describeDirectConnectGatewayAssociationsResponse_httpStatus' - The response's http status code.
newDescribeDirectConnectGatewayAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectConnectGatewayAssociationsResponse
newDescribeDirectConnectGatewayAssociationsResponse
  pHttpStatus_ =
    DescribeDirectConnectGatewayAssociationsResponse'
      { directConnectGatewayAssociations =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the associations.
describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Prelude.Maybe [DirectConnectGatewayAssociation])
describeDirectConnectGatewayAssociationsResponse_directConnectGatewayAssociations = Lens.lens (\DescribeDirectConnectGatewayAssociationsResponse' {directConnectGatewayAssociations} -> directConnectGatewayAssociations) (\s@DescribeDirectConnectGatewayAssociationsResponse' {} a -> s {directConnectGatewayAssociations = a} :: DescribeDirectConnectGatewayAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next page.
describeDirectConnectGatewayAssociationsResponse_nextToken :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewayAssociationsResponse_nextToken = Lens.lens (\DescribeDirectConnectGatewayAssociationsResponse' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewayAssociationsResponse' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewayAssociationsResponse)

-- | The response's http status code.
describeDirectConnectGatewayAssociationsResponse_httpStatus :: Lens.Lens' DescribeDirectConnectGatewayAssociationsResponse Prelude.Int
describeDirectConnectGatewayAssociationsResponse_httpStatus = Lens.lens (\DescribeDirectConnectGatewayAssociationsResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectConnectGatewayAssociationsResponse' {} a -> s {httpStatus = a} :: DescribeDirectConnectGatewayAssociationsResponse)

instance
  Prelude.NFData
    DescribeDirectConnectGatewayAssociationsResponse
  where
  rnf
    DescribeDirectConnectGatewayAssociationsResponse' {..} =
      Prelude.rnf directConnectGatewayAssociations
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
