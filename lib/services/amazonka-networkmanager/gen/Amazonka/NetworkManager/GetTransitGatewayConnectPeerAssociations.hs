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
-- Module      : Amazonka.NetworkManager.GetTransitGatewayConnectPeerAssociations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more of your transit gateway Connect peer
-- associations in a global network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetTransitGatewayConnectPeerAssociations
  ( -- * Creating a Request
    GetTransitGatewayConnectPeerAssociations (..),
    newGetTransitGatewayConnectPeerAssociations,

    -- * Request Lenses
    getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns,
    getTransitGatewayConnectPeerAssociations_nextToken,
    getTransitGatewayConnectPeerAssociations_maxResults,
    getTransitGatewayConnectPeerAssociations_globalNetworkId,

    -- * Destructuring the Response
    GetTransitGatewayConnectPeerAssociationsResponse (..),
    newGetTransitGatewayConnectPeerAssociationsResponse,

    -- * Response Lenses
    getTransitGatewayConnectPeerAssociationsResponse_nextToken,
    getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations,
    getTransitGatewayConnectPeerAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTransitGatewayConnectPeerAssociations' smart constructor.
data GetTransitGatewayConnectPeerAssociations = GetTransitGatewayConnectPeerAssociations'
  { -- | One or more transit gateway Connect peer Amazon Resource Names (ARNs).
    transitGatewayConnectPeerArns :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayConnectPeerAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayConnectPeerArns', 'getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns' - One or more transit gateway Connect peer Amazon Resource Names (ARNs).
--
-- 'nextToken', 'getTransitGatewayConnectPeerAssociations_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'getTransitGatewayConnectPeerAssociations_maxResults' - The maximum number of results to return.
--
-- 'globalNetworkId', 'getTransitGatewayConnectPeerAssociations_globalNetworkId' - The ID of the global network.
newGetTransitGatewayConnectPeerAssociations ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetTransitGatewayConnectPeerAssociations
newGetTransitGatewayConnectPeerAssociations
  pGlobalNetworkId_ =
    GetTransitGatewayConnectPeerAssociations'
      { transitGatewayConnectPeerArns =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        globalNetworkId =
          pGlobalNetworkId_
      }

-- | One or more transit gateway Connect peer Amazon Resource Names (ARNs).
getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns :: Lens.Lens' GetTransitGatewayConnectPeerAssociations (Prelude.Maybe [Prelude.Text])
getTransitGatewayConnectPeerAssociations_transitGatewayConnectPeerArns = Lens.lens (\GetTransitGatewayConnectPeerAssociations' {transitGatewayConnectPeerArns} -> transitGatewayConnectPeerArns) (\s@GetTransitGatewayConnectPeerAssociations' {} a -> s {transitGatewayConnectPeerArns = a} :: GetTransitGatewayConnectPeerAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getTransitGatewayConnectPeerAssociations_nextToken :: Lens.Lens' GetTransitGatewayConnectPeerAssociations (Prelude.Maybe Prelude.Text)
getTransitGatewayConnectPeerAssociations_nextToken = Lens.lens (\GetTransitGatewayConnectPeerAssociations' {nextToken} -> nextToken) (\s@GetTransitGatewayConnectPeerAssociations' {} a -> s {nextToken = a} :: GetTransitGatewayConnectPeerAssociations)

-- | The maximum number of results to return.
getTransitGatewayConnectPeerAssociations_maxResults :: Lens.Lens' GetTransitGatewayConnectPeerAssociations (Prelude.Maybe Prelude.Natural)
getTransitGatewayConnectPeerAssociations_maxResults = Lens.lens (\GetTransitGatewayConnectPeerAssociations' {maxResults} -> maxResults) (\s@GetTransitGatewayConnectPeerAssociations' {} a -> s {maxResults = a} :: GetTransitGatewayConnectPeerAssociations)

-- | The ID of the global network.
getTransitGatewayConnectPeerAssociations_globalNetworkId :: Lens.Lens' GetTransitGatewayConnectPeerAssociations Prelude.Text
getTransitGatewayConnectPeerAssociations_globalNetworkId = Lens.lens (\GetTransitGatewayConnectPeerAssociations' {globalNetworkId} -> globalNetworkId) (\s@GetTransitGatewayConnectPeerAssociations' {} a -> s {globalNetworkId = a} :: GetTransitGatewayConnectPeerAssociations)

instance
  Core.AWSPager
    GetTransitGatewayConnectPeerAssociations
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayConnectPeerAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayConnectPeerAssociations_nextToken
          Lens..~ rs
            Lens.^? getTransitGatewayConnectPeerAssociationsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayConnectPeerAssociations
  where
  type
    AWSResponse
      GetTransitGatewayConnectPeerAssociations =
      GetTransitGatewayConnectPeerAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTransitGatewayConnectPeerAssociationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "TransitGatewayConnectPeerAssociations"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayConnectPeerAssociations
  where
  hashWithSalt
    _salt
    GetTransitGatewayConnectPeerAssociations' {..} =
      _salt
        `Prelude.hashWithSalt` transitGatewayConnectPeerArns
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` globalNetworkId

instance
  Prelude.NFData
    GetTransitGatewayConnectPeerAssociations
  where
  rnf GetTransitGatewayConnectPeerAssociations' {..} =
    Prelude.rnf transitGatewayConnectPeerArns
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf globalNetworkId

instance
  Core.ToHeaders
    GetTransitGatewayConnectPeerAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToPath
    GetTransitGatewayConnectPeerAssociations
  where
  toPath GetTransitGatewayConnectPeerAssociations' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/transit-gateway-connect-peer-associations"
      ]

instance
  Core.ToQuery
    GetTransitGatewayConnectPeerAssociations
  where
  toQuery GetTransitGatewayConnectPeerAssociations' {..} =
    Prelude.mconcat
      [ "transitGatewayConnectPeerArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> transitGatewayConnectPeerArns
            ),
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetTransitGatewayConnectPeerAssociationsResponse' smart constructor.
data GetTransitGatewayConnectPeerAssociationsResponse = GetTransitGatewayConnectPeerAssociationsResponse'
  { -- | The token to use for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the transit gateway Connect peer associations.
    transitGatewayConnectPeerAssociations :: Prelude.Maybe [TransitGatewayConnectPeerAssociation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayConnectPeerAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayConnectPeerAssociationsResponse_nextToken' - The token to use for the next page of results.
--
-- 'transitGatewayConnectPeerAssociations', 'getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations' - Information about the transit gateway Connect peer associations.
--
-- 'httpStatus', 'getTransitGatewayConnectPeerAssociationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayConnectPeerAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayConnectPeerAssociationsResponse
newGetTransitGatewayConnectPeerAssociationsResponse
  pHttpStatus_ =
    GetTransitGatewayConnectPeerAssociationsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayConnectPeerAssociations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use for the next page of results.
getTransitGatewayConnectPeerAssociationsResponse_nextToken :: Lens.Lens' GetTransitGatewayConnectPeerAssociationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayConnectPeerAssociationsResponse_nextToken = Lens.lens (\GetTransitGatewayConnectPeerAssociationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayConnectPeerAssociationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayConnectPeerAssociationsResponse)

-- | Information about the transit gateway Connect peer associations.
getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations :: Lens.Lens' GetTransitGatewayConnectPeerAssociationsResponse (Prelude.Maybe [TransitGatewayConnectPeerAssociation])
getTransitGatewayConnectPeerAssociationsResponse_transitGatewayConnectPeerAssociations = Lens.lens (\GetTransitGatewayConnectPeerAssociationsResponse' {transitGatewayConnectPeerAssociations} -> transitGatewayConnectPeerAssociations) (\s@GetTransitGatewayConnectPeerAssociationsResponse' {} a -> s {transitGatewayConnectPeerAssociations = a} :: GetTransitGatewayConnectPeerAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTransitGatewayConnectPeerAssociationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayConnectPeerAssociationsResponse Prelude.Int
getTransitGatewayConnectPeerAssociationsResponse_httpStatus = Lens.lens (\GetTransitGatewayConnectPeerAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayConnectPeerAssociationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayConnectPeerAssociationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayConnectPeerAssociationsResponse
  where
  rnf
    GetTransitGatewayConnectPeerAssociationsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf transitGatewayConnectPeerAssociations
        `Prelude.seq` Prelude.rnf httpStatus
