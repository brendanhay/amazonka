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
-- Module      : Amazonka.NetworkManager.GetTransitGatewayRegistrations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the transit gateway registrations in a specified
-- global network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetTransitGatewayRegistrations
  ( -- * Creating a Request
    GetTransitGatewayRegistrations (..),
    newGetTransitGatewayRegistrations,

    -- * Request Lenses
    getTransitGatewayRegistrations_nextToken,
    getTransitGatewayRegistrations_transitGatewayArns,
    getTransitGatewayRegistrations_maxResults,
    getTransitGatewayRegistrations_globalNetworkId,

    -- * Destructuring the Response
    GetTransitGatewayRegistrationsResponse (..),
    newGetTransitGatewayRegistrationsResponse,

    -- * Response Lenses
    getTransitGatewayRegistrationsResponse_nextToken,
    getTransitGatewayRegistrationsResponse_transitGatewayRegistrations,
    getTransitGatewayRegistrationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTransitGatewayRegistrations' smart constructor.
data GetTransitGatewayRegistrations = GetTransitGatewayRegistrations'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of one or more transit gateways. The
    -- maximum is 10.
    transitGatewayArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRegistrations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayRegistrations_nextToken' - The token for the next page of results.
--
-- 'transitGatewayArns', 'getTransitGatewayRegistrations_transitGatewayArns' - The Amazon Resource Names (ARNs) of one or more transit gateways. The
-- maximum is 10.
--
-- 'maxResults', 'getTransitGatewayRegistrations_maxResults' - The maximum number of results to return.
--
-- 'globalNetworkId', 'getTransitGatewayRegistrations_globalNetworkId' - The ID of the global network.
newGetTransitGatewayRegistrations ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetTransitGatewayRegistrations
newGetTransitGatewayRegistrations pGlobalNetworkId_ =
  GetTransitGatewayRegistrations'
    { nextToken =
        Prelude.Nothing,
      transitGatewayArns = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The token for the next page of results.
getTransitGatewayRegistrations_nextToken :: Lens.Lens' GetTransitGatewayRegistrations (Prelude.Maybe Prelude.Text)
getTransitGatewayRegistrations_nextToken = Lens.lens (\GetTransitGatewayRegistrations' {nextToken} -> nextToken) (\s@GetTransitGatewayRegistrations' {} a -> s {nextToken = a} :: GetTransitGatewayRegistrations)

-- | The Amazon Resource Names (ARNs) of one or more transit gateways. The
-- maximum is 10.
getTransitGatewayRegistrations_transitGatewayArns :: Lens.Lens' GetTransitGatewayRegistrations (Prelude.Maybe [Prelude.Text])
getTransitGatewayRegistrations_transitGatewayArns = Lens.lens (\GetTransitGatewayRegistrations' {transitGatewayArns} -> transitGatewayArns) (\s@GetTransitGatewayRegistrations' {} a -> s {transitGatewayArns = a} :: GetTransitGatewayRegistrations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
getTransitGatewayRegistrations_maxResults :: Lens.Lens' GetTransitGatewayRegistrations (Prelude.Maybe Prelude.Natural)
getTransitGatewayRegistrations_maxResults = Lens.lens (\GetTransitGatewayRegistrations' {maxResults} -> maxResults) (\s@GetTransitGatewayRegistrations' {} a -> s {maxResults = a} :: GetTransitGatewayRegistrations)

-- | The ID of the global network.
getTransitGatewayRegistrations_globalNetworkId :: Lens.Lens' GetTransitGatewayRegistrations Prelude.Text
getTransitGatewayRegistrations_globalNetworkId = Lens.lens (\GetTransitGatewayRegistrations' {globalNetworkId} -> globalNetworkId) (\s@GetTransitGatewayRegistrations' {} a -> s {globalNetworkId = a} :: GetTransitGatewayRegistrations)

instance Core.AWSPager GetTransitGatewayRegistrations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRegistrationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getTransitGatewayRegistrationsResponse_transitGatewayRegistrations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getTransitGatewayRegistrations_nextToken
          Lens..~ rs
          Lens.^? getTransitGatewayRegistrationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetTransitGatewayRegistrations
  where
  type
    AWSResponse GetTransitGatewayRegistrations =
      GetTransitGatewayRegistrationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTransitGatewayRegistrationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "TransitGatewayRegistrations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetTransitGatewayRegistrations
  where
  hashWithSalt
    _salt
    GetTransitGatewayRegistrations' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` transitGatewayArns
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` globalNetworkId

instance
  Prelude.NFData
    GetTransitGatewayRegistrations
  where
  rnf GetTransitGatewayRegistrations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf globalNetworkId

instance
  Core.ToHeaders
    GetTransitGatewayRegistrations
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

instance Core.ToPath GetTransitGatewayRegistrations where
  toPath GetTransitGatewayRegistrations' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/transit-gateway-registrations"
      ]

instance Core.ToQuery GetTransitGatewayRegistrations where
  toQuery GetTransitGatewayRegistrations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "transitGatewayArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> transitGatewayArns
            ),
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetTransitGatewayRegistrationsResponse' smart constructor.
data GetTransitGatewayRegistrationsResponse = GetTransitGatewayRegistrationsResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The transit gateway registrations.
    transitGatewayRegistrations :: Prelude.Maybe [TransitGatewayRegistration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayRegistrationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getTransitGatewayRegistrationsResponse_nextToken' - The token for the next page of results.
--
-- 'transitGatewayRegistrations', 'getTransitGatewayRegistrationsResponse_transitGatewayRegistrations' - The transit gateway registrations.
--
-- 'httpStatus', 'getTransitGatewayRegistrationsResponse_httpStatus' - The response's http status code.
newGetTransitGatewayRegistrationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayRegistrationsResponse
newGetTransitGatewayRegistrationsResponse
  pHttpStatus_ =
    GetTransitGatewayRegistrationsResponse'
      { nextToken =
          Prelude.Nothing,
        transitGatewayRegistrations =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next page of results.
getTransitGatewayRegistrationsResponse_nextToken :: Lens.Lens' GetTransitGatewayRegistrationsResponse (Prelude.Maybe Prelude.Text)
getTransitGatewayRegistrationsResponse_nextToken = Lens.lens (\GetTransitGatewayRegistrationsResponse' {nextToken} -> nextToken) (\s@GetTransitGatewayRegistrationsResponse' {} a -> s {nextToken = a} :: GetTransitGatewayRegistrationsResponse)

-- | The transit gateway registrations.
getTransitGatewayRegistrationsResponse_transitGatewayRegistrations :: Lens.Lens' GetTransitGatewayRegistrationsResponse (Prelude.Maybe [TransitGatewayRegistration])
getTransitGatewayRegistrationsResponse_transitGatewayRegistrations = Lens.lens (\GetTransitGatewayRegistrationsResponse' {transitGatewayRegistrations} -> transitGatewayRegistrations) (\s@GetTransitGatewayRegistrationsResponse' {} a -> s {transitGatewayRegistrations = a} :: GetTransitGatewayRegistrationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getTransitGatewayRegistrationsResponse_httpStatus :: Lens.Lens' GetTransitGatewayRegistrationsResponse Prelude.Int
getTransitGatewayRegistrationsResponse_httpStatus = Lens.lens (\GetTransitGatewayRegistrationsResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayRegistrationsResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayRegistrationsResponse)

instance
  Prelude.NFData
    GetTransitGatewayRegistrationsResponse
  where
  rnf GetTransitGatewayRegistrationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf transitGatewayRegistrations
      `Prelude.seq` Prelude.rnf httpStatus
