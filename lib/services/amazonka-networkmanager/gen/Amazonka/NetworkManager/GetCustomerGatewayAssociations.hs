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
-- Module      : Amazonka.NetworkManager.GetCustomerGatewayAssociations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the association information for customer gateways that are
-- associated with devices and links in your global network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetCustomerGatewayAssociations
  ( -- * Creating a Request
    GetCustomerGatewayAssociations (..),
    newGetCustomerGatewayAssociations,

    -- * Request Lenses
    getCustomerGatewayAssociations_customerGatewayArns,
    getCustomerGatewayAssociations_maxResults,
    getCustomerGatewayAssociations_nextToken,
    getCustomerGatewayAssociations_globalNetworkId,

    -- * Destructuring the Response
    GetCustomerGatewayAssociationsResponse (..),
    newGetCustomerGatewayAssociationsResponse,

    -- * Response Lenses
    getCustomerGatewayAssociationsResponse_customerGatewayAssociations,
    getCustomerGatewayAssociationsResponse_nextToken,
    getCustomerGatewayAssociationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCustomerGatewayAssociations' smart constructor.
data GetCustomerGatewayAssociations = GetCustomerGatewayAssociations'
  { -- | One or more customer gateway Amazon Resource Names (ARNs). The maximum
    -- is 10.
    customerGatewayArns :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCustomerGatewayAssociations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayArns', 'getCustomerGatewayAssociations_customerGatewayArns' - One or more customer gateway Amazon Resource Names (ARNs). The maximum
-- is 10.
--
-- 'maxResults', 'getCustomerGatewayAssociations_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getCustomerGatewayAssociations_nextToken' - The token for the next page of results.
--
-- 'globalNetworkId', 'getCustomerGatewayAssociations_globalNetworkId' - The ID of the global network.
newGetCustomerGatewayAssociations ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetCustomerGatewayAssociations
newGetCustomerGatewayAssociations pGlobalNetworkId_ =
  GetCustomerGatewayAssociations'
    { customerGatewayArns =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | One or more customer gateway Amazon Resource Names (ARNs). The maximum
-- is 10.
getCustomerGatewayAssociations_customerGatewayArns :: Lens.Lens' GetCustomerGatewayAssociations (Prelude.Maybe [Prelude.Text])
getCustomerGatewayAssociations_customerGatewayArns = Lens.lens (\GetCustomerGatewayAssociations' {customerGatewayArns} -> customerGatewayArns) (\s@GetCustomerGatewayAssociations' {} a -> s {customerGatewayArns = a} :: GetCustomerGatewayAssociations) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
getCustomerGatewayAssociations_maxResults :: Lens.Lens' GetCustomerGatewayAssociations (Prelude.Maybe Prelude.Natural)
getCustomerGatewayAssociations_maxResults = Lens.lens (\GetCustomerGatewayAssociations' {maxResults} -> maxResults) (\s@GetCustomerGatewayAssociations' {} a -> s {maxResults = a} :: GetCustomerGatewayAssociations)

-- | The token for the next page of results.
getCustomerGatewayAssociations_nextToken :: Lens.Lens' GetCustomerGatewayAssociations (Prelude.Maybe Prelude.Text)
getCustomerGatewayAssociations_nextToken = Lens.lens (\GetCustomerGatewayAssociations' {nextToken} -> nextToken) (\s@GetCustomerGatewayAssociations' {} a -> s {nextToken = a} :: GetCustomerGatewayAssociations)

-- | The ID of the global network.
getCustomerGatewayAssociations_globalNetworkId :: Lens.Lens' GetCustomerGatewayAssociations Prelude.Text
getCustomerGatewayAssociations_globalNetworkId = Lens.lens (\GetCustomerGatewayAssociations' {globalNetworkId} -> globalNetworkId) (\s@GetCustomerGatewayAssociations' {} a -> s {globalNetworkId = a} :: GetCustomerGatewayAssociations)

instance Core.AWSPager GetCustomerGatewayAssociations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCustomerGatewayAssociationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCustomerGatewayAssociationsResponse_customerGatewayAssociations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCustomerGatewayAssociations_nextToken
          Lens..~ rs
          Lens.^? getCustomerGatewayAssociationsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetCustomerGatewayAssociations
  where
  type
    AWSResponse GetCustomerGatewayAssociations =
      GetCustomerGatewayAssociationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCustomerGatewayAssociationsResponse'
            Prelude.<$> ( x Data..?> "CustomerGatewayAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCustomerGatewayAssociations
  where
  hashWithSalt
    _salt
    GetCustomerGatewayAssociations' {..} =
      _salt `Prelude.hashWithSalt` customerGatewayArns
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` globalNetworkId

instance
  Prelude.NFData
    GetCustomerGatewayAssociations
  where
  rnf GetCustomerGatewayAssociations' {..} =
    Prelude.rnf customerGatewayArns
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf globalNetworkId

instance
  Data.ToHeaders
    GetCustomerGatewayAssociations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetCustomerGatewayAssociations where
  toPath GetCustomerGatewayAssociations' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/customer-gateway-associations"
      ]

instance Data.ToQuery GetCustomerGatewayAssociations where
  toQuery GetCustomerGatewayAssociations' {..} =
    Prelude.mconcat
      [ "customerGatewayArns"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> customerGatewayArns
            ),
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetCustomerGatewayAssociationsResponse' smart constructor.
data GetCustomerGatewayAssociationsResponse = GetCustomerGatewayAssociationsResponse'
  { -- | The customer gateway associations.
    customerGatewayAssociations :: Prelude.Maybe [CustomerGatewayAssociation],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCustomerGatewayAssociationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customerGatewayAssociations', 'getCustomerGatewayAssociationsResponse_customerGatewayAssociations' - The customer gateway associations.
--
-- 'nextToken', 'getCustomerGatewayAssociationsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getCustomerGatewayAssociationsResponse_httpStatus' - The response's http status code.
newGetCustomerGatewayAssociationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCustomerGatewayAssociationsResponse
newGetCustomerGatewayAssociationsResponse
  pHttpStatus_ =
    GetCustomerGatewayAssociationsResponse'
      { customerGatewayAssociations =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The customer gateway associations.
getCustomerGatewayAssociationsResponse_customerGatewayAssociations :: Lens.Lens' GetCustomerGatewayAssociationsResponse (Prelude.Maybe [CustomerGatewayAssociation])
getCustomerGatewayAssociationsResponse_customerGatewayAssociations = Lens.lens (\GetCustomerGatewayAssociationsResponse' {customerGatewayAssociations} -> customerGatewayAssociations) (\s@GetCustomerGatewayAssociationsResponse' {} a -> s {customerGatewayAssociations = a} :: GetCustomerGatewayAssociationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getCustomerGatewayAssociationsResponse_nextToken :: Lens.Lens' GetCustomerGatewayAssociationsResponse (Prelude.Maybe Prelude.Text)
getCustomerGatewayAssociationsResponse_nextToken = Lens.lens (\GetCustomerGatewayAssociationsResponse' {nextToken} -> nextToken) (\s@GetCustomerGatewayAssociationsResponse' {} a -> s {nextToken = a} :: GetCustomerGatewayAssociationsResponse)

-- | The response's http status code.
getCustomerGatewayAssociationsResponse_httpStatus :: Lens.Lens' GetCustomerGatewayAssociationsResponse Prelude.Int
getCustomerGatewayAssociationsResponse_httpStatus = Lens.lens (\GetCustomerGatewayAssociationsResponse' {httpStatus} -> httpStatus) (\s@GetCustomerGatewayAssociationsResponse' {} a -> s {httpStatus = a} :: GetCustomerGatewayAssociationsResponse)

instance
  Prelude.NFData
    GetCustomerGatewayAssociationsResponse
  where
  rnf GetCustomerGatewayAssociationsResponse' {..} =
    Prelude.rnf customerGatewayAssociations
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
