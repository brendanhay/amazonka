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
-- Module      : Amazonka.Route53Resolver.ListResolverEndpointIpAddresses
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the IP addresses for a specified Resolver endpoint.
--
-- This operation returns paginated results.
module Amazonka.Route53Resolver.ListResolverEndpointIpAddresses
  ( -- * Creating a Request
    ListResolverEndpointIpAddresses (..),
    newListResolverEndpointIpAddresses,

    -- * Request Lenses
    listResolverEndpointIpAddresses_nextToken,
    listResolverEndpointIpAddresses_maxResults,
    listResolverEndpointIpAddresses_resolverEndpointId,

    -- * Destructuring the Response
    ListResolverEndpointIpAddressesResponse (..),
    newListResolverEndpointIpAddressesResponse,

    -- * Response Lenses
    listResolverEndpointIpAddressesResponse_nextToken,
    listResolverEndpointIpAddressesResponse_maxResults,
    listResolverEndpointIpAddressesResponse_ipAddresses,
    listResolverEndpointIpAddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newListResolverEndpointIpAddresses' smart constructor.
data ListResolverEndpointIpAddresses = ListResolverEndpointIpAddresses'
  { -- | For the first @ListResolverEndpointIpAddresses@ request, omit this
    -- value.
    --
    -- If the specified Resolver endpoint has more than @MaxResults@ IP
    -- addresses, you can submit another @ListResolverEndpointIpAddresses@
    -- request to get the next group of IP addresses. In the next request,
    -- specify the value of @NextToken@ from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of IP addresses that you want to return in the
    -- response to a @ListResolverEndpointIpAddresses@ request. If you don\'t
    -- specify a value for @MaxResults@, Resolver returns up to 100 IP
    -- addresses.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Resolver endpoint that you want to get IP addresses for.
    resolverEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverEndpointIpAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolverEndpointIpAddresses_nextToken' - For the first @ListResolverEndpointIpAddresses@ request, omit this
-- value.
--
-- If the specified Resolver endpoint has more than @MaxResults@ IP
-- addresses, you can submit another @ListResolverEndpointIpAddresses@
-- request to get the next group of IP addresses. In the next request,
-- specify the value of @NextToken@ from the previous response.
--
-- 'maxResults', 'listResolverEndpointIpAddresses_maxResults' - The maximum number of IP addresses that you want to return in the
-- response to a @ListResolverEndpointIpAddresses@ request. If you don\'t
-- specify a value for @MaxResults@, Resolver returns up to 100 IP
-- addresses.
--
-- 'resolverEndpointId', 'listResolverEndpointIpAddresses_resolverEndpointId' - The ID of the Resolver endpoint that you want to get IP addresses for.
newListResolverEndpointIpAddresses ::
  -- | 'resolverEndpointId'
  Prelude.Text ->
  ListResolverEndpointIpAddresses
newListResolverEndpointIpAddresses
  pResolverEndpointId_ =
    ListResolverEndpointIpAddresses'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        resolverEndpointId = pResolverEndpointId_
      }

-- | For the first @ListResolverEndpointIpAddresses@ request, omit this
-- value.
--
-- If the specified Resolver endpoint has more than @MaxResults@ IP
-- addresses, you can submit another @ListResolverEndpointIpAddresses@
-- request to get the next group of IP addresses. In the next request,
-- specify the value of @NextToken@ from the previous response.
listResolverEndpointIpAddresses_nextToken :: Lens.Lens' ListResolverEndpointIpAddresses (Prelude.Maybe Prelude.Text)
listResolverEndpointIpAddresses_nextToken = Lens.lens (\ListResolverEndpointIpAddresses' {nextToken} -> nextToken) (\s@ListResolverEndpointIpAddresses' {} a -> s {nextToken = a} :: ListResolverEndpointIpAddresses)

-- | The maximum number of IP addresses that you want to return in the
-- response to a @ListResolverEndpointIpAddresses@ request. If you don\'t
-- specify a value for @MaxResults@, Resolver returns up to 100 IP
-- addresses.
listResolverEndpointIpAddresses_maxResults :: Lens.Lens' ListResolverEndpointIpAddresses (Prelude.Maybe Prelude.Natural)
listResolverEndpointIpAddresses_maxResults = Lens.lens (\ListResolverEndpointIpAddresses' {maxResults} -> maxResults) (\s@ListResolverEndpointIpAddresses' {} a -> s {maxResults = a} :: ListResolverEndpointIpAddresses)

-- | The ID of the Resolver endpoint that you want to get IP addresses for.
listResolverEndpointIpAddresses_resolverEndpointId :: Lens.Lens' ListResolverEndpointIpAddresses Prelude.Text
listResolverEndpointIpAddresses_resolverEndpointId = Lens.lens (\ListResolverEndpointIpAddresses' {resolverEndpointId} -> resolverEndpointId) (\s@ListResolverEndpointIpAddresses' {} a -> s {resolverEndpointId = a} :: ListResolverEndpointIpAddresses)

instance
  Core.AWSPager
    ListResolverEndpointIpAddresses
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listResolverEndpointIpAddressesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listResolverEndpointIpAddressesResponse_ipAddresses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listResolverEndpointIpAddresses_nextToken
          Lens..~ rs
          Lens.^? listResolverEndpointIpAddressesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListResolverEndpointIpAddresses
  where
  type
    AWSResponse ListResolverEndpointIpAddresses =
      ListResolverEndpointIpAddressesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListResolverEndpointIpAddressesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "MaxResults")
            Prelude.<*> (x Data..?> "IpAddresses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListResolverEndpointIpAddresses
  where
  hashWithSalt
    _salt
    ListResolverEndpointIpAddresses' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` resolverEndpointId

instance
  Prelude.NFData
    ListResolverEndpointIpAddresses
  where
  rnf ListResolverEndpointIpAddresses' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resolverEndpointId

instance
  Data.ToHeaders
    ListResolverEndpointIpAddresses
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.ListResolverEndpointIpAddresses" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListResolverEndpointIpAddresses where
  toJSON ListResolverEndpointIpAddresses' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just
              ("ResolverEndpointId" Data..= resolverEndpointId)
          ]
      )

instance Data.ToPath ListResolverEndpointIpAddresses where
  toPath = Prelude.const "/"

instance Data.ToQuery ListResolverEndpointIpAddresses where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListResolverEndpointIpAddressesResponse' smart constructor.
data ListResolverEndpointIpAddressesResponse = ListResolverEndpointIpAddressesResponse'
  { -- | If the specified endpoint has more than @MaxResults@ IP addresses, you
    -- can submit another @ListResolverEndpointIpAddresses@ request to get the
    -- next group of IP addresses. In the next request, specify the value of
    -- @NextToken@ from the previous response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The value that you specified for @MaxResults@ in the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Information about the IP addresses in your VPC that DNS queries
    -- originate from (for outbound endpoints) or that you forward DNS queries
    -- to (for inbound endpoints).
    ipAddresses :: Prelude.Maybe [IpAddressResponse],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListResolverEndpointIpAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listResolverEndpointIpAddressesResponse_nextToken' - If the specified endpoint has more than @MaxResults@ IP addresses, you
-- can submit another @ListResolverEndpointIpAddresses@ request to get the
-- next group of IP addresses. In the next request, specify the value of
-- @NextToken@ from the previous response.
--
-- 'maxResults', 'listResolverEndpointIpAddressesResponse_maxResults' - The value that you specified for @MaxResults@ in the request.
--
-- 'ipAddresses', 'listResolverEndpointIpAddressesResponse_ipAddresses' - Information about the IP addresses in your VPC that DNS queries
-- originate from (for outbound endpoints) or that you forward DNS queries
-- to (for inbound endpoints).
--
-- 'httpStatus', 'listResolverEndpointIpAddressesResponse_httpStatus' - The response's http status code.
newListResolverEndpointIpAddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListResolverEndpointIpAddressesResponse
newListResolverEndpointIpAddressesResponse
  pHttpStatus_ =
    ListResolverEndpointIpAddressesResponse'
      { nextToken =
          Prelude.Nothing,
        maxResults = Prelude.Nothing,
        ipAddresses = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the specified endpoint has more than @MaxResults@ IP addresses, you
-- can submit another @ListResolverEndpointIpAddresses@ request to get the
-- next group of IP addresses. In the next request, specify the value of
-- @NextToken@ from the previous response.
listResolverEndpointIpAddressesResponse_nextToken :: Lens.Lens' ListResolverEndpointIpAddressesResponse (Prelude.Maybe Prelude.Text)
listResolverEndpointIpAddressesResponse_nextToken = Lens.lens (\ListResolverEndpointIpAddressesResponse' {nextToken} -> nextToken) (\s@ListResolverEndpointIpAddressesResponse' {} a -> s {nextToken = a} :: ListResolverEndpointIpAddressesResponse)

-- | The value that you specified for @MaxResults@ in the request.
listResolverEndpointIpAddressesResponse_maxResults :: Lens.Lens' ListResolverEndpointIpAddressesResponse (Prelude.Maybe Prelude.Natural)
listResolverEndpointIpAddressesResponse_maxResults = Lens.lens (\ListResolverEndpointIpAddressesResponse' {maxResults} -> maxResults) (\s@ListResolverEndpointIpAddressesResponse' {} a -> s {maxResults = a} :: ListResolverEndpointIpAddressesResponse)

-- | Information about the IP addresses in your VPC that DNS queries
-- originate from (for outbound endpoints) or that you forward DNS queries
-- to (for inbound endpoints).
listResolverEndpointIpAddressesResponse_ipAddresses :: Lens.Lens' ListResolverEndpointIpAddressesResponse (Prelude.Maybe [IpAddressResponse])
listResolverEndpointIpAddressesResponse_ipAddresses = Lens.lens (\ListResolverEndpointIpAddressesResponse' {ipAddresses} -> ipAddresses) (\s@ListResolverEndpointIpAddressesResponse' {} a -> s {ipAddresses = a} :: ListResolverEndpointIpAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listResolverEndpointIpAddressesResponse_httpStatus :: Lens.Lens' ListResolverEndpointIpAddressesResponse Prelude.Int
listResolverEndpointIpAddressesResponse_httpStatus = Lens.lens (\ListResolverEndpointIpAddressesResponse' {httpStatus} -> httpStatus) (\s@ListResolverEndpointIpAddressesResponse' {} a -> s {httpStatus = a} :: ListResolverEndpointIpAddressesResponse)

instance
  Prelude.NFData
    ListResolverEndpointIpAddressesResponse
  where
  rnf ListResolverEndpointIpAddressesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf ipAddresses
      `Prelude.seq` Prelude.rnf httpStatus
