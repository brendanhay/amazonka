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
-- Module      : Amazonka.DirectConnect.DescribeDirectConnectGateways
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all your Direct Connect gateways or only the specified Direct
-- Connect gateway. Deleted Direct Connect gateways are not returned.
--
-- This operation returns paginated results.
module Amazonka.DirectConnect.DescribeDirectConnectGateways
  ( -- * Creating a Request
    DescribeDirectConnectGateways (..),
    newDescribeDirectConnectGateways,

    -- * Request Lenses
    describeDirectConnectGateways_nextToken,
    describeDirectConnectGateways_directConnectGatewayId,
    describeDirectConnectGateways_maxResults,

    -- * Destructuring the Response
    DescribeDirectConnectGatewaysResponse (..),
    newDescribeDirectConnectGatewaysResponse,

    -- * Response Lenses
    describeDirectConnectGatewaysResponse_nextToken,
    describeDirectConnectGatewaysResponse_directConnectGateways,
    describeDirectConnectGatewaysResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DirectConnect.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDirectConnectGateways' smart constructor.
data DescribeDirectConnectGateways = DescribeDirectConnectGateways'
  { -- | The token provided in the previous call to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    --
    -- If @MaxResults@ is given a value larger than 100, only 100 results are
    -- returned.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGateways' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectConnectGateways_nextToken' - The token provided in the previous call to retrieve the next page.
--
-- 'directConnectGatewayId', 'describeDirectConnectGateways_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'maxResults', 'describeDirectConnectGateways_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
newDescribeDirectConnectGateways ::
  DescribeDirectConnectGateways
newDescribeDirectConnectGateways =
  DescribeDirectConnectGateways'
    { nextToken =
        Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token provided in the previous call to retrieve the next page.
describeDirectConnectGateways_nextToken :: Lens.Lens' DescribeDirectConnectGateways (Prelude.Maybe Prelude.Text)
describeDirectConnectGateways_nextToken = Lens.lens (\DescribeDirectConnectGateways' {nextToken} -> nextToken) (\s@DescribeDirectConnectGateways' {} a -> s {nextToken = a} :: DescribeDirectConnectGateways)

-- | The ID of the Direct Connect gateway.
describeDirectConnectGateways_directConnectGatewayId :: Lens.Lens' DescribeDirectConnectGateways (Prelude.Maybe Prelude.Text)
describeDirectConnectGateways_directConnectGatewayId = Lens.lens (\DescribeDirectConnectGateways' {directConnectGatewayId} -> directConnectGatewayId) (\s@DescribeDirectConnectGateways' {} a -> s {directConnectGatewayId = a} :: DescribeDirectConnectGateways)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- If @MaxResults@ is given a value larger than 100, only 100 results are
-- returned.
describeDirectConnectGateways_maxResults :: Lens.Lens' DescribeDirectConnectGateways (Prelude.Maybe Prelude.Int)
describeDirectConnectGateways_maxResults = Lens.lens (\DescribeDirectConnectGateways' {maxResults} -> maxResults) (\s@DescribeDirectConnectGateways' {} a -> s {maxResults = a} :: DescribeDirectConnectGateways)

instance Core.AWSPager DescribeDirectConnectGateways where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewaysResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeDirectConnectGatewaysResponse_directConnectGateways
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeDirectConnectGateways_nextToken
          Lens..~ rs
          Lens.^? describeDirectConnectGatewaysResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeDirectConnectGateways
  where
  type
    AWSResponse DescribeDirectConnectGateways =
      DescribeDirectConnectGatewaysResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDirectConnectGatewaysResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "directConnectGateways"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDirectConnectGateways
  where
  hashWithSalt _salt DescribeDirectConnectGateways' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directConnectGatewayId
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeDirectConnectGateways where
  rnf DescribeDirectConnectGateways' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeDirectConnectGateways where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DescribeDirectConnectGateways" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDirectConnectGateways where
  toJSON DescribeDirectConnectGateways' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("directConnectGatewayId" Core..=)
              Prelude.<$> directConnectGatewayId,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeDirectConnectGateways where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDirectConnectGateways where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDirectConnectGatewaysResponse' smart constructor.
data DescribeDirectConnectGatewaysResponse = DescribeDirectConnectGatewaysResponse'
  { -- | The token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect gateways.
    directConnectGateways :: Prelude.Maybe [DirectConnectGateway],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDirectConnectGatewaysResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeDirectConnectGatewaysResponse_nextToken' - The token to retrieve the next page.
--
-- 'directConnectGateways', 'describeDirectConnectGatewaysResponse_directConnectGateways' - The Direct Connect gateways.
--
-- 'httpStatus', 'describeDirectConnectGatewaysResponse_httpStatus' - The response's http status code.
newDescribeDirectConnectGatewaysResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDirectConnectGatewaysResponse
newDescribeDirectConnectGatewaysResponse pHttpStatus_ =
  DescribeDirectConnectGatewaysResponse'
    { nextToken =
        Prelude.Nothing,
      directConnectGateways =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to retrieve the next page.
describeDirectConnectGatewaysResponse_nextToken :: Lens.Lens' DescribeDirectConnectGatewaysResponse (Prelude.Maybe Prelude.Text)
describeDirectConnectGatewaysResponse_nextToken = Lens.lens (\DescribeDirectConnectGatewaysResponse' {nextToken} -> nextToken) (\s@DescribeDirectConnectGatewaysResponse' {} a -> s {nextToken = a} :: DescribeDirectConnectGatewaysResponse)

-- | The Direct Connect gateways.
describeDirectConnectGatewaysResponse_directConnectGateways :: Lens.Lens' DescribeDirectConnectGatewaysResponse (Prelude.Maybe [DirectConnectGateway])
describeDirectConnectGatewaysResponse_directConnectGateways = Lens.lens (\DescribeDirectConnectGatewaysResponse' {directConnectGateways} -> directConnectGateways) (\s@DescribeDirectConnectGatewaysResponse' {} a -> s {directConnectGateways = a} :: DescribeDirectConnectGatewaysResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDirectConnectGatewaysResponse_httpStatus :: Lens.Lens' DescribeDirectConnectGatewaysResponse Prelude.Int
describeDirectConnectGatewaysResponse_httpStatus = Lens.lens (\DescribeDirectConnectGatewaysResponse' {httpStatus} -> httpStatus) (\s@DescribeDirectConnectGatewaysResponse' {} a -> s {httpStatus = a} :: DescribeDirectConnectGatewaysResponse)

instance
  Prelude.NFData
    DescribeDirectConnectGatewaysResponse
  where
  rnf DescribeDirectConnectGatewaysResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directConnectGateways
      `Prelude.seq` Prelude.rnf httpStatus
