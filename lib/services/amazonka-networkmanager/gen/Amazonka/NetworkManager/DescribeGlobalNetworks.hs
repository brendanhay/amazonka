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
-- Module      : Amazonka.NetworkManager.DescribeGlobalNetworks
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more global networks. By default, all global networks
-- are described. To describe the objects in your global network, you must
-- use the appropriate @Get*@ action. For example, to list the transit
-- gateways in your global network, use GetTransitGatewayRegistrations.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.DescribeGlobalNetworks
  ( -- * Creating a Request
    DescribeGlobalNetworks (..),
    newDescribeGlobalNetworks,

    -- * Request Lenses
    describeGlobalNetworks_nextToken,
    describeGlobalNetworks_globalNetworkIds,
    describeGlobalNetworks_maxResults,

    -- * Destructuring the Response
    DescribeGlobalNetworksResponse (..),
    newDescribeGlobalNetworksResponse,

    -- * Response Lenses
    describeGlobalNetworksResponse_nextToken,
    describeGlobalNetworksResponse_globalNetworks,
    describeGlobalNetworksResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeGlobalNetworks' smart constructor.
data DescribeGlobalNetworks = DescribeGlobalNetworks'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The IDs of one or more global networks. The maximum is 10.
    globalNetworkIds :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalNetworks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGlobalNetworks_nextToken' - The token for the next page of results.
--
-- 'globalNetworkIds', 'describeGlobalNetworks_globalNetworkIds' - The IDs of one or more global networks. The maximum is 10.
--
-- 'maxResults', 'describeGlobalNetworks_maxResults' - The maximum number of results to return.
newDescribeGlobalNetworks ::
  DescribeGlobalNetworks
newDescribeGlobalNetworks =
  DescribeGlobalNetworks'
    { nextToken =
        Prelude.Nothing,
      globalNetworkIds = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
describeGlobalNetworks_nextToken :: Lens.Lens' DescribeGlobalNetworks (Prelude.Maybe Prelude.Text)
describeGlobalNetworks_nextToken = Lens.lens (\DescribeGlobalNetworks' {nextToken} -> nextToken) (\s@DescribeGlobalNetworks' {} a -> s {nextToken = a} :: DescribeGlobalNetworks)

-- | The IDs of one or more global networks. The maximum is 10.
describeGlobalNetworks_globalNetworkIds :: Lens.Lens' DescribeGlobalNetworks (Prelude.Maybe [Prelude.Text])
describeGlobalNetworks_globalNetworkIds = Lens.lens (\DescribeGlobalNetworks' {globalNetworkIds} -> globalNetworkIds) (\s@DescribeGlobalNetworks' {} a -> s {globalNetworkIds = a} :: DescribeGlobalNetworks) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return.
describeGlobalNetworks_maxResults :: Lens.Lens' DescribeGlobalNetworks (Prelude.Maybe Prelude.Natural)
describeGlobalNetworks_maxResults = Lens.lens (\DescribeGlobalNetworks' {maxResults} -> maxResults) (\s@DescribeGlobalNetworks' {} a -> s {maxResults = a} :: DescribeGlobalNetworks)

instance Core.AWSPager DescribeGlobalNetworks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeGlobalNetworksResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeGlobalNetworksResponse_globalNetworks
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeGlobalNetworks_nextToken
          Lens..~ rs
          Lens.^? describeGlobalNetworksResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeGlobalNetworks where
  type
    AWSResponse DescribeGlobalNetworks =
      DescribeGlobalNetworksResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeGlobalNetworksResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "GlobalNetworks" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeGlobalNetworks where
  hashWithSalt _salt DescribeGlobalNetworks' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` globalNetworkIds
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData DescribeGlobalNetworks where
  rnf DescribeGlobalNetworks' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf globalNetworkIds
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders DescribeGlobalNetworks where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeGlobalNetworks where
  toPath = Prelude.const "/global-networks"

instance Core.ToQuery DescribeGlobalNetworks where
  toQuery DescribeGlobalNetworks' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "globalNetworkIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> globalNetworkIds
            ),
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeGlobalNetworksResponse' smart constructor.
data DescribeGlobalNetworksResponse = DescribeGlobalNetworksResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the global networks.
    globalNetworks :: Prelude.Maybe [GlobalNetwork],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeGlobalNetworksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeGlobalNetworksResponse_nextToken' - The token for the next page of results.
--
-- 'globalNetworks', 'describeGlobalNetworksResponse_globalNetworks' - Information about the global networks.
--
-- 'httpStatus', 'describeGlobalNetworksResponse_httpStatus' - The response's http status code.
newDescribeGlobalNetworksResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeGlobalNetworksResponse
newDescribeGlobalNetworksResponse pHttpStatus_ =
  DescribeGlobalNetworksResponse'
    { nextToken =
        Prelude.Nothing,
      globalNetworks = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next page of results.
describeGlobalNetworksResponse_nextToken :: Lens.Lens' DescribeGlobalNetworksResponse (Prelude.Maybe Prelude.Text)
describeGlobalNetworksResponse_nextToken = Lens.lens (\DescribeGlobalNetworksResponse' {nextToken} -> nextToken) (\s@DescribeGlobalNetworksResponse' {} a -> s {nextToken = a} :: DescribeGlobalNetworksResponse)

-- | Information about the global networks.
describeGlobalNetworksResponse_globalNetworks :: Lens.Lens' DescribeGlobalNetworksResponse (Prelude.Maybe [GlobalNetwork])
describeGlobalNetworksResponse_globalNetworks = Lens.lens (\DescribeGlobalNetworksResponse' {globalNetworks} -> globalNetworks) (\s@DescribeGlobalNetworksResponse' {} a -> s {globalNetworks = a} :: DescribeGlobalNetworksResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeGlobalNetworksResponse_httpStatus :: Lens.Lens' DescribeGlobalNetworksResponse Prelude.Int
describeGlobalNetworksResponse_httpStatus = Lens.lens (\DescribeGlobalNetworksResponse' {httpStatus} -> httpStatus) (\s@DescribeGlobalNetworksResponse' {} a -> s {httpStatus = a} :: DescribeGlobalNetworksResponse)

instance
  Prelude.NFData
    DescribeGlobalNetworksResponse
  where
  rnf DescribeGlobalNetworksResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf globalNetworks
      `Prelude.seq` Prelude.rnf httpStatus
