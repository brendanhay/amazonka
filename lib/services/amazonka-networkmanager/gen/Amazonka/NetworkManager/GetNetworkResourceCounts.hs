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
-- Module      : Amazonka.NetworkManager.GetNetworkResourceCounts
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the count of network resources, by resource type, for the specified
-- global network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetNetworkResourceCounts
  ( -- * Creating a Request
    GetNetworkResourceCounts (..),
    newGetNetworkResourceCounts,

    -- * Request Lenses
    getNetworkResourceCounts_maxResults,
    getNetworkResourceCounts_nextToken,
    getNetworkResourceCounts_resourceType,
    getNetworkResourceCounts_globalNetworkId,

    -- * Destructuring the Response
    GetNetworkResourceCountsResponse (..),
    newGetNetworkResourceCountsResponse,

    -- * Response Lenses
    getNetworkResourceCountsResponse_networkResourceCounts,
    getNetworkResourceCountsResponse_nextToken,
    getNetworkResourceCountsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkResourceCounts' smart constructor.
data GetNetworkResourceCounts = GetNetworkResourceCounts'
  { -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource type.
    --
    -- The following are the supported resource types for Direct Connect:
    --
    -- -   @dxcon@
    --
    -- -   @dx-gateway@
    --
    -- -   @dx-vif@
    --
    -- The following are the supported resource types for Network Manager:
    --
    -- -   @connection@
    --
    -- -   @device@
    --
    -- -   @link@
    --
    -- -   @site@
    --
    -- The following are the supported resource types for Amazon VPC:
    --
    -- -   @customer-gateway@
    --
    -- -   @transit-gateway@
    --
    -- -   @transit-gateway-attachment@
    --
    -- -   @transit-gateway-connect-peer@
    --
    -- -   @transit-gateway-route-table@
    --
    -- -   @vpn-connection@
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResourceCounts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getNetworkResourceCounts_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getNetworkResourceCounts_nextToken' - The token for the next page of results.
--
-- 'resourceType', 'getNetworkResourceCounts_resourceType' - The resource type.
--
-- The following are the supported resource types for Direct Connect:
--
-- -   @dxcon@
--
-- -   @dx-gateway@
--
-- -   @dx-vif@
--
-- The following are the supported resource types for Network Manager:
--
-- -   @connection@
--
-- -   @device@
--
-- -   @link@
--
-- -   @site@
--
-- The following are the supported resource types for Amazon VPC:
--
-- -   @customer-gateway@
--
-- -   @transit-gateway@
--
-- -   @transit-gateway-attachment@
--
-- -   @transit-gateway-connect-peer@
--
-- -   @transit-gateway-route-table@
--
-- -   @vpn-connection@
--
-- 'globalNetworkId', 'getNetworkResourceCounts_globalNetworkId' - The ID of the global network.
newGetNetworkResourceCounts ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetNetworkResourceCounts
newGetNetworkResourceCounts pGlobalNetworkId_ =
  GetNetworkResourceCounts'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The maximum number of results to return.
getNetworkResourceCounts_maxResults :: Lens.Lens' GetNetworkResourceCounts (Prelude.Maybe Prelude.Natural)
getNetworkResourceCounts_maxResults = Lens.lens (\GetNetworkResourceCounts' {maxResults} -> maxResults) (\s@GetNetworkResourceCounts' {} a -> s {maxResults = a} :: GetNetworkResourceCounts)

-- | The token for the next page of results.
getNetworkResourceCounts_nextToken :: Lens.Lens' GetNetworkResourceCounts (Prelude.Maybe Prelude.Text)
getNetworkResourceCounts_nextToken = Lens.lens (\GetNetworkResourceCounts' {nextToken} -> nextToken) (\s@GetNetworkResourceCounts' {} a -> s {nextToken = a} :: GetNetworkResourceCounts)

-- | The resource type.
--
-- The following are the supported resource types for Direct Connect:
--
-- -   @dxcon@
--
-- -   @dx-gateway@
--
-- -   @dx-vif@
--
-- The following are the supported resource types for Network Manager:
--
-- -   @connection@
--
-- -   @device@
--
-- -   @link@
--
-- -   @site@
--
-- The following are the supported resource types for Amazon VPC:
--
-- -   @customer-gateway@
--
-- -   @transit-gateway@
--
-- -   @transit-gateway-attachment@
--
-- -   @transit-gateway-connect-peer@
--
-- -   @transit-gateway-route-table@
--
-- -   @vpn-connection@
getNetworkResourceCounts_resourceType :: Lens.Lens' GetNetworkResourceCounts (Prelude.Maybe Prelude.Text)
getNetworkResourceCounts_resourceType = Lens.lens (\GetNetworkResourceCounts' {resourceType} -> resourceType) (\s@GetNetworkResourceCounts' {} a -> s {resourceType = a} :: GetNetworkResourceCounts)

-- | The ID of the global network.
getNetworkResourceCounts_globalNetworkId :: Lens.Lens' GetNetworkResourceCounts Prelude.Text
getNetworkResourceCounts_globalNetworkId = Lens.lens (\GetNetworkResourceCounts' {globalNetworkId} -> globalNetworkId) (\s@GetNetworkResourceCounts' {} a -> s {globalNetworkId = a} :: GetNetworkResourceCounts)

instance Core.AWSPager GetNetworkResourceCounts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getNetworkResourceCountsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getNetworkResourceCountsResponse_networkResourceCounts
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& getNetworkResourceCounts_nextToken
              Lens..~ rs
              Lens.^? getNetworkResourceCountsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest GetNetworkResourceCounts where
  type
    AWSResponse GetNetworkResourceCounts =
      GetNetworkResourceCountsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkResourceCountsResponse'
            Prelude.<$> ( x
                            Data..?> "NetworkResourceCounts"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkResourceCounts where
  hashWithSalt _salt GetNetworkResourceCounts' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData GetNetworkResourceCounts where
  rnf GetNetworkResourceCounts' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf resourceType `Prelude.seq`
          Prelude.rnf globalNetworkId

instance Data.ToHeaders GetNetworkResourceCounts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNetworkResourceCounts where
  toPath GetNetworkResourceCounts' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/network-resource-count"
      ]

instance Data.ToQuery GetNetworkResourceCounts where
  toQuery GetNetworkResourceCounts' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "resourceType" Data.=: resourceType
      ]

-- | /See:/ 'newGetNetworkResourceCountsResponse' smart constructor.
data GetNetworkResourceCountsResponse = GetNetworkResourceCountsResponse'
  { -- | The count of resources.
    networkResourceCounts :: Prelude.Maybe [NetworkResourceCount],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResourceCountsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkResourceCounts', 'getNetworkResourceCountsResponse_networkResourceCounts' - The count of resources.
--
-- 'nextToken', 'getNetworkResourceCountsResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getNetworkResourceCountsResponse_httpStatus' - The response's http status code.
newGetNetworkResourceCountsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkResourceCountsResponse
newGetNetworkResourceCountsResponse pHttpStatus_ =
  GetNetworkResourceCountsResponse'
    { networkResourceCounts =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The count of resources.
getNetworkResourceCountsResponse_networkResourceCounts :: Lens.Lens' GetNetworkResourceCountsResponse (Prelude.Maybe [NetworkResourceCount])
getNetworkResourceCountsResponse_networkResourceCounts = Lens.lens (\GetNetworkResourceCountsResponse' {networkResourceCounts} -> networkResourceCounts) (\s@GetNetworkResourceCountsResponse' {} a -> s {networkResourceCounts = a} :: GetNetworkResourceCountsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getNetworkResourceCountsResponse_nextToken :: Lens.Lens' GetNetworkResourceCountsResponse (Prelude.Maybe Prelude.Text)
getNetworkResourceCountsResponse_nextToken = Lens.lens (\GetNetworkResourceCountsResponse' {nextToken} -> nextToken) (\s@GetNetworkResourceCountsResponse' {} a -> s {nextToken = a} :: GetNetworkResourceCountsResponse)

-- | The response's http status code.
getNetworkResourceCountsResponse_httpStatus :: Lens.Lens' GetNetworkResourceCountsResponse Prelude.Int
getNetworkResourceCountsResponse_httpStatus = Lens.lens (\GetNetworkResourceCountsResponse' {httpStatus} -> httpStatus) (\s@GetNetworkResourceCountsResponse' {} a -> s {httpStatus = a} :: GetNetworkResourceCountsResponse)

instance
  Prelude.NFData
    GetNetworkResourceCountsResponse
  where
  rnf GetNetworkResourceCountsResponse' {..} =
    Prelude.rnf networkResourceCounts `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
