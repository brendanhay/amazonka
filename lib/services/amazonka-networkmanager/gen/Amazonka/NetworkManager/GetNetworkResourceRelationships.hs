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
-- Module      : Amazonka.NetworkManager.GetNetworkResourceRelationships
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the network resource relationships for the specified global
-- network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetNetworkResourceRelationships
  ( -- * Creating a Request
    GetNetworkResourceRelationships (..),
    newGetNetworkResourceRelationships,

    -- * Request Lenses
    getNetworkResourceRelationships_resourceType,
    getNetworkResourceRelationships_coreNetworkId,
    getNetworkResourceRelationships_nextToken,
    getNetworkResourceRelationships_accountId,
    getNetworkResourceRelationships_maxResults,
    getNetworkResourceRelationships_registeredGatewayArn,
    getNetworkResourceRelationships_awsRegion,
    getNetworkResourceRelationships_resourceArn,
    getNetworkResourceRelationships_globalNetworkId,

    -- * Destructuring the Response
    GetNetworkResourceRelationshipsResponse (..),
    newGetNetworkResourceRelationshipsResponse,

    -- * Response Lenses
    getNetworkResourceRelationshipsResponse_nextToken,
    getNetworkResourceRelationshipsResponse_relationships,
    getNetworkResourceRelationshipsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkResourceRelationships' smart constructor.
data GetNetworkResourceRelationships = GetNetworkResourceRelationships'
  { -- | The resource type.
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
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the registered gateway.
    registeredGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResourceRelationships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'getNetworkResourceRelationships_resourceType' - The resource type.
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
-- 'coreNetworkId', 'getNetworkResourceRelationships_coreNetworkId' - The ID of a core network.
--
-- 'nextToken', 'getNetworkResourceRelationships_nextToken' - The token for the next page of results.
--
-- 'accountId', 'getNetworkResourceRelationships_accountId' - The Amazon Web Services account ID.
--
-- 'maxResults', 'getNetworkResourceRelationships_maxResults' - The maximum number of results to return.
--
-- 'registeredGatewayArn', 'getNetworkResourceRelationships_registeredGatewayArn' - The ARN of the registered gateway.
--
-- 'awsRegion', 'getNetworkResourceRelationships_awsRegion' - The Amazon Web Services Region.
--
-- 'resourceArn', 'getNetworkResourceRelationships_resourceArn' - The ARN of the gateway.
--
-- 'globalNetworkId', 'getNetworkResourceRelationships_globalNetworkId' - The ID of the global network.
newGetNetworkResourceRelationships ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetNetworkResourceRelationships
newGetNetworkResourceRelationships pGlobalNetworkId_ =
  GetNetworkResourceRelationships'
    { resourceType =
        Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      accountId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      registeredGatewayArn = Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

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
getNetworkResourceRelationships_resourceType :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_resourceType = Lens.lens (\GetNetworkResourceRelationships' {resourceType} -> resourceType) (\s@GetNetworkResourceRelationships' {} a -> s {resourceType = a} :: GetNetworkResourceRelationships)

-- | The ID of a core network.
getNetworkResourceRelationships_coreNetworkId :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_coreNetworkId = Lens.lens (\GetNetworkResourceRelationships' {coreNetworkId} -> coreNetworkId) (\s@GetNetworkResourceRelationships' {} a -> s {coreNetworkId = a} :: GetNetworkResourceRelationships)

-- | The token for the next page of results.
getNetworkResourceRelationships_nextToken :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_nextToken = Lens.lens (\GetNetworkResourceRelationships' {nextToken} -> nextToken) (\s@GetNetworkResourceRelationships' {} a -> s {nextToken = a} :: GetNetworkResourceRelationships)

-- | The Amazon Web Services account ID.
getNetworkResourceRelationships_accountId :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_accountId = Lens.lens (\GetNetworkResourceRelationships' {accountId} -> accountId) (\s@GetNetworkResourceRelationships' {} a -> s {accountId = a} :: GetNetworkResourceRelationships)

-- | The maximum number of results to return.
getNetworkResourceRelationships_maxResults :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Natural)
getNetworkResourceRelationships_maxResults = Lens.lens (\GetNetworkResourceRelationships' {maxResults} -> maxResults) (\s@GetNetworkResourceRelationships' {} a -> s {maxResults = a} :: GetNetworkResourceRelationships)

-- | The ARN of the registered gateway.
getNetworkResourceRelationships_registeredGatewayArn :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_registeredGatewayArn = Lens.lens (\GetNetworkResourceRelationships' {registeredGatewayArn} -> registeredGatewayArn) (\s@GetNetworkResourceRelationships' {} a -> s {registeredGatewayArn = a} :: GetNetworkResourceRelationships)

-- | The Amazon Web Services Region.
getNetworkResourceRelationships_awsRegion :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_awsRegion = Lens.lens (\GetNetworkResourceRelationships' {awsRegion} -> awsRegion) (\s@GetNetworkResourceRelationships' {} a -> s {awsRegion = a} :: GetNetworkResourceRelationships)

-- | The ARN of the gateway.
getNetworkResourceRelationships_resourceArn :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_resourceArn = Lens.lens (\GetNetworkResourceRelationships' {resourceArn} -> resourceArn) (\s@GetNetworkResourceRelationships' {} a -> s {resourceArn = a} :: GetNetworkResourceRelationships)

-- | The ID of the global network.
getNetworkResourceRelationships_globalNetworkId :: Lens.Lens' GetNetworkResourceRelationships Prelude.Text
getNetworkResourceRelationships_globalNetworkId = Lens.lens (\GetNetworkResourceRelationships' {globalNetworkId} -> globalNetworkId) (\s@GetNetworkResourceRelationships' {} a -> s {globalNetworkId = a} :: GetNetworkResourceRelationships)

instance
  Core.AWSPager
    GetNetworkResourceRelationships
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getNetworkResourceRelationshipsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getNetworkResourceRelationshipsResponse_relationships
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getNetworkResourceRelationships_nextToken
          Lens..~ rs
          Lens.^? getNetworkResourceRelationshipsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetNetworkResourceRelationships
  where
  type
    AWSResponse GetNetworkResourceRelationships =
      GetNetworkResourceRelationshipsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkResourceRelationshipsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Relationships" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetNetworkResourceRelationships
  where
  hashWithSalt
    _salt
    GetNetworkResourceRelationships' {..} =
      _salt `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` coreNetworkId
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` registeredGatewayArn
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` globalNetworkId

instance
  Prelude.NFData
    GetNetworkResourceRelationships
  where
  rnf GetNetworkResourceRelationships' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf globalNetworkId

instance
  Core.ToHeaders
    GetNetworkResourceRelationships
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

instance Core.ToPath GetNetworkResourceRelationships where
  toPath GetNetworkResourceRelationships' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Core.toBS globalNetworkId,
        "/network-resource-relationships"
      ]

instance Core.ToQuery GetNetworkResourceRelationships where
  toQuery GetNetworkResourceRelationships' {..} =
    Prelude.mconcat
      [ "resourceType" Core.=: resourceType,
        "coreNetworkId" Core.=: coreNetworkId,
        "nextToken" Core.=: nextToken,
        "accountId" Core.=: accountId,
        "maxResults" Core.=: maxResults,
        "registeredGatewayArn" Core.=: registeredGatewayArn,
        "awsRegion" Core.=: awsRegion,
        "resourceArn" Core.=: resourceArn
      ]

-- | /See:/ 'newGetNetworkResourceRelationshipsResponse' smart constructor.
data GetNetworkResourceRelationshipsResponse = GetNetworkResourceRelationshipsResponse'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The resource relationships.
    relationships :: Prelude.Maybe [Relationship],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResourceRelationshipsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getNetworkResourceRelationshipsResponse_nextToken' - The token for the next page of results.
--
-- 'relationships', 'getNetworkResourceRelationshipsResponse_relationships' - The resource relationships.
--
-- 'httpStatus', 'getNetworkResourceRelationshipsResponse_httpStatus' - The response's http status code.
newGetNetworkResourceRelationshipsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkResourceRelationshipsResponse
newGetNetworkResourceRelationshipsResponse
  pHttpStatus_ =
    GetNetworkResourceRelationshipsResponse'
      { nextToken =
          Prelude.Nothing,
        relationships = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token for the next page of results.
getNetworkResourceRelationshipsResponse_nextToken :: Lens.Lens' GetNetworkResourceRelationshipsResponse (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationshipsResponse_nextToken = Lens.lens (\GetNetworkResourceRelationshipsResponse' {nextToken} -> nextToken) (\s@GetNetworkResourceRelationshipsResponse' {} a -> s {nextToken = a} :: GetNetworkResourceRelationshipsResponse)

-- | The resource relationships.
getNetworkResourceRelationshipsResponse_relationships :: Lens.Lens' GetNetworkResourceRelationshipsResponse (Prelude.Maybe [Relationship])
getNetworkResourceRelationshipsResponse_relationships = Lens.lens (\GetNetworkResourceRelationshipsResponse' {relationships} -> relationships) (\s@GetNetworkResourceRelationshipsResponse' {} a -> s {relationships = a} :: GetNetworkResourceRelationshipsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getNetworkResourceRelationshipsResponse_httpStatus :: Lens.Lens' GetNetworkResourceRelationshipsResponse Prelude.Int
getNetworkResourceRelationshipsResponse_httpStatus = Lens.lens (\GetNetworkResourceRelationshipsResponse' {httpStatus} -> httpStatus) (\s@GetNetworkResourceRelationshipsResponse' {} a -> s {httpStatus = a} :: GetNetworkResourceRelationshipsResponse)

instance
  Prelude.NFData
    GetNetworkResourceRelationshipsResponse
  where
  rnf GetNetworkResourceRelationshipsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf relationships
      `Prelude.seq` Prelude.rnf httpStatus
