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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
    getNetworkResourceRelationships_accountId,
    getNetworkResourceRelationships_awsRegion,
    getNetworkResourceRelationships_coreNetworkId,
    getNetworkResourceRelationships_maxResults,
    getNetworkResourceRelationships_nextToken,
    getNetworkResourceRelationships_registeredGatewayArn,
    getNetworkResourceRelationships_resourceArn,
    getNetworkResourceRelationships_resourceType,
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
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkResourceRelationships' smart constructor.
data GetNetworkResourceRelationships = GetNetworkResourceRelationships'
  { -- | The Amazon Web Services account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the registered gateway.
    registeredGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the gateway.
    resourceArn :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'GetNetworkResourceRelationships' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getNetworkResourceRelationships_accountId' - The Amazon Web Services account ID.
--
-- 'awsRegion', 'getNetworkResourceRelationships_awsRegion' - The Amazon Web Services Region.
--
-- 'coreNetworkId', 'getNetworkResourceRelationships_coreNetworkId' - The ID of a core network.
--
-- 'maxResults', 'getNetworkResourceRelationships_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getNetworkResourceRelationships_nextToken' - The token for the next page of results.
--
-- 'registeredGatewayArn', 'getNetworkResourceRelationships_registeredGatewayArn' - The ARN of the registered gateway.
--
-- 'resourceArn', 'getNetworkResourceRelationships_resourceArn' - The ARN of the gateway.
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
-- 'globalNetworkId', 'getNetworkResourceRelationships_globalNetworkId' - The ID of the global network.
newGetNetworkResourceRelationships ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetNetworkResourceRelationships
newGetNetworkResourceRelationships pGlobalNetworkId_ =
  GetNetworkResourceRelationships'
    { accountId =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      coreNetworkId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      registeredGatewayArn = Prelude.Nothing,
      resourceArn = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      globalNetworkId = pGlobalNetworkId_
    }

-- | The Amazon Web Services account ID.
getNetworkResourceRelationships_accountId :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_accountId = Lens.lens (\GetNetworkResourceRelationships' {accountId} -> accountId) (\s@GetNetworkResourceRelationships' {} a -> s {accountId = a} :: GetNetworkResourceRelationships)

-- | The Amazon Web Services Region.
getNetworkResourceRelationships_awsRegion :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_awsRegion = Lens.lens (\GetNetworkResourceRelationships' {awsRegion} -> awsRegion) (\s@GetNetworkResourceRelationships' {} a -> s {awsRegion = a} :: GetNetworkResourceRelationships)

-- | The ID of a core network.
getNetworkResourceRelationships_coreNetworkId :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_coreNetworkId = Lens.lens (\GetNetworkResourceRelationships' {coreNetworkId} -> coreNetworkId) (\s@GetNetworkResourceRelationships' {} a -> s {coreNetworkId = a} :: GetNetworkResourceRelationships)

-- | The maximum number of results to return.
getNetworkResourceRelationships_maxResults :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Natural)
getNetworkResourceRelationships_maxResults = Lens.lens (\GetNetworkResourceRelationships' {maxResults} -> maxResults) (\s@GetNetworkResourceRelationships' {} a -> s {maxResults = a} :: GetNetworkResourceRelationships)

-- | The token for the next page of results.
getNetworkResourceRelationships_nextToken :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_nextToken = Lens.lens (\GetNetworkResourceRelationships' {nextToken} -> nextToken) (\s@GetNetworkResourceRelationships' {} a -> s {nextToken = a} :: GetNetworkResourceRelationships)

-- | The ARN of the registered gateway.
getNetworkResourceRelationships_registeredGatewayArn :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_registeredGatewayArn = Lens.lens (\GetNetworkResourceRelationships' {registeredGatewayArn} -> registeredGatewayArn) (\s@GetNetworkResourceRelationships' {} a -> s {registeredGatewayArn = a} :: GetNetworkResourceRelationships)

-- | The ARN of the gateway.
getNetworkResourceRelationships_resourceArn :: Lens.Lens' GetNetworkResourceRelationships (Prelude.Maybe Prelude.Text)
getNetworkResourceRelationships_resourceArn = Lens.lens (\GetNetworkResourceRelationships' {resourceArn} -> resourceArn) (\s@GetNetworkResourceRelationships' {} a -> s {resourceArn = a} :: GetNetworkResourceRelationships)

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
        Prelude.Just
          Prelude.$ rq
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
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Relationships" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetNetworkResourceRelationships
  where
  hashWithSalt
    _salt
    GetNetworkResourceRelationships' {..} =
      _salt
        `Prelude.hashWithSalt` accountId
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` coreNetworkId
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` registeredGatewayArn
        `Prelude.hashWithSalt` resourceArn
        `Prelude.hashWithSalt` resourceType
        `Prelude.hashWithSalt` globalNetworkId

instance
  Prelude.NFData
    GetNetworkResourceRelationships
  where
  rnf GetNetworkResourceRelationships' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf globalNetworkId

instance
  Data.ToHeaders
    GetNetworkResourceRelationships
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

instance Data.ToPath GetNetworkResourceRelationships where
  toPath GetNetworkResourceRelationships' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/network-resource-relationships"
      ]

instance Data.ToQuery GetNetworkResourceRelationships where
  toQuery GetNetworkResourceRelationships' {..} =
    Prelude.mconcat
      [ "accountId" Data.=: accountId,
        "awsRegion" Data.=: awsRegion,
        "coreNetworkId" Data.=: coreNetworkId,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "registeredGatewayArn" Data.=: registeredGatewayArn,
        "resourceArn" Data.=: resourceArn,
        "resourceType" Data.=: resourceType
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
