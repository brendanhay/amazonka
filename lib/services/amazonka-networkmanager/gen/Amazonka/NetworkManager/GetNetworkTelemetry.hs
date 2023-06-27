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
-- Module      : Amazonka.NetworkManager.GetNetworkTelemetry
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the network telemetry of the specified global network.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetNetworkTelemetry
  ( -- * Creating a Request
    GetNetworkTelemetry (..),
    newGetNetworkTelemetry,

    -- * Request Lenses
    getNetworkTelemetry_accountId,
    getNetworkTelemetry_awsRegion,
    getNetworkTelemetry_coreNetworkId,
    getNetworkTelemetry_maxResults,
    getNetworkTelemetry_nextToken,
    getNetworkTelemetry_registeredGatewayArn,
    getNetworkTelemetry_resourceArn,
    getNetworkTelemetry_resourceType,
    getNetworkTelemetry_globalNetworkId,

    -- * Destructuring the Response
    GetNetworkTelemetryResponse (..),
    newGetNetworkTelemetryResponse,

    -- * Response Lenses
    getNetworkTelemetryResponse_networkTelemetry,
    getNetworkTelemetryResponse_nextToken,
    getNetworkTelemetryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkTelemetry' smart constructor.
data GetNetworkTelemetry = GetNetworkTelemetry'
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
    -- | The ARN of the gateway.
    registeredGatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the resource.
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
-- Create a value of 'GetNetworkTelemetry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getNetworkTelemetry_accountId' - The Amazon Web Services account ID.
--
-- 'awsRegion', 'getNetworkTelemetry_awsRegion' - The Amazon Web Services Region.
--
-- 'coreNetworkId', 'getNetworkTelemetry_coreNetworkId' - The ID of a core network.
--
-- 'maxResults', 'getNetworkTelemetry_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getNetworkTelemetry_nextToken' - The token for the next page of results.
--
-- 'registeredGatewayArn', 'getNetworkTelemetry_registeredGatewayArn' - The ARN of the gateway.
--
-- 'resourceArn', 'getNetworkTelemetry_resourceArn' - The ARN of the resource.
--
-- 'resourceType', 'getNetworkTelemetry_resourceType' - The resource type.
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
-- 'globalNetworkId', 'getNetworkTelemetry_globalNetworkId' - The ID of the global network.
newGetNetworkTelemetry ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetNetworkTelemetry
newGetNetworkTelemetry pGlobalNetworkId_ =
  GetNetworkTelemetry'
    { accountId = Prelude.Nothing,
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
getNetworkTelemetry_accountId :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_accountId = Lens.lens (\GetNetworkTelemetry' {accountId} -> accountId) (\s@GetNetworkTelemetry' {} a -> s {accountId = a} :: GetNetworkTelemetry)

-- | The Amazon Web Services Region.
getNetworkTelemetry_awsRegion :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_awsRegion = Lens.lens (\GetNetworkTelemetry' {awsRegion} -> awsRegion) (\s@GetNetworkTelemetry' {} a -> s {awsRegion = a} :: GetNetworkTelemetry)

-- | The ID of a core network.
getNetworkTelemetry_coreNetworkId :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_coreNetworkId = Lens.lens (\GetNetworkTelemetry' {coreNetworkId} -> coreNetworkId) (\s@GetNetworkTelemetry' {} a -> s {coreNetworkId = a} :: GetNetworkTelemetry)

-- | The maximum number of results to return.
getNetworkTelemetry_maxResults :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Natural)
getNetworkTelemetry_maxResults = Lens.lens (\GetNetworkTelemetry' {maxResults} -> maxResults) (\s@GetNetworkTelemetry' {} a -> s {maxResults = a} :: GetNetworkTelemetry)

-- | The token for the next page of results.
getNetworkTelemetry_nextToken :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_nextToken = Lens.lens (\GetNetworkTelemetry' {nextToken} -> nextToken) (\s@GetNetworkTelemetry' {} a -> s {nextToken = a} :: GetNetworkTelemetry)

-- | The ARN of the gateway.
getNetworkTelemetry_registeredGatewayArn :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_registeredGatewayArn = Lens.lens (\GetNetworkTelemetry' {registeredGatewayArn} -> registeredGatewayArn) (\s@GetNetworkTelemetry' {} a -> s {registeredGatewayArn = a} :: GetNetworkTelemetry)

-- | The ARN of the resource.
getNetworkTelemetry_resourceArn :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_resourceArn = Lens.lens (\GetNetworkTelemetry' {resourceArn} -> resourceArn) (\s@GetNetworkTelemetry' {} a -> s {resourceArn = a} :: GetNetworkTelemetry)

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
getNetworkTelemetry_resourceType :: Lens.Lens' GetNetworkTelemetry (Prelude.Maybe Prelude.Text)
getNetworkTelemetry_resourceType = Lens.lens (\GetNetworkTelemetry' {resourceType} -> resourceType) (\s@GetNetworkTelemetry' {} a -> s {resourceType = a} :: GetNetworkTelemetry)

-- | The ID of the global network.
getNetworkTelemetry_globalNetworkId :: Lens.Lens' GetNetworkTelemetry Prelude.Text
getNetworkTelemetry_globalNetworkId = Lens.lens (\GetNetworkTelemetry' {globalNetworkId} -> globalNetworkId) (\s@GetNetworkTelemetry' {} a -> s {globalNetworkId = a} :: GetNetworkTelemetry)

instance Core.AWSPager GetNetworkTelemetry where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getNetworkTelemetryResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getNetworkTelemetryResponse_networkTelemetry
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getNetworkTelemetry_nextToken
          Lens..~ rs
          Lens.^? getNetworkTelemetryResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetNetworkTelemetry where
  type
    AWSResponse GetNetworkTelemetry =
      GetNetworkTelemetryResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkTelemetryResponse'
            Prelude.<$> ( x
                            Data..?> "NetworkTelemetry"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkTelemetry where
  hashWithSalt _salt GetNetworkTelemetry' {..} =
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

instance Prelude.NFData GetNetworkTelemetry where
  rnf GetNetworkTelemetry' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders GetNetworkTelemetry where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNetworkTelemetry where
  toPath GetNetworkTelemetry' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/network-telemetry"
      ]

instance Data.ToQuery GetNetworkTelemetry where
  toQuery GetNetworkTelemetry' {..} =
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

-- | /See:/ 'newGetNetworkTelemetryResponse' smart constructor.
data GetNetworkTelemetryResponse = GetNetworkTelemetryResponse'
  { -- | The network telemetry.
    networkTelemetry :: Prelude.Maybe [NetworkTelemetry],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkTelemetryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkTelemetry', 'getNetworkTelemetryResponse_networkTelemetry' - The network telemetry.
--
-- 'nextToken', 'getNetworkTelemetryResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getNetworkTelemetryResponse_httpStatus' - The response's http status code.
newGetNetworkTelemetryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkTelemetryResponse
newGetNetworkTelemetryResponse pHttpStatus_ =
  GetNetworkTelemetryResponse'
    { networkTelemetry =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network telemetry.
getNetworkTelemetryResponse_networkTelemetry :: Lens.Lens' GetNetworkTelemetryResponse (Prelude.Maybe [NetworkTelemetry])
getNetworkTelemetryResponse_networkTelemetry = Lens.lens (\GetNetworkTelemetryResponse' {networkTelemetry} -> networkTelemetry) (\s@GetNetworkTelemetryResponse' {} a -> s {networkTelemetry = a} :: GetNetworkTelemetryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getNetworkTelemetryResponse_nextToken :: Lens.Lens' GetNetworkTelemetryResponse (Prelude.Maybe Prelude.Text)
getNetworkTelemetryResponse_nextToken = Lens.lens (\GetNetworkTelemetryResponse' {nextToken} -> nextToken) (\s@GetNetworkTelemetryResponse' {} a -> s {nextToken = a} :: GetNetworkTelemetryResponse)

-- | The response's http status code.
getNetworkTelemetryResponse_httpStatus :: Lens.Lens' GetNetworkTelemetryResponse Prelude.Int
getNetworkTelemetryResponse_httpStatus = Lens.lens (\GetNetworkTelemetryResponse' {httpStatus} -> httpStatus) (\s@GetNetworkTelemetryResponse' {} a -> s {httpStatus = a} :: GetNetworkTelemetryResponse)

instance Prelude.NFData GetNetworkTelemetryResponse where
  rnf GetNetworkTelemetryResponse' {..} =
    Prelude.rnf networkTelemetry
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
