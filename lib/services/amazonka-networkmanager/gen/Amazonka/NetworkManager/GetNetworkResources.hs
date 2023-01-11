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
-- Module      : Amazonka.NetworkManager.GetNetworkResources
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the network resources for the specified global network.
--
-- The results include information from the corresponding Describe call for
-- the resource, minus any sensitive information such as pre-shared keys.
--
-- This operation returns paginated results.
module Amazonka.NetworkManager.GetNetworkResources
  ( -- * Creating a Request
    GetNetworkResources (..),
    newGetNetworkResources,

    -- * Request Lenses
    getNetworkResources_accountId,
    getNetworkResources_awsRegion,
    getNetworkResources_coreNetworkId,
    getNetworkResources_maxResults,
    getNetworkResources_nextToken,
    getNetworkResources_registeredGatewayArn,
    getNetworkResources_resourceArn,
    getNetworkResources_resourceType,
    getNetworkResources_globalNetworkId,

    -- * Destructuring the Response
    GetNetworkResourcesResponse (..),
    newGetNetworkResourcesResponse,

    -- * Response Lenses
    getNetworkResourcesResponse_networkResources,
    getNetworkResourcesResponse_nextToken,
    getNetworkResourcesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetNetworkResources' smart constructor.
data GetNetworkResources = GetNetworkResources'
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
    -- -   @dxcon@ - The definition model is
    --     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_Connection.html Connection>.
    --
    -- -   @dx-gateway@ - The definition model is
    --     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_DirectConnectGateway.html DirectConnectGateway>.
    --
    -- -   @dx-vif@ - The definition model is
    --     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_VirtualInterface.html VirtualInterface>.
    --
    -- The following are the supported resource types for Network Manager:
    --
    -- -   @connection@ - The definition model is
    --     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Connection.html Connection>.
    --
    -- -   @device@ - The definition model is
    --     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Device.html Device>.
    --
    -- -   @link@ - The definition model is
    --     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Link.html Link>.
    --
    -- -   @site@ - The definition model is
    --     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Site.html Site>.
    --
    -- The following are the supported resource types for Amazon VPC:
    --
    -- -   @customer-gateway@ - The definition model is
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CustomerGateway.html CustomerGateway>.
    --
    -- -   @transit-gateway@ - The definition model is
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGateway.html TransitGateway>.
    --
    -- -   @transit-gateway-attachment@ - The definition model is
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayAttachment.html TransitGatewayAttachment>.
    --
    -- -   @transit-gateway-connect-peer@ - The definition model is
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayConnectPeer.html TransitGatewayConnectPeer>.
    --
    -- -   @transit-gateway-route-table@ - The definition model is
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayRouteTable.html TransitGatewayRouteTable>.
    --
    -- -   @vpn-connection@ - The definition model is
    --     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpnConnection.html VpnConnection>.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResources' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getNetworkResources_accountId' - The Amazon Web Services account ID.
--
-- 'awsRegion', 'getNetworkResources_awsRegion' - The Amazon Web Services Region.
--
-- 'coreNetworkId', 'getNetworkResources_coreNetworkId' - The ID of a core network.
--
-- 'maxResults', 'getNetworkResources_maxResults' - The maximum number of results to return.
--
-- 'nextToken', 'getNetworkResources_nextToken' - The token for the next page of results.
--
-- 'registeredGatewayArn', 'getNetworkResources_registeredGatewayArn' - The ARN of the gateway.
--
-- 'resourceArn', 'getNetworkResources_resourceArn' - The ARN of the resource.
--
-- 'resourceType', 'getNetworkResources_resourceType' - The resource type.
--
-- The following are the supported resource types for Direct Connect:
--
-- -   @dxcon@ - The definition model is
--     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_Connection.html Connection>.
--
-- -   @dx-gateway@ - The definition model is
--     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_DirectConnectGateway.html DirectConnectGateway>.
--
-- -   @dx-vif@ - The definition model is
--     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_VirtualInterface.html VirtualInterface>.
--
-- The following are the supported resource types for Network Manager:
--
-- -   @connection@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Connection.html Connection>.
--
-- -   @device@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Device.html Device>.
--
-- -   @link@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Link.html Link>.
--
-- -   @site@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Site.html Site>.
--
-- The following are the supported resource types for Amazon VPC:
--
-- -   @customer-gateway@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CustomerGateway.html CustomerGateway>.
--
-- -   @transit-gateway@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGateway.html TransitGateway>.
--
-- -   @transit-gateway-attachment@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayAttachment.html TransitGatewayAttachment>.
--
-- -   @transit-gateway-connect-peer@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayConnectPeer.html TransitGatewayConnectPeer>.
--
-- -   @transit-gateway-route-table@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayRouteTable.html TransitGatewayRouteTable>.
--
-- -   @vpn-connection@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpnConnection.html VpnConnection>.
--
-- 'globalNetworkId', 'getNetworkResources_globalNetworkId' - The ID of the global network.
newGetNetworkResources ::
  -- | 'globalNetworkId'
  Prelude.Text ->
  GetNetworkResources
newGetNetworkResources pGlobalNetworkId_ =
  GetNetworkResources'
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
getNetworkResources_accountId :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_accountId = Lens.lens (\GetNetworkResources' {accountId} -> accountId) (\s@GetNetworkResources' {} a -> s {accountId = a} :: GetNetworkResources)

-- | The Amazon Web Services Region.
getNetworkResources_awsRegion :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_awsRegion = Lens.lens (\GetNetworkResources' {awsRegion} -> awsRegion) (\s@GetNetworkResources' {} a -> s {awsRegion = a} :: GetNetworkResources)

-- | The ID of a core network.
getNetworkResources_coreNetworkId :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_coreNetworkId = Lens.lens (\GetNetworkResources' {coreNetworkId} -> coreNetworkId) (\s@GetNetworkResources' {} a -> s {coreNetworkId = a} :: GetNetworkResources)

-- | The maximum number of results to return.
getNetworkResources_maxResults :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Natural)
getNetworkResources_maxResults = Lens.lens (\GetNetworkResources' {maxResults} -> maxResults) (\s@GetNetworkResources' {} a -> s {maxResults = a} :: GetNetworkResources)

-- | The token for the next page of results.
getNetworkResources_nextToken :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_nextToken = Lens.lens (\GetNetworkResources' {nextToken} -> nextToken) (\s@GetNetworkResources' {} a -> s {nextToken = a} :: GetNetworkResources)

-- | The ARN of the gateway.
getNetworkResources_registeredGatewayArn :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_registeredGatewayArn = Lens.lens (\GetNetworkResources' {registeredGatewayArn} -> registeredGatewayArn) (\s@GetNetworkResources' {} a -> s {registeredGatewayArn = a} :: GetNetworkResources)

-- | The ARN of the resource.
getNetworkResources_resourceArn :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_resourceArn = Lens.lens (\GetNetworkResources' {resourceArn} -> resourceArn) (\s@GetNetworkResources' {} a -> s {resourceArn = a} :: GetNetworkResources)

-- | The resource type.
--
-- The following are the supported resource types for Direct Connect:
--
-- -   @dxcon@ - The definition model is
--     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_Connection.html Connection>.
--
-- -   @dx-gateway@ - The definition model is
--     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_DirectConnectGateway.html DirectConnectGateway>.
--
-- -   @dx-vif@ - The definition model is
--     <https://docs.aws.amazon.com/directconnect/latest/APIReference/API_VirtualInterface.html VirtualInterface>.
--
-- The following are the supported resource types for Network Manager:
--
-- -   @connection@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Connection.html Connection>.
--
-- -   @device@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Device.html Device>.
--
-- -   @link@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Link.html Link>.
--
-- -   @site@ - The definition model is
--     <https://docs.aws.amazon.com/networkmanager/latest/APIReference/API_Site.html Site>.
--
-- The following are the supported resource types for Amazon VPC:
--
-- -   @customer-gateway@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_CustomerGateway.html CustomerGateway>.
--
-- -   @transit-gateway@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGateway.html TransitGateway>.
--
-- -   @transit-gateway-attachment@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayAttachment.html TransitGatewayAttachment>.
--
-- -   @transit-gateway-connect-peer@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayConnectPeer.html TransitGatewayConnectPeer>.
--
-- -   @transit-gateway-route-table@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_TransitGatewayRouteTable.html TransitGatewayRouteTable>.
--
-- -   @vpn-connection@ - The definition model is
--     <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_VpnConnection.html VpnConnection>.
getNetworkResources_resourceType :: Lens.Lens' GetNetworkResources (Prelude.Maybe Prelude.Text)
getNetworkResources_resourceType = Lens.lens (\GetNetworkResources' {resourceType} -> resourceType) (\s@GetNetworkResources' {} a -> s {resourceType = a} :: GetNetworkResources)

-- | The ID of the global network.
getNetworkResources_globalNetworkId :: Lens.Lens' GetNetworkResources Prelude.Text
getNetworkResources_globalNetworkId = Lens.lens (\GetNetworkResources' {globalNetworkId} -> globalNetworkId) (\s@GetNetworkResources' {} a -> s {globalNetworkId = a} :: GetNetworkResources)

instance Core.AWSPager GetNetworkResources where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getNetworkResourcesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getNetworkResourcesResponse_networkResources
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getNetworkResources_nextToken
          Lens..~ rs
          Lens.^? getNetworkResourcesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetNetworkResources where
  type
    AWSResponse GetNetworkResources =
      GetNetworkResourcesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNetworkResourcesResponse'
            Prelude.<$> ( x Data..?> "NetworkResources"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetNetworkResources where
  hashWithSalt _salt GetNetworkResources' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` registeredGatewayArn
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` globalNetworkId

instance Prelude.NFData GetNetworkResources where
  rnf GetNetworkResources' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf registeredGatewayArn
      `Prelude.seq` Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf globalNetworkId

instance Data.ToHeaders GetNetworkResources where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetNetworkResources where
  toPath GetNetworkResources' {..} =
    Prelude.mconcat
      [ "/global-networks/",
        Data.toBS globalNetworkId,
        "/network-resources"
      ]

instance Data.ToQuery GetNetworkResources where
  toQuery GetNetworkResources' {..} =
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

-- | /See:/ 'newGetNetworkResourcesResponse' smart constructor.
data GetNetworkResourcesResponse = GetNetworkResourcesResponse'
  { -- | The network resources.
    networkResources :: Prelude.Maybe [NetworkResource],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetNetworkResourcesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkResources', 'getNetworkResourcesResponse_networkResources' - The network resources.
--
-- 'nextToken', 'getNetworkResourcesResponse_nextToken' - The token for the next page of results.
--
-- 'httpStatus', 'getNetworkResourcesResponse_httpStatus' - The response's http status code.
newGetNetworkResourcesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetNetworkResourcesResponse
newGetNetworkResourcesResponse pHttpStatus_ =
  GetNetworkResourcesResponse'
    { networkResources =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The network resources.
getNetworkResourcesResponse_networkResources :: Lens.Lens' GetNetworkResourcesResponse (Prelude.Maybe [NetworkResource])
getNetworkResourcesResponse_networkResources = Lens.lens (\GetNetworkResourcesResponse' {networkResources} -> networkResources) (\s@GetNetworkResourcesResponse' {} a -> s {networkResources = a} :: GetNetworkResourcesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
getNetworkResourcesResponse_nextToken :: Lens.Lens' GetNetworkResourcesResponse (Prelude.Maybe Prelude.Text)
getNetworkResourcesResponse_nextToken = Lens.lens (\GetNetworkResourcesResponse' {nextToken} -> nextToken) (\s@GetNetworkResourcesResponse' {} a -> s {nextToken = a} :: GetNetworkResourcesResponse)

-- | The response's http status code.
getNetworkResourcesResponse_httpStatus :: Lens.Lens' GetNetworkResourcesResponse Prelude.Int
getNetworkResourcesResponse_httpStatus = Lens.lens (\GetNetworkResourcesResponse' {httpStatus} -> httpStatus) (\s@GetNetworkResourcesResponse' {} a -> s {httpStatus = a} :: GetNetworkResourcesResponse)

instance Prelude.NFData GetNetworkResourcesResponse where
  rnf GetNetworkResourcesResponse' {..} =
    Prelude.rnf networkResources
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
