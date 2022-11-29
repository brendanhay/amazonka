{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxyConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxyConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayEndpointType
import qualified Amazonka.Prelude as Prelude

-- | A wrapper object holding the Amazon API Gateway proxy configuration.
--
-- /See:/ 'newApiGatewayProxyConfig' smart constructor.
data ApiGatewayProxyConfig = ApiGatewayProxyConfig'
  { -- | The name of the API Gateway stage. The name defaults to @prod@.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | The endpoint URL of the API Gateway proxy.
    proxyUrl :: Prelude.Maybe Prelude.Text,
    -- | The type of API Gateway endpoint created.
    endpointType :: Prelude.Maybe ApiGatewayEndpointType,
    -- | The resource ID of the API Gateway for the proxy.
    apiGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The name of the Network Load Balancer that is configured by the API
    -- Gateway proxy.
    nlbName :: Prelude.Maybe Prelude.Text,
    -- | The @VpcLink@ ID of the API Gateway proxy.
    vpcLinkId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Network Load Balancer configured
    -- by the API Gateway proxy.
    nlbArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiGatewayProxyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'apiGatewayProxyConfig_stageName' - The name of the API Gateway stage. The name defaults to @prod@.
--
-- 'proxyUrl', 'apiGatewayProxyConfig_proxyUrl' - The endpoint URL of the API Gateway proxy.
--
-- 'endpointType', 'apiGatewayProxyConfig_endpointType' - The type of API Gateway endpoint created.
--
-- 'apiGatewayId', 'apiGatewayProxyConfig_apiGatewayId' - The resource ID of the API Gateway for the proxy.
--
-- 'nlbName', 'apiGatewayProxyConfig_nlbName' - The name of the Network Load Balancer that is configured by the API
-- Gateway proxy.
--
-- 'vpcLinkId', 'apiGatewayProxyConfig_vpcLinkId' - The @VpcLink@ ID of the API Gateway proxy.
--
-- 'nlbArn', 'apiGatewayProxyConfig_nlbArn' - The Amazon Resource Name (ARN) of the Network Load Balancer configured
-- by the API Gateway proxy.
newApiGatewayProxyConfig ::
  ApiGatewayProxyConfig
newApiGatewayProxyConfig =
  ApiGatewayProxyConfig'
    { stageName = Prelude.Nothing,
      proxyUrl = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      apiGatewayId = Prelude.Nothing,
      nlbName = Prelude.Nothing,
      vpcLinkId = Prelude.Nothing,
      nlbArn = Prelude.Nothing
    }

-- | The name of the API Gateway stage. The name defaults to @prod@.
apiGatewayProxyConfig_stageName :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe Prelude.Text)
apiGatewayProxyConfig_stageName = Lens.lens (\ApiGatewayProxyConfig' {stageName} -> stageName) (\s@ApiGatewayProxyConfig' {} a -> s {stageName = a} :: ApiGatewayProxyConfig)

-- | The endpoint URL of the API Gateway proxy.
apiGatewayProxyConfig_proxyUrl :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe Prelude.Text)
apiGatewayProxyConfig_proxyUrl = Lens.lens (\ApiGatewayProxyConfig' {proxyUrl} -> proxyUrl) (\s@ApiGatewayProxyConfig' {} a -> s {proxyUrl = a} :: ApiGatewayProxyConfig)

-- | The type of API Gateway endpoint created.
apiGatewayProxyConfig_endpointType :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe ApiGatewayEndpointType)
apiGatewayProxyConfig_endpointType = Lens.lens (\ApiGatewayProxyConfig' {endpointType} -> endpointType) (\s@ApiGatewayProxyConfig' {} a -> s {endpointType = a} :: ApiGatewayProxyConfig)

-- | The resource ID of the API Gateway for the proxy.
apiGatewayProxyConfig_apiGatewayId :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe Prelude.Text)
apiGatewayProxyConfig_apiGatewayId = Lens.lens (\ApiGatewayProxyConfig' {apiGatewayId} -> apiGatewayId) (\s@ApiGatewayProxyConfig' {} a -> s {apiGatewayId = a} :: ApiGatewayProxyConfig)

-- | The name of the Network Load Balancer that is configured by the API
-- Gateway proxy.
apiGatewayProxyConfig_nlbName :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe Prelude.Text)
apiGatewayProxyConfig_nlbName = Lens.lens (\ApiGatewayProxyConfig' {nlbName} -> nlbName) (\s@ApiGatewayProxyConfig' {} a -> s {nlbName = a} :: ApiGatewayProxyConfig)

-- | The @VpcLink@ ID of the API Gateway proxy.
apiGatewayProxyConfig_vpcLinkId :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe Prelude.Text)
apiGatewayProxyConfig_vpcLinkId = Lens.lens (\ApiGatewayProxyConfig' {vpcLinkId} -> vpcLinkId) (\s@ApiGatewayProxyConfig' {} a -> s {vpcLinkId = a} :: ApiGatewayProxyConfig)

-- | The Amazon Resource Name (ARN) of the Network Load Balancer configured
-- by the API Gateway proxy.
apiGatewayProxyConfig_nlbArn :: Lens.Lens' ApiGatewayProxyConfig (Prelude.Maybe Prelude.Text)
apiGatewayProxyConfig_nlbArn = Lens.lens (\ApiGatewayProxyConfig' {nlbArn} -> nlbArn) (\s@ApiGatewayProxyConfig' {} a -> s {nlbArn = a} :: ApiGatewayProxyConfig)

instance Core.FromJSON ApiGatewayProxyConfig where
  parseJSON =
    Core.withObject
      "ApiGatewayProxyConfig"
      ( \x ->
          ApiGatewayProxyConfig'
            Prelude.<$> (x Core..:? "StageName")
            Prelude.<*> (x Core..:? "ProxyUrl")
            Prelude.<*> (x Core..:? "EndpointType")
            Prelude.<*> (x Core..:? "ApiGatewayId")
            Prelude.<*> (x Core..:? "NlbName")
            Prelude.<*> (x Core..:? "VpcLinkId")
            Prelude.<*> (x Core..:? "NlbArn")
      )

instance Prelude.Hashable ApiGatewayProxyConfig where
  hashWithSalt _salt ApiGatewayProxyConfig' {..} =
    _salt `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` proxyUrl
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` apiGatewayId
      `Prelude.hashWithSalt` nlbName
      `Prelude.hashWithSalt` vpcLinkId
      `Prelude.hashWithSalt` nlbArn

instance Prelude.NFData ApiGatewayProxyConfig where
  rnf ApiGatewayProxyConfig' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf proxyUrl
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf apiGatewayId
      `Prelude.seq` Prelude.rnf nlbName
      `Prelude.seq` Prelude.rnf vpcLinkId
      `Prelude.seq` Prelude.rnf nlbArn
