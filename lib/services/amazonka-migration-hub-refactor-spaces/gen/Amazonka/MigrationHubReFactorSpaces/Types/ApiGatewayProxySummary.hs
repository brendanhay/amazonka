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
-- Module      : Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxySummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayProxySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubReFactorSpaces.Types.ApiGatewayEndpointType
import qualified Amazonka.Prelude as Prelude

-- | A wrapper object holding the Amazon API Gateway proxy summary.
--
-- /See:/ 'newApiGatewayProxySummary' smart constructor.
data ApiGatewayProxySummary = ApiGatewayProxySummary'
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
-- Create a value of 'ApiGatewayProxySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stageName', 'apiGatewayProxySummary_stageName' - The name of the API Gateway stage. The name defaults to @prod@.
--
-- 'proxyUrl', 'apiGatewayProxySummary_proxyUrl' - The endpoint URL of the API Gateway proxy.
--
-- 'endpointType', 'apiGatewayProxySummary_endpointType' - The type of API Gateway endpoint created.
--
-- 'apiGatewayId', 'apiGatewayProxySummary_apiGatewayId' - The resource ID of the API Gateway for the proxy.
--
-- 'nlbName', 'apiGatewayProxySummary_nlbName' - The name of the Network Load Balancer that is configured by the API
-- Gateway proxy.
--
-- 'vpcLinkId', 'apiGatewayProxySummary_vpcLinkId' - The @VpcLink@ ID of the API Gateway proxy.
--
-- 'nlbArn', 'apiGatewayProxySummary_nlbArn' - The Amazon Resource Name (ARN) of the Network Load Balancer configured
-- by the API Gateway proxy.
newApiGatewayProxySummary ::
  ApiGatewayProxySummary
newApiGatewayProxySummary =
  ApiGatewayProxySummary'
    { stageName =
        Prelude.Nothing,
      proxyUrl = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      apiGatewayId = Prelude.Nothing,
      nlbName = Prelude.Nothing,
      vpcLinkId = Prelude.Nothing,
      nlbArn = Prelude.Nothing
    }

-- | The name of the API Gateway stage. The name defaults to @prod@.
apiGatewayProxySummary_stageName :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe Prelude.Text)
apiGatewayProxySummary_stageName = Lens.lens (\ApiGatewayProxySummary' {stageName} -> stageName) (\s@ApiGatewayProxySummary' {} a -> s {stageName = a} :: ApiGatewayProxySummary)

-- | The endpoint URL of the API Gateway proxy.
apiGatewayProxySummary_proxyUrl :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe Prelude.Text)
apiGatewayProxySummary_proxyUrl = Lens.lens (\ApiGatewayProxySummary' {proxyUrl} -> proxyUrl) (\s@ApiGatewayProxySummary' {} a -> s {proxyUrl = a} :: ApiGatewayProxySummary)

-- | The type of API Gateway endpoint created.
apiGatewayProxySummary_endpointType :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe ApiGatewayEndpointType)
apiGatewayProxySummary_endpointType = Lens.lens (\ApiGatewayProxySummary' {endpointType} -> endpointType) (\s@ApiGatewayProxySummary' {} a -> s {endpointType = a} :: ApiGatewayProxySummary)

-- | The resource ID of the API Gateway for the proxy.
apiGatewayProxySummary_apiGatewayId :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe Prelude.Text)
apiGatewayProxySummary_apiGatewayId = Lens.lens (\ApiGatewayProxySummary' {apiGatewayId} -> apiGatewayId) (\s@ApiGatewayProxySummary' {} a -> s {apiGatewayId = a} :: ApiGatewayProxySummary)

-- | The name of the Network Load Balancer that is configured by the API
-- Gateway proxy.
apiGatewayProxySummary_nlbName :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe Prelude.Text)
apiGatewayProxySummary_nlbName = Lens.lens (\ApiGatewayProxySummary' {nlbName} -> nlbName) (\s@ApiGatewayProxySummary' {} a -> s {nlbName = a} :: ApiGatewayProxySummary)

-- | The @VpcLink@ ID of the API Gateway proxy.
apiGatewayProxySummary_vpcLinkId :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe Prelude.Text)
apiGatewayProxySummary_vpcLinkId = Lens.lens (\ApiGatewayProxySummary' {vpcLinkId} -> vpcLinkId) (\s@ApiGatewayProxySummary' {} a -> s {vpcLinkId = a} :: ApiGatewayProxySummary)

-- | The Amazon Resource Name (ARN) of the Network Load Balancer configured
-- by the API Gateway proxy.
apiGatewayProxySummary_nlbArn :: Lens.Lens' ApiGatewayProxySummary (Prelude.Maybe Prelude.Text)
apiGatewayProxySummary_nlbArn = Lens.lens (\ApiGatewayProxySummary' {nlbArn} -> nlbArn) (\s@ApiGatewayProxySummary' {} a -> s {nlbArn = a} :: ApiGatewayProxySummary)

instance Data.FromJSON ApiGatewayProxySummary where
  parseJSON =
    Data.withObject
      "ApiGatewayProxySummary"
      ( \x ->
          ApiGatewayProxySummary'
            Prelude.<$> (x Data..:? "StageName")
            Prelude.<*> (x Data..:? "ProxyUrl")
            Prelude.<*> (x Data..:? "EndpointType")
            Prelude.<*> (x Data..:? "ApiGatewayId")
            Prelude.<*> (x Data..:? "NlbName")
            Prelude.<*> (x Data..:? "VpcLinkId")
            Prelude.<*> (x Data..:? "NlbArn")
      )

instance Prelude.Hashable ApiGatewayProxySummary where
  hashWithSalt _salt ApiGatewayProxySummary' {..} =
    _salt `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` proxyUrl
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` apiGatewayId
      `Prelude.hashWithSalt` nlbName
      `Prelude.hashWithSalt` vpcLinkId
      `Prelude.hashWithSalt` nlbArn

instance Prelude.NFData ApiGatewayProxySummary where
  rnf ApiGatewayProxySummary' {..} =
    Prelude.rnf stageName
      `Prelude.seq` Prelude.rnf proxyUrl
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf apiGatewayId
      `Prelude.seq` Prelude.rnf nlbName
      `Prelude.seq` Prelude.rnf vpcLinkId
      `Prelude.seq` Prelude.rnf nlbArn
