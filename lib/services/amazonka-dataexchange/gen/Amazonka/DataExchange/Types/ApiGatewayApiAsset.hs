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
-- Module      : Amazonka.DataExchange.Types.ApiGatewayApiAsset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ApiGatewayApiAsset where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataExchange.Types.ProtocolType
import qualified Amazonka.Prelude as Prelude

-- | The API Gateway API that is the asset.
--
-- /See:/ 'newApiGatewayApiAsset' smart constructor.
data ApiGatewayApiAsset = ApiGatewayApiAsset'
  { -- | The API endpoint of the API asset.
    apiEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the API asset.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the upload URL expires, in ISO 8601 format.
    apiSpecificationDownloadUrlExpiresAt :: Prelude.Maybe Core.POSIX,
    -- | The API key of the API asset.
    apiKey :: Prelude.Maybe Prelude.Text,
    -- | The download URL of the API specification of the API asset.
    apiSpecificationDownloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The protocol type of the API asset.
    protocolType :: Prelude.Maybe ProtocolType,
    -- | The stage of the API asset.
    stage :: Prelude.Maybe Prelude.Text,
    -- | The API name of the API asset.
    apiName :: Prelude.Maybe Prelude.Text,
    -- | The API description of the API asset.
    apiDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApiGatewayApiAsset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiEndpoint', 'apiGatewayApiAsset_apiEndpoint' - The API endpoint of the API asset.
--
-- 'apiId', 'apiGatewayApiAsset_apiId' - The unique identifier of the API asset.
--
-- 'apiSpecificationDownloadUrlExpiresAt', 'apiGatewayApiAsset_apiSpecificationDownloadUrlExpiresAt' - The date and time that the upload URL expires, in ISO 8601 format.
--
-- 'apiKey', 'apiGatewayApiAsset_apiKey' - The API key of the API asset.
--
-- 'apiSpecificationDownloadUrl', 'apiGatewayApiAsset_apiSpecificationDownloadUrl' - The download URL of the API specification of the API asset.
--
-- 'protocolType', 'apiGatewayApiAsset_protocolType' - The protocol type of the API asset.
--
-- 'stage', 'apiGatewayApiAsset_stage' - The stage of the API asset.
--
-- 'apiName', 'apiGatewayApiAsset_apiName' - The API name of the API asset.
--
-- 'apiDescription', 'apiGatewayApiAsset_apiDescription' - The API description of the API asset.
newApiGatewayApiAsset ::
  ApiGatewayApiAsset
newApiGatewayApiAsset =
  ApiGatewayApiAsset'
    { apiEndpoint = Prelude.Nothing,
      apiId = Prelude.Nothing,
      apiSpecificationDownloadUrlExpiresAt =
        Prelude.Nothing,
      apiKey = Prelude.Nothing,
      apiSpecificationDownloadUrl = Prelude.Nothing,
      protocolType = Prelude.Nothing,
      stage = Prelude.Nothing,
      apiName = Prelude.Nothing,
      apiDescription = Prelude.Nothing
    }

-- | The API endpoint of the API asset.
apiGatewayApiAsset_apiEndpoint :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_apiEndpoint = Lens.lens (\ApiGatewayApiAsset' {apiEndpoint} -> apiEndpoint) (\s@ApiGatewayApiAsset' {} a -> s {apiEndpoint = a} :: ApiGatewayApiAsset)

-- | The unique identifier of the API asset.
apiGatewayApiAsset_apiId :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_apiId = Lens.lens (\ApiGatewayApiAsset' {apiId} -> apiId) (\s@ApiGatewayApiAsset' {} a -> s {apiId = a} :: ApiGatewayApiAsset)

-- | The date and time that the upload URL expires, in ISO 8601 format.
apiGatewayApiAsset_apiSpecificationDownloadUrlExpiresAt :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.UTCTime)
apiGatewayApiAsset_apiSpecificationDownloadUrlExpiresAt = Lens.lens (\ApiGatewayApiAsset' {apiSpecificationDownloadUrlExpiresAt} -> apiSpecificationDownloadUrlExpiresAt) (\s@ApiGatewayApiAsset' {} a -> s {apiSpecificationDownloadUrlExpiresAt = a} :: ApiGatewayApiAsset) Prelude.. Lens.mapping Core._Time

-- | The API key of the API asset.
apiGatewayApiAsset_apiKey :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_apiKey = Lens.lens (\ApiGatewayApiAsset' {apiKey} -> apiKey) (\s@ApiGatewayApiAsset' {} a -> s {apiKey = a} :: ApiGatewayApiAsset)

-- | The download URL of the API specification of the API asset.
apiGatewayApiAsset_apiSpecificationDownloadUrl :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_apiSpecificationDownloadUrl = Lens.lens (\ApiGatewayApiAsset' {apiSpecificationDownloadUrl} -> apiSpecificationDownloadUrl) (\s@ApiGatewayApiAsset' {} a -> s {apiSpecificationDownloadUrl = a} :: ApiGatewayApiAsset)

-- | The protocol type of the API asset.
apiGatewayApiAsset_protocolType :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe ProtocolType)
apiGatewayApiAsset_protocolType = Lens.lens (\ApiGatewayApiAsset' {protocolType} -> protocolType) (\s@ApiGatewayApiAsset' {} a -> s {protocolType = a} :: ApiGatewayApiAsset)

-- | The stage of the API asset.
apiGatewayApiAsset_stage :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_stage = Lens.lens (\ApiGatewayApiAsset' {stage} -> stage) (\s@ApiGatewayApiAsset' {} a -> s {stage = a} :: ApiGatewayApiAsset)

-- | The API name of the API asset.
apiGatewayApiAsset_apiName :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_apiName = Lens.lens (\ApiGatewayApiAsset' {apiName} -> apiName) (\s@ApiGatewayApiAsset' {} a -> s {apiName = a} :: ApiGatewayApiAsset)

-- | The API description of the API asset.
apiGatewayApiAsset_apiDescription :: Lens.Lens' ApiGatewayApiAsset (Prelude.Maybe Prelude.Text)
apiGatewayApiAsset_apiDescription = Lens.lens (\ApiGatewayApiAsset' {apiDescription} -> apiDescription) (\s@ApiGatewayApiAsset' {} a -> s {apiDescription = a} :: ApiGatewayApiAsset)

instance Core.FromJSON ApiGatewayApiAsset where
  parseJSON =
    Core.withObject
      "ApiGatewayApiAsset"
      ( \x ->
          ApiGatewayApiAsset'
            Prelude.<$> (x Core..:? "ApiEndpoint")
            Prelude.<*> (x Core..:? "ApiId")
            Prelude.<*> (x Core..:? "ApiSpecificationDownloadUrlExpiresAt")
            Prelude.<*> (x Core..:? "ApiKey")
            Prelude.<*> (x Core..:? "ApiSpecificationDownloadUrl")
            Prelude.<*> (x Core..:? "ProtocolType")
            Prelude.<*> (x Core..:? "Stage")
            Prelude.<*> (x Core..:? "ApiName")
            Prelude.<*> (x Core..:? "ApiDescription")
      )

instance Prelude.Hashable ApiGatewayApiAsset where
  hashWithSalt _salt ApiGatewayApiAsset' {..} =
    _salt `Prelude.hashWithSalt` apiEndpoint
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` apiSpecificationDownloadUrlExpiresAt
      `Prelude.hashWithSalt` apiKey
      `Prelude.hashWithSalt` apiSpecificationDownloadUrl
      `Prelude.hashWithSalt` protocolType
      `Prelude.hashWithSalt` stage
      `Prelude.hashWithSalt` apiName
      `Prelude.hashWithSalt` apiDescription

instance Prelude.NFData ApiGatewayApiAsset where
  rnf ApiGatewayApiAsset' {..} =
    Prelude.rnf apiEndpoint
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf apiSpecificationDownloadUrlExpiresAt
      `Prelude.seq` Prelude.rnf apiKey
      `Prelude.seq` Prelude.rnf apiSpecificationDownloadUrl
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf stage
      `Prelude.seq` Prelude.rnf apiName
      `Prelude.seq` Prelude.rnf apiDescription
