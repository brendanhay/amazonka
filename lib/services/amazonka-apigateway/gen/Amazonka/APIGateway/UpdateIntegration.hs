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
-- Module      : Amazonka.APIGateway.UpdateIntegration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration.
module Amazonka.APIGateway.UpdateIntegration
  ( -- * Creating a Request
    UpdateIntegration (..),
    newUpdateIntegration,

    -- * Request Lenses
    updateIntegration_patchOperations,
    updateIntegration_restApiId,
    updateIntegration_resourceId,
    updateIntegration_httpMethod,

    -- * Destructuring the Response
    Integration (..),
    newIntegration,

    -- * Response Lenses
    integration_cacheKeyParameters,
    integration_requestParameters,
    integration_type,
    integration_connectionType,
    integration_tlsConfig,
    integration_cacheNamespace,
    integration_uri,
    integration_connectionId,
    integration_httpMethod,
    integration_credentials,
    integration_integrationResponses,
    integration_timeoutInMillis,
    integration_contentHandling,
    integration_requestTemplates,
    integration_passthroughBehavior,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents an update integration request.
--
-- /See:/ 'newUpdateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Represents an update integration request\'s resource identifier.
    resourceId :: Prelude.Text,
    -- | Represents an update integration request\'s HTTP method.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateIntegration_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateIntegration_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateIntegration_resourceId' - Represents an update integration request\'s resource identifier.
--
-- 'httpMethod', 'updateIntegration_httpMethod' - Represents an update integration request\'s HTTP method.
newUpdateIntegration ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  UpdateIntegration
newUpdateIntegration
  pRestApiId_
  pResourceId_
  pHttpMethod_ =
    UpdateIntegration'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_
      }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateIntegration_patchOperations :: Lens.Lens' UpdateIntegration (Prelude.Maybe [PatchOperation])
updateIntegration_patchOperations = Lens.lens (\UpdateIntegration' {patchOperations} -> patchOperations) (\s@UpdateIntegration' {} a -> s {patchOperations = a} :: UpdateIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateIntegration_restApiId :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_restApiId = Lens.lens (\UpdateIntegration' {restApiId} -> restApiId) (\s@UpdateIntegration' {} a -> s {restApiId = a} :: UpdateIntegration)

-- | Represents an update integration request\'s resource identifier.
updateIntegration_resourceId :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_resourceId = Lens.lens (\UpdateIntegration' {resourceId} -> resourceId) (\s@UpdateIntegration' {} a -> s {resourceId = a} :: UpdateIntegration)

-- | Represents an update integration request\'s HTTP method.
updateIntegration_httpMethod :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_httpMethod = Lens.lens (\UpdateIntegration' {httpMethod} -> httpMethod) (\s@UpdateIntegration' {} a -> s {httpMethod = a} :: UpdateIntegration)

instance Core.AWSRequest UpdateIntegration where
  type AWSResponse UpdateIntegration = Integration
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateIntegration where
  hashWithSalt _salt UpdateIntegration' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod

instance Prelude.NFData UpdateIntegration where
  rnf UpdateIntegration' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod

instance Data.ToHeaders UpdateIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateIntegration where
  toJSON UpdateIntegration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateIntegration where
  toPath UpdateIntegration' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/integration"
      ]

instance Data.ToQuery UpdateIntegration where
  toQuery = Prelude.const Prelude.mempty
