{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.UpdateIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration.
module Network.AWS.APIGateway.UpdateIntegration
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
    integration_httpMethod,
    integration_passthroughBehavior,
    integration_contentHandling,
    integration_uri,
    integration_connectionType,
    integration_connectionId,
    integration_requestTemplates,
    integration_timeoutInMillis,
    integration_cacheNamespace,
    integration_cacheKeyParameters,
    integration_tlsConfig,
    integration_integrationResponses,
    integration_requestParameters,
    integration_type,
    integration_credentials,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents an update integration request.
--
-- /See:/ 'newUpdateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Represents an update integration request\'s resource
    -- identifier.
    resourceId :: Prelude.Text,
    -- | [Required] Represents an update integration request\'s HTTP method.
    httpMethod :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateIntegration_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateIntegration_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateIntegration_resourceId' - [Required] Represents an update integration request\'s resource
-- identifier.
--
-- 'httpMethod', 'updateIntegration_httpMethod' - [Required] Represents an update integration request\'s HTTP method.
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

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateIntegration_patchOperations :: Lens.Lens' UpdateIntegration (Prelude.Maybe [PatchOperation])
updateIntegration_patchOperations = Lens.lens (\UpdateIntegration' {patchOperations} -> patchOperations) (\s@UpdateIntegration' {} a -> s {patchOperations = a} :: UpdateIntegration) Prelude.. Lens.mapping Prelude._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateIntegration_restApiId :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_restApiId = Lens.lens (\UpdateIntegration' {restApiId} -> restApiId) (\s@UpdateIntegration' {} a -> s {restApiId = a} :: UpdateIntegration)

-- | [Required] Represents an update integration request\'s resource
-- identifier.
updateIntegration_resourceId :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_resourceId = Lens.lens (\UpdateIntegration' {resourceId} -> resourceId) (\s@UpdateIntegration' {} a -> s {resourceId = a} :: UpdateIntegration)

-- | [Required] Represents an update integration request\'s HTTP method.
updateIntegration_httpMethod :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_httpMethod = Lens.lens (\UpdateIntegration' {httpMethod} -> httpMethod) (\s@UpdateIntegration' {} a -> s {httpMethod = a} :: UpdateIntegration)

instance Prelude.AWSRequest UpdateIntegration where
  type Rs UpdateIntegration = Integration
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable UpdateIntegration

instance Prelude.NFData UpdateIntegration

instance Prelude.ToHeaders UpdateIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON UpdateIntegration where
  toJSON UpdateIntegration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("patchOperations" Prelude..=)
              Prelude.<$> patchOperations
          ]
      )

instance Prelude.ToPath UpdateIntegration where
  toPath UpdateIntegration' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/resources/",
        Prelude.toBS resourceId,
        "/methods/",
        Prelude.toBS httpMethod,
        "/integration"
      ]

instance Prelude.ToQuery UpdateIntegration where
  toQuery = Prelude.const Prelude.mempty
