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
-- Module      : Network.AWS.APIGateway.UpdateIntegrationResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration response.
module Network.AWS.APIGateway.UpdateIntegrationResponse
  ( -- * Creating a Request
    UpdateIntegrationResponse (..),
    newUpdateIntegrationResponse,

    -- * Request Lenses
    updateIntegrationResponse_patchOperations,
    updateIntegrationResponse_restApiId,
    updateIntegrationResponse_resourceId,
    updateIntegrationResponse_httpMethod,
    updateIntegrationResponse_statusCode,

    -- * Destructuring the Response
    IntegrationResponse (..),
    newIntegrationResponse,

    -- * Response Lenses
    integrationResponse_contentHandling,
    integrationResponse_responseTemplates,
    integrationResponse_statusCode,
    integrationResponse_responseParameters,
    integrationResponse_selectionPattern,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents an update integration response request.
--
-- /See:/ 'newUpdateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] Specifies an update integration response request\'s resource
    -- identifier.
    resourceId :: Prelude.Text,
    -- | [Required] Specifies an update integration response request\'s HTTP
    -- method.
    httpMethod :: Prelude.Text,
    -- | [Required] Specifies an update integration response request\'s status
    -- code.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateIntegrationResponse_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateIntegrationResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateIntegrationResponse_resourceId' - [Required] Specifies an update integration response request\'s resource
-- identifier.
--
-- 'httpMethod', 'updateIntegrationResponse_httpMethod' - [Required] Specifies an update integration response request\'s HTTP
-- method.
--
-- 'statusCode', 'updateIntegrationResponse_statusCode' - [Required] Specifies an update integration response request\'s status
-- code.
newUpdateIntegrationResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
  UpdateIntegrationResponse
newUpdateIntegrationResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    UpdateIntegrationResponse'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateIntegrationResponse_patchOperations :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe [PatchOperation])
updateIntegrationResponse_patchOperations = Lens.lens (\UpdateIntegrationResponse' {patchOperations} -> patchOperations) (\s@UpdateIntegrationResponse' {} a -> s {patchOperations = a} :: UpdateIntegrationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateIntegrationResponse_restApiId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_restApiId = Lens.lens (\UpdateIntegrationResponse' {restApiId} -> restApiId) (\s@UpdateIntegrationResponse' {} a -> s {restApiId = a} :: UpdateIntegrationResponse)

-- | [Required] Specifies an update integration response request\'s resource
-- identifier.
updateIntegrationResponse_resourceId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_resourceId = Lens.lens (\UpdateIntegrationResponse' {resourceId} -> resourceId) (\s@UpdateIntegrationResponse' {} a -> s {resourceId = a} :: UpdateIntegrationResponse)

-- | [Required] Specifies an update integration response request\'s HTTP
-- method.
updateIntegrationResponse_httpMethod :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_httpMethod = Lens.lens (\UpdateIntegrationResponse' {httpMethod} -> httpMethod) (\s@UpdateIntegrationResponse' {} a -> s {httpMethod = a} :: UpdateIntegrationResponse)

-- | [Required] Specifies an update integration response request\'s status
-- code.
updateIntegrationResponse_statusCode :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_statusCode = Lens.lens (\UpdateIntegrationResponse' {statusCode} -> statusCode) (\s@UpdateIntegrationResponse' {} a -> s {statusCode = a} :: UpdateIntegrationResponse)

instance Core.AWSRequest UpdateIntegrationResponse where
  type
    AWSResponse UpdateIntegrationResponse =
      IntegrationResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateIntegrationResponse

instance Prelude.NFData UpdateIntegrationResponse

instance Core.ToHeaders UpdateIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToJSON UpdateIntegrationResponse where
  toJSON UpdateIntegrationResponse' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("patchOperations" Core..=)
              Prelude.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateIntegrationResponse where
  toPath UpdateIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/resources/",
        Core.toBS resourceId,
        "/methods/",
        Core.toBS httpMethod,
        "/integration/responses/",
        Core.toBS statusCode
      ]

instance Core.ToQuery UpdateIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty
