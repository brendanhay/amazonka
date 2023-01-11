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
-- Module      : Amazonka.APIGateway.UpdateIntegrationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents an update integration response.
module Amazonka.APIGateway.UpdateIntegrationResponse
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
    integrationResponse_responseParameters,
    integrationResponse_responseTemplates,
    integrationResponse_selectionPattern,
    integrationResponse_statusCode,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents an update integration response request.
--
-- /See:/ 'newUpdateIntegrationResponse' smart constructor.
data UpdateIntegrationResponse = UpdateIntegrationResponse'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | Specifies an update integration response request\'s resource identifier.
    resourceId :: Prelude.Text,
    -- | Specifies an update integration response request\'s HTTP method.
    httpMethod :: Prelude.Text,
    -- | Specifies an update integration response request\'s status code.
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
-- 'patchOperations', 'updateIntegrationResponse_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateIntegrationResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateIntegrationResponse_resourceId' - Specifies an update integration response request\'s resource identifier.
--
-- 'httpMethod', 'updateIntegrationResponse_httpMethod' - Specifies an update integration response request\'s HTTP method.
--
-- 'statusCode', 'updateIntegrationResponse_statusCode' - Specifies an update integration response request\'s status code.
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

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateIntegrationResponse_patchOperations :: Lens.Lens' UpdateIntegrationResponse (Prelude.Maybe [PatchOperation])
updateIntegrationResponse_patchOperations = Lens.lens (\UpdateIntegrationResponse' {patchOperations} -> patchOperations) (\s@UpdateIntegrationResponse' {} a -> s {patchOperations = a} :: UpdateIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateIntegrationResponse_restApiId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_restApiId = Lens.lens (\UpdateIntegrationResponse' {restApiId} -> restApiId) (\s@UpdateIntegrationResponse' {} a -> s {restApiId = a} :: UpdateIntegrationResponse)

-- | Specifies an update integration response request\'s resource identifier.
updateIntegrationResponse_resourceId :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_resourceId = Lens.lens (\UpdateIntegrationResponse' {resourceId} -> resourceId) (\s@UpdateIntegrationResponse' {} a -> s {resourceId = a} :: UpdateIntegrationResponse)

-- | Specifies an update integration response request\'s HTTP method.
updateIntegrationResponse_httpMethod :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_httpMethod = Lens.lens (\UpdateIntegrationResponse' {httpMethod} -> httpMethod) (\s@UpdateIntegrationResponse' {} a -> s {httpMethod = a} :: UpdateIntegrationResponse)

-- | Specifies an update integration response request\'s status code.
updateIntegrationResponse_statusCode :: Lens.Lens' UpdateIntegrationResponse Prelude.Text
updateIntegrationResponse_statusCode = Lens.lens (\UpdateIntegrationResponse' {statusCode} -> statusCode) (\s@UpdateIntegrationResponse' {} a -> s {statusCode = a} :: UpdateIntegrationResponse)

instance Core.AWSRequest UpdateIntegrationResponse where
  type
    AWSResponse UpdateIntegrationResponse =
      IntegrationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateIntegrationResponse where
  hashWithSalt _salt UpdateIntegrationResponse' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData UpdateIntegrationResponse where
  rnf UpdateIntegrationResponse' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToHeaders UpdateIntegrationResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateIntegrationResponse where
  toJSON UpdateIntegrationResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateIntegrationResponse where
  toPath UpdateIntegrationResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/integration/responses/",
        Data.toBS statusCode
      ]

instance Data.ToQuery UpdateIntegrationResponse where
  toQuery = Prelude.const Prelude.mempty
