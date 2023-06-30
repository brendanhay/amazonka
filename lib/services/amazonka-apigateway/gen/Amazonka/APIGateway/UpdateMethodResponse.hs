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
-- Module      : Amazonka.APIGateway.UpdateMethodResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing MethodResponse resource.
module Amazonka.APIGateway.UpdateMethodResponse
  ( -- * Creating a Request
    UpdateMethodResponse (..),
    newUpdateMethodResponse,

    -- * Request Lenses
    updateMethodResponse_patchOperations,
    updateMethodResponse_restApiId,
    updateMethodResponse_resourceId,
    updateMethodResponse_httpMethod,
    updateMethodResponse_statusCode,

    -- * Destructuring the Response
    MethodResponse (..),
    newMethodResponse,

    -- * Response Lenses
    methodResponse_responseModels,
    methodResponse_responseParameters,
    methodResponse_statusCode,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to update an existing MethodResponse resource.
--
-- /See:/ 'newUpdateMethodResponse' smart constructor.
data UpdateMethodResponse = UpdateMethodResponse'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The Resource identifier for the MethodResponse resource.
    resourceId :: Prelude.Text,
    -- | The HTTP verb of the Method resource.
    httpMethod :: Prelude.Text,
    -- | The status code for the MethodResponse resource.
    statusCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMethodResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateMethodResponse_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateMethodResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'resourceId', 'updateMethodResponse_resourceId' - The Resource identifier for the MethodResponse resource.
--
-- 'httpMethod', 'updateMethodResponse_httpMethod' - The HTTP verb of the Method resource.
--
-- 'statusCode', 'updateMethodResponse_statusCode' - The status code for the MethodResponse resource.
newUpdateMethodResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'httpMethod'
  Prelude.Text ->
  -- | 'statusCode'
  Prelude.Text ->
  UpdateMethodResponse
newUpdateMethodResponse
  pRestApiId_
  pResourceId_
  pHttpMethod_
  pStatusCode_ =
    UpdateMethodResponse'
      { patchOperations =
          Prelude.Nothing,
        restApiId = pRestApiId_,
        resourceId = pResourceId_,
        httpMethod = pHttpMethod_,
        statusCode = pStatusCode_
      }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateMethodResponse_patchOperations :: Lens.Lens' UpdateMethodResponse (Prelude.Maybe [PatchOperation])
updateMethodResponse_patchOperations = Lens.lens (\UpdateMethodResponse' {patchOperations} -> patchOperations) (\s@UpdateMethodResponse' {} a -> s {patchOperations = a} :: UpdateMethodResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateMethodResponse_restApiId :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_restApiId = Lens.lens (\UpdateMethodResponse' {restApiId} -> restApiId) (\s@UpdateMethodResponse' {} a -> s {restApiId = a} :: UpdateMethodResponse)

-- | The Resource identifier for the MethodResponse resource.
updateMethodResponse_resourceId :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_resourceId = Lens.lens (\UpdateMethodResponse' {resourceId} -> resourceId) (\s@UpdateMethodResponse' {} a -> s {resourceId = a} :: UpdateMethodResponse)

-- | The HTTP verb of the Method resource.
updateMethodResponse_httpMethod :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_httpMethod = Lens.lens (\UpdateMethodResponse' {httpMethod} -> httpMethod) (\s@UpdateMethodResponse' {} a -> s {httpMethod = a} :: UpdateMethodResponse)

-- | The status code for the MethodResponse resource.
updateMethodResponse_statusCode :: Lens.Lens' UpdateMethodResponse Prelude.Text
updateMethodResponse_statusCode = Lens.lens (\UpdateMethodResponse' {statusCode} -> statusCode) (\s@UpdateMethodResponse' {} a -> s {statusCode = a} :: UpdateMethodResponse)

instance Core.AWSRequest UpdateMethodResponse where
  type
    AWSResponse UpdateMethodResponse =
      MethodResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateMethodResponse where
  hashWithSalt _salt UpdateMethodResponse' {..} =
    _salt
      `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` httpMethod
      `Prelude.hashWithSalt` statusCode

instance Prelude.NFData UpdateMethodResponse where
  rnf UpdateMethodResponse' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf httpMethod
      `Prelude.seq` Prelude.rnf statusCode

instance Data.ToHeaders UpdateMethodResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateMethodResponse where
  toJSON UpdateMethodResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateMethodResponse where
  toPath UpdateMethodResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/resources/",
        Data.toBS resourceId,
        "/methods/",
        Data.toBS httpMethod,
        "/responses/",
        Data.toBS statusCode
      ]

instance Data.ToQuery UpdateMethodResponse where
  toQuery = Prelude.const Prelude.mempty
