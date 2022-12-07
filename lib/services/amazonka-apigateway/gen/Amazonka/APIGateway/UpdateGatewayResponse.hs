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
-- Module      : Amazonka.APIGateway.UpdateGatewayResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a GatewayResponse of a specified response type on the given
-- RestApi.
module Amazonka.APIGateway.UpdateGatewayResponse
  ( -- * Creating a Request
    UpdateGatewayResponse (..),
    newUpdateGatewayResponse,

    -- * Request Lenses
    updateGatewayResponse_patchOperations,
    updateGatewayResponse_restApiId,
    updateGatewayResponse_responseType,

    -- * Destructuring the Response
    GatewayResponse (..),
    newGatewayResponse,

    -- * Response Lenses
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
    gatewayResponse_responseParameters,
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a GatewayResponse of a specified response type on the given
-- RestApi.
--
-- /See:/ 'newUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | The response type of the associated GatewayResponse.
    responseType :: GatewayResponseType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateGatewayResponse_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'restApiId', 'updateGatewayResponse_restApiId' - The string identifier of the associated RestApi.
--
-- 'responseType', 'updateGatewayResponse_responseType' - The response type of the associated GatewayResponse.
newUpdateGatewayResponse ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  UpdateGatewayResponse
newUpdateGatewayResponse pRestApiId_ pResponseType_ =
  UpdateGatewayResponse'
    { patchOperations =
        Prelude.Nothing,
      restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateGatewayResponse_patchOperations :: Lens.Lens' UpdateGatewayResponse (Prelude.Maybe [PatchOperation])
updateGatewayResponse_patchOperations = Lens.lens (\UpdateGatewayResponse' {patchOperations} -> patchOperations) (\s@UpdateGatewayResponse' {} a -> s {patchOperations = a} :: UpdateGatewayResponse) Prelude.. Lens.mapping Lens.coerced

-- | The string identifier of the associated RestApi.
updateGatewayResponse_restApiId :: Lens.Lens' UpdateGatewayResponse Prelude.Text
updateGatewayResponse_restApiId = Lens.lens (\UpdateGatewayResponse' {restApiId} -> restApiId) (\s@UpdateGatewayResponse' {} a -> s {restApiId = a} :: UpdateGatewayResponse)

-- | The response type of the associated GatewayResponse.
updateGatewayResponse_responseType :: Lens.Lens' UpdateGatewayResponse GatewayResponseType
updateGatewayResponse_responseType = Lens.lens (\UpdateGatewayResponse' {responseType} -> responseType) (\s@UpdateGatewayResponse' {} a -> s {responseType = a} :: UpdateGatewayResponse)

instance Core.AWSRequest UpdateGatewayResponse where
  type
    AWSResponse UpdateGatewayResponse =
      GatewayResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateGatewayResponse where
  hashWithSalt _salt UpdateGatewayResponse' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` restApiId
      `Prelude.hashWithSalt` responseType

instance Prelude.NFData UpdateGatewayResponse where
  rnf UpdateGatewayResponse' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf restApiId
      `Prelude.seq` Prelude.rnf responseType

instance Data.ToHeaders UpdateGatewayResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateGatewayResponse where
  toJSON UpdateGatewayResponse' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateGatewayResponse where
  toPath UpdateGatewayResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Data.toBS restApiId,
        "/gatewayresponses/",
        Data.toBS responseType
      ]

instance Data.ToQuery UpdateGatewayResponse where
  toQuery = Prelude.const Prelude.mempty
