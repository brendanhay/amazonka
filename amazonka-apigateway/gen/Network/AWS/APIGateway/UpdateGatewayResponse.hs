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
-- Module      : Network.AWS.APIGateway.UpdateGatewayResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a GatewayResponse of a specified response type on the given
-- RestApi.
module Network.AWS.APIGateway.UpdateGatewayResponse
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
    gatewayResponse_responseTemplates,
    gatewayResponse_statusCode,
    gatewayResponse_responseParameters,
    gatewayResponse_responseType,
    gatewayResponse_defaultResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Updates a GatewayResponse of a specified response type on the given
-- RestApi.
--
-- /See:/ 'newUpdateGatewayResponse' smart constructor.
data UpdateGatewayResponse = UpdateGatewayResponse'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Core.Text,
    -- | [Required]
    --
    -- The response type of the associated GatewayResponse. Valid values are
    --
    -- -   ACCESS_DENIED
    -- -   API_CONFIGURATION_ERROR
    -- -   AUTHORIZER_FAILURE
    -- -   AUTHORIZER_CONFIGURATION_ERROR
    -- -   BAD_REQUEST_PARAMETERS
    -- -   BAD_REQUEST_BODY
    -- -   DEFAULT_4XX
    -- -   DEFAULT_5XX
    -- -   EXPIRED_TOKEN
    -- -   INVALID_SIGNATURE
    -- -   INTEGRATION_FAILURE
    -- -   INTEGRATION_TIMEOUT
    -- -   INVALID_API_KEY
    -- -   MISSING_AUTHENTICATION_TOKEN
    -- -   QUOTA_EXCEEDED
    -- -   REQUEST_TOO_LARGE
    -- -   RESOURCE_NOT_FOUND
    -- -   THROTTLED
    -- -   UNAUTHORIZED
    -- -   UNSUPPORTED_MEDIA_TYPE
    responseType :: GatewayResponseType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateGatewayResponse_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'restApiId', 'updateGatewayResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'responseType', 'updateGatewayResponse_responseType' - [Required]
--
-- The response type of the associated GatewayResponse. Valid values are
--
-- -   ACCESS_DENIED
-- -   API_CONFIGURATION_ERROR
-- -   AUTHORIZER_FAILURE
-- -   AUTHORIZER_CONFIGURATION_ERROR
-- -   BAD_REQUEST_PARAMETERS
-- -   BAD_REQUEST_BODY
-- -   DEFAULT_4XX
-- -   DEFAULT_5XX
-- -   EXPIRED_TOKEN
-- -   INVALID_SIGNATURE
-- -   INTEGRATION_FAILURE
-- -   INTEGRATION_TIMEOUT
-- -   INVALID_API_KEY
-- -   MISSING_AUTHENTICATION_TOKEN
-- -   QUOTA_EXCEEDED
-- -   REQUEST_TOO_LARGE
-- -   RESOURCE_NOT_FOUND
-- -   THROTTLED
-- -   UNAUTHORIZED
-- -   UNSUPPORTED_MEDIA_TYPE
newUpdateGatewayResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  UpdateGatewayResponse
newUpdateGatewayResponse pRestApiId_ pResponseType_ =
  UpdateGatewayResponse'
    { patchOperations =
        Core.Nothing,
      restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateGatewayResponse_patchOperations :: Lens.Lens' UpdateGatewayResponse (Core.Maybe [PatchOperation])
updateGatewayResponse_patchOperations = Lens.lens (\UpdateGatewayResponse' {patchOperations} -> patchOperations) (\s@UpdateGatewayResponse' {} a -> s {patchOperations = a} :: UpdateGatewayResponse) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
updateGatewayResponse_restApiId :: Lens.Lens' UpdateGatewayResponse Core.Text
updateGatewayResponse_restApiId = Lens.lens (\UpdateGatewayResponse' {restApiId} -> restApiId) (\s@UpdateGatewayResponse' {} a -> s {restApiId = a} :: UpdateGatewayResponse)

-- | [Required]
--
-- The response type of the associated GatewayResponse. Valid values are
--
-- -   ACCESS_DENIED
-- -   API_CONFIGURATION_ERROR
-- -   AUTHORIZER_FAILURE
-- -   AUTHORIZER_CONFIGURATION_ERROR
-- -   BAD_REQUEST_PARAMETERS
-- -   BAD_REQUEST_BODY
-- -   DEFAULT_4XX
-- -   DEFAULT_5XX
-- -   EXPIRED_TOKEN
-- -   INVALID_SIGNATURE
-- -   INTEGRATION_FAILURE
-- -   INTEGRATION_TIMEOUT
-- -   INVALID_API_KEY
-- -   MISSING_AUTHENTICATION_TOKEN
-- -   QUOTA_EXCEEDED
-- -   REQUEST_TOO_LARGE
-- -   RESOURCE_NOT_FOUND
-- -   THROTTLED
-- -   UNAUTHORIZED
-- -   UNSUPPORTED_MEDIA_TYPE
updateGatewayResponse_responseType :: Lens.Lens' UpdateGatewayResponse GatewayResponseType
updateGatewayResponse_responseType = Lens.lens (\UpdateGatewayResponse' {responseType} -> responseType) (\s@UpdateGatewayResponse' {} a -> s {responseType = a} :: UpdateGatewayResponse)

instance Core.AWSRequest UpdateGatewayResponse where
  type
    AWSResponse UpdateGatewayResponse =
      GatewayResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateGatewayResponse

instance Core.NFData UpdateGatewayResponse

instance Core.ToHeaders UpdateGatewayResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGatewayResponse where
  toJSON UpdateGatewayResponse' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateGatewayResponse where
  toPath UpdateGatewayResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/gatewayresponses/",
        Core.toBS responseType
      ]

instance Core.ToQuery UpdateGatewayResponse where
  toQuery = Core.const Core.mempty
