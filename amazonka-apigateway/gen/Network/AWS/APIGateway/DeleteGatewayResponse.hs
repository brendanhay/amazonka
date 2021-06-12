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
-- Module      : Network.AWS.APIGateway.DeleteGatewayResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Clears any customization of a GatewayResponse of a specified response
-- type on the given RestApi and resets it with the default settings.
module Network.AWS.APIGateway.DeleteGatewayResponse
  ( -- * Creating a Request
    DeleteGatewayResponse (..),
    newDeleteGatewayResponse,

    -- * Request Lenses
    deleteGatewayResponse_restApiId,
    deleteGatewayResponse_responseType,

    -- * Destructuring the Response
    DeleteGatewayResponseResponse (..),
    newDeleteGatewayResponseResponse,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Clears any customization of a GatewayResponse of a specified response
-- type on the given RestApi and resets it with the default settings.
--
-- /See:/ 'newDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { -- | [Required] The string identifier of the associated RestApi.
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
-- Create a value of 'DeleteGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restApiId', 'deleteGatewayResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'responseType', 'deleteGatewayResponse_responseType' - [Required]
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
newDeleteGatewayResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  DeleteGatewayResponse
newDeleteGatewayResponse pRestApiId_ pResponseType_ =
  DeleteGatewayResponse'
    { restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | [Required] The string identifier of the associated RestApi.
deleteGatewayResponse_restApiId :: Lens.Lens' DeleteGatewayResponse Core.Text
deleteGatewayResponse_restApiId = Lens.lens (\DeleteGatewayResponse' {restApiId} -> restApiId) (\s@DeleteGatewayResponse' {} a -> s {restApiId = a} :: DeleteGatewayResponse)

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
deleteGatewayResponse_responseType :: Lens.Lens' DeleteGatewayResponse GatewayResponseType
deleteGatewayResponse_responseType = Lens.lens (\DeleteGatewayResponse' {responseType} -> responseType) (\s@DeleteGatewayResponse' {} a -> s {responseType = a} :: DeleteGatewayResponse)

instance Core.AWSRequest DeleteGatewayResponse where
  type
    AWSResponse DeleteGatewayResponse =
      DeleteGatewayResponseResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteGatewayResponseResponse'

instance Core.Hashable DeleteGatewayResponse

instance Core.NFData DeleteGatewayResponse

instance Core.ToHeaders DeleteGatewayResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteGatewayResponse where
  toPath DeleteGatewayResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/gatewayresponses/",
        Core.toBS responseType
      ]

instance Core.ToQuery DeleteGatewayResponse where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteGatewayResponseResponse' smart constructor.
data DeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGatewayResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGatewayResponseResponse ::
  DeleteGatewayResponseResponse
newDeleteGatewayResponseResponse =
  DeleteGatewayResponseResponse'

instance Core.NFData DeleteGatewayResponseResponse
