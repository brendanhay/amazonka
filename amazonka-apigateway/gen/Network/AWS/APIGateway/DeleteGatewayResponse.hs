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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Clears any customization of a GatewayResponse of a specified response
-- type on the given RestApi and resets it with the default settings.
--
-- /See:/ 'newDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  DeleteGatewayResponse
newDeleteGatewayResponse pRestApiId_ pResponseType_ =
  DeleteGatewayResponse'
    { restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | [Required] The string identifier of the associated RestApi.
deleteGatewayResponse_restApiId :: Lens.Lens' DeleteGatewayResponse Prelude.Text
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

instance Prelude.AWSRequest DeleteGatewayResponse where
  type
    Rs DeleteGatewayResponse =
      DeleteGatewayResponseResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteGatewayResponseResponse'

instance Prelude.Hashable DeleteGatewayResponse

instance Prelude.NFData DeleteGatewayResponse

instance Prelude.ToHeaders DeleteGatewayResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath DeleteGatewayResponse where
  toPath DeleteGatewayResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/gatewayresponses/",
        Prelude.toBS responseType
      ]

instance Prelude.ToQuery DeleteGatewayResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteGatewayResponseResponse' smart constructor.
data DeleteGatewayResponseResponse = DeleteGatewayResponseResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteGatewayResponseResponse ::
  DeleteGatewayResponseResponse
newDeleteGatewayResponseResponse =
  DeleteGatewayResponseResponse'

instance Prelude.NFData DeleteGatewayResponseResponse
