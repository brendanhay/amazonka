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
-- Module      : Network.AWS.APIGateway.PutGatewayResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a customization of a GatewayResponse of a specified response
-- type and status code on the given RestApi.
module Network.AWS.APIGateway.PutGatewayResponse
  ( -- * Creating a Request
    PutGatewayResponse (..),
    newPutGatewayResponse,

    -- * Request Lenses
    putGatewayResponse_responseTemplates,
    putGatewayResponse_statusCode,
    putGatewayResponse_responseParameters,
    putGatewayResponse_restApiId,
    putGatewayResponse_responseType,

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

-- | Creates a customization of a GatewayResponse of a specified response
-- type and status code on the given RestApi.
--
-- /See:/ 'newPutGatewayResponse' smart constructor.
data PutGatewayResponse = PutGatewayResponse'
  { -- | Response templates of the GatewayResponse as a string-to-string map of
    -- key-value pairs.
    responseTemplates :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The HTTP status code of the GatewayResponse.
    statusCode :: Core.Maybe Core.Text,
    -- | Response parameters (paths, query strings and headers) of the
    -- GatewayResponse as a string-to-string map of key-value pairs.
    responseParameters :: Core.Maybe (Core.HashMap Core.Text Core.Text),
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
-- Create a value of 'PutGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseTemplates', 'putGatewayResponse_responseTemplates' - Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
--
-- 'statusCode', 'putGatewayResponse_statusCode' - The HTTP status code of the GatewayResponse.
--
-- 'responseParameters', 'putGatewayResponse_responseParameters' - Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
--
-- 'restApiId', 'putGatewayResponse_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'responseType', 'putGatewayResponse_responseType' - [Required]
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
newPutGatewayResponse ::
  -- | 'restApiId'
  Core.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  PutGatewayResponse
newPutGatewayResponse pRestApiId_ pResponseType_ =
  PutGatewayResponse'
    { responseTemplates =
        Core.Nothing,
      statusCode = Core.Nothing,
      responseParameters = Core.Nothing,
      restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
putGatewayResponse_responseTemplates :: Lens.Lens' PutGatewayResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
putGatewayResponse_responseTemplates = Lens.lens (\PutGatewayResponse' {responseTemplates} -> responseTemplates) (\s@PutGatewayResponse' {} a -> s {responseTemplates = a} :: PutGatewayResponse) Core.. Lens.mapping Lens._Coerce

-- | The HTTP status code of the GatewayResponse.
putGatewayResponse_statusCode :: Lens.Lens' PutGatewayResponse (Core.Maybe Core.Text)
putGatewayResponse_statusCode = Lens.lens (\PutGatewayResponse' {statusCode} -> statusCode) (\s@PutGatewayResponse' {} a -> s {statusCode = a} :: PutGatewayResponse)

-- | Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
putGatewayResponse_responseParameters :: Lens.Lens' PutGatewayResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
putGatewayResponse_responseParameters = Lens.lens (\PutGatewayResponse' {responseParameters} -> responseParameters) (\s@PutGatewayResponse' {} a -> s {responseParameters = a} :: PutGatewayResponse) Core.. Lens.mapping Lens._Coerce

-- | [Required] The string identifier of the associated RestApi.
putGatewayResponse_restApiId :: Lens.Lens' PutGatewayResponse Core.Text
putGatewayResponse_restApiId = Lens.lens (\PutGatewayResponse' {restApiId} -> restApiId) (\s@PutGatewayResponse' {} a -> s {restApiId = a} :: PutGatewayResponse)

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
putGatewayResponse_responseType :: Lens.Lens' PutGatewayResponse GatewayResponseType
putGatewayResponse_responseType = Lens.lens (\PutGatewayResponse' {responseType} -> responseType) (\s@PutGatewayResponse' {} a -> s {responseType = a} :: PutGatewayResponse)

instance Core.AWSRequest PutGatewayResponse where
  type AWSResponse PutGatewayResponse = GatewayResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable PutGatewayResponse

instance Core.NFData PutGatewayResponse

instance Core.ToHeaders PutGatewayResponse where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON PutGatewayResponse where
  toJSON PutGatewayResponse' {..} =
    Core.object
      ( Core.catMaybes
          [ ("responseTemplates" Core..=)
              Core.<$> responseTemplates,
            ("statusCode" Core..=) Core.<$> statusCode,
            ("responseParameters" Core..=)
              Core.<$> responseParameters
          ]
      )

instance Core.ToPath PutGatewayResponse where
  toPath PutGatewayResponse' {..} =
    Core.mconcat
      [ "/restapis/",
        Core.toBS restApiId,
        "/gatewayresponses/",
        Core.toBS responseType
      ]

instance Core.ToQuery PutGatewayResponse where
  toQuery = Core.const Core.mempty
