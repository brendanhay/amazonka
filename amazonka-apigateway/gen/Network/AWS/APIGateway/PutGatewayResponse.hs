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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Creates a customization of a GatewayResponse of a specified response
-- type and status code on the given RestApi.
--
-- /See:/ 'newPutGatewayResponse' smart constructor.
data PutGatewayResponse = PutGatewayResponse'
  { -- | Response templates of the GatewayResponse as a string-to-string map of
    -- key-value pairs.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The HTTP status code of the GatewayResponse.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | Response parameters (paths, query strings and headers) of the
    -- GatewayResponse as a string-to-string map of key-value pairs.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | [Required] The string identifier of the associated RestApi.
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
  Prelude.Text ->
  -- | 'responseType'
  GatewayResponseType ->
  PutGatewayResponse
newPutGatewayResponse pRestApiId_ pResponseType_ =
  PutGatewayResponse'
    { responseTemplates =
        Prelude.Nothing,
      statusCode = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      restApiId = pRestApiId_,
      responseType = pResponseType_
    }

-- | Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
putGatewayResponse_responseTemplates :: Lens.Lens' PutGatewayResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putGatewayResponse_responseTemplates = Lens.lens (\PutGatewayResponse' {responseTemplates} -> responseTemplates) (\s@PutGatewayResponse' {} a -> s {responseTemplates = a} :: PutGatewayResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The HTTP status code of the GatewayResponse.
putGatewayResponse_statusCode :: Lens.Lens' PutGatewayResponse (Prelude.Maybe Prelude.Text)
putGatewayResponse_statusCode = Lens.lens (\PutGatewayResponse' {statusCode} -> statusCode) (\s@PutGatewayResponse' {} a -> s {statusCode = a} :: PutGatewayResponse)

-- | Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
putGatewayResponse_responseParameters :: Lens.Lens' PutGatewayResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putGatewayResponse_responseParameters = Lens.lens (\PutGatewayResponse' {responseParameters} -> responseParameters) (\s@PutGatewayResponse' {} a -> s {responseParameters = a} :: PutGatewayResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | [Required] The string identifier of the associated RestApi.
putGatewayResponse_restApiId :: Lens.Lens' PutGatewayResponse Prelude.Text
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

instance Prelude.AWSRequest PutGatewayResponse where
  type Rs PutGatewayResponse = GatewayResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable PutGatewayResponse

instance Prelude.NFData PutGatewayResponse

instance Prelude.ToHeaders PutGatewayResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON PutGatewayResponse where
  toJSON PutGatewayResponse' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("responseTemplates" Prelude..=)
              Prelude.<$> responseTemplates,
            ("statusCode" Prelude..=) Prelude.<$> statusCode,
            ("responseParameters" Prelude..=)
              Prelude.<$> responseParameters
          ]
      )

instance Prelude.ToPath PutGatewayResponse where
  toPath PutGatewayResponse' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/gatewayresponses/",
        Prelude.toBS responseType
      ]

instance Prelude.ToQuery PutGatewayResponse where
  toQuery = Prelude.const Prelude.mempty
