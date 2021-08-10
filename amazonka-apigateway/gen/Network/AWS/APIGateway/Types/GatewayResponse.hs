{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.GatewayResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.GatewayResponse where

import Network.AWS.APIGateway.Types.GatewayResponseType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A gateway response of a given response type and status code, with
-- optional response parameters and mapping templates.
--
-- For more information about valid gateway response types, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/supported-gateway-response-types.html Gateway Response Types Supported by API Gateway>
--
-- ==== Example: Get a Gateway Response of a given response type
--
-- ===== Request
--
-- This example shows how to get a gateway response of the
-- @MISSING_AUTHENTICATION_TOKEN@ type.
--
-- > GET /restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN HTTP/1.1 Host: beta-apigateway.us-east-1.amazonaws.com Content-Type: application/json X-Amz-Date: 20170503T202516Z Authorization: AWS4-HMAC-SHA256 Credential={access-key-id}/20170503/us-east-1/apigateway/aws4_request, SignedHeaders=content-type;host;x-amz-date, Signature=1b52460e3159c1a26cff29093855d50ea141c1c5b937528fecaf60f51129697a Cache-Control: no-cache Postman-Token: 3b2a1ce9-c848-2e26-2e2f-9c2caefbed45
--
-- The response type is specified as a URL path.
--
-- ===== Response
--
-- The successful operation returns the @200 OK@ status code and a payload
-- similar to the following:
--
-- > { "_links": { "curies": { "href": "http://docs.aws.amazon.com/apigateway/latest/developerguide/restapi-gatewayresponse-{rel}.html", "name": "gatewayresponse", "templated": true }, "self": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:delete": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" }, "gatewayresponse:put": { "href": "/restapis/o81lxisefl/gatewayresponses/{response_type}", "templated": true }, "gatewayresponse:update": { "href": "/restapis/o81lxisefl/gatewayresponses/MISSING_AUTHENTICATION_TOKEN" } }, "defaultResponse": false, "responseParameters": { "gatewayresponse.header.x-request-path": "method.request.path.petId", "gatewayresponse.header.Access-Control-Allow-Origin": "'a.b.c'", "gatewayresponse.header.x-request-query": "method.request.querystring.q", "gatewayresponse.header.x-request-header": "method.request.header.Accept" }, "responseTemplates": { "application/json": "{\n \"message\": $context.error.messageString,\n \"type\": \"$context.error.responseType\",\n \"stage\": \"$context.stage\",\n \"resourcePath\": \"$context.resourcePath\",\n \"stageVariables.a\": \"$stageVariables.a\",\n \"statusCode\": \"'404'\"\n}" }, "responseType": "MISSING_AUTHENTICATION_TOKEN", "statusCode": "404" }
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/customize-gateway-responses.html Customize Gateway Responses>
--
-- /See:/ 'newGatewayResponse' smart constructor.
data GatewayResponse = GatewayResponse'
  { -- | Response templates of the GatewayResponse as a string-to-string map of
    -- key-value pairs.
    responseTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The HTTP status code for this GatewayResponse.
    statusCode :: Prelude.Maybe Prelude.Text,
    -- | Response parameters (paths, query strings and headers) of the
    -- GatewayResponse as a string-to-string map of key-value pairs.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response type of the associated GatewayResponse. Valid values are
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
    responseType :: Prelude.Maybe GatewayResponseType,
    -- | A Boolean flag to indicate whether this GatewayResponse is the default
    -- gateway response (@true@) or not (@false@). A default gateway response
    -- is one generated by API Gateway without any customization by an API
    -- developer.
    defaultResponse :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'responseTemplates', 'gatewayResponse_responseTemplates' - Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
--
-- 'statusCode', 'gatewayResponse_statusCode' - The HTTP status code for this GatewayResponse.
--
-- 'responseParameters', 'gatewayResponse_responseParameters' - Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
--
-- 'responseType', 'gatewayResponse_responseType' - The response type of the associated GatewayResponse. Valid values are
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
--
-- 'defaultResponse', 'gatewayResponse_defaultResponse' - A Boolean flag to indicate whether this GatewayResponse is the default
-- gateway response (@true@) or not (@false@). A default gateway response
-- is one generated by API Gateway without any customization by an API
-- developer.
newGatewayResponse ::
  GatewayResponse
newGatewayResponse =
  GatewayResponse'
    { responseTemplates =
        Prelude.Nothing,
      statusCode = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      responseType = Prelude.Nothing,
      defaultResponse = Prelude.Nothing
    }

-- | Response templates of the GatewayResponse as a string-to-string map of
-- key-value pairs.
gatewayResponse_responseTemplates :: Lens.Lens' GatewayResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
gatewayResponse_responseTemplates = Lens.lens (\GatewayResponse' {responseTemplates} -> responseTemplates) (\s@GatewayResponse' {} a -> s {responseTemplates = a} :: GatewayResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The HTTP status code for this GatewayResponse.
gatewayResponse_statusCode :: Lens.Lens' GatewayResponse (Prelude.Maybe Prelude.Text)
gatewayResponse_statusCode = Lens.lens (\GatewayResponse' {statusCode} -> statusCode) (\s@GatewayResponse' {} a -> s {statusCode = a} :: GatewayResponse)

-- | Response parameters (paths, query strings and headers) of the
-- GatewayResponse as a string-to-string map of key-value pairs.
gatewayResponse_responseParameters :: Lens.Lens' GatewayResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
gatewayResponse_responseParameters = Lens.lens (\GatewayResponse' {responseParameters} -> responseParameters) (\s@GatewayResponse' {} a -> s {responseParameters = a} :: GatewayResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response type of the associated GatewayResponse. Valid values are
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
gatewayResponse_responseType :: Lens.Lens' GatewayResponse (Prelude.Maybe GatewayResponseType)
gatewayResponse_responseType = Lens.lens (\GatewayResponse' {responseType} -> responseType) (\s@GatewayResponse' {} a -> s {responseType = a} :: GatewayResponse)

-- | A Boolean flag to indicate whether this GatewayResponse is the default
-- gateway response (@true@) or not (@false@). A default gateway response
-- is one generated by API Gateway without any customization by an API
-- developer.
gatewayResponse_defaultResponse :: Lens.Lens' GatewayResponse (Prelude.Maybe Prelude.Bool)
gatewayResponse_defaultResponse = Lens.lens (\GatewayResponse' {defaultResponse} -> defaultResponse) (\s@GatewayResponse' {} a -> s {defaultResponse = a} :: GatewayResponse)

instance Core.FromJSON GatewayResponse where
  parseJSON =
    Core.withObject
      "GatewayResponse"
      ( \x ->
          GatewayResponse'
            Prelude.<$> ( x Core..:? "responseTemplates"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "statusCode")
            Prelude.<*> ( x Core..:? "responseParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "responseType")
            Prelude.<*> (x Core..:? "defaultResponse")
      )

instance Prelude.Hashable GatewayResponse

instance Prelude.NFData GatewayResponse
