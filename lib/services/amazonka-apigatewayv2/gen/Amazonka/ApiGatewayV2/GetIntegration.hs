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
-- Module      : Amazonka.ApiGatewayV2.GetIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Integration.
module Amazonka.ApiGatewayV2.GetIntegration
  ( -- * Creating a Request
    GetIntegration (..),
    newGetIntegration,

    -- * Request Lenses
    getIntegration_apiId,
    getIntegration_integrationId,

    -- * Destructuring the Response
    GetIntegrationResponse' (..),
    newGetIntegrationResponse',

    -- * Response Lenses
    getIntegrationResponse'_apiGatewayManaged,
    getIntegrationResponse'_connectionId,
    getIntegrationResponse'_connectionType,
    getIntegrationResponse'_contentHandlingStrategy,
    getIntegrationResponse'_credentialsArn,
    getIntegrationResponse'_description,
    getIntegrationResponse'_integrationId,
    getIntegrationResponse'_integrationMethod,
    getIntegrationResponse'_integrationResponseSelectionExpression,
    getIntegrationResponse'_integrationSubtype,
    getIntegrationResponse'_integrationType,
    getIntegrationResponse'_integrationUri,
    getIntegrationResponse'_passthroughBehavior,
    getIntegrationResponse'_payloadFormatVersion,
    getIntegrationResponse'_requestParameters,
    getIntegrationResponse'_requestTemplates,
    getIntegrationResponse'_responseParameters,
    getIntegrationResponse'_templateSelectionExpression,
    getIntegrationResponse'_timeoutInMillis,
    getIntegrationResponse'_tlsConfig,
    getIntegrationResponse'_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetIntegration' smart constructor.
data GetIntegration = GetIntegration'
  { -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'getIntegration_apiId' - The API identifier.
--
-- 'integrationId', 'getIntegration_integrationId' - The integration ID.
newGetIntegration ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  GetIntegration
newGetIntegration pApiId_ pIntegrationId_ =
  GetIntegration'
    { apiId = pApiId_,
      integrationId = pIntegrationId_
    }

-- | The API identifier.
getIntegration_apiId :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_apiId = Lens.lens (\GetIntegration' {apiId} -> apiId) (\s@GetIntegration' {} a -> s {apiId = a} :: GetIntegration)

-- | The integration ID.
getIntegration_integrationId :: Lens.Lens' GetIntegration Prelude.Text
getIntegration_integrationId = Lens.lens (\GetIntegration' {integrationId} -> integrationId) (\s@GetIntegration' {} a -> s {integrationId = a} :: GetIntegration)

instance Core.AWSRequest GetIntegration where
  type
    AWSResponse GetIntegration =
      GetIntegrationResponse'
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIntegrationResponse''
            Prelude.<$> (x Data..?> "apiGatewayManaged")
            Prelude.<*> (x Data..?> "connectionId")
            Prelude.<*> (x Data..?> "connectionType")
            Prelude.<*> (x Data..?> "contentHandlingStrategy")
            Prelude.<*> (x Data..?> "credentialsArn")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "integrationId")
            Prelude.<*> (x Data..?> "integrationMethod")
            Prelude.<*> (x Data..?> "integrationResponseSelectionExpression")
            Prelude.<*> (x Data..?> "integrationSubtype")
            Prelude.<*> (x Data..?> "integrationType")
            Prelude.<*> (x Data..?> "integrationUri")
            Prelude.<*> (x Data..?> "passthroughBehavior")
            Prelude.<*> (x Data..?> "payloadFormatVersion")
            Prelude.<*> ( x
                            Data..?> "requestParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "requestTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "responseParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "templateSelectionExpression")
            Prelude.<*> (x Data..?> "timeoutInMillis")
            Prelude.<*> (x Data..?> "tlsConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetIntegration where
  hashWithSalt _salt GetIntegration' {..} =
    _salt
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` integrationId

instance Prelude.NFData GetIntegration where
  rnf GetIntegration' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf integrationId

instance Data.ToHeaders GetIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIntegration where
  toPath GetIntegration' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/integrations/",
        Data.toBS integrationId
      ]

instance Data.ToQuery GetIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetIntegrationResponse'' smart constructor.
data GetIntegrationResponse' = GetIntegrationResponse''
  { -- | Specifies whether an integration is managed by API Gateway. If you
    -- created an API using using quick create, the resulting integration is
    -- managed by API Gateway. You can update a managed integration, but you
    -- can\'t delete it.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the VPC link for a private integration. Supported only for
    -- HTTP APIs.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The type of the network connection to the integration endpoint. Specify
    -- INTERNET for connections through the public routable internet or
    -- VPC_LINK for private connections between API Gateway and resources in a
    -- VPC. The default value is INTERNET.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | Supported only for WebSocket APIs. Specifies how to handle response
    -- payload content type conversions. Supported values are CONVERT_TO_BINARY
    -- and CONVERT_TO_TEXT, with the following behaviors:
    --
    -- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
    -- string to the corresponding binary blob.
    --
    -- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
    -- Base64-encoded string.
    --
    -- If this property is not defined, the response payload will be passed
    -- through from the integration response to the route response or method
    -- response without modification.
    contentHandlingStrategy :: Prelude.Maybe ContentHandlingStrategy,
    -- | Specifies the credentials required for the integration, if any. For AWS
    -- integrations, three options are available. To specify an IAM Role for
    -- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
    -- require that the caller\'s identity be passed through from the request,
    -- specify the string arn:aws:iam::*:user\/*. To use resource-based
    -- permissions on supported AWS services, specify null.
    credentialsArn :: Prelude.Maybe Prelude.Text,
    -- | Represents the description of an integration.
    description :: Prelude.Maybe Prelude.Text,
    -- | Represents the identifier of an integration.
    integrationId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the integration\'s HTTP method type.
    integrationMethod :: Prelude.Maybe Prelude.Text,
    -- | The integration response selection expression for the integration.
    -- Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
    integrationResponseSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
    -- service action to invoke. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
    integrationSubtype :: Prelude.Maybe Prelude.Text,
    -- | The integration type of an integration. One of the following:
    --
    -- AWS: for integrating the route or method request with an AWS service
    -- action, including the Lambda function-invoking action. With the Lambda
    -- function-invoking action, this is referred to as the Lambda custom
    -- integration. With any other AWS service action, this is known as AWS
    -- integration. Supported only for WebSocket APIs.
    --
    -- AWS_PROXY: for integrating the route or method request with a Lambda
    -- function or other AWS service action. This integration is also referred
    -- to as a Lambda proxy integration.
    --
    -- HTTP: for integrating the route or method request with an HTTP endpoint.
    -- This integration is also referred to as the HTTP custom integration.
    -- Supported only for WebSocket APIs.
    --
    -- HTTP_PROXY: for integrating the route or method request with an HTTP
    -- endpoint, with the client request passed through as-is. This is also
    -- referred to as HTTP proxy integration.
    --
    -- MOCK: for integrating the route or method request with API Gateway as a
    -- \"loopback\" endpoint without invoking any backend. Supported only for
    -- WebSocket APIs.
    integrationType :: Prelude.Maybe IntegrationType,
    -- | For a Lambda integration, specify the URI of a Lambda function.
    --
    -- For an HTTP integration, specify a fully-qualified URL.
    --
    -- For an HTTP API private integration, specify the ARN of an Application
    -- Load Balancer listener, Network Load Balancer listener, or AWS Cloud Map
    -- service. If you specify the ARN of an AWS Cloud Map service, API Gateway
    -- uses DiscoverInstances to identify resources. You can use query
    -- parameters to target specific resources. To learn more, see
    -- <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances>.
    -- For private integrations, all resources must be owned by the same AWS
    -- account.
    integrationUri :: Prelude.Maybe Prelude.Text,
    -- | Specifies the pass-through behavior for incoming requests based on the
    -- Content-Type header in the request, and the available mapping templates
    -- specified as the requestTemplates property on the Integration resource.
    -- There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and
    -- NEVER. Supported only for WebSocket APIs.
    --
    -- WHEN_NO_MATCH passes the request body for unmapped content types through
    -- to the integration backend without transformation.
    --
    -- NEVER rejects unmapped content types with an HTTP 415 Unsupported Media
    -- Type response.
    --
    -- WHEN_NO_TEMPLATES allows pass-through when the integration has no
    -- content types mapped to templates. However, if there is at least one
    -- content type defined, unmapped content types will be rejected with the
    -- same HTTP 415 Unsupported Media Type response.
    passthroughBehavior :: Prelude.Maybe PassthroughBehavior,
    -- | Specifies the format of the payload sent to an integration. Required for
    -- HTTP APIs.
    payloadFormatVersion :: Prelude.Maybe Prelude.Text,
    -- | For WebSocket APIs, a key-value map specifying request parameters that
    -- are passed from the method request to the backend. The key is an
    -- integration request parameter name and the associated value is a method
    -- request parameter value or static value that must be enclosed within
    -- single quotes and pre-encoded as required by the backend. The method
    -- request parameter value must match the pattern of
    -- method.request.{location}.{name} , where {location} is querystring,
    -- path, or header; and {name} must be a valid and unique method request
    -- parameter name.
    --
    -- For HTTP API integrations with a specified integrationSubtype, request
    -- parameters are a key-value map specifying parameters that are passed to
    -- AWS_PROXY integrations. You can provide static values, or map request
    -- data, stage variables, or context variables that are evaluated at
    -- runtime. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services.html Working with AWS service integrations for HTTP APIs>.
    --
    -- For HTTP API itegrations, without a specified integrationSubtype request
    -- parameters are a key-value map specifying how to transform HTTP requests
    -- before sending them to backend integrations. The key should follow the
    -- pattern \<action>:\<header|querystring|path>.\<location>. The action can
    -- be append, overwrite or remove. For values, you can provide static
    -- values, or map request data, stage variables, or context variables that
    -- are evaluated at runtime. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping.html Transforming API requests and responses>.
    requestParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Represents a map of Velocity templates that are applied on the request
    -- payload based on the value of the Content-Type header sent by the
    -- client. The content type value is the key in this map, and the template
    -- (as a String) is the value. Supported only for WebSocket APIs.
    requestTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Supported only for HTTP APIs. You use response parameters to transform
    -- the HTTP response from a backend integration before returning the
    -- response to clients. Specify a key-value map from a selection key to
    -- response parameters. The selection key must be a valid HTTP status code
    -- within the range of 200-599. Response parameters are a key-value map.
    -- The key must match pattern \<action>:\<header>.\<location> or
    -- overwrite.statuscode. The action can be append, overwrite or remove. The
    -- value can be a static value, or map to response data, stage variables,
    -- or context variables that are evaluated at runtime. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping.html Transforming API requests and responses>.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | The template selection expression for the integration. Supported only
    -- for WebSocket APIs.
    templateSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
    -- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
    -- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
    timeoutInMillis :: Prelude.Maybe Prelude.Natural,
    -- | The TLS configuration for a private integration. If you specify a TLS
    -- configuration, private integration traffic uses the HTTPS protocol.
    -- Supported only for HTTP APIs.
    tlsConfig :: Prelude.Maybe TlsConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIntegrationResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayManaged', 'getIntegrationResponse'_apiGatewayManaged' - Specifies whether an integration is managed by API Gateway. If you
-- created an API using using quick create, the resulting integration is
-- managed by API Gateway. You can update a managed integration, but you
-- can\'t delete it.
--
-- 'connectionId', 'getIntegrationResponse'_connectionId' - The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
--
-- 'connectionType', 'getIntegrationResponse'_connectionType' - The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
--
-- 'contentHandlingStrategy', 'getIntegrationResponse'_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
-- payload content type conversions. Supported values are CONVERT_TO_BINARY
-- and CONVERT_TO_TEXT, with the following behaviors:
--
-- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
-- string to the corresponding binary blob.
--
-- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
-- Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the route response or method
-- response without modification.
--
-- 'credentialsArn', 'getIntegrationResponse'_credentialsArn' - Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
--
-- 'description', 'getIntegrationResponse'_description' - Represents the description of an integration.
--
-- 'integrationId', 'getIntegrationResponse'_integrationId' - Represents the identifier of an integration.
--
-- 'integrationMethod', 'getIntegrationResponse'_integrationMethod' - Specifies the integration\'s HTTP method type.
--
-- 'integrationResponseSelectionExpression', 'getIntegrationResponse'_integrationResponseSelectionExpression' - The integration response selection expression for the integration.
-- Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
--
-- 'integrationSubtype', 'getIntegrationResponse'_integrationSubtype' - Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
--
-- 'integrationType', 'getIntegrationResponse'_integrationType' - The integration type of an integration. One of the following:
--
-- AWS: for integrating the route or method request with an AWS service
-- action, including the Lambda function-invoking action. With the Lambda
-- function-invoking action, this is referred to as the Lambda custom
-- integration. With any other AWS service action, this is known as AWS
-- integration. Supported only for WebSocket APIs.
--
-- AWS_PROXY: for integrating the route or method request with a Lambda
-- function or other AWS service action. This integration is also referred
-- to as a Lambda proxy integration.
--
-- HTTP: for integrating the route or method request with an HTTP endpoint.
-- This integration is also referred to as the HTTP custom integration.
-- Supported only for WebSocket APIs.
--
-- HTTP_PROXY: for integrating the route or method request with an HTTP
-- endpoint, with the client request passed through as-is. This is also
-- referred to as HTTP proxy integration.
--
-- MOCK: for integrating the route or method request with API Gateway as a
-- \"loopback\" endpoint without invoking any backend. Supported only for
-- WebSocket APIs.
--
-- 'integrationUri', 'getIntegrationResponse'_integrationUri' - For a Lambda integration, specify the URI of a Lambda function.
--
-- For an HTTP integration, specify a fully-qualified URL.
--
-- For an HTTP API private integration, specify the ARN of an Application
-- Load Balancer listener, Network Load Balancer listener, or AWS Cloud Map
-- service. If you specify the ARN of an AWS Cloud Map service, API Gateway
-- uses DiscoverInstances to identify resources. You can use query
-- parameters to target specific resources. To learn more, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances>.
-- For private integrations, all resources must be owned by the same AWS
-- account.
--
-- 'passthroughBehavior', 'getIntegrationResponse'_passthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the
-- Content-Type header in the request, and the available mapping templates
-- specified as the requestTemplates property on the Integration resource.
-- There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and
-- NEVER. Supported only for WebSocket APIs.
--
-- WHEN_NO_MATCH passes the request body for unmapped content types through
-- to the integration backend without transformation.
--
-- NEVER rejects unmapped content types with an HTTP 415 Unsupported Media
-- Type response.
--
-- WHEN_NO_TEMPLATES allows pass-through when the integration has no
-- content types mapped to templates. However, if there is at least one
-- content type defined, unmapped content types will be rejected with the
-- same HTTP 415 Unsupported Media Type response.
--
-- 'payloadFormatVersion', 'getIntegrationResponse'_payloadFormatVersion' - Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
--
-- 'requestParameters', 'getIntegrationResponse'_requestParameters' - For WebSocket APIs, a key-value map specifying request parameters that
-- are passed from the method request to the backend. The key is an
-- integration request parameter name and the associated value is a method
-- request parameter value or static value that must be enclosed within
-- single quotes and pre-encoded as required by the backend. The method
-- request parameter value must match the pattern of
-- method.request.{location}.{name} , where {location} is querystring,
-- path, or header; and {name} must be a valid and unique method request
-- parameter name.
--
-- For HTTP API integrations with a specified integrationSubtype, request
-- parameters are a key-value map specifying parameters that are passed to
-- AWS_PROXY integrations. You can provide static values, or map request
-- data, stage variables, or context variables that are evaluated at
-- runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services.html Working with AWS service integrations for HTTP APIs>.
--
-- For HTTP API itegrations, without a specified integrationSubtype request
-- parameters are a key-value map specifying how to transform HTTP requests
-- before sending them to backend integrations. The key should follow the
-- pattern \<action>:\<header|querystring|path>.\<location>. The action can
-- be append, overwrite or remove. For values, you can provide static
-- values, or map request data, stage variables, or context variables that
-- are evaluated at runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping.html Transforming API requests and responses>.
--
-- 'requestTemplates', 'getIntegrationResponse'_requestTemplates' - Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
--
-- 'responseParameters', 'getIntegrationResponse'_responseParameters' - Supported only for HTTP APIs. You use response parameters to transform
-- the HTTP response from a backend integration before returning the
-- response to clients. Specify a key-value map from a selection key to
-- response parameters. The selection key must be a valid HTTP status code
-- within the range of 200-599. Response parameters are a key-value map.
-- The key must match pattern \<action>:\<header>.\<location> or
-- overwrite.statuscode. The action can be append, overwrite or remove. The
-- value can be a static value, or map to response data, stage variables,
-- or context variables that are evaluated at runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping.html Transforming API requests and responses>.
--
-- 'templateSelectionExpression', 'getIntegrationResponse'_templateSelectionExpression' - The template selection expression for the integration. Supported only
-- for WebSocket APIs.
--
-- 'timeoutInMillis', 'getIntegrationResponse'_timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
--
-- 'tlsConfig', 'getIntegrationResponse'_tlsConfig' - The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
--
-- 'httpStatus', 'getIntegrationResponse'_httpStatus' - The response's http status code.
newGetIntegrationResponse' ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetIntegrationResponse'
newGetIntegrationResponse' pHttpStatus_ =
  GetIntegrationResponse''
    { apiGatewayManaged =
        Prelude.Nothing,
      connectionId = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      contentHandlingStrategy = Prelude.Nothing,
      credentialsArn = Prelude.Nothing,
      description = Prelude.Nothing,
      integrationId = Prelude.Nothing,
      integrationMethod = Prelude.Nothing,
      integrationResponseSelectionExpression =
        Prelude.Nothing,
      integrationSubtype = Prelude.Nothing,
      integrationType = Prelude.Nothing,
      integrationUri = Prelude.Nothing,
      passthroughBehavior = Prelude.Nothing,
      payloadFormatVersion = Prelude.Nothing,
      requestParameters = Prelude.Nothing,
      requestTemplates = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      templateSelectionExpression = Prelude.Nothing,
      timeoutInMillis = Prelude.Nothing,
      tlsConfig = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies whether an integration is managed by API Gateway. If you
-- created an API using using quick create, the resulting integration is
-- managed by API Gateway. You can update a managed integration, but you
-- can\'t delete it.
getIntegrationResponse'_apiGatewayManaged :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Bool)
getIntegrationResponse'_apiGatewayManaged = Lens.lens (\GetIntegrationResponse'' {apiGatewayManaged} -> apiGatewayManaged) (\s@GetIntegrationResponse'' {} a -> s {apiGatewayManaged = a} :: GetIntegrationResponse')

-- | The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
getIntegrationResponse'_connectionId :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_connectionId = Lens.lens (\GetIntegrationResponse'' {connectionId} -> connectionId) (\s@GetIntegrationResponse'' {} a -> s {connectionId = a} :: GetIntegrationResponse')

-- | The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
getIntegrationResponse'_connectionType :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe ConnectionType)
getIntegrationResponse'_connectionType = Lens.lens (\GetIntegrationResponse'' {connectionType} -> connectionType) (\s@GetIntegrationResponse'' {} a -> s {connectionType = a} :: GetIntegrationResponse')

-- | Supported only for WebSocket APIs. Specifies how to handle response
-- payload content type conversions. Supported values are CONVERT_TO_BINARY
-- and CONVERT_TO_TEXT, with the following behaviors:
--
-- CONVERT_TO_BINARY: Converts a response payload from a Base64-encoded
-- string to the corresponding binary blob.
--
-- CONVERT_TO_TEXT: Converts a response payload from a binary blob to a
-- Base64-encoded string.
--
-- If this property is not defined, the response payload will be passed
-- through from the integration response to the route response or method
-- response without modification.
getIntegrationResponse'_contentHandlingStrategy :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe ContentHandlingStrategy)
getIntegrationResponse'_contentHandlingStrategy = Lens.lens (\GetIntegrationResponse'' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@GetIntegrationResponse'' {} a -> s {contentHandlingStrategy = a} :: GetIntegrationResponse')

-- | Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
getIntegrationResponse'_credentialsArn :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_credentialsArn = Lens.lens (\GetIntegrationResponse'' {credentialsArn} -> credentialsArn) (\s@GetIntegrationResponse'' {} a -> s {credentialsArn = a} :: GetIntegrationResponse')

-- | Represents the description of an integration.
getIntegrationResponse'_description :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_description = Lens.lens (\GetIntegrationResponse'' {description} -> description) (\s@GetIntegrationResponse'' {} a -> s {description = a} :: GetIntegrationResponse')

-- | Represents the identifier of an integration.
getIntegrationResponse'_integrationId :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_integrationId = Lens.lens (\GetIntegrationResponse'' {integrationId} -> integrationId) (\s@GetIntegrationResponse'' {} a -> s {integrationId = a} :: GetIntegrationResponse')

-- | Specifies the integration\'s HTTP method type.
getIntegrationResponse'_integrationMethod :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_integrationMethod = Lens.lens (\GetIntegrationResponse'' {integrationMethod} -> integrationMethod) (\s@GetIntegrationResponse'' {} a -> s {integrationMethod = a} :: GetIntegrationResponse')

-- | The integration response selection expression for the integration.
-- Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
getIntegrationResponse'_integrationResponseSelectionExpression :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_integrationResponseSelectionExpression = Lens.lens (\GetIntegrationResponse'' {integrationResponseSelectionExpression} -> integrationResponseSelectionExpression) (\s@GetIntegrationResponse'' {} a -> s {integrationResponseSelectionExpression = a} :: GetIntegrationResponse')

-- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
getIntegrationResponse'_integrationSubtype :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_integrationSubtype = Lens.lens (\GetIntegrationResponse'' {integrationSubtype} -> integrationSubtype) (\s@GetIntegrationResponse'' {} a -> s {integrationSubtype = a} :: GetIntegrationResponse')

-- | The integration type of an integration. One of the following:
--
-- AWS: for integrating the route or method request with an AWS service
-- action, including the Lambda function-invoking action. With the Lambda
-- function-invoking action, this is referred to as the Lambda custom
-- integration. With any other AWS service action, this is known as AWS
-- integration. Supported only for WebSocket APIs.
--
-- AWS_PROXY: for integrating the route or method request with a Lambda
-- function or other AWS service action. This integration is also referred
-- to as a Lambda proxy integration.
--
-- HTTP: for integrating the route or method request with an HTTP endpoint.
-- This integration is also referred to as the HTTP custom integration.
-- Supported only for WebSocket APIs.
--
-- HTTP_PROXY: for integrating the route or method request with an HTTP
-- endpoint, with the client request passed through as-is. This is also
-- referred to as HTTP proxy integration.
--
-- MOCK: for integrating the route or method request with API Gateway as a
-- \"loopback\" endpoint without invoking any backend. Supported only for
-- WebSocket APIs.
getIntegrationResponse'_integrationType :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe IntegrationType)
getIntegrationResponse'_integrationType = Lens.lens (\GetIntegrationResponse'' {integrationType} -> integrationType) (\s@GetIntegrationResponse'' {} a -> s {integrationType = a} :: GetIntegrationResponse')

-- | For a Lambda integration, specify the URI of a Lambda function.
--
-- For an HTTP integration, specify a fully-qualified URL.
--
-- For an HTTP API private integration, specify the ARN of an Application
-- Load Balancer listener, Network Load Balancer listener, or AWS Cloud Map
-- service. If you specify the ARN of an AWS Cloud Map service, API Gateway
-- uses DiscoverInstances to identify resources. You can use query
-- parameters to target specific resources. To learn more, see
-- <https://docs.aws.amazon.com/cloud-map/latest/api/API_DiscoverInstances.html DiscoverInstances>.
-- For private integrations, all resources must be owned by the same AWS
-- account.
getIntegrationResponse'_integrationUri :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_integrationUri = Lens.lens (\GetIntegrationResponse'' {integrationUri} -> integrationUri) (\s@GetIntegrationResponse'' {} a -> s {integrationUri = a} :: GetIntegrationResponse')

-- | Specifies the pass-through behavior for incoming requests based on the
-- Content-Type header in the request, and the available mapping templates
-- specified as the requestTemplates property on the Integration resource.
-- There are three valid values: WHEN_NO_MATCH, WHEN_NO_TEMPLATES, and
-- NEVER. Supported only for WebSocket APIs.
--
-- WHEN_NO_MATCH passes the request body for unmapped content types through
-- to the integration backend without transformation.
--
-- NEVER rejects unmapped content types with an HTTP 415 Unsupported Media
-- Type response.
--
-- WHEN_NO_TEMPLATES allows pass-through when the integration has no
-- content types mapped to templates. However, if there is at least one
-- content type defined, unmapped content types will be rejected with the
-- same HTTP 415 Unsupported Media Type response.
getIntegrationResponse'_passthroughBehavior :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe PassthroughBehavior)
getIntegrationResponse'_passthroughBehavior = Lens.lens (\GetIntegrationResponse'' {passthroughBehavior} -> passthroughBehavior) (\s@GetIntegrationResponse'' {} a -> s {passthroughBehavior = a} :: GetIntegrationResponse')

-- | Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
getIntegrationResponse'_payloadFormatVersion :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_payloadFormatVersion = Lens.lens (\GetIntegrationResponse'' {payloadFormatVersion} -> payloadFormatVersion) (\s@GetIntegrationResponse'' {} a -> s {payloadFormatVersion = a} :: GetIntegrationResponse')

-- | For WebSocket APIs, a key-value map specifying request parameters that
-- are passed from the method request to the backend. The key is an
-- integration request parameter name and the associated value is a method
-- request parameter value or static value that must be enclosed within
-- single quotes and pre-encoded as required by the backend. The method
-- request parameter value must match the pattern of
-- method.request.{location}.{name} , where {location} is querystring,
-- path, or header; and {name} must be a valid and unique method request
-- parameter name.
--
-- For HTTP API integrations with a specified integrationSubtype, request
-- parameters are a key-value map specifying parameters that are passed to
-- AWS_PROXY integrations. You can provide static values, or map request
-- data, stage variables, or context variables that are evaluated at
-- runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services.html Working with AWS service integrations for HTTP APIs>.
--
-- For HTTP API itegrations, without a specified integrationSubtype request
-- parameters are a key-value map specifying how to transform HTTP requests
-- before sending them to backend integrations. The key should follow the
-- pattern \<action>:\<header|querystring|path>.\<location>. The action can
-- be append, overwrite or remove. For values, you can provide static
-- values, or map request data, stage variables, or context variables that
-- are evaluated at runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping.html Transforming API requests and responses>.
getIntegrationResponse'_requestParameters :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponse'_requestParameters = Lens.lens (\GetIntegrationResponse'' {requestParameters} -> requestParameters) (\s@GetIntegrationResponse'' {} a -> s {requestParameters = a} :: GetIntegrationResponse') Prelude.. Lens.mapping Lens.coerced

-- | Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
getIntegrationResponse'_requestTemplates :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getIntegrationResponse'_requestTemplates = Lens.lens (\GetIntegrationResponse'' {requestTemplates} -> requestTemplates) (\s@GetIntegrationResponse'' {} a -> s {requestTemplates = a} :: GetIntegrationResponse') Prelude.. Lens.mapping Lens.coerced

-- | Supported only for HTTP APIs. You use response parameters to transform
-- the HTTP response from a backend integration before returning the
-- response to clients. Specify a key-value map from a selection key to
-- response parameters. The selection key must be a valid HTTP status code
-- within the range of 200-599. Response parameters are a key-value map.
-- The key must match pattern \<action>:\<header>.\<location> or
-- overwrite.statuscode. The action can be append, overwrite or remove. The
-- value can be a static value, or map to response data, stage variables,
-- or context variables that are evaluated at runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping.html Transforming API requests and responses>.
getIntegrationResponse'_responseParameters :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
getIntegrationResponse'_responseParameters = Lens.lens (\GetIntegrationResponse'' {responseParameters} -> responseParameters) (\s@GetIntegrationResponse'' {} a -> s {responseParameters = a} :: GetIntegrationResponse') Prelude.. Lens.mapping Lens.coerced

-- | The template selection expression for the integration. Supported only
-- for WebSocket APIs.
getIntegrationResponse'_templateSelectionExpression :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Text)
getIntegrationResponse'_templateSelectionExpression = Lens.lens (\GetIntegrationResponse'' {templateSelectionExpression} -> templateSelectionExpression) (\s@GetIntegrationResponse'' {} a -> s {templateSelectionExpression = a} :: GetIntegrationResponse')

-- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
getIntegrationResponse'_timeoutInMillis :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe Prelude.Natural)
getIntegrationResponse'_timeoutInMillis = Lens.lens (\GetIntegrationResponse'' {timeoutInMillis} -> timeoutInMillis) (\s@GetIntegrationResponse'' {} a -> s {timeoutInMillis = a} :: GetIntegrationResponse')

-- | The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
getIntegrationResponse'_tlsConfig :: Lens.Lens' GetIntegrationResponse' (Prelude.Maybe TlsConfig)
getIntegrationResponse'_tlsConfig = Lens.lens (\GetIntegrationResponse'' {tlsConfig} -> tlsConfig) (\s@GetIntegrationResponse'' {} a -> s {tlsConfig = a} :: GetIntegrationResponse')

-- | The response's http status code.
getIntegrationResponse'_httpStatus :: Lens.Lens' GetIntegrationResponse' Prelude.Int
getIntegrationResponse'_httpStatus = Lens.lens (\GetIntegrationResponse'' {httpStatus} -> httpStatus) (\s@GetIntegrationResponse'' {} a -> s {httpStatus = a} :: GetIntegrationResponse')

instance Prelude.NFData GetIntegrationResponse' where
  rnf GetIntegrationResponse'' {..} =
    Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf integrationId
      `Prelude.seq` Prelude.rnf integrationMethod
      `Prelude.seq` Prelude.rnf integrationResponseSelectionExpression
      `Prelude.seq` Prelude.rnf integrationSubtype
      `Prelude.seq` Prelude.rnf integrationType
      `Prelude.seq` Prelude.rnf integrationUri
      `Prelude.seq` Prelude.rnf passthroughBehavior
      `Prelude.seq` Prelude.rnf payloadFormatVersion
      `Prelude.seq` Prelude.rnf requestParameters
      `Prelude.seq` Prelude.rnf requestTemplates
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf
        templateSelectionExpression
      `Prelude.seq` Prelude.rnf timeoutInMillis
      `Prelude.seq` Prelude.rnf tlsConfig
      `Prelude.seq` Prelude.rnf httpStatus
