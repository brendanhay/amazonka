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
-- Module      : Amazonka.ApiGatewayV2.Types.Integration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Integration where

import Amazonka.ApiGatewayV2.Types.ConnectionType
import Amazonka.ApiGatewayV2.Types.ContentHandlingStrategy
import Amazonka.ApiGatewayV2.Types.IntegrationType
import Amazonka.ApiGatewayV2.Types.PassthroughBehavior
import Amazonka.ApiGatewayV2.Types.TlsConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an integration.
--
-- /See:/ 'newIntegration' smart constructor.
data Integration = Integration'
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
    tlsConfig :: Prelude.Maybe TlsConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Integration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiGatewayManaged', 'integration_apiGatewayManaged' - Specifies whether an integration is managed by API Gateway. If you
-- created an API using using quick create, the resulting integration is
-- managed by API Gateway. You can update a managed integration, but you
-- can\'t delete it.
--
-- 'connectionId', 'integration_connectionId' - The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
--
-- 'connectionType', 'integration_connectionType' - The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
--
-- 'contentHandlingStrategy', 'integration_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'credentialsArn', 'integration_credentialsArn' - Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
--
-- 'description', 'integration_description' - Represents the description of an integration.
--
-- 'integrationId', 'integration_integrationId' - Represents the identifier of an integration.
--
-- 'integrationMethod', 'integration_integrationMethod' - Specifies the integration\'s HTTP method type.
--
-- 'integrationResponseSelectionExpression', 'integration_integrationResponseSelectionExpression' - The integration response selection expression for the integration.
-- Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
--
-- 'integrationSubtype', 'integration_integrationSubtype' - Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
--
-- 'integrationType', 'integration_integrationType' - The integration type of an integration. One of the following:
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
-- 'integrationUri', 'integration_integrationUri' - For a Lambda integration, specify the URI of a Lambda function.
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
-- 'passthroughBehavior', 'integration_passthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the
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
-- 'payloadFormatVersion', 'integration_payloadFormatVersion' - Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
--
-- 'requestParameters', 'integration_requestParameters' - For WebSocket APIs, a key-value map specifying request parameters that
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
-- 'requestTemplates', 'integration_requestTemplates' - Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
--
-- 'responseParameters', 'integration_responseParameters' - Supported only for HTTP APIs. You use response parameters to transform
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
-- 'templateSelectionExpression', 'integration_templateSelectionExpression' - The template selection expression for the integration. Supported only
-- for WebSocket APIs.
--
-- 'timeoutInMillis', 'integration_timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
--
-- 'tlsConfig', 'integration_tlsConfig' - The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
newIntegration ::
  Integration
newIntegration =
  Integration'
    { apiGatewayManaged = Prelude.Nothing,
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
      tlsConfig = Prelude.Nothing
    }

-- | Specifies whether an integration is managed by API Gateway. If you
-- created an API using using quick create, the resulting integration is
-- managed by API Gateway. You can update a managed integration, but you
-- can\'t delete it.
integration_apiGatewayManaged :: Lens.Lens' Integration (Prelude.Maybe Prelude.Bool)
integration_apiGatewayManaged = Lens.lens (\Integration' {apiGatewayManaged} -> apiGatewayManaged) (\s@Integration' {} a -> s {apiGatewayManaged = a} :: Integration)

-- | The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
integration_connectionId :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_connectionId = Lens.lens (\Integration' {connectionId} -> connectionId) (\s@Integration' {} a -> s {connectionId = a} :: Integration)

-- | The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
integration_connectionType :: Lens.Lens' Integration (Prelude.Maybe ConnectionType)
integration_connectionType = Lens.lens (\Integration' {connectionType} -> connectionType) (\s@Integration' {} a -> s {connectionType = a} :: Integration)

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
integration_contentHandlingStrategy :: Lens.Lens' Integration (Prelude.Maybe ContentHandlingStrategy)
integration_contentHandlingStrategy = Lens.lens (\Integration' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@Integration' {} a -> s {contentHandlingStrategy = a} :: Integration)

-- | Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
integration_credentialsArn :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_credentialsArn = Lens.lens (\Integration' {credentialsArn} -> credentialsArn) (\s@Integration' {} a -> s {credentialsArn = a} :: Integration)

-- | Represents the description of an integration.
integration_description :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_description = Lens.lens (\Integration' {description} -> description) (\s@Integration' {} a -> s {description = a} :: Integration)

-- | Represents the identifier of an integration.
integration_integrationId :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_integrationId = Lens.lens (\Integration' {integrationId} -> integrationId) (\s@Integration' {} a -> s {integrationId = a} :: Integration)

-- | Specifies the integration\'s HTTP method type.
integration_integrationMethod :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_integrationMethod = Lens.lens (\Integration' {integrationMethod} -> integrationMethod) (\s@Integration' {} a -> s {integrationMethod = a} :: Integration)

-- | The integration response selection expression for the integration.
-- Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
integration_integrationResponseSelectionExpression :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_integrationResponseSelectionExpression = Lens.lens (\Integration' {integrationResponseSelectionExpression} -> integrationResponseSelectionExpression) (\s@Integration' {} a -> s {integrationResponseSelectionExpression = a} :: Integration)

-- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
integration_integrationSubtype :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_integrationSubtype = Lens.lens (\Integration' {integrationSubtype} -> integrationSubtype) (\s@Integration' {} a -> s {integrationSubtype = a} :: Integration)

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
integration_integrationType :: Lens.Lens' Integration (Prelude.Maybe IntegrationType)
integration_integrationType = Lens.lens (\Integration' {integrationType} -> integrationType) (\s@Integration' {} a -> s {integrationType = a} :: Integration)

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
integration_integrationUri :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_integrationUri = Lens.lens (\Integration' {integrationUri} -> integrationUri) (\s@Integration' {} a -> s {integrationUri = a} :: Integration)

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
integration_passthroughBehavior :: Lens.Lens' Integration (Prelude.Maybe PassthroughBehavior)
integration_passthroughBehavior = Lens.lens (\Integration' {passthroughBehavior} -> passthroughBehavior) (\s@Integration' {} a -> s {passthroughBehavior = a} :: Integration)

-- | Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
integration_payloadFormatVersion :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_payloadFormatVersion = Lens.lens (\Integration' {payloadFormatVersion} -> payloadFormatVersion) (\s@Integration' {} a -> s {payloadFormatVersion = a} :: Integration)

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
integration_requestParameters :: Lens.Lens' Integration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
integration_requestParameters = Lens.lens (\Integration' {requestParameters} -> requestParameters) (\s@Integration' {} a -> s {requestParameters = a} :: Integration) Prelude.. Lens.mapping Lens.coerced

-- | Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
integration_requestTemplates :: Lens.Lens' Integration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
integration_requestTemplates = Lens.lens (\Integration' {requestTemplates} -> requestTemplates) (\s@Integration' {} a -> s {requestTemplates = a} :: Integration) Prelude.. Lens.mapping Lens.coerced

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
integration_responseParameters :: Lens.Lens' Integration (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
integration_responseParameters = Lens.lens (\Integration' {responseParameters} -> responseParameters) (\s@Integration' {} a -> s {responseParameters = a} :: Integration) Prelude.. Lens.mapping Lens.coerced

-- | The template selection expression for the integration. Supported only
-- for WebSocket APIs.
integration_templateSelectionExpression :: Lens.Lens' Integration (Prelude.Maybe Prelude.Text)
integration_templateSelectionExpression = Lens.lens (\Integration' {templateSelectionExpression} -> templateSelectionExpression) (\s@Integration' {} a -> s {templateSelectionExpression = a} :: Integration)

-- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
integration_timeoutInMillis :: Lens.Lens' Integration (Prelude.Maybe Prelude.Natural)
integration_timeoutInMillis = Lens.lens (\Integration' {timeoutInMillis} -> timeoutInMillis) (\s@Integration' {} a -> s {timeoutInMillis = a} :: Integration)

-- | The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
integration_tlsConfig :: Lens.Lens' Integration (Prelude.Maybe TlsConfig)
integration_tlsConfig = Lens.lens (\Integration' {tlsConfig} -> tlsConfig) (\s@Integration' {} a -> s {tlsConfig = a} :: Integration)

instance Data.FromJSON Integration where
  parseJSON =
    Data.withObject
      "Integration"
      ( \x ->
          Integration'
            Prelude.<$> (x Data..:? "apiGatewayManaged")
            Prelude.<*> (x Data..:? "connectionId")
            Prelude.<*> (x Data..:? "connectionType")
            Prelude.<*> (x Data..:? "contentHandlingStrategy")
            Prelude.<*> (x Data..:? "credentialsArn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "integrationId")
            Prelude.<*> (x Data..:? "integrationMethod")
            Prelude.<*> (x Data..:? "integrationResponseSelectionExpression")
            Prelude.<*> (x Data..:? "integrationSubtype")
            Prelude.<*> (x Data..:? "integrationType")
            Prelude.<*> (x Data..:? "integrationUri")
            Prelude.<*> (x Data..:? "passthroughBehavior")
            Prelude.<*> (x Data..:? "payloadFormatVersion")
            Prelude.<*> ( x
                            Data..:? "requestParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "requestTemplates"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "responseParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "templateSelectionExpression")
            Prelude.<*> (x Data..:? "timeoutInMillis")
            Prelude.<*> (x Data..:? "tlsConfig")
      )

instance Prelude.Hashable Integration where
  hashWithSalt _salt Integration' {..} =
    _salt
      `Prelude.hashWithSalt` apiGatewayManaged
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` contentHandlingStrategy
      `Prelude.hashWithSalt` credentialsArn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` integrationId
      `Prelude.hashWithSalt` integrationMethod
      `Prelude.hashWithSalt` integrationResponseSelectionExpression
      `Prelude.hashWithSalt` integrationSubtype
      `Prelude.hashWithSalt` integrationType
      `Prelude.hashWithSalt` integrationUri
      `Prelude.hashWithSalt` passthroughBehavior
      `Prelude.hashWithSalt` payloadFormatVersion
      `Prelude.hashWithSalt` requestParameters
      `Prelude.hashWithSalt` requestTemplates
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` templateSelectionExpression
      `Prelude.hashWithSalt` timeoutInMillis
      `Prelude.hashWithSalt` tlsConfig

instance Prelude.NFData Integration where
  rnf Integration' {..} =
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
