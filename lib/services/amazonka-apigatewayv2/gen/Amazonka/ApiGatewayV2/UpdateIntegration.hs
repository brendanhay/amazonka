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
-- Module      : Amazonka.ApiGatewayV2.UpdateIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Integration.
module Amazonka.ApiGatewayV2.UpdateIntegration
  ( -- * Creating a Request
    UpdateIntegration (..),
    newUpdateIntegration,

    -- * Request Lenses
    updateIntegration_requestTemplates,
    updateIntegration_integrationSubtype,
    updateIntegration_credentialsArn,
    updateIntegration_integrationUri,
    updateIntegration_requestParameters,
    updateIntegration_connectionId,
    updateIntegration_passthroughBehavior,
    updateIntegration_integrationMethod,
    updateIntegration_tlsConfig,
    updateIntegration_payloadFormatVersion,
    updateIntegration_templateSelectionExpression,
    updateIntegration_timeoutInMillis,
    updateIntegration_contentHandlingStrategy,
    updateIntegration_integrationType,
    updateIntegration_description,
    updateIntegration_connectionType,
    updateIntegration_responseParameters,
    updateIntegration_apiId,
    updateIntegration_integrationId,

    -- * Destructuring the Response
    UpdateIntegrationResponse' (..),
    newUpdateIntegrationResponse',

    -- * Response Lenses
    updateIntegrationResponse'_integrationResponseSelectionExpression,
    updateIntegrationResponse'_requestTemplates,
    updateIntegrationResponse'_integrationSubtype,
    updateIntegrationResponse'_credentialsArn,
    updateIntegrationResponse'_integrationUri,
    updateIntegrationResponse'_integrationId,
    updateIntegrationResponse'_requestParameters,
    updateIntegrationResponse'_connectionId,
    updateIntegrationResponse'_passthroughBehavior,
    updateIntegrationResponse'_integrationMethod,
    updateIntegrationResponse'_tlsConfig,
    updateIntegrationResponse'_payloadFormatVersion,
    updateIntegrationResponse'_templateSelectionExpression,
    updateIntegrationResponse'_timeoutInMillis,
    updateIntegrationResponse'_apiGatewayManaged,
    updateIntegrationResponse'_contentHandlingStrategy,
    updateIntegrationResponse'_integrationType,
    updateIntegrationResponse'_description,
    updateIntegrationResponse'_connectionType,
    updateIntegrationResponse'_responseParameters,
    updateIntegrationResponse'_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an Integration.
--
-- /See:/ 'newUpdateIntegration' smart constructor.
data UpdateIntegration = UpdateIntegration'
  { -- | Represents a map of Velocity templates that are applied on the request
    -- payload based on the value of the Content-Type header sent by the
    -- client. The content type value is the key in this map, and the template
    -- (as a String) is the value. Supported only for WebSocket APIs.
    requestTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
    -- service action to invoke. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
    integrationSubtype :: Prelude.Maybe Prelude.Text,
    -- | Specifies the credentials required for the integration, if any. For AWS
    -- integrations, three options are available. To specify an IAM Role for
    -- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
    -- require that the caller\'s identity be passed through from the request,
    -- specify the string arn:aws:iam::*:user\/*. To use resource-based
    -- permissions on supported AWS services, specify null.
    credentialsArn :: Prelude.Maybe Prelude.Text,
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
    -- For HTTP API integrations, without a specified integrationSubtype
    -- request parameters are a key-value map specifying how to transform HTTP
    -- requests before sending them to the backend. The key should follow the
    -- pattern \<action>:\<header|querystring|path>.\<location> where action
    -- can be append, overwrite or remove. For values, you can provide static
    -- values, or map request data, stage variables, or context variables that
    -- are evaluated at runtime. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping. Transforming API requests and responses>.
    requestParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the VPC link for a private integration. Supported only for
    -- HTTP APIs.
    connectionId :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the integration\'s HTTP method type.
    integrationMethod :: Prelude.Maybe Prelude.Text,
    -- | The TLS configuration for a private integration. If you specify a TLS
    -- configuration, private integration traffic uses the HTTPS protocol.
    -- Supported only for HTTP APIs.
    tlsConfig :: Prelude.Maybe TlsConfigInput,
    -- | Specifies the format of the payload sent to an integration. Required for
    -- HTTP APIs.
    payloadFormatVersion :: Prelude.Maybe Prelude.Text,
    -- | The template selection expression for the integration.
    templateSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
    -- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
    -- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
    timeoutInMillis :: Prelude.Maybe Prelude.Natural,
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
    -- referred to as HTTP proxy integration. For HTTP API private
    -- integrations, use an HTTP_PROXY integration.
    --
    -- MOCK: for integrating the route or method request with API Gateway as a
    -- \"loopback\" endpoint without invoking any backend. Supported only for
    -- WebSocket APIs.
    integrationType :: Prelude.Maybe IntegrationType,
    -- | The description of the integration
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the network connection to the integration endpoint. Specify
    -- INTERNET for connections through the public routable internet or
    -- VPC_LINK for private connections between API Gateway and resources in a
    -- VPC. The default value is INTERNET.
    connectionType :: Prelude.Maybe ConnectionType,
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
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The integration ID.
    integrationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestTemplates', 'updateIntegration_requestTemplates' - Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
--
-- 'integrationSubtype', 'updateIntegration_integrationSubtype' - Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
--
-- 'credentialsArn', 'updateIntegration_credentialsArn' - Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
--
-- 'integrationUri', 'updateIntegration_integrationUri' - For a Lambda integration, specify the URI of a Lambda function.
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
-- 'requestParameters', 'updateIntegration_requestParameters' - For WebSocket APIs, a key-value map specifying request parameters that
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
-- For HTTP API integrations, without a specified integrationSubtype
-- request parameters are a key-value map specifying how to transform HTTP
-- requests before sending them to the backend. The key should follow the
-- pattern \<action>:\<header|querystring|path>.\<location> where action
-- can be append, overwrite or remove. For values, you can provide static
-- values, or map request data, stage variables, or context variables that
-- are evaluated at runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping. Transforming API requests and responses>.
--
-- 'connectionId', 'updateIntegration_connectionId' - The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
--
-- 'passthroughBehavior', 'updateIntegration_passthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the
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
-- 'integrationMethod', 'updateIntegration_integrationMethod' - Specifies the integration\'s HTTP method type.
--
-- 'tlsConfig', 'updateIntegration_tlsConfig' - The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
--
-- 'payloadFormatVersion', 'updateIntegration_payloadFormatVersion' - Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
--
-- 'templateSelectionExpression', 'updateIntegration_templateSelectionExpression' - The template selection expression for the integration.
--
-- 'timeoutInMillis', 'updateIntegration_timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
--
-- 'contentHandlingStrategy', 'updateIntegration_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'integrationType', 'updateIntegration_integrationType' - The integration type of an integration. One of the following:
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
-- referred to as HTTP proxy integration. For HTTP API private
-- integrations, use an HTTP_PROXY integration.
--
-- MOCK: for integrating the route or method request with API Gateway as a
-- \"loopback\" endpoint without invoking any backend. Supported only for
-- WebSocket APIs.
--
-- 'description', 'updateIntegration_description' - The description of the integration
--
-- 'connectionType', 'updateIntegration_connectionType' - The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
--
-- 'responseParameters', 'updateIntegration_responseParameters' - Supported only for HTTP APIs. You use response parameters to transform
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
-- 'apiId', 'updateIntegration_apiId' - The API identifier.
--
-- 'integrationId', 'updateIntegration_integrationId' - The integration ID.
newUpdateIntegration ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'integrationId'
  Prelude.Text ->
  UpdateIntegration
newUpdateIntegration pApiId_ pIntegrationId_ =
  UpdateIntegration'
    { requestTemplates =
        Prelude.Nothing,
      integrationSubtype = Prelude.Nothing,
      credentialsArn = Prelude.Nothing,
      integrationUri = Prelude.Nothing,
      requestParameters = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      passthroughBehavior = Prelude.Nothing,
      integrationMethod = Prelude.Nothing,
      tlsConfig = Prelude.Nothing,
      payloadFormatVersion = Prelude.Nothing,
      templateSelectionExpression = Prelude.Nothing,
      timeoutInMillis = Prelude.Nothing,
      contentHandlingStrategy = Prelude.Nothing,
      integrationType = Prelude.Nothing,
      description = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      apiId = pApiId_,
      integrationId = pIntegrationId_
    }

-- | Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
updateIntegration_requestTemplates :: Lens.Lens' UpdateIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegration_requestTemplates = Lens.lens (\UpdateIntegration' {requestTemplates} -> requestTemplates) (\s@UpdateIntegration' {} a -> s {requestTemplates = a} :: UpdateIntegration) Prelude.. Lens.mapping Lens.coerced

-- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
updateIntegration_integrationSubtype :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_integrationSubtype = Lens.lens (\UpdateIntegration' {integrationSubtype} -> integrationSubtype) (\s@UpdateIntegration' {} a -> s {integrationSubtype = a} :: UpdateIntegration)

-- | Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
updateIntegration_credentialsArn :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_credentialsArn = Lens.lens (\UpdateIntegration' {credentialsArn} -> credentialsArn) (\s@UpdateIntegration' {} a -> s {credentialsArn = a} :: UpdateIntegration)

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
updateIntegration_integrationUri :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_integrationUri = Lens.lens (\UpdateIntegration' {integrationUri} -> integrationUri) (\s@UpdateIntegration' {} a -> s {integrationUri = a} :: UpdateIntegration)

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
-- For HTTP API integrations, without a specified integrationSubtype
-- request parameters are a key-value map specifying how to transform HTTP
-- requests before sending them to the backend. The key should follow the
-- pattern \<action>:\<header|querystring|path>.\<location> where action
-- can be append, overwrite or remove. For values, you can provide static
-- values, or map request data, stage variables, or context variables that
-- are evaluated at runtime. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-parameter-mapping. Transforming API requests and responses>.
updateIntegration_requestParameters :: Lens.Lens' UpdateIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegration_requestParameters = Lens.lens (\UpdateIntegration' {requestParameters} -> requestParameters) (\s@UpdateIntegration' {} a -> s {requestParameters = a} :: UpdateIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
updateIntegration_connectionId :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_connectionId = Lens.lens (\UpdateIntegration' {connectionId} -> connectionId) (\s@UpdateIntegration' {} a -> s {connectionId = a} :: UpdateIntegration)

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
updateIntegration_passthroughBehavior :: Lens.Lens' UpdateIntegration (Prelude.Maybe PassthroughBehavior)
updateIntegration_passthroughBehavior = Lens.lens (\UpdateIntegration' {passthroughBehavior} -> passthroughBehavior) (\s@UpdateIntegration' {} a -> s {passthroughBehavior = a} :: UpdateIntegration)

-- | Specifies the integration\'s HTTP method type.
updateIntegration_integrationMethod :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_integrationMethod = Lens.lens (\UpdateIntegration' {integrationMethod} -> integrationMethod) (\s@UpdateIntegration' {} a -> s {integrationMethod = a} :: UpdateIntegration)

-- | The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
updateIntegration_tlsConfig :: Lens.Lens' UpdateIntegration (Prelude.Maybe TlsConfigInput)
updateIntegration_tlsConfig = Lens.lens (\UpdateIntegration' {tlsConfig} -> tlsConfig) (\s@UpdateIntegration' {} a -> s {tlsConfig = a} :: UpdateIntegration)

-- | Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
updateIntegration_payloadFormatVersion :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_payloadFormatVersion = Lens.lens (\UpdateIntegration' {payloadFormatVersion} -> payloadFormatVersion) (\s@UpdateIntegration' {} a -> s {payloadFormatVersion = a} :: UpdateIntegration)

-- | The template selection expression for the integration.
updateIntegration_templateSelectionExpression :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_templateSelectionExpression = Lens.lens (\UpdateIntegration' {templateSelectionExpression} -> templateSelectionExpression) (\s@UpdateIntegration' {} a -> s {templateSelectionExpression = a} :: UpdateIntegration)

-- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
updateIntegration_timeoutInMillis :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Natural)
updateIntegration_timeoutInMillis = Lens.lens (\UpdateIntegration' {timeoutInMillis} -> timeoutInMillis) (\s@UpdateIntegration' {} a -> s {timeoutInMillis = a} :: UpdateIntegration)

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
updateIntegration_contentHandlingStrategy :: Lens.Lens' UpdateIntegration (Prelude.Maybe ContentHandlingStrategy)
updateIntegration_contentHandlingStrategy = Lens.lens (\UpdateIntegration' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@UpdateIntegration' {} a -> s {contentHandlingStrategy = a} :: UpdateIntegration)

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
-- referred to as HTTP proxy integration. For HTTP API private
-- integrations, use an HTTP_PROXY integration.
--
-- MOCK: for integrating the route or method request with API Gateway as a
-- \"loopback\" endpoint without invoking any backend. Supported only for
-- WebSocket APIs.
updateIntegration_integrationType :: Lens.Lens' UpdateIntegration (Prelude.Maybe IntegrationType)
updateIntegration_integrationType = Lens.lens (\UpdateIntegration' {integrationType} -> integrationType) (\s@UpdateIntegration' {} a -> s {integrationType = a} :: UpdateIntegration)

-- | The description of the integration
updateIntegration_description :: Lens.Lens' UpdateIntegration (Prelude.Maybe Prelude.Text)
updateIntegration_description = Lens.lens (\UpdateIntegration' {description} -> description) (\s@UpdateIntegration' {} a -> s {description = a} :: UpdateIntegration)

-- | The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
updateIntegration_connectionType :: Lens.Lens' UpdateIntegration (Prelude.Maybe ConnectionType)
updateIntegration_connectionType = Lens.lens (\UpdateIntegration' {connectionType} -> connectionType) (\s@UpdateIntegration' {} a -> s {connectionType = a} :: UpdateIntegration)

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
updateIntegration_responseParameters :: Lens.Lens' UpdateIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
updateIntegration_responseParameters = Lens.lens (\UpdateIntegration' {responseParameters} -> responseParameters) (\s@UpdateIntegration' {} a -> s {responseParameters = a} :: UpdateIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The API identifier.
updateIntegration_apiId :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_apiId = Lens.lens (\UpdateIntegration' {apiId} -> apiId) (\s@UpdateIntegration' {} a -> s {apiId = a} :: UpdateIntegration)

-- | The integration ID.
updateIntegration_integrationId :: Lens.Lens' UpdateIntegration Prelude.Text
updateIntegration_integrationId = Lens.lens (\UpdateIntegration' {integrationId} -> integrationId) (\s@UpdateIntegration' {} a -> s {integrationId = a} :: UpdateIntegration)

instance Core.AWSRequest UpdateIntegration where
  type
    AWSResponse UpdateIntegration =
      UpdateIntegrationResponse'
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateIntegrationResponse''
            Prelude.<$> (x Core..?> "integrationResponseSelectionExpression")
            Prelude.<*> ( x Core..?> "requestTemplates"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "integrationSubtype")
            Prelude.<*> (x Core..?> "credentialsArn")
            Prelude.<*> (x Core..?> "integrationUri")
            Prelude.<*> (x Core..?> "integrationId")
            Prelude.<*> ( x Core..?> "requestParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "connectionId")
            Prelude.<*> (x Core..?> "passthroughBehavior")
            Prelude.<*> (x Core..?> "integrationMethod")
            Prelude.<*> (x Core..?> "tlsConfig")
            Prelude.<*> (x Core..?> "payloadFormatVersion")
            Prelude.<*> (x Core..?> "templateSelectionExpression")
            Prelude.<*> (x Core..?> "timeoutInMillis")
            Prelude.<*> (x Core..?> "apiGatewayManaged")
            Prelude.<*> (x Core..?> "contentHandlingStrategy")
            Prelude.<*> (x Core..?> "integrationType")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "connectionType")
            Prelude.<*> ( x Core..?> "responseParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIntegration where
  hashWithSalt _salt UpdateIntegration' {..} =
    _salt `Prelude.hashWithSalt` requestTemplates
      `Prelude.hashWithSalt` integrationSubtype
      `Prelude.hashWithSalt` credentialsArn
      `Prelude.hashWithSalt` integrationUri
      `Prelude.hashWithSalt` requestParameters
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` passthroughBehavior
      `Prelude.hashWithSalt` integrationMethod
      `Prelude.hashWithSalt` tlsConfig
      `Prelude.hashWithSalt` payloadFormatVersion
      `Prelude.hashWithSalt` templateSelectionExpression
      `Prelude.hashWithSalt` timeoutInMillis
      `Prelude.hashWithSalt` contentHandlingStrategy
      `Prelude.hashWithSalt` integrationType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` integrationId

instance Prelude.NFData UpdateIntegration where
  rnf UpdateIntegration' {..} =
    Prelude.rnf requestTemplates
      `Prelude.seq` Prelude.rnf integrationSubtype
      `Prelude.seq` Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf integrationUri
      `Prelude.seq` Prelude.rnf requestParameters
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf passthroughBehavior
      `Prelude.seq` Prelude.rnf integrationMethod
      `Prelude.seq` Prelude.rnf tlsConfig
      `Prelude.seq` Prelude.rnf payloadFormatVersion
      `Prelude.seq` Prelude.rnf templateSelectionExpression
      `Prelude.seq` Prelude.rnf timeoutInMillis
      `Prelude.seq` Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf integrationType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf integrationId

instance Core.ToHeaders UpdateIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateIntegration where
  toJSON UpdateIntegration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("requestTemplates" Core..=)
              Prelude.<$> requestTemplates,
            ("integrationSubtype" Core..=)
              Prelude.<$> integrationSubtype,
            ("credentialsArn" Core..=)
              Prelude.<$> credentialsArn,
            ("integrationUri" Core..=)
              Prelude.<$> integrationUri,
            ("requestParameters" Core..=)
              Prelude.<$> requestParameters,
            ("connectionId" Core..=) Prelude.<$> connectionId,
            ("passthroughBehavior" Core..=)
              Prelude.<$> passthroughBehavior,
            ("integrationMethod" Core..=)
              Prelude.<$> integrationMethod,
            ("tlsConfig" Core..=) Prelude.<$> tlsConfig,
            ("payloadFormatVersion" Core..=)
              Prelude.<$> payloadFormatVersion,
            ("templateSelectionExpression" Core..=)
              Prelude.<$> templateSelectionExpression,
            ("timeoutInMillis" Core..=)
              Prelude.<$> timeoutInMillis,
            ("contentHandlingStrategy" Core..=)
              Prelude.<$> contentHandlingStrategy,
            ("integrationType" Core..=)
              Prelude.<$> integrationType,
            ("description" Core..=) Prelude.<$> description,
            ("connectionType" Core..=)
              Prelude.<$> connectionType,
            ("responseParameters" Core..=)
              Prelude.<$> responseParameters
          ]
      )

instance Core.ToPath UpdateIntegration where
  toPath UpdateIntegration' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/integrations/",
        Core.toBS integrationId
      ]

instance Core.ToQuery UpdateIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIntegrationResponse'' smart constructor.
data UpdateIntegrationResponse' = UpdateIntegrationResponse''
  { -- | The integration response selection expression for the integration.
    -- Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
    integrationResponseSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Represents a map of Velocity templates that are applied on the request
    -- payload based on the value of the Content-Type header sent by the
    -- client. The content type value is the key in this map, and the template
    -- (as a String) is the value. Supported only for WebSocket APIs.
    requestTemplates :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
    -- service action to invoke. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
    integrationSubtype :: Prelude.Maybe Prelude.Text,
    -- | Specifies the credentials required for the integration, if any. For AWS
    -- integrations, three options are available. To specify an IAM Role for
    -- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
    -- require that the caller\'s identity be passed through from the request,
    -- specify the string arn:aws:iam::*:user\/*. To use resource-based
    -- permissions on supported AWS services, specify null.
    credentialsArn :: Prelude.Maybe Prelude.Text,
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
    -- | Represents the identifier of an integration.
    integrationId :: Prelude.Maybe Prelude.Text,
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
    -- | The ID of the VPC link for a private integration. Supported only for
    -- HTTP APIs.
    connectionId :: Prelude.Maybe Prelude.Text,
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
    -- | Specifies the integration\'s HTTP method type.
    integrationMethod :: Prelude.Maybe Prelude.Text,
    -- | The TLS configuration for a private integration. If you specify a TLS
    -- configuration, private integration traffic uses the HTTPS protocol.
    -- Supported only for HTTP APIs.
    tlsConfig :: Prelude.Maybe TlsConfig,
    -- | Specifies the format of the payload sent to an integration. Required for
    -- HTTP APIs.
    payloadFormatVersion :: Prelude.Maybe Prelude.Text,
    -- | The template selection expression for the integration. Supported only
    -- for WebSocket APIs.
    templateSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
    -- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
    -- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
    timeoutInMillis :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether an integration is managed by API Gateway. If you
    -- created an API using using quick create, the resulting integration is
    -- managed by API Gateway. You can update a managed integration, but you
    -- can\'t delete it.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
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
    -- | Represents the description of an integration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The type of the network connection to the integration endpoint. Specify
    -- INTERNET for connections through the public routable internet or
    -- VPC_LINK for private connections between API Gateway and resources in a
    -- VPC. The default value is INTERNET.
    connectionType :: Prelude.Maybe ConnectionType,
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIntegrationResponse'' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integrationResponseSelectionExpression', 'updateIntegrationResponse'_integrationResponseSelectionExpression' - The integration response selection expression for the integration.
-- Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
--
-- 'requestTemplates', 'updateIntegrationResponse'_requestTemplates' - Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
--
-- 'integrationSubtype', 'updateIntegrationResponse'_integrationSubtype' - Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
--
-- 'credentialsArn', 'updateIntegrationResponse'_credentialsArn' - Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
--
-- 'integrationUri', 'updateIntegrationResponse'_integrationUri' - For a Lambda integration, specify the URI of a Lambda function.
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
-- 'integrationId', 'updateIntegrationResponse'_integrationId' - Represents the identifier of an integration.
--
-- 'requestParameters', 'updateIntegrationResponse'_requestParameters' - For WebSocket APIs, a key-value map specifying request parameters that
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
-- 'connectionId', 'updateIntegrationResponse'_connectionId' - The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
--
-- 'passthroughBehavior', 'updateIntegrationResponse'_passthroughBehavior' - Specifies the pass-through behavior for incoming requests based on the
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
-- 'integrationMethod', 'updateIntegrationResponse'_integrationMethod' - Specifies the integration\'s HTTP method type.
--
-- 'tlsConfig', 'updateIntegrationResponse'_tlsConfig' - The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
--
-- 'payloadFormatVersion', 'updateIntegrationResponse'_payloadFormatVersion' - Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
--
-- 'templateSelectionExpression', 'updateIntegrationResponse'_templateSelectionExpression' - The template selection expression for the integration. Supported only
-- for WebSocket APIs.
--
-- 'timeoutInMillis', 'updateIntegrationResponse'_timeoutInMillis' - Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
--
-- 'apiGatewayManaged', 'updateIntegrationResponse'_apiGatewayManaged' - Specifies whether an integration is managed by API Gateway. If you
-- created an API using using quick create, the resulting integration is
-- managed by API Gateway. You can update a managed integration, but you
-- can\'t delete it.
--
-- 'contentHandlingStrategy', 'updateIntegrationResponse'_contentHandlingStrategy' - Supported only for WebSocket APIs. Specifies how to handle response
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
-- 'integrationType', 'updateIntegrationResponse'_integrationType' - The integration type of an integration. One of the following:
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
-- 'description', 'updateIntegrationResponse'_description' - Represents the description of an integration.
--
-- 'connectionType', 'updateIntegrationResponse'_connectionType' - The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
--
-- 'responseParameters', 'updateIntegrationResponse'_responseParameters' - Supported only for HTTP APIs. You use response parameters to transform
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
-- 'httpStatus', 'updateIntegrationResponse'_httpStatus' - The response's http status code.
newUpdateIntegrationResponse' ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIntegrationResponse'
newUpdateIntegrationResponse' pHttpStatus_ =
  UpdateIntegrationResponse''
    { integrationResponseSelectionExpression =
        Prelude.Nothing,
      requestTemplates = Prelude.Nothing,
      integrationSubtype = Prelude.Nothing,
      credentialsArn = Prelude.Nothing,
      integrationUri = Prelude.Nothing,
      integrationId = Prelude.Nothing,
      requestParameters = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      passthroughBehavior = Prelude.Nothing,
      integrationMethod = Prelude.Nothing,
      tlsConfig = Prelude.Nothing,
      payloadFormatVersion = Prelude.Nothing,
      templateSelectionExpression = Prelude.Nothing,
      timeoutInMillis = Prelude.Nothing,
      apiGatewayManaged = Prelude.Nothing,
      contentHandlingStrategy = Prelude.Nothing,
      integrationType = Prelude.Nothing,
      description = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The integration response selection expression for the integration.
-- Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-integration-response-selection-expressions Integration Response Selection Expressions>.
updateIntegrationResponse'_integrationResponseSelectionExpression :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_integrationResponseSelectionExpression = Lens.lens (\UpdateIntegrationResponse'' {integrationResponseSelectionExpression} -> integrationResponseSelectionExpression) (\s@UpdateIntegrationResponse'' {} a -> s {integrationResponseSelectionExpression = a} :: UpdateIntegrationResponse')

-- | Represents a map of Velocity templates that are applied on the request
-- payload based on the value of the Content-Type header sent by the
-- client. The content type value is the key in this map, and the template
-- (as a String) is the value. Supported only for WebSocket APIs.
updateIntegrationResponse'_requestTemplates :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegrationResponse'_requestTemplates = Lens.lens (\UpdateIntegrationResponse'' {requestTemplates} -> requestTemplates) (\s@UpdateIntegrationResponse'' {} a -> s {requestTemplates = a} :: UpdateIntegrationResponse') Prelude.. Lens.mapping Lens.coerced

-- | Supported only for HTTP API AWS_PROXY integrations. Specifies the AWS
-- service action to invoke. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-develop-integrations-aws-services-reference.html Integration subtype reference>.
updateIntegrationResponse'_integrationSubtype :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_integrationSubtype = Lens.lens (\UpdateIntegrationResponse'' {integrationSubtype} -> integrationSubtype) (\s@UpdateIntegrationResponse'' {} a -> s {integrationSubtype = a} :: UpdateIntegrationResponse')

-- | Specifies the credentials required for the integration, if any. For AWS
-- integrations, three options are available. To specify an IAM Role for
-- API Gateway to assume, use the role\'s Amazon Resource Name (ARN). To
-- require that the caller\'s identity be passed through from the request,
-- specify the string arn:aws:iam::*:user\/*. To use resource-based
-- permissions on supported AWS services, specify null.
updateIntegrationResponse'_credentialsArn :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_credentialsArn = Lens.lens (\UpdateIntegrationResponse'' {credentialsArn} -> credentialsArn) (\s@UpdateIntegrationResponse'' {} a -> s {credentialsArn = a} :: UpdateIntegrationResponse')

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
updateIntegrationResponse'_integrationUri :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_integrationUri = Lens.lens (\UpdateIntegrationResponse'' {integrationUri} -> integrationUri) (\s@UpdateIntegrationResponse'' {} a -> s {integrationUri = a} :: UpdateIntegrationResponse')

-- | Represents the identifier of an integration.
updateIntegrationResponse'_integrationId :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_integrationId = Lens.lens (\UpdateIntegrationResponse'' {integrationId} -> integrationId) (\s@UpdateIntegrationResponse'' {} a -> s {integrationId = a} :: UpdateIntegrationResponse')

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
updateIntegrationResponse'_requestParameters :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateIntegrationResponse'_requestParameters = Lens.lens (\UpdateIntegrationResponse'' {requestParameters} -> requestParameters) (\s@UpdateIntegrationResponse'' {} a -> s {requestParameters = a} :: UpdateIntegrationResponse') Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC link for a private integration. Supported only for
-- HTTP APIs.
updateIntegrationResponse'_connectionId :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_connectionId = Lens.lens (\UpdateIntegrationResponse'' {connectionId} -> connectionId) (\s@UpdateIntegrationResponse'' {} a -> s {connectionId = a} :: UpdateIntegrationResponse')

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
updateIntegrationResponse'_passthroughBehavior :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe PassthroughBehavior)
updateIntegrationResponse'_passthroughBehavior = Lens.lens (\UpdateIntegrationResponse'' {passthroughBehavior} -> passthroughBehavior) (\s@UpdateIntegrationResponse'' {} a -> s {passthroughBehavior = a} :: UpdateIntegrationResponse')

-- | Specifies the integration\'s HTTP method type.
updateIntegrationResponse'_integrationMethod :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_integrationMethod = Lens.lens (\UpdateIntegrationResponse'' {integrationMethod} -> integrationMethod) (\s@UpdateIntegrationResponse'' {} a -> s {integrationMethod = a} :: UpdateIntegrationResponse')

-- | The TLS configuration for a private integration. If you specify a TLS
-- configuration, private integration traffic uses the HTTPS protocol.
-- Supported only for HTTP APIs.
updateIntegrationResponse'_tlsConfig :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe TlsConfig)
updateIntegrationResponse'_tlsConfig = Lens.lens (\UpdateIntegrationResponse'' {tlsConfig} -> tlsConfig) (\s@UpdateIntegrationResponse'' {} a -> s {tlsConfig = a} :: UpdateIntegrationResponse')

-- | Specifies the format of the payload sent to an integration. Required for
-- HTTP APIs.
updateIntegrationResponse'_payloadFormatVersion :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_payloadFormatVersion = Lens.lens (\UpdateIntegrationResponse'' {payloadFormatVersion} -> payloadFormatVersion) (\s@UpdateIntegrationResponse'' {} a -> s {payloadFormatVersion = a} :: UpdateIntegrationResponse')

-- | The template selection expression for the integration. Supported only
-- for WebSocket APIs.
updateIntegrationResponse'_templateSelectionExpression :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_templateSelectionExpression = Lens.lens (\UpdateIntegrationResponse'' {templateSelectionExpression} -> templateSelectionExpression) (\s@UpdateIntegrationResponse'' {} a -> s {templateSelectionExpression = a} :: UpdateIntegrationResponse')

-- | Custom timeout between 50 and 29,000 milliseconds for WebSocket APIs and
-- between 50 and 30,000 milliseconds for HTTP APIs. The default timeout is
-- 29 seconds for WebSocket APIs and 30 seconds for HTTP APIs.
updateIntegrationResponse'_timeoutInMillis :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Natural)
updateIntegrationResponse'_timeoutInMillis = Lens.lens (\UpdateIntegrationResponse'' {timeoutInMillis} -> timeoutInMillis) (\s@UpdateIntegrationResponse'' {} a -> s {timeoutInMillis = a} :: UpdateIntegrationResponse')

-- | Specifies whether an integration is managed by API Gateway. If you
-- created an API using using quick create, the resulting integration is
-- managed by API Gateway. You can update a managed integration, but you
-- can\'t delete it.
updateIntegrationResponse'_apiGatewayManaged :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Bool)
updateIntegrationResponse'_apiGatewayManaged = Lens.lens (\UpdateIntegrationResponse'' {apiGatewayManaged} -> apiGatewayManaged) (\s@UpdateIntegrationResponse'' {} a -> s {apiGatewayManaged = a} :: UpdateIntegrationResponse')

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
updateIntegrationResponse'_contentHandlingStrategy :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe ContentHandlingStrategy)
updateIntegrationResponse'_contentHandlingStrategy = Lens.lens (\UpdateIntegrationResponse'' {contentHandlingStrategy} -> contentHandlingStrategy) (\s@UpdateIntegrationResponse'' {} a -> s {contentHandlingStrategy = a} :: UpdateIntegrationResponse')

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
updateIntegrationResponse'_integrationType :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe IntegrationType)
updateIntegrationResponse'_integrationType = Lens.lens (\UpdateIntegrationResponse'' {integrationType} -> integrationType) (\s@UpdateIntegrationResponse'' {} a -> s {integrationType = a} :: UpdateIntegrationResponse')

-- | Represents the description of an integration.
updateIntegrationResponse'_description :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe Prelude.Text)
updateIntegrationResponse'_description = Lens.lens (\UpdateIntegrationResponse'' {description} -> description) (\s@UpdateIntegrationResponse'' {} a -> s {description = a} :: UpdateIntegrationResponse')

-- | The type of the network connection to the integration endpoint. Specify
-- INTERNET for connections through the public routable internet or
-- VPC_LINK for private connections between API Gateway and resources in a
-- VPC. The default value is INTERNET.
updateIntegrationResponse'_connectionType :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe ConnectionType)
updateIntegrationResponse'_connectionType = Lens.lens (\UpdateIntegrationResponse'' {connectionType} -> connectionType) (\s@UpdateIntegrationResponse'' {} a -> s {connectionType = a} :: UpdateIntegrationResponse')

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
updateIntegrationResponse'_responseParameters :: Lens.Lens' UpdateIntegrationResponse' (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text Prelude.Text)))
updateIntegrationResponse'_responseParameters = Lens.lens (\UpdateIntegrationResponse'' {responseParameters} -> responseParameters) (\s@UpdateIntegrationResponse'' {} a -> s {responseParameters = a} :: UpdateIntegrationResponse') Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateIntegrationResponse'_httpStatus :: Lens.Lens' UpdateIntegrationResponse' Prelude.Int
updateIntegrationResponse'_httpStatus = Lens.lens (\UpdateIntegrationResponse'' {httpStatus} -> httpStatus) (\s@UpdateIntegrationResponse'' {} a -> s {httpStatus = a} :: UpdateIntegrationResponse')

instance Prelude.NFData UpdateIntegrationResponse' where
  rnf UpdateIntegrationResponse'' {..} =
    Prelude.rnf integrationResponseSelectionExpression
      `Prelude.seq` Prelude.rnf requestTemplates
      `Prelude.seq` Prelude.rnf integrationSubtype
      `Prelude.seq` Prelude.rnf credentialsArn
      `Prelude.seq` Prelude.rnf integrationUri
      `Prelude.seq` Prelude.rnf integrationId
      `Prelude.seq` Prelude.rnf requestParameters
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf passthroughBehavior
      `Prelude.seq` Prelude.rnf integrationMethod
      `Prelude.seq` Prelude.rnf tlsConfig
      `Prelude.seq` Prelude.rnf payloadFormatVersion
      `Prelude.seq` Prelude.rnf templateSelectionExpression
      `Prelude.seq` Prelude.rnf timeoutInMillis
      `Prelude.seq` Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf contentHandlingStrategy
      `Prelude.seq` Prelude.rnf integrationType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf httpStatus
