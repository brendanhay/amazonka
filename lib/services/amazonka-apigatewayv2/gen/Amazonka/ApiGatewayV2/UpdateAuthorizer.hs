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
-- Module      : Amazonka.ApiGatewayV2.UpdateAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Authorizer.
module Amazonka.ApiGatewayV2.UpdateAuthorizer
  ( -- * Creating a Request
    UpdateAuthorizer (..),
    newUpdateAuthorizer,

    -- * Request Lenses
    updateAuthorizer_authorizerCredentialsArn,
    updateAuthorizer_authorizerPayloadFormatVersion,
    updateAuthorizer_authorizerResultTtlInSeconds,
    updateAuthorizer_authorizerType,
    updateAuthorizer_authorizerUri,
    updateAuthorizer_enableSimpleResponses,
    updateAuthorizer_identitySource,
    updateAuthorizer_identityValidationExpression,
    updateAuthorizer_jwtConfiguration,
    updateAuthorizer_name,
    updateAuthorizer_authorizerId,
    updateAuthorizer_apiId,

    -- * Destructuring the Response
    UpdateAuthorizerResponse (..),
    newUpdateAuthorizerResponse,

    -- * Response Lenses
    updateAuthorizerResponse_authorizerCredentialsArn,
    updateAuthorizerResponse_authorizerId,
    updateAuthorizerResponse_authorizerPayloadFormatVersion,
    updateAuthorizerResponse_authorizerResultTtlInSeconds,
    updateAuthorizerResponse_authorizerType,
    updateAuthorizerResponse_authorizerUri,
    updateAuthorizerResponse_enableSimpleResponses,
    updateAuthorizerResponse_identitySource,
    updateAuthorizerResponse_identityValidationExpression,
    updateAuthorizerResponse_jwtConfiguration,
    updateAuthorizerResponse_name,
    updateAuthorizerResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates an Authorizer.
--
-- /See:/ 'newUpdateAuthorizer' smart constructor.
data UpdateAuthorizer = UpdateAuthorizer'
  { -- | Specifies the required credentials as an IAM role for API Gateway to
    -- invoke the authorizer. To specify an IAM role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To use resource-based
    -- permissions on the Lambda function, don\'t specify this parameter.
    authorizerCredentialsArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format of the payload sent to an HTTP API Lambda
    -- authorizer. Required for HTTP API Lambda authorizers. Supported values
    -- are 1.0 and 2.0. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
    authorizerPayloadFormatVersion :: Prelude.Maybe Prelude.Text,
    -- | The time to live (TTL) for cached authorizer results, in seconds. If it
    -- equals 0, authorization caching is disabled. If it is greater than 0,
    -- API Gateway caches authorizer responses. The maximum value is 3600, or 1
    -- hour. Supported only for HTTP API Lambda authorizers.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The authorizer type. Specify REQUEST for a Lambda function using
    -- incoming request parameters. Specify JWT to use JSON Web Tokens
    -- (supported only for HTTP APIs).
    authorizerType :: Prelude.Maybe AuthorizerType,
    -- | The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
    -- authorizers, this must be a well-formed Lambda function URI, for
    -- example,
    -- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
    -- In general, the URI has this form:
    -- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
    -- is the same as the region hosting the Lambda function, path indicates
    -- that the remaining substring in the URI should be treated as the path to
    -- the resource, including the initial \/. For Lambda functions, this is
    -- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
    -- Supported only for REQUEST authorizers.
    authorizerUri :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a Lambda authorizer returns a response in a simple
    -- format. By default, a Lambda authorizer must return an IAM policy. If
    -- enabled, the Lambda authorizer can return a boolean value instead of an
    -- IAM policy. Supported only for HTTP APIs. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
    enableSimpleResponses :: Prelude.Maybe Prelude.Bool,
    -- | The identity source for which authorization is requested.
    --
    -- For a REQUEST authorizer, this is optional. The value is a set of one or
    -- more mapping expressions of the specified request parameters. The
    -- identity source can be headers, query string parameters, stage
    -- variables, and context parameters. For example, if an Auth header and a
    -- Name query string parameter are defined as identity sources, this value
    -- is route.request.header.Auth, route.request.querystring.Name for
    -- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
    -- \$, for example, $request.header.Auth, $request.querystring.Name. These
    -- parameters are used to perform runtime validation for Lambda-based
    -- authorizers by verifying all of the identity-related request parameters
    -- are present in the request, not null, and non-empty. Only when this is
    -- true does the authorizer invoke the authorizer Lambda function.
    -- Otherwise, it returns a 401 Unauthorized response without calling the
    -- Lambda function. For HTTP APIs, identity sources are also used as the
    -- cache key when caching is enabled. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
    --
    -- For JWT, a single entry that specifies where to extract the JSON Web
    -- Token (JWT) from inbound requests. Currently only header-based and query
    -- parameter-based selections are supported, for example
    -- \$request.header.Authorization.
    identitySource :: Prelude.Maybe [Prelude.Text],
    -- | This parameter is not used.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | Represents the configuration of a JWT authorizer. Required for the JWT
    -- authorizer type. Supported only for HTTP APIs.
    jwtConfiguration :: Prelude.Maybe JWTConfiguration,
    -- | The name of the authorizer.
    name :: Prelude.Maybe Prelude.Text,
    -- | The authorizer identifier.
    authorizerId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerCredentialsArn', 'updateAuthorizer_authorizerCredentialsArn' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
--
-- 'authorizerPayloadFormatVersion', 'updateAuthorizer_authorizerPayloadFormatVersion' - Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- 'authorizerResultTtlInSeconds', 'updateAuthorizer_authorizerResultTtlInSeconds' - The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
--
-- 'authorizerType', 'updateAuthorizer_authorizerType' - The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
--
-- 'authorizerUri', 'updateAuthorizer_authorizerUri' - The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
-- authorizers, this must be a well-formed Lambda function URI, for
-- example,
-- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
-- In general, the URI has this form:
-- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
-- is the same as the region hosting the Lambda function, path indicates
-- that the remaining substring in the URI should be treated as the path to
-- the resource, including the initial \/. For Lambda functions, this is
-- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
-- Supported only for REQUEST authorizers.
--
-- 'enableSimpleResponses', 'updateAuthorizer_enableSimpleResponses' - Specifies whether a Lambda authorizer returns a response in a simple
-- format. By default, a Lambda authorizer must return an IAM policy. If
-- enabled, the Lambda authorizer can return a boolean value instead of an
-- IAM policy. Supported only for HTTP APIs. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
--
-- 'identitySource', 'updateAuthorizer_identitySource' - The identity source for which authorization is requested.
--
-- For a REQUEST authorizer, this is optional. The value is a set of one or
-- more mapping expressions of the specified request parameters. The
-- identity source can be headers, query string parameters, stage
-- variables, and context parameters. For example, if an Auth header and a
-- Name query string parameter are defined as identity sources, this value
-- is route.request.header.Auth, route.request.querystring.Name for
-- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
-- \$, for example, $request.header.Auth, $request.querystring.Name. These
-- parameters are used to perform runtime validation for Lambda-based
-- authorizers by verifying all of the identity-related request parameters
-- are present in the request, not null, and non-empty. Only when this is
-- true does the authorizer invoke the authorizer Lambda function.
-- Otherwise, it returns a 401 Unauthorized response without calling the
-- Lambda function. For HTTP APIs, identity sources are also used as the
-- cache key when caching is enabled. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- For JWT, a single entry that specifies where to extract the JSON Web
-- Token (JWT) from inbound requests. Currently only header-based and query
-- parameter-based selections are supported, for example
-- \$request.header.Authorization.
--
-- 'identityValidationExpression', 'updateAuthorizer_identityValidationExpression' - This parameter is not used.
--
-- 'jwtConfiguration', 'updateAuthorizer_jwtConfiguration' - Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- 'name', 'updateAuthorizer_name' - The name of the authorizer.
--
-- 'authorizerId', 'updateAuthorizer_authorizerId' - The authorizer identifier.
--
-- 'apiId', 'updateAuthorizer_apiId' - The API identifier.
newUpdateAuthorizer ::
  -- | 'authorizerId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  UpdateAuthorizer
newUpdateAuthorizer pAuthorizerId_ pApiId_ =
  UpdateAuthorizer'
    { authorizerCredentialsArn =
        Prelude.Nothing,
      authorizerPayloadFormatVersion = Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing,
      authorizerType = Prelude.Nothing,
      authorizerUri = Prelude.Nothing,
      enableSimpleResponses = Prelude.Nothing,
      identitySource = Prelude.Nothing,
      identityValidationExpression = Prelude.Nothing,
      jwtConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      authorizerId = pAuthorizerId_,
      apiId = pApiId_
    }

-- | Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
updateAuthorizer_authorizerCredentialsArn :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_authorizerCredentialsArn = Lens.lens (\UpdateAuthorizer' {authorizerCredentialsArn} -> authorizerCredentialsArn) (\s@UpdateAuthorizer' {} a -> s {authorizerCredentialsArn = a} :: UpdateAuthorizer)

-- | Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
updateAuthorizer_authorizerPayloadFormatVersion :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_authorizerPayloadFormatVersion = Lens.lens (\UpdateAuthorizer' {authorizerPayloadFormatVersion} -> authorizerPayloadFormatVersion) (\s@UpdateAuthorizer' {} a -> s {authorizerPayloadFormatVersion = a} :: UpdateAuthorizer)

-- | The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
updateAuthorizer_authorizerResultTtlInSeconds :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Natural)
updateAuthorizer_authorizerResultTtlInSeconds = Lens.lens (\UpdateAuthorizer' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@UpdateAuthorizer' {} a -> s {authorizerResultTtlInSeconds = a} :: UpdateAuthorizer)

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
updateAuthorizer_authorizerType :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe AuthorizerType)
updateAuthorizer_authorizerType = Lens.lens (\UpdateAuthorizer' {authorizerType} -> authorizerType) (\s@UpdateAuthorizer' {} a -> s {authorizerType = a} :: UpdateAuthorizer)

-- | The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
-- authorizers, this must be a well-formed Lambda function URI, for
-- example,
-- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
-- In general, the URI has this form:
-- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
-- is the same as the region hosting the Lambda function, path indicates
-- that the remaining substring in the URI should be treated as the path to
-- the resource, including the initial \/. For Lambda functions, this is
-- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
-- Supported only for REQUEST authorizers.
updateAuthorizer_authorizerUri :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_authorizerUri = Lens.lens (\UpdateAuthorizer' {authorizerUri} -> authorizerUri) (\s@UpdateAuthorizer' {} a -> s {authorizerUri = a} :: UpdateAuthorizer)

-- | Specifies whether a Lambda authorizer returns a response in a simple
-- format. By default, a Lambda authorizer must return an IAM policy. If
-- enabled, the Lambda authorizer can return a boolean value instead of an
-- IAM policy. Supported only for HTTP APIs. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
updateAuthorizer_enableSimpleResponses :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Bool)
updateAuthorizer_enableSimpleResponses = Lens.lens (\UpdateAuthorizer' {enableSimpleResponses} -> enableSimpleResponses) (\s@UpdateAuthorizer' {} a -> s {enableSimpleResponses = a} :: UpdateAuthorizer)

-- | The identity source for which authorization is requested.
--
-- For a REQUEST authorizer, this is optional. The value is a set of one or
-- more mapping expressions of the specified request parameters. The
-- identity source can be headers, query string parameters, stage
-- variables, and context parameters. For example, if an Auth header and a
-- Name query string parameter are defined as identity sources, this value
-- is route.request.header.Auth, route.request.querystring.Name for
-- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
-- \$, for example, $request.header.Auth, $request.querystring.Name. These
-- parameters are used to perform runtime validation for Lambda-based
-- authorizers by verifying all of the identity-related request parameters
-- are present in the request, not null, and non-empty. Only when this is
-- true does the authorizer invoke the authorizer Lambda function.
-- Otherwise, it returns a 401 Unauthorized response without calling the
-- Lambda function. For HTTP APIs, identity sources are also used as the
-- cache key when caching is enabled. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- For JWT, a single entry that specifies where to extract the JSON Web
-- Token (JWT) from inbound requests. Currently only header-based and query
-- parameter-based selections are supported, for example
-- \$request.header.Authorization.
updateAuthorizer_identitySource :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe [Prelude.Text])
updateAuthorizer_identitySource = Lens.lens (\UpdateAuthorizer' {identitySource} -> identitySource) (\s@UpdateAuthorizer' {} a -> s {identitySource = a} :: UpdateAuthorizer) Prelude.. Lens.mapping Lens.coerced

-- | This parameter is not used.
updateAuthorizer_identityValidationExpression :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_identityValidationExpression = Lens.lens (\UpdateAuthorizer' {identityValidationExpression} -> identityValidationExpression) (\s@UpdateAuthorizer' {} a -> s {identityValidationExpression = a} :: UpdateAuthorizer)

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
updateAuthorizer_jwtConfiguration :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe JWTConfiguration)
updateAuthorizer_jwtConfiguration = Lens.lens (\UpdateAuthorizer' {jwtConfiguration} -> jwtConfiguration) (\s@UpdateAuthorizer' {} a -> s {jwtConfiguration = a} :: UpdateAuthorizer)

-- | The name of the authorizer.
updateAuthorizer_name :: Lens.Lens' UpdateAuthorizer (Prelude.Maybe Prelude.Text)
updateAuthorizer_name = Lens.lens (\UpdateAuthorizer' {name} -> name) (\s@UpdateAuthorizer' {} a -> s {name = a} :: UpdateAuthorizer)

-- | The authorizer identifier.
updateAuthorizer_authorizerId :: Lens.Lens' UpdateAuthorizer Prelude.Text
updateAuthorizer_authorizerId = Lens.lens (\UpdateAuthorizer' {authorizerId} -> authorizerId) (\s@UpdateAuthorizer' {} a -> s {authorizerId = a} :: UpdateAuthorizer)

-- | The API identifier.
updateAuthorizer_apiId :: Lens.Lens' UpdateAuthorizer Prelude.Text
updateAuthorizer_apiId = Lens.lens (\UpdateAuthorizer' {apiId} -> apiId) (\s@UpdateAuthorizer' {} a -> s {apiId = a} :: UpdateAuthorizer)

instance Core.AWSRequest UpdateAuthorizer where
  type
    AWSResponse UpdateAuthorizer =
      UpdateAuthorizerResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAuthorizerResponse'
            Prelude.<$> (x Data..?> "authorizerCredentialsArn")
            Prelude.<*> (x Data..?> "authorizerId")
            Prelude.<*> (x Data..?> "authorizerPayloadFormatVersion")
            Prelude.<*> (x Data..?> "authorizerResultTtlInSeconds")
            Prelude.<*> (x Data..?> "authorizerType")
            Prelude.<*> (x Data..?> "authorizerUri")
            Prelude.<*> (x Data..?> "enableSimpleResponses")
            Prelude.<*> (x Data..?> "identitySource" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "identityValidationExpression")
            Prelude.<*> (x Data..?> "jwtConfiguration")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateAuthorizer where
  hashWithSalt _salt UpdateAuthorizer' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerCredentialsArn
      `Prelude.hashWithSalt` authorizerPayloadFormatVersion
      `Prelude.hashWithSalt` authorizerResultTtlInSeconds
      `Prelude.hashWithSalt` authorizerType
      `Prelude.hashWithSalt` authorizerUri
      `Prelude.hashWithSalt` enableSimpleResponses
      `Prelude.hashWithSalt` identitySource
      `Prelude.hashWithSalt` identityValidationExpression
      `Prelude.hashWithSalt` jwtConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` authorizerId
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData UpdateAuthorizer where
  rnf UpdateAuthorizer' {..} =
    Prelude.rnf authorizerCredentialsArn
      `Prelude.seq` Prelude.rnf authorizerPayloadFormatVersion
      `Prelude.seq` Prelude.rnf authorizerResultTtlInSeconds
      `Prelude.seq` Prelude.rnf authorizerType
      `Prelude.seq` Prelude.rnf authorizerUri
      `Prelude.seq` Prelude.rnf enableSimpleResponses
      `Prelude.seq` Prelude.rnf identitySource
      `Prelude.seq` Prelude.rnf identityValidationExpression
      `Prelude.seq` Prelude.rnf jwtConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf authorizerId
      `Prelude.seq` Prelude.rnf apiId

instance Data.ToHeaders UpdateAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateAuthorizer where
  toJSON UpdateAuthorizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizerCredentialsArn" Data..=)
              Prelude.<$> authorizerCredentialsArn,
            ("authorizerPayloadFormatVersion" Data..=)
              Prelude.<$> authorizerPayloadFormatVersion,
            ("authorizerResultTtlInSeconds" Data..=)
              Prelude.<$> authorizerResultTtlInSeconds,
            ("authorizerType" Data..=)
              Prelude.<$> authorizerType,
            ("authorizerUri" Data..=) Prelude.<$> authorizerUri,
            ("enableSimpleResponses" Data..=)
              Prelude.<$> enableSimpleResponses,
            ("identitySource" Data..=)
              Prelude.<$> identitySource,
            ("identityValidationExpression" Data..=)
              Prelude.<$> identityValidationExpression,
            ("jwtConfiguration" Data..=)
              Prelude.<$> jwtConfiguration,
            ("name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateAuthorizer where
  toPath UpdateAuthorizer' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/authorizers/",
        Data.toBS authorizerId
      ]

instance Data.ToQuery UpdateAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAuthorizerResponse' smart constructor.
data UpdateAuthorizerResponse = UpdateAuthorizerResponse'
  { -- | Specifies the required credentials as an IAM role for API Gateway to
    -- invoke the authorizer. To specify an IAM role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To use resource-based
    -- permissions on the Lambda function, don\'t specify this parameter.
    -- Supported only for REQUEST authorizers.
    authorizerCredentialsArn :: Prelude.Maybe Prelude.Text,
    -- | The authorizer identifier.
    authorizerId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the format of the payload sent to an HTTP API Lambda
    -- authorizer. Required for HTTP API Lambda authorizers. Supported values
    -- are 1.0 and 2.0. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
    authorizerPayloadFormatVersion :: Prelude.Maybe Prelude.Text,
    -- | The time to live (TTL) for cached authorizer results, in seconds. If it
    -- equals 0, authorization caching is disabled. If it is greater than 0,
    -- API Gateway caches authorizer responses. The maximum value is 3600, or 1
    -- hour. Supported only for HTTP API Lambda authorizers.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The authorizer type. Specify REQUEST for a Lambda function using
    -- incoming request parameters. Specify JWT to use JSON Web Tokens
    -- (supported only for HTTP APIs).
    authorizerType :: Prelude.Maybe AuthorizerType,
    -- | The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
    -- authorizers, this must be a well-formed Lambda function URI, for
    -- example,
    -- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
    -- In general, the URI has this form:
    -- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
    -- is the same as the region hosting the Lambda function, path indicates
    -- that the remaining substring in the URI should be treated as the path to
    -- the resource, including the initial \/. For Lambda functions, this is
    -- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
    -- Supported only for REQUEST authorizers.
    authorizerUri :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether a Lambda authorizer returns a response in a simple
    -- format. If enabled, the Lambda authorizer can return a boolean value
    -- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
    -- see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
    enableSimpleResponses :: Prelude.Maybe Prelude.Bool,
    -- | The identity source for which authorization is requested.
    --
    -- For a REQUEST authorizer, this is optional. The value is a set of one or
    -- more mapping expressions of the specified request parameters. The
    -- identity source can be headers, query string parameters, stage
    -- variables, and context parameters. For example, if an Auth header and a
    -- Name query string parameter are defined as identity sources, this value
    -- is route.request.header.Auth, route.request.querystring.Name for
    -- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
    -- \$, for example, $request.header.Auth, $request.querystring.Name. These
    -- parameters are used to perform runtime validation for Lambda-based
    -- authorizers by verifying all of the identity-related request parameters
    -- are present in the request, not null, and non-empty. Only when this is
    -- true does the authorizer invoke the authorizer Lambda function.
    -- Otherwise, it returns a 401 Unauthorized response without calling the
    -- Lambda function. For HTTP APIs, identity sources are also used as the
    -- cache key when caching is enabled. To learn more, see
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
    --
    -- For JWT, a single entry that specifies where to extract the JSON Web
    -- Token (JWT) from inbound requests. Currently only header-based and query
    -- parameter-based selections are supported, for example
    -- \$request.header.Authorization.
    identitySource :: Prelude.Maybe [Prelude.Text],
    -- | The validation expression does not apply to the REQUEST authorizer.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | Represents the configuration of a JWT authorizer. Required for the JWT
    -- authorizer type. Supported only for HTTP APIs.
    jwtConfiguration :: Prelude.Maybe JWTConfiguration,
    -- | The name of the authorizer.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerCredentialsArn', 'updateAuthorizerResponse_authorizerCredentialsArn' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
--
-- 'authorizerId', 'updateAuthorizerResponse_authorizerId' - The authorizer identifier.
--
-- 'authorizerPayloadFormatVersion', 'updateAuthorizerResponse_authorizerPayloadFormatVersion' - Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- 'authorizerResultTtlInSeconds', 'updateAuthorizerResponse_authorizerResultTtlInSeconds' - The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
--
-- 'authorizerType', 'updateAuthorizerResponse_authorizerType' - The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
--
-- 'authorizerUri', 'updateAuthorizerResponse_authorizerUri' - The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
-- authorizers, this must be a well-formed Lambda function URI, for
-- example,
-- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
-- In general, the URI has this form:
-- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
-- is the same as the region hosting the Lambda function, path indicates
-- that the remaining substring in the URI should be treated as the path to
-- the resource, including the initial \/. For Lambda functions, this is
-- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
-- Supported only for REQUEST authorizers.
--
-- 'enableSimpleResponses', 'updateAuthorizerResponse_enableSimpleResponses' - Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
--
-- 'identitySource', 'updateAuthorizerResponse_identitySource' - The identity source for which authorization is requested.
--
-- For a REQUEST authorizer, this is optional. The value is a set of one or
-- more mapping expressions of the specified request parameters. The
-- identity source can be headers, query string parameters, stage
-- variables, and context parameters. For example, if an Auth header and a
-- Name query string parameter are defined as identity sources, this value
-- is route.request.header.Auth, route.request.querystring.Name for
-- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
-- \$, for example, $request.header.Auth, $request.querystring.Name. These
-- parameters are used to perform runtime validation for Lambda-based
-- authorizers by verifying all of the identity-related request parameters
-- are present in the request, not null, and non-empty. Only when this is
-- true does the authorizer invoke the authorizer Lambda function.
-- Otherwise, it returns a 401 Unauthorized response without calling the
-- Lambda function. For HTTP APIs, identity sources are also used as the
-- cache key when caching is enabled. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- For JWT, a single entry that specifies where to extract the JSON Web
-- Token (JWT) from inbound requests. Currently only header-based and query
-- parameter-based selections are supported, for example
-- \$request.header.Authorization.
--
-- 'identityValidationExpression', 'updateAuthorizerResponse_identityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- 'jwtConfiguration', 'updateAuthorizerResponse_jwtConfiguration' - Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- 'name', 'updateAuthorizerResponse_name' - The name of the authorizer.
--
-- 'httpStatus', 'updateAuthorizerResponse_httpStatus' - The response's http status code.
newUpdateAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateAuthorizerResponse
newUpdateAuthorizerResponse pHttpStatus_ =
  UpdateAuthorizerResponse'
    { authorizerCredentialsArn =
        Prelude.Nothing,
      authorizerId = Prelude.Nothing,
      authorizerPayloadFormatVersion = Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing,
      authorizerType = Prelude.Nothing,
      authorizerUri = Prelude.Nothing,
      enableSimpleResponses = Prelude.Nothing,
      identitySource = Prelude.Nothing,
      identityValidationExpression = Prelude.Nothing,
      jwtConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
updateAuthorizerResponse_authorizerCredentialsArn :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_authorizerCredentialsArn = Lens.lens (\UpdateAuthorizerResponse' {authorizerCredentialsArn} -> authorizerCredentialsArn) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerCredentialsArn = a} :: UpdateAuthorizerResponse)

-- | The authorizer identifier.
updateAuthorizerResponse_authorizerId :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_authorizerId = Lens.lens (\UpdateAuthorizerResponse' {authorizerId} -> authorizerId) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerId = a} :: UpdateAuthorizerResponse)

-- | Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
updateAuthorizerResponse_authorizerPayloadFormatVersion :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_authorizerPayloadFormatVersion = Lens.lens (\UpdateAuthorizerResponse' {authorizerPayloadFormatVersion} -> authorizerPayloadFormatVersion) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerPayloadFormatVersion = a} :: UpdateAuthorizerResponse)

-- | The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
updateAuthorizerResponse_authorizerResultTtlInSeconds :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Natural)
updateAuthorizerResponse_authorizerResultTtlInSeconds = Lens.lens (\UpdateAuthorizerResponse' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerResultTtlInSeconds = a} :: UpdateAuthorizerResponse)

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
updateAuthorizerResponse_authorizerType :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe AuthorizerType)
updateAuthorizerResponse_authorizerType = Lens.lens (\UpdateAuthorizerResponse' {authorizerType} -> authorizerType) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerType = a} :: UpdateAuthorizerResponse)

-- | The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
-- authorizers, this must be a well-formed Lambda function URI, for
-- example,
-- arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations.
-- In general, the URI has this form:
-- arn:aws:apigateway:{region}:lambda:path\/{service_api} , where {region}
-- is the same as the region hosting the Lambda function, path indicates
-- that the remaining substring in the URI should be treated as the path to
-- the resource, including the initial \/. For Lambda functions, this is
-- usually of the form \/2015-03-31\/functions\/[FunctionARN]\/invocations.
-- Supported only for REQUEST authorizers.
updateAuthorizerResponse_authorizerUri :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_authorizerUri = Lens.lens (\UpdateAuthorizerResponse' {authorizerUri} -> authorizerUri) (\s@UpdateAuthorizerResponse' {} a -> s {authorizerUri = a} :: UpdateAuthorizerResponse)

-- | Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
updateAuthorizerResponse_enableSimpleResponses :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Bool)
updateAuthorizerResponse_enableSimpleResponses = Lens.lens (\UpdateAuthorizerResponse' {enableSimpleResponses} -> enableSimpleResponses) (\s@UpdateAuthorizerResponse' {} a -> s {enableSimpleResponses = a} :: UpdateAuthorizerResponse)

-- | The identity source for which authorization is requested.
--
-- For a REQUEST authorizer, this is optional. The value is a set of one or
-- more mapping expressions of the specified request parameters. The
-- identity source can be headers, query string parameters, stage
-- variables, and context parameters. For example, if an Auth header and a
-- Name query string parameter are defined as identity sources, this value
-- is route.request.header.Auth, route.request.querystring.Name for
-- WebSocket APIs. For HTTP APIs, use selection expressions prefixed with
-- \$, for example, $request.header.Auth, $request.querystring.Name. These
-- parameters are used to perform runtime validation for Lambda-based
-- authorizers by verifying all of the identity-related request parameters
-- are present in the request, not null, and non-empty. Only when this is
-- true does the authorizer invoke the authorizer Lambda function.
-- Otherwise, it returns a 401 Unauthorized response without calling the
-- Lambda function. For HTTP APIs, identity sources are also used as the
-- cache key when caching is enabled. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- For JWT, a single entry that specifies where to extract the JSON Web
-- Token (JWT) from inbound requests. Currently only header-based and query
-- parameter-based selections are supported, for example
-- \$request.header.Authorization.
updateAuthorizerResponse_identitySource :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe [Prelude.Text])
updateAuthorizerResponse_identitySource = Lens.lens (\UpdateAuthorizerResponse' {identitySource} -> identitySource) (\s@UpdateAuthorizerResponse' {} a -> s {identitySource = a} :: UpdateAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The validation expression does not apply to the REQUEST authorizer.
updateAuthorizerResponse_identityValidationExpression :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_identityValidationExpression = Lens.lens (\UpdateAuthorizerResponse' {identityValidationExpression} -> identityValidationExpression) (\s@UpdateAuthorizerResponse' {} a -> s {identityValidationExpression = a} :: UpdateAuthorizerResponse)

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
updateAuthorizerResponse_jwtConfiguration :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe JWTConfiguration)
updateAuthorizerResponse_jwtConfiguration = Lens.lens (\UpdateAuthorizerResponse' {jwtConfiguration} -> jwtConfiguration) (\s@UpdateAuthorizerResponse' {} a -> s {jwtConfiguration = a} :: UpdateAuthorizerResponse)

-- | The name of the authorizer.
updateAuthorizerResponse_name :: Lens.Lens' UpdateAuthorizerResponse (Prelude.Maybe Prelude.Text)
updateAuthorizerResponse_name = Lens.lens (\UpdateAuthorizerResponse' {name} -> name) (\s@UpdateAuthorizerResponse' {} a -> s {name = a} :: UpdateAuthorizerResponse)

-- | The response's http status code.
updateAuthorizerResponse_httpStatus :: Lens.Lens' UpdateAuthorizerResponse Prelude.Int
updateAuthorizerResponse_httpStatus = Lens.lens (\UpdateAuthorizerResponse' {httpStatus} -> httpStatus) (\s@UpdateAuthorizerResponse' {} a -> s {httpStatus = a} :: UpdateAuthorizerResponse)

instance Prelude.NFData UpdateAuthorizerResponse where
  rnf UpdateAuthorizerResponse' {..} =
    Prelude.rnf authorizerCredentialsArn
      `Prelude.seq` Prelude.rnf authorizerId
      `Prelude.seq` Prelude.rnf authorizerPayloadFormatVersion
      `Prelude.seq` Prelude.rnf authorizerResultTtlInSeconds
      `Prelude.seq` Prelude.rnf authorizerType
      `Prelude.seq` Prelude.rnf authorizerUri
      `Prelude.seq` Prelude.rnf enableSimpleResponses
      `Prelude.seq` Prelude.rnf identitySource
      `Prelude.seq` Prelude.rnf identityValidationExpression
      `Prelude.seq` Prelude.rnf jwtConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
