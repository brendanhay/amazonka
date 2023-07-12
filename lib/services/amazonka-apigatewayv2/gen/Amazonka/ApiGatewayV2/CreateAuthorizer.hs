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
-- Module      : Amazonka.ApiGatewayV2.CreateAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Authorizer for an API.
module Amazonka.ApiGatewayV2.CreateAuthorizer
  ( -- * Creating a Request
    CreateAuthorizer (..),
    newCreateAuthorizer,

    -- * Request Lenses
    createAuthorizer_authorizerCredentialsArn,
    createAuthorizer_authorizerPayloadFormatVersion,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_authorizerUri,
    createAuthorizer_enableSimpleResponses,
    createAuthorizer_identityValidationExpression,
    createAuthorizer_jwtConfiguration,
    createAuthorizer_apiId,
    createAuthorizer_authorizerType,
    createAuthorizer_identitySource,
    createAuthorizer_name,

    -- * Destructuring the Response
    CreateAuthorizerResponse (..),
    newCreateAuthorizerResponse,

    -- * Response Lenses
    createAuthorizerResponse_authorizerCredentialsArn,
    createAuthorizerResponse_authorizerId,
    createAuthorizerResponse_authorizerPayloadFormatVersion,
    createAuthorizerResponse_authorizerResultTtlInSeconds,
    createAuthorizerResponse_authorizerType,
    createAuthorizerResponse_authorizerUri,
    createAuthorizerResponse_enableSimpleResponses,
    createAuthorizerResponse_identitySource,
    createAuthorizerResponse_identityValidationExpression,
    createAuthorizerResponse_jwtConfiguration,
    createAuthorizerResponse_name,
    createAuthorizerResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates a new Authorizer resource to represent an authorizer.
--
-- /See:/ 'newCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { -- | Specifies the required credentials as an IAM role for API Gateway to
    -- invoke the authorizer. To specify an IAM role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To use resource-based
    -- permissions on the Lambda function, don\'t specify this parameter.
    -- Supported only for REQUEST authorizers.
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
    -- | This parameter is not used.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | Represents the configuration of a JWT authorizer. Required for the JWT
    -- authorizer type. Supported only for HTTP APIs.
    jwtConfiguration :: Prelude.Maybe JWTConfiguration,
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The authorizer type. Specify REQUEST for a Lambda function using
    -- incoming request parameters. Specify JWT to use JSON Web Tokens
    -- (supported only for HTTP APIs).
    authorizerType :: AuthorizerType,
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
    identitySource :: [Prelude.Text],
    -- | The name of the authorizer.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerCredentialsArn', 'createAuthorizer_authorizerCredentialsArn' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
--
-- 'authorizerPayloadFormatVersion', 'createAuthorizer_authorizerPayloadFormatVersion' - Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- 'authorizerResultTtlInSeconds', 'createAuthorizer_authorizerResultTtlInSeconds' - The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
--
-- 'authorizerUri', 'createAuthorizer_authorizerUri' - The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
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
-- 'enableSimpleResponses', 'createAuthorizer_enableSimpleResponses' - Specifies whether a Lambda authorizer returns a response in a simple
-- format. By default, a Lambda authorizer must return an IAM policy. If
-- enabled, the Lambda authorizer can return a boolean value instead of an
-- IAM policy. Supported only for HTTP APIs. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
--
-- 'identityValidationExpression', 'createAuthorizer_identityValidationExpression' - This parameter is not used.
--
-- 'jwtConfiguration', 'createAuthorizer_jwtConfiguration' - Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- 'apiId', 'createAuthorizer_apiId' - The API identifier.
--
-- 'authorizerType', 'createAuthorizer_authorizerType' - The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
--
-- 'identitySource', 'createAuthorizer_identitySource' - The identity source for which authorization is requested.
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
-- 'name', 'createAuthorizer_name' - The name of the authorizer.
newCreateAuthorizer ::
  -- | 'apiId'
  Prelude.Text ->
  -- | 'authorizerType'
  AuthorizerType ->
  -- | 'name'
  Prelude.Text ->
  CreateAuthorizer
newCreateAuthorizer pApiId_ pAuthorizerType_ pName_ =
  CreateAuthorizer'
    { authorizerCredentialsArn =
        Prelude.Nothing,
      authorizerPayloadFormatVersion = Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing,
      authorizerUri = Prelude.Nothing,
      enableSimpleResponses = Prelude.Nothing,
      identityValidationExpression = Prelude.Nothing,
      jwtConfiguration = Prelude.Nothing,
      apiId = pApiId_,
      authorizerType = pAuthorizerType_,
      identitySource = Prelude.mempty,
      name = pName_
    }

-- | Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
createAuthorizer_authorizerCredentialsArn :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_authorizerCredentialsArn = Lens.lens (\CreateAuthorizer' {authorizerCredentialsArn} -> authorizerCredentialsArn) (\s@CreateAuthorizer' {} a -> s {authorizerCredentialsArn = a} :: CreateAuthorizer)

-- | Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
createAuthorizer_authorizerPayloadFormatVersion :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_authorizerPayloadFormatVersion = Lens.lens (\CreateAuthorizer' {authorizerPayloadFormatVersion} -> authorizerPayloadFormatVersion) (\s@CreateAuthorizer' {} a -> s {authorizerPayloadFormatVersion = a} :: CreateAuthorizer)

-- | The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
createAuthorizer_authorizerResultTtlInSeconds :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Natural)
createAuthorizer_authorizerResultTtlInSeconds = Lens.lens (\CreateAuthorizer' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@CreateAuthorizer' {} a -> s {authorizerResultTtlInSeconds = a} :: CreateAuthorizer)

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
createAuthorizer_authorizerUri :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_authorizerUri = Lens.lens (\CreateAuthorizer' {authorizerUri} -> authorizerUri) (\s@CreateAuthorizer' {} a -> s {authorizerUri = a} :: CreateAuthorizer)

-- | Specifies whether a Lambda authorizer returns a response in a simple
-- format. By default, a Lambda authorizer must return an IAM policy. If
-- enabled, the Lambda authorizer can return a boolean value instead of an
-- IAM policy. Supported only for HTTP APIs. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
createAuthorizer_enableSimpleResponses :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Bool)
createAuthorizer_enableSimpleResponses = Lens.lens (\CreateAuthorizer' {enableSimpleResponses} -> enableSimpleResponses) (\s@CreateAuthorizer' {} a -> s {enableSimpleResponses = a} :: CreateAuthorizer)

-- | This parameter is not used.
createAuthorizer_identityValidationExpression :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_identityValidationExpression = Lens.lens (\CreateAuthorizer' {identityValidationExpression} -> identityValidationExpression) (\s@CreateAuthorizer' {} a -> s {identityValidationExpression = a} :: CreateAuthorizer)

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
createAuthorizer_jwtConfiguration :: Lens.Lens' CreateAuthorizer (Prelude.Maybe JWTConfiguration)
createAuthorizer_jwtConfiguration = Lens.lens (\CreateAuthorizer' {jwtConfiguration} -> jwtConfiguration) (\s@CreateAuthorizer' {} a -> s {jwtConfiguration = a} :: CreateAuthorizer)

-- | The API identifier.
createAuthorizer_apiId :: Lens.Lens' CreateAuthorizer Prelude.Text
createAuthorizer_apiId = Lens.lens (\CreateAuthorizer' {apiId} -> apiId) (\s@CreateAuthorizer' {} a -> s {apiId = a} :: CreateAuthorizer)

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
createAuthorizer_authorizerType :: Lens.Lens' CreateAuthorizer AuthorizerType
createAuthorizer_authorizerType = Lens.lens (\CreateAuthorizer' {authorizerType} -> authorizerType) (\s@CreateAuthorizer' {} a -> s {authorizerType = a} :: CreateAuthorizer)

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
createAuthorizer_identitySource :: Lens.Lens' CreateAuthorizer [Prelude.Text]
createAuthorizer_identitySource = Lens.lens (\CreateAuthorizer' {identitySource} -> identitySource) (\s@CreateAuthorizer' {} a -> s {identitySource = a} :: CreateAuthorizer) Prelude.. Lens.coerced

-- | The name of the authorizer.
createAuthorizer_name :: Lens.Lens' CreateAuthorizer Prelude.Text
createAuthorizer_name = Lens.lens (\CreateAuthorizer' {name} -> name) (\s@CreateAuthorizer' {} a -> s {name = a} :: CreateAuthorizer)

instance Core.AWSRequest CreateAuthorizer where
  type
    AWSResponse CreateAuthorizer =
      CreateAuthorizerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAuthorizerResponse'
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

instance Prelude.Hashable CreateAuthorizer where
  hashWithSalt _salt CreateAuthorizer' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerCredentialsArn
      `Prelude.hashWithSalt` authorizerPayloadFormatVersion
      `Prelude.hashWithSalt` authorizerResultTtlInSeconds
      `Prelude.hashWithSalt` authorizerUri
      `Prelude.hashWithSalt` enableSimpleResponses
      `Prelude.hashWithSalt` identityValidationExpression
      `Prelude.hashWithSalt` jwtConfiguration
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` authorizerType
      `Prelude.hashWithSalt` identitySource
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateAuthorizer where
  rnf CreateAuthorizer' {..} =
    Prelude.rnf authorizerCredentialsArn
      `Prelude.seq` Prelude.rnf authorizerPayloadFormatVersion
      `Prelude.seq` Prelude.rnf authorizerResultTtlInSeconds
      `Prelude.seq` Prelude.rnf authorizerUri
      `Prelude.seq` Prelude.rnf enableSimpleResponses
      `Prelude.seq` Prelude.rnf identityValidationExpression
      `Prelude.seq` Prelude.rnf jwtConfiguration
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf authorizerType
      `Prelude.seq` Prelude.rnf identitySource
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("authorizerCredentialsArn" Data..=)
              Prelude.<$> authorizerCredentialsArn,
            ("authorizerPayloadFormatVersion" Data..=)
              Prelude.<$> authorizerPayloadFormatVersion,
            ("authorizerResultTtlInSeconds" Data..=)
              Prelude.<$> authorizerResultTtlInSeconds,
            ("authorizerUri" Data..=) Prelude.<$> authorizerUri,
            ("enableSimpleResponses" Data..=)
              Prelude.<$> enableSimpleResponses,
            ("identityValidationExpression" Data..=)
              Prelude.<$> identityValidationExpression,
            ("jwtConfiguration" Data..=)
              Prelude.<$> jwtConfiguration,
            Prelude.Just
              ("authorizerType" Data..= authorizerType),
            Prelude.Just
              ("identitySource" Data..= identitySource),
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    Prelude.mconcat
      ["/v2/apis/", Data.toBS apiId, "/authorizers"]

instance Data.ToQuery CreateAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAuthorizerResponse' smart constructor.
data CreateAuthorizerResponse = CreateAuthorizerResponse'
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
-- Create a value of 'CreateAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerCredentialsArn', 'createAuthorizerResponse_authorizerCredentialsArn' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
--
-- 'authorizerId', 'createAuthorizerResponse_authorizerId' - The authorizer identifier.
--
-- 'authorizerPayloadFormatVersion', 'createAuthorizerResponse_authorizerPayloadFormatVersion' - Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- 'authorizerResultTtlInSeconds', 'createAuthorizerResponse_authorizerResultTtlInSeconds' - The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
--
-- 'authorizerType', 'createAuthorizerResponse_authorizerType' - The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
--
-- 'authorizerUri', 'createAuthorizerResponse_authorizerUri' - The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
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
-- 'enableSimpleResponses', 'createAuthorizerResponse_enableSimpleResponses' - Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
--
-- 'identitySource', 'createAuthorizerResponse_identitySource' - The identity source for which authorization is requested.
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
-- 'identityValidationExpression', 'createAuthorizerResponse_identityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- 'jwtConfiguration', 'createAuthorizerResponse_jwtConfiguration' - Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- 'name', 'createAuthorizerResponse_name' - The name of the authorizer.
--
-- 'httpStatus', 'createAuthorizerResponse_httpStatus' - The response's http status code.
newCreateAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAuthorizerResponse
newCreateAuthorizerResponse pHttpStatus_ =
  CreateAuthorizerResponse'
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
createAuthorizerResponse_authorizerCredentialsArn :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_authorizerCredentialsArn = Lens.lens (\CreateAuthorizerResponse' {authorizerCredentialsArn} -> authorizerCredentialsArn) (\s@CreateAuthorizerResponse' {} a -> s {authorizerCredentialsArn = a} :: CreateAuthorizerResponse)

-- | The authorizer identifier.
createAuthorizerResponse_authorizerId :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_authorizerId = Lens.lens (\CreateAuthorizerResponse' {authorizerId} -> authorizerId) (\s@CreateAuthorizerResponse' {} a -> s {authorizerId = a} :: CreateAuthorizerResponse)

-- | Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
createAuthorizerResponse_authorizerPayloadFormatVersion :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_authorizerPayloadFormatVersion = Lens.lens (\CreateAuthorizerResponse' {authorizerPayloadFormatVersion} -> authorizerPayloadFormatVersion) (\s@CreateAuthorizerResponse' {} a -> s {authorizerPayloadFormatVersion = a} :: CreateAuthorizerResponse)

-- | The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
createAuthorizerResponse_authorizerResultTtlInSeconds :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Natural)
createAuthorizerResponse_authorizerResultTtlInSeconds = Lens.lens (\CreateAuthorizerResponse' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@CreateAuthorizerResponse' {} a -> s {authorizerResultTtlInSeconds = a} :: CreateAuthorizerResponse)

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
createAuthorizerResponse_authorizerType :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe AuthorizerType)
createAuthorizerResponse_authorizerType = Lens.lens (\CreateAuthorizerResponse' {authorizerType} -> authorizerType) (\s@CreateAuthorizerResponse' {} a -> s {authorizerType = a} :: CreateAuthorizerResponse)

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
createAuthorizerResponse_authorizerUri :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_authorizerUri = Lens.lens (\CreateAuthorizerResponse' {authorizerUri} -> authorizerUri) (\s@CreateAuthorizerResponse' {} a -> s {authorizerUri = a} :: CreateAuthorizerResponse)

-- | Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
createAuthorizerResponse_enableSimpleResponses :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Bool)
createAuthorizerResponse_enableSimpleResponses = Lens.lens (\CreateAuthorizerResponse' {enableSimpleResponses} -> enableSimpleResponses) (\s@CreateAuthorizerResponse' {} a -> s {enableSimpleResponses = a} :: CreateAuthorizerResponse)

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
createAuthorizerResponse_identitySource :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe [Prelude.Text])
createAuthorizerResponse_identitySource = Lens.lens (\CreateAuthorizerResponse' {identitySource} -> identitySource) (\s@CreateAuthorizerResponse' {} a -> s {identitySource = a} :: CreateAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The validation expression does not apply to the REQUEST authorizer.
createAuthorizerResponse_identityValidationExpression :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_identityValidationExpression = Lens.lens (\CreateAuthorizerResponse' {identityValidationExpression} -> identityValidationExpression) (\s@CreateAuthorizerResponse' {} a -> s {identityValidationExpression = a} :: CreateAuthorizerResponse)

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
createAuthorizerResponse_jwtConfiguration :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe JWTConfiguration)
createAuthorizerResponse_jwtConfiguration = Lens.lens (\CreateAuthorizerResponse' {jwtConfiguration} -> jwtConfiguration) (\s@CreateAuthorizerResponse' {} a -> s {jwtConfiguration = a} :: CreateAuthorizerResponse)

-- | The name of the authorizer.
createAuthorizerResponse_name :: Lens.Lens' CreateAuthorizerResponse (Prelude.Maybe Prelude.Text)
createAuthorizerResponse_name = Lens.lens (\CreateAuthorizerResponse' {name} -> name) (\s@CreateAuthorizerResponse' {} a -> s {name = a} :: CreateAuthorizerResponse)

-- | The response's http status code.
createAuthorizerResponse_httpStatus :: Lens.Lens' CreateAuthorizerResponse Prelude.Int
createAuthorizerResponse_httpStatus = Lens.lens (\CreateAuthorizerResponse' {httpStatus} -> httpStatus) (\s@CreateAuthorizerResponse' {} a -> s {httpStatus = a} :: CreateAuthorizerResponse)

instance Prelude.NFData CreateAuthorizerResponse where
  rnf CreateAuthorizerResponse' {..} =
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
