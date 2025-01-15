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
-- Module      : Amazonka.ApiGatewayV2.GetAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an Authorizer.
module Amazonka.ApiGatewayV2.GetAuthorizer
  ( -- * Creating a Request
    GetAuthorizer (..),
    newGetAuthorizer,

    -- * Request Lenses
    getAuthorizer_authorizerId,
    getAuthorizer_apiId,

    -- * Destructuring the Response
    GetAuthorizerResponse (..),
    newGetAuthorizerResponse,

    -- * Response Lenses
    getAuthorizerResponse_authorizerCredentialsArn,
    getAuthorizerResponse_authorizerId,
    getAuthorizerResponse_authorizerPayloadFormatVersion,
    getAuthorizerResponse_authorizerResultTtlInSeconds,
    getAuthorizerResponse_authorizerType,
    getAuthorizerResponse_authorizerUri,
    getAuthorizerResponse_enableSimpleResponses,
    getAuthorizerResponse_identitySource,
    getAuthorizerResponse_identityValidationExpression,
    getAuthorizerResponse_jwtConfiguration,
    getAuthorizerResponse_name,
    getAuthorizerResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAuthorizer' smart constructor.
data GetAuthorizer = GetAuthorizer'
  { -- | The authorizer identifier.
    authorizerId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerId', 'getAuthorizer_authorizerId' - The authorizer identifier.
--
-- 'apiId', 'getAuthorizer_apiId' - The API identifier.
newGetAuthorizer ::
  -- | 'authorizerId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  GetAuthorizer
newGetAuthorizer pAuthorizerId_ pApiId_ =
  GetAuthorizer'
    { authorizerId = pAuthorizerId_,
      apiId = pApiId_
    }

-- | The authorizer identifier.
getAuthorizer_authorizerId :: Lens.Lens' GetAuthorizer Prelude.Text
getAuthorizer_authorizerId = Lens.lens (\GetAuthorizer' {authorizerId} -> authorizerId) (\s@GetAuthorizer' {} a -> s {authorizerId = a} :: GetAuthorizer)

-- | The API identifier.
getAuthorizer_apiId :: Lens.Lens' GetAuthorizer Prelude.Text
getAuthorizer_apiId = Lens.lens (\GetAuthorizer' {apiId} -> apiId) (\s@GetAuthorizer' {} a -> s {apiId = a} :: GetAuthorizer)

instance Core.AWSRequest GetAuthorizer where
  type
    AWSResponse GetAuthorizer =
      GetAuthorizerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAuthorizerResponse'
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

instance Prelude.Hashable GetAuthorizer where
  hashWithSalt _salt GetAuthorizer' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerId
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData GetAuthorizer where
  rnf GetAuthorizer' {..} =
    Prelude.rnf authorizerId `Prelude.seq`
      Prelude.rnf apiId

instance Data.ToHeaders GetAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetAuthorizer where
  toPath GetAuthorizer' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Data.toBS apiId,
        "/authorizers/",
        Data.toBS authorizerId
      ]

instance Data.ToQuery GetAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAuthorizerResponse' smart constructor.
data GetAuthorizerResponse = GetAuthorizerResponse'
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
-- Create a value of 'GetAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerCredentialsArn', 'getAuthorizerResponse_authorizerCredentialsArn' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, don\'t specify this parameter.
-- Supported only for REQUEST authorizers.
--
-- 'authorizerId', 'getAuthorizerResponse_authorizerId' - The authorizer identifier.
--
-- 'authorizerPayloadFormatVersion', 'getAuthorizerResponse_authorizerPayloadFormatVersion' - Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
--
-- 'authorizerResultTtlInSeconds', 'getAuthorizerResponse_authorizerResultTtlInSeconds' - The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
--
-- 'authorizerType', 'getAuthorizerResponse_authorizerType' - The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
--
-- 'authorizerUri', 'getAuthorizerResponse_authorizerUri' - The authorizer\'s Uniform Resource Identifier (URI). For REQUEST
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
-- 'enableSimpleResponses', 'getAuthorizerResponse_enableSimpleResponses' - Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
--
-- 'identitySource', 'getAuthorizerResponse_identitySource' - The identity source for which authorization is requested.
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
-- 'identityValidationExpression', 'getAuthorizerResponse_identityValidationExpression' - The validation expression does not apply to the REQUEST authorizer.
--
-- 'jwtConfiguration', 'getAuthorizerResponse_jwtConfiguration' - Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
--
-- 'name', 'getAuthorizerResponse_name' - The name of the authorizer.
--
-- 'httpStatus', 'getAuthorizerResponse_httpStatus' - The response's http status code.
newGetAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAuthorizerResponse
newGetAuthorizerResponse pHttpStatus_ =
  GetAuthorizerResponse'
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
getAuthorizerResponse_authorizerCredentialsArn :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Text)
getAuthorizerResponse_authorizerCredentialsArn = Lens.lens (\GetAuthorizerResponse' {authorizerCredentialsArn} -> authorizerCredentialsArn) (\s@GetAuthorizerResponse' {} a -> s {authorizerCredentialsArn = a} :: GetAuthorizerResponse)

-- | The authorizer identifier.
getAuthorizerResponse_authorizerId :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Text)
getAuthorizerResponse_authorizerId = Lens.lens (\GetAuthorizerResponse' {authorizerId} -> authorizerId) (\s@GetAuthorizerResponse' {} a -> s {authorizerId = a} :: GetAuthorizerResponse)

-- | Specifies the format of the payload sent to an HTTP API Lambda
-- authorizer. Required for HTTP API Lambda authorizers. Supported values
-- are 1.0 and 2.0. To learn more, see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>.
getAuthorizerResponse_authorizerPayloadFormatVersion :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Text)
getAuthorizerResponse_authorizerPayloadFormatVersion = Lens.lens (\GetAuthorizerResponse' {authorizerPayloadFormatVersion} -> authorizerPayloadFormatVersion) (\s@GetAuthorizerResponse' {} a -> s {authorizerPayloadFormatVersion = a} :: GetAuthorizerResponse)

-- | The time to live (TTL) for cached authorizer results, in seconds. If it
-- equals 0, authorization caching is disabled. If it is greater than 0,
-- API Gateway caches authorizer responses. The maximum value is 3600, or 1
-- hour. Supported only for HTTP API Lambda authorizers.
getAuthorizerResponse_authorizerResultTtlInSeconds :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Natural)
getAuthorizerResponse_authorizerResultTtlInSeconds = Lens.lens (\GetAuthorizerResponse' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@GetAuthorizerResponse' {} a -> s {authorizerResultTtlInSeconds = a} :: GetAuthorizerResponse)

-- | The authorizer type. Specify REQUEST for a Lambda function using
-- incoming request parameters. Specify JWT to use JSON Web Tokens
-- (supported only for HTTP APIs).
getAuthorizerResponse_authorizerType :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe AuthorizerType)
getAuthorizerResponse_authorizerType = Lens.lens (\GetAuthorizerResponse' {authorizerType} -> authorizerType) (\s@GetAuthorizerResponse' {} a -> s {authorizerType = a} :: GetAuthorizerResponse)

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
getAuthorizerResponse_authorizerUri :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Text)
getAuthorizerResponse_authorizerUri = Lens.lens (\GetAuthorizerResponse' {authorizerUri} -> authorizerUri) (\s@GetAuthorizerResponse' {} a -> s {authorizerUri = a} :: GetAuthorizerResponse)

-- | Specifies whether a Lambda authorizer returns a response in a simple
-- format. If enabled, the Lambda authorizer can return a boolean value
-- instead of an IAM policy. Supported only for HTTP APIs. To learn more,
-- see
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/http-api-lambda-authorizer.html Working with AWS Lambda authorizers for HTTP APIs>
getAuthorizerResponse_enableSimpleResponses :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Bool)
getAuthorizerResponse_enableSimpleResponses = Lens.lens (\GetAuthorizerResponse' {enableSimpleResponses} -> enableSimpleResponses) (\s@GetAuthorizerResponse' {} a -> s {enableSimpleResponses = a} :: GetAuthorizerResponse)

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
getAuthorizerResponse_identitySource :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe [Prelude.Text])
getAuthorizerResponse_identitySource = Lens.lens (\GetAuthorizerResponse' {identitySource} -> identitySource) (\s@GetAuthorizerResponse' {} a -> s {identitySource = a} :: GetAuthorizerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The validation expression does not apply to the REQUEST authorizer.
getAuthorizerResponse_identityValidationExpression :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Text)
getAuthorizerResponse_identityValidationExpression = Lens.lens (\GetAuthorizerResponse' {identityValidationExpression} -> identityValidationExpression) (\s@GetAuthorizerResponse' {} a -> s {identityValidationExpression = a} :: GetAuthorizerResponse)

-- | Represents the configuration of a JWT authorizer. Required for the JWT
-- authorizer type. Supported only for HTTP APIs.
getAuthorizerResponse_jwtConfiguration :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe JWTConfiguration)
getAuthorizerResponse_jwtConfiguration = Lens.lens (\GetAuthorizerResponse' {jwtConfiguration} -> jwtConfiguration) (\s@GetAuthorizerResponse' {} a -> s {jwtConfiguration = a} :: GetAuthorizerResponse)

-- | The name of the authorizer.
getAuthorizerResponse_name :: Lens.Lens' GetAuthorizerResponse (Prelude.Maybe Prelude.Text)
getAuthorizerResponse_name = Lens.lens (\GetAuthorizerResponse' {name} -> name) (\s@GetAuthorizerResponse' {} a -> s {name = a} :: GetAuthorizerResponse)

-- | The response's http status code.
getAuthorizerResponse_httpStatus :: Lens.Lens' GetAuthorizerResponse Prelude.Int
getAuthorizerResponse_httpStatus = Lens.lens (\GetAuthorizerResponse' {httpStatus} -> httpStatus) (\s@GetAuthorizerResponse' {} a -> s {httpStatus = a} :: GetAuthorizerResponse)

instance Prelude.NFData GetAuthorizerResponse where
  rnf GetAuthorizerResponse' {..} =
    Prelude.rnf authorizerCredentialsArn `Prelude.seq`
      Prelude.rnf authorizerId `Prelude.seq`
        Prelude.rnf authorizerPayloadFormatVersion `Prelude.seq`
          Prelude.rnf authorizerResultTtlInSeconds `Prelude.seq`
            Prelude.rnf authorizerType `Prelude.seq`
              Prelude.rnf authorizerUri `Prelude.seq`
                Prelude.rnf enableSimpleResponses `Prelude.seq`
                  Prelude.rnf identitySource `Prelude.seq`
                    Prelude.rnf identityValidationExpression `Prelude.seq`
                      Prelude.rnf jwtConfiguration `Prelude.seq`
                        Prelude.rnf name `Prelude.seq`
                          Prelude.rnf httpStatus
