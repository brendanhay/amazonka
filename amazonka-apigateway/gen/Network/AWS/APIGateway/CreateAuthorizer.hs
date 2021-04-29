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
-- Module      : Network.AWS.APIGateway.CreateAuthorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new Authorizer resource to an existing RestApi resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/create-authorizer.html AWS CLI>
module Network.AWS.APIGateway.CreateAuthorizer
  ( -- * Creating a Request
    CreateAuthorizer (..),
    newCreateAuthorizer,

    -- * Request Lenses
    createAuthorizer_identityValidationExpression,
    createAuthorizer_authorizerCredentials,
    createAuthorizer_providerARNs,
    createAuthorizer_authorizerUri,
    createAuthorizer_identitySource,
    createAuthorizer_authType,
    createAuthorizer_authorizerResultTtlInSeconds,
    createAuthorizer_restApiId,
    createAuthorizer_name,
    createAuthorizer_type,

    -- * Destructuring the Response
    Authorizer (..),
    newAuthorizer,

    -- * Response Lenses
    authorizer_identityValidationExpression,
    authorizer_authorizerCredentials,
    authorizer_id,
    authorizer_name,
    authorizer_providerARNs,
    authorizer_authorizerUri,
    authorizer_identitySource,
    authorizer_type,
    authorizer_authType,
    authorizer_authorizerResultTtlInSeconds,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add a new Authorizer to an existing RestApi resource.
--
-- /See:/ 'newCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { -- | A validation expression for the incoming identity token. For @TOKEN@
    -- authorizers, this value is a regular expression. For
    -- @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field
    -- of the incoming token from the client against the specified regular
    -- expression. It will invoke the authorizer\'s Lambda function when there
    -- is a match. Otherwise, it will return a 401 Unauthorized response
    -- without calling the Lambda function. The validation expression does not
    -- apply to the @REQUEST@ authorizer.
    identityValidationExpression :: Prelude.Maybe Prelude.Text,
    -- | Specifies the required credentials as an IAM role for API Gateway to
    -- invoke the authorizer. To specify an IAM role for API Gateway to assume,
    -- use the role\'s Amazon Resource Name (ARN). To use resource-based
    -- permissions on the Lambda function, specify null.
    authorizerCredentials :: Prelude.Maybe Prelude.Text,
    -- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@
    -- authorizer. Each element is of this format:
    -- @arn:aws:cognito-idp:{region}:{account_id}:userpool\/{user_pool_id}@.
    -- For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
    providerARNs :: Prelude.Maybe [Prelude.Text],
    -- | Specifies the authorizer\'s Uniform Resource Identifier (URI). For
    -- @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda
    -- function URI, for example,
    -- @arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations@.
    -- In general, the URI has this form
    -- @arn:aws:apigateway:{region}:lambda:path\/{service_api}@, where
    -- @{region}@ is the same as the region hosting the Lambda function, @path@
    -- indicates that the remaining substring in the URI should be treated as
    -- the path to the resource, including the initial @\/@. For Lambda
    -- functions, this is usually of the form
    -- @\/2015-03-31\/functions\/[FunctionARN]\/invocations@.
    authorizerUri :: Prelude.Maybe Prelude.Text,
    -- | The identity source for which authorization is requested.
    --
    -- -   For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required
    --     and specifies the request header mapping expression for the custom
    --     header holding the authorization token submitted by the client. For
    --     example, if the token header name is @Auth@, the header mapping
    --     expression is @method.request.header.Auth@.
    -- -   For the @REQUEST@ authorizer, this is required when authorization
    --     caching is enabled. The value is a comma-separated string of one or
    --     more mapping expressions of the specified request parameters. For
    --     example, if an @Auth@ header, a @Name@ query string parameter are
    --     defined as identity sources, this value is
    --     @method.request.header.Auth, method.request.querystring.Name@. These
    --     parameters will be used to derive the authorization caching key and
    --     to perform runtime validation of the @REQUEST@ authorizer by
    --     verifying all of the identity-related request parameters are
    --     present, not null and non-empty. Only when this is true does the
    --     authorizer invoke the authorizer Lambda function, otherwise, it
    --     returns a 401 Unauthorized response without calling the Lambda
    --     function. The valid value is a string of comma-separated mapping
    --     expressions of the specified request parameters. When the
    --     authorization caching is not enabled, this property is optional.
    identitySource :: Prelude.Maybe Prelude.Text,
    -- | Optional customer-defined field, used in OpenAPI imports and exports
    -- without functional impact.
    authType :: Prelude.Maybe Prelude.Text,
    -- | The TTL in seconds of cached authorizer results. If it equals 0,
    -- authorization caching is disabled. If it is greater than 0, API Gateway
    -- will cache authorizer responses. If this field is not set, the default
    -- value is 300. The maximum value is 3600, or 1 hour.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Int,
    -- | [Required] The string identifier of the associated RestApi.
    restApiId :: Prelude.Text,
    -- | [Required] The name of the authorizer.
    name :: Prelude.Text,
    -- | [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda
    -- function using a single authorization token submitted in a custom
    -- header, @REQUEST@ for a Lambda function using incoming request
    -- parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user
    -- pool.
    type' :: AuthorizerType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityValidationExpression', 'createAuthorizer_identityValidationExpression' - A validation expression for the incoming identity token. For @TOKEN@
-- authorizers, this value is a regular expression. For
-- @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field
-- of the incoming token from the client against the specified regular
-- expression. It will invoke the authorizer\'s Lambda function when there
-- is a match. Otherwise, it will return a 401 Unauthorized response
-- without calling the Lambda function. The validation expression does not
-- apply to the @REQUEST@ authorizer.
--
-- 'authorizerCredentials', 'createAuthorizer_authorizerCredentials' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, specify null.
--
-- 'providerARNs', 'createAuthorizer_providerARNs' - A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@
-- authorizer. Each element is of this format:
-- @arn:aws:cognito-idp:{region}:{account_id}:userpool\/{user_pool_id}@.
-- For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- 'authorizerUri', 'createAuthorizer_authorizerUri' - Specifies the authorizer\'s Uniform Resource Identifier (URI). For
-- @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda
-- function URI, for example,
-- @arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations@.
-- In general, the URI has this form
-- @arn:aws:apigateway:{region}:lambda:path\/{service_api}@, where
-- @{region}@ is the same as the region hosting the Lambda function, @path@
-- indicates that the remaining substring in the URI should be treated as
-- the path to the resource, including the initial @\/@. For Lambda
-- functions, this is usually of the form
-- @\/2015-03-31\/functions\/[FunctionARN]\/invocations@.
--
-- 'identitySource', 'createAuthorizer_identitySource' - The identity source for which authorization is requested.
--
-- -   For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required
--     and specifies the request header mapping expression for the custom
--     header holding the authorization token submitted by the client. For
--     example, if the token header name is @Auth@, the header mapping
--     expression is @method.request.header.Auth@.
-- -   For the @REQUEST@ authorizer, this is required when authorization
--     caching is enabled. The value is a comma-separated string of one or
--     more mapping expressions of the specified request parameters. For
--     example, if an @Auth@ header, a @Name@ query string parameter are
--     defined as identity sources, this value is
--     @method.request.header.Auth, method.request.querystring.Name@. These
--     parameters will be used to derive the authorization caching key and
--     to perform runtime validation of the @REQUEST@ authorizer by
--     verifying all of the identity-related request parameters are
--     present, not null and non-empty. Only when this is true does the
--     authorizer invoke the authorizer Lambda function, otherwise, it
--     returns a 401 Unauthorized response without calling the Lambda
--     function. The valid value is a string of comma-separated mapping
--     expressions of the specified request parameters. When the
--     authorization caching is not enabled, this property is optional.
--
-- 'authType', 'createAuthorizer_authType' - Optional customer-defined field, used in OpenAPI imports and exports
-- without functional impact.
--
-- 'authorizerResultTtlInSeconds', 'createAuthorizer_authorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If it equals 0,
-- authorization caching is disabled. If it is greater than 0, API Gateway
-- will cache authorizer responses. If this field is not set, the default
-- value is 300. The maximum value is 3600, or 1 hour.
--
-- 'restApiId', 'createAuthorizer_restApiId' - [Required] The string identifier of the associated RestApi.
--
-- 'name', 'createAuthorizer_name' - [Required] The name of the authorizer.
--
-- 'type'', 'createAuthorizer_type' - [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda
-- function using a single authorization token submitted in a custom
-- header, @REQUEST@ for a Lambda function using incoming request
-- parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user
-- pool.
newCreateAuthorizer ::
  -- | 'restApiId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  AuthorizerType ->
  CreateAuthorizer
newCreateAuthorizer pRestApiId_ pName_ pType_ =
  CreateAuthorizer'
    { identityValidationExpression =
        Prelude.Nothing,
      authorizerCredentials = Prelude.Nothing,
      providerARNs = Prelude.Nothing,
      authorizerUri = Prelude.Nothing,
      identitySource = Prelude.Nothing,
      authType = Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing,
      restApiId = pRestApiId_,
      name = pName_,
      type' = pType_
    }

-- | A validation expression for the incoming identity token. For @TOKEN@
-- authorizers, this value is a regular expression. For
-- @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field
-- of the incoming token from the client against the specified regular
-- expression. It will invoke the authorizer\'s Lambda function when there
-- is a match. Otherwise, it will return a 401 Unauthorized response
-- without calling the Lambda function. The validation expression does not
-- apply to the @REQUEST@ authorizer.
createAuthorizer_identityValidationExpression :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_identityValidationExpression = Lens.lens (\CreateAuthorizer' {identityValidationExpression} -> identityValidationExpression) (\s@CreateAuthorizer' {} a -> s {identityValidationExpression = a} :: CreateAuthorizer)

-- | Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, specify null.
createAuthorizer_authorizerCredentials :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_authorizerCredentials = Lens.lens (\CreateAuthorizer' {authorizerCredentials} -> authorizerCredentials) (\s@CreateAuthorizer' {} a -> s {authorizerCredentials = a} :: CreateAuthorizer)

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@
-- authorizer. Each element is of this format:
-- @arn:aws:cognito-idp:{region}:{account_id}:userpool\/{user_pool_id}@.
-- For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
createAuthorizer_providerARNs :: Lens.Lens' CreateAuthorizer (Prelude.Maybe [Prelude.Text])
createAuthorizer_providerARNs = Lens.lens (\CreateAuthorizer' {providerARNs} -> providerARNs) (\s@CreateAuthorizer' {} a -> s {providerARNs = a} :: CreateAuthorizer) Prelude.. Lens.mapping Prelude._Coerce

-- | Specifies the authorizer\'s Uniform Resource Identifier (URI). For
-- @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda
-- function URI, for example,
-- @arn:aws:apigateway:us-west-2:lambda:path\/2015-03-31\/functions\/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}\/invocations@.
-- In general, the URI has this form
-- @arn:aws:apigateway:{region}:lambda:path\/{service_api}@, where
-- @{region}@ is the same as the region hosting the Lambda function, @path@
-- indicates that the remaining substring in the URI should be treated as
-- the path to the resource, including the initial @\/@. For Lambda
-- functions, this is usually of the form
-- @\/2015-03-31\/functions\/[FunctionARN]\/invocations@.
createAuthorizer_authorizerUri :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_authorizerUri = Lens.lens (\CreateAuthorizer' {authorizerUri} -> authorizerUri) (\s@CreateAuthorizer' {} a -> s {authorizerUri = a} :: CreateAuthorizer)

-- | The identity source for which authorization is requested.
--
-- -   For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required
--     and specifies the request header mapping expression for the custom
--     header holding the authorization token submitted by the client. For
--     example, if the token header name is @Auth@, the header mapping
--     expression is @method.request.header.Auth@.
-- -   For the @REQUEST@ authorizer, this is required when authorization
--     caching is enabled. The value is a comma-separated string of one or
--     more mapping expressions of the specified request parameters. For
--     example, if an @Auth@ header, a @Name@ query string parameter are
--     defined as identity sources, this value is
--     @method.request.header.Auth, method.request.querystring.Name@. These
--     parameters will be used to derive the authorization caching key and
--     to perform runtime validation of the @REQUEST@ authorizer by
--     verifying all of the identity-related request parameters are
--     present, not null and non-empty. Only when this is true does the
--     authorizer invoke the authorizer Lambda function, otherwise, it
--     returns a 401 Unauthorized response without calling the Lambda
--     function. The valid value is a string of comma-separated mapping
--     expressions of the specified request parameters. When the
--     authorization caching is not enabled, this property is optional.
createAuthorizer_identitySource :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_identitySource = Lens.lens (\CreateAuthorizer' {identitySource} -> identitySource) (\s@CreateAuthorizer' {} a -> s {identitySource = a} :: CreateAuthorizer)

-- | Optional customer-defined field, used in OpenAPI imports and exports
-- without functional impact.
createAuthorizer_authType :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Text)
createAuthorizer_authType = Lens.lens (\CreateAuthorizer' {authType} -> authType) (\s@CreateAuthorizer' {} a -> s {authType = a} :: CreateAuthorizer)

-- | The TTL in seconds of cached authorizer results. If it equals 0,
-- authorization caching is disabled. If it is greater than 0, API Gateway
-- will cache authorizer responses. If this field is not set, the default
-- value is 300. The maximum value is 3600, or 1 hour.
createAuthorizer_authorizerResultTtlInSeconds :: Lens.Lens' CreateAuthorizer (Prelude.Maybe Prelude.Int)
createAuthorizer_authorizerResultTtlInSeconds = Lens.lens (\CreateAuthorizer' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@CreateAuthorizer' {} a -> s {authorizerResultTtlInSeconds = a} :: CreateAuthorizer)

-- | [Required] The string identifier of the associated RestApi.
createAuthorizer_restApiId :: Lens.Lens' CreateAuthorizer Prelude.Text
createAuthorizer_restApiId = Lens.lens (\CreateAuthorizer' {restApiId} -> restApiId) (\s@CreateAuthorizer' {} a -> s {restApiId = a} :: CreateAuthorizer)

-- | [Required] The name of the authorizer.
createAuthorizer_name :: Lens.Lens' CreateAuthorizer Prelude.Text
createAuthorizer_name = Lens.lens (\CreateAuthorizer' {name} -> name) (\s@CreateAuthorizer' {} a -> s {name = a} :: CreateAuthorizer)

-- | [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda
-- function using a single authorization token submitted in a custom
-- header, @REQUEST@ for a Lambda function using incoming request
-- parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user
-- pool.
createAuthorizer_type :: Lens.Lens' CreateAuthorizer AuthorizerType
createAuthorizer_type = Lens.lens (\CreateAuthorizer' {type'} -> type') (\s@CreateAuthorizer' {} a -> s {type' = a} :: CreateAuthorizer)

instance Prelude.AWSRequest CreateAuthorizer where
  type Rs CreateAuthorizer = Authorizer
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Prelude.eitherParseJSON x)

instance Prelude.Hashable CreateAuthorizer

instance Prelude.NFData CreateAuthorizer

instance Prelude.ToHeaders CreateAuthorizer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("identityValidationExpression" Prelude..=)
              Prelude.<$> identityValidationExpression,
            ("authorizerCredentials" Prelude..=)
              Prelude.<$> authorizerCredentials,
            ("providerARNs" Prelude..=) Prelude.<$> providerARNs,
            ("authorizerUri" Prelude..=)
              Prelude.<$> authorizerUri,
            ("identitySource" Prelude..=)
              Prelude.<$> identitySource,
            ("authType" Prelude..=) Prelude.<$> authType,
            ("authorizerResultTtlInSeconds" Prelude..=)
              Prelude.<$> authorizerResultTtlInSeconds,
            Prelude.Just ("name" Prelude..= name),
            Prelude.Just ("type" Prelude..= type')
          ]
      )

instance Prelude.ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    Prelude.mconcat
      [ "/restapis/",
        Prelude.toBS restApiId,
        "/authorizers"
      ]

instance Prelude.ToQuery CreateAuthorizer where
  toQuery = Prelude.const Prelude.mempty
