{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.Types.Authorizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Authorizer where

import Network.AWS.APIGateway.Types.AuthorizerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents an authorization layer for methods. If enabled on a method,
-- API Gateway will activate the authorizer when a client calls the method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer>
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
--
-- /See:/ 'newAuthorizer' smart constructor.
data Authorizer = Authorizer'
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
    -- | The identifier for the authorizer resource.
    id :: Prelude.Maybe Prelude.Text,
    -- | [Required] The name of the authorizer.
    name :: Prelude.Maybe Prelude.Text,
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
    -- | The authorizer type. Valid values are @TOKEN@ for a Lambda function
    -- using a single authorization token submitted in a custom header,
    -- @REQUEST@ for a Lambda function using incoming request parameters, and
    -- @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
    type' :: Prelude.Maybe AuthorizerType,
    -- | Optional customer-defined field, used in OpenAPI imports and exports
    -- without functional impact.
    authType :: Prelude.Maybe Prelude.Text,
    -- | The TTL in seconds of cached authorizer results. If it equals 0,
    -- authorization caching is disabled. If it is greater than 0, API Gateway
    -- will cache authorizer responses. If this field is not set, the default
    -- value is 300. The maximum value is 3600, or 1 hour.
    authorizerResultTtlInSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Authorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityValidationExpression', 'authorizer_identityValidationExpression' - A validation expression for the incoming identity token. For @TOKEN@
-- authorizers, this value is a regular expression. For
-- @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field
-- of the incoming token from the client against the specified regular
-- expression. It will invoke the authorizer\'s Lambda function when there
-- is a match. Otherwise, it will return a 401 Unauthorized response
-- without calling the Lambda function. The validation expression does not
-- apply to the @REQUEST@ authorizer.
--
-- 'authorizerCredentials', 'authorizer_authorizerCredentials' - Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, specify null.
--
-- 'id', 'authorizer_id' - The identifier for the authorizer resource.
--
-- 'name', 'authorizer_name' - [Required] The name of the authorizer.
--
-- 'providerARNs', 'authorizer_providerARNs' - A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@
-- authorizer. Each element is of this format:
-- @arn:aws:cognito-idp:{region}:{account_id}:userpool\/{user_pool_id}@.
-- For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- 'authorizerUri', 'authorizer_authorizerUri' - Specifies the authorizer\'s Uniform Resource Identifier (URI). For
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
-- 'identitySource', 'authorizer_identitySource' - The identity source for which authorization is requested.
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
-- 'type'', 'authorizer_type' - The authorizer type. Valid values are @TOKEN@ for a Lambda function
-- using a single authorization token submitted in a custom header,
-- @REQUEST@ for a Lambda function using incoming request parameters, and
-- @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
-- 'authType', 'authorizer_authType' - Optional customer-defined field, used in OpenAPI imports and exports
-- without functional impact.
--
-- 'authorizerResultTtlInSeconds', 'authorizer_authorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If it equals 0,
-- authorization caching is disabled. If it is greater than 0, API Gateway
-- will cache authorizer responses. If this field is not set, the default
-- value is 300. The maximum value is 3600, or 1 hour.
newAuthorizer ::
  Authorizer
newAuthorizer =
  Authorizer'
    { identityValidationExpression =
        Prelude.Nothing,
      authorizerCredentials = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      providerARNs = Prelude.Nothing,
      authorizerUri = Prelude.Nothing,
      identitySource = Prelude.Nothing,
      type' = Prelude.Nothing,
      authType = Prelude.Nothing,
      authorizerResultTtlInSeconds = Prelude.Nothing
    }

-- | A validation expression for the incoming identity token. For @TOKEN@
-- authorizers, this value is a regular expression. For
-- @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field
-- of the incoming token from the client against the specified regular
-- expression. It will invoke the authorizer\'s Lambda function when there
-- is a match. Otherwise, it will return a 401 Unauthorized response
-- without calling the Lambda function. The validation expression does not
-- apply to the @REQUEST@ authorizer.
authorizer_identityValidationExpression :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_identityValidationExpression = Lens.lens (\Authorizer' {identityValidationExpression} -> identityValidationExpression) (\s@Authorizer' {} a -> s {identityValidationExpression = a} :: Authorizer)

-- | Specifies the required credentials as an IAM role for API Gateway to
-- invoke the authorizer. To specify an IAM role for API Gateway to assume,
-- use the role\'s Amazon Resource Name (ARN). To use resource-based
-- permissions on the Lambda function, specify null.
authorizer_authorizerCredentials :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authorizerCredentials = Lens.lens (\Authorizer' {authorizerCredentials} -> authorizerCredentials) (\s@Authorizer' {} a -> s {authorizerCredentials = a} :: Authorizer)

-- | The identifier for the authorizer resource.
authorizer_id :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_id = Lens.lens (\Authorizer' {id} -> id) (\s@Authorizer' {} a -> s {id = a} :: Authorizer)

-- | [Required] The name of the authorizer.
authorizer_name :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_name = Lens.lens (\Authorizer' {name} -> name) (\s@Authorizer' {} a -> s {name = a} :: Authorizer)

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@
-- authorizer. Each element is of this format:
-- @arn:aws:cognito-idp:{region}:{account_id}:userpool\/{user_pool_id}@.
-- For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
authorizer_providerARNs :: Lens.Lens' Authorizer (Prelude.Maybe [Prelude.Text])
authorizer_providerARNs = Lens.lens (\Authorizer' {providerARNs} -> providerARNs) (\s@Authorizer' {} a -> s {providerARNs = a} :: Authorizer) Prelude.. Lens.mapping Prelude._Coerce

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
authorizer_authorizerUri :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authorizerUri = Lens.lens (\Authorizer' {authorizerUri} -> authorizerUri) (\s@Authorizer' {} a -> s {authorizerUri = a} :: Authorizer)

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
authorizer_identitySource :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_identitySource = Lens.lens (\Authorizer' {identitySource} -> identitySource) (\s@Authorizer' {} a -> s {identitySource = a} :: Authorizer)

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function
-- using a single authorization token submitted in a custom header,
-- @REQUEST@ for a Lambda function using incoming request parameters, and
-- @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
authorizer_type :: Lens.Lens' Authorizer (Prelude.Maybe AuthorizerType)
authorizer_type = Lens.lens (\Authorizer' {type'} -> type') (\s@Authorizer' {} a -> s {type' = a} :: Authorizer)

-- | Optional customer-defined field, used in OpenAPI imports and exports
-- without functional impact.
authorizer_authType :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Text)
authorizer_authType = Lens.lens (\Authorizer' {authType} -> authType) (\s@Authorizer' {} a -> s {authType = a} :: Authorizer)

-- | The TTL in seconds of cached authorizer results. If it equals 0,
-- authorization caching is disabled. If it is greater than 0, API Gateway
-- will cache authorizer responses. If this field is not set, the default
-- value is 300. The maximum value is 3600, or 1 hour.
authorizer_authorizerResultTtlInSeconds :: Lens.Lens' Authorizer (Prelude.Maybe Prelude.Int)
authorizer_authorizerResultTtlInSeconds = Lens.lens (\Authorizer' {authorizerResultTtlInSeconds} -> authorizerResultTtlInSeconds) (\s@Authorizer' {} a -> s {authorizerResultTtlInSeconds = a} :: Authorizer)

instance Prelude.FromJSON Authorizer where
  parseJSON =
    Prelude.withObject
      "Authorizer"
      ( \x ->
          Authorizer'
            Prelude.<$> (x Prelude..:? "identityValidationExpression")
            Prelude.<*> (x Prelude..:? "authorizerCredentials")
            Prelude.<*> (x Prelude..:? "id")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> ( x Prelude..:? "providerARNs"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "authorizerUri")
            Prelude.<*> (x Prelude..:? "identitySource")
            Prelude.<*> (x Prelude..:? "type")
            Prelude.<*> (x Prelude..:? "authType")
            Prelude.<*> (x Prelude..:? "authorizerResultTtlInSeconds")
      )

instance Prelude.Hashable Authorizer

instance Prelude.NFData Authorizer
