{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.CreateAuthorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a new 'Authorizer' resource to an existing 'RestApi' resource.
--
-- <https://docs.aws.amazon.com/cli/latest/reference/apigateway/create-authorizer.html AWS CLI>
module Network.AWS.APIGateway.CreateAuthorizer
  ( -- * Creating a request
    CreateAuthorizer (..),
    mkCreateAuthorizer,

    -- ** Request lenses
    caAuthorizerURI,
    caIdentityValidationExpression,
    caProviderARNs,
    caName,
    caRestAPIId,
    caAuthorizerResultTtlInSeconds,
    caAuthType,
    caType,
    caIdentitySource,
    caAuthorizerCredentials,

    -- * Destructuring the response
    Authorizer (..),
    mkAuthorizer,

    -- ** Response lenses
    aAuthorizerURI,
    aIdentityValidationExpression,
    aProviderARNs,
    aName,
    aId,
    aAuthorizerResultTtlInSeconds,
    aAuthType,
    aType,
    aIdentitySource,
    aAuthorizerCredentials,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to add a new 'Authorizer' to an existing 'RestApi' resource.
--
-- /See:/ 'mkCreateAuthorizer' smart constructor.
data CreateAuthorizer = CreateAuthorizer'
  { -- | Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
    authorizerURI :: Lude.Maybe Lude.Text,
    -- | A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
    identityValidationExpression :: Lude.Maybe Lude.Text,
    -- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
    providerARNs :: Lude.Maybe [Lude.Text],
    -- | [Required] The name of the authorizer.
    name :: Lude.Text,
    -- | [Required] The string identifier of the associated 'RestApi' .
    restAPIId :: Lude.Text,
    -- | The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
    authorizerResultTtlInSeconds :: Lude.Maybe Lude.Int,
    -- | Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
    authType :: Lude.Maybe Lude.Text,
    -- | [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
    type' :: AuthorizerType,
    -- | The identity source for which authorization is requested.
    --
    --     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
    --
    --     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
    identitySource :: Lude.Maybe Lude.Text,
    -- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
    authorizerCredentials :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateAuthorizer' with the minimum fields required to make a request.
--
-- * 'authorizerURI' - Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
-- * 'identityValidationExpression' - A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
-- * 'providerARNs' - A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
-- * 'name' - [Required] The name of the authorizer.
-- * 'restAPIId' - [Required] The string identifier of the associated 'RestApi' .
-- * 'authorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
-- * 'authType' - Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
-- * 'type'' - [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
-- * 'identitySource' - The identity source for which authorization is requested.
--
--     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
--
--     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
--
-- * 'authorizerCredentials' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
mkCreateAuthorizer ::
  -- | 'name'
  Lude.Text ->
  -- | 'restAPIId'
  Lude.Text ->
  -- | 'type''
  AuthorizerType ->
  CreateAuthorizer
mkCreateAuthorizer pName_ pRestAPIId_ pType_ =
  CreateAuthorizer'
    { authorizerURI = Lude.Nothing,
      identityValidationExpression = Lude.Nothing,
      providerARNs = Lude.Nothing,
      name = pName_,
      restAPIId = pRestAPIId_,
      authorizerResultTtlInSeconds = Lude.Nothing,
      authType = Lude.Nothing,
      type' = pType_,
      identitySource = Lude.Nothing,
      authorizerCredentials = Lude.Nothing
    }

-- | Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
--
-- /Note:/ Consider using 'authorizerURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerURI :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Text)
caAuthorizerURI = Lens.lens (authorizerURI :: CreateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {authorizerURI = a} :: CreateAuthorizer)
{-# DEPRECATED caAuthorizerURI "Use generic-lens or generic-optics with 'authorizerURI' instead." #-}

-- | A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
--
-- /Note:/ Consider using 'identityValidationExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caIdentityValidationExpression :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Text)
caIdentityValidationExpression = Lens.lens (identityValidationExpression :: CreateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {identityValidationExpression = a} :: CreateAuthorizer)
{-# DEPRECATED caIdentityValidationExpression "Use generic-lens or generic-optics with 'identityValidationExpression' instead." #-}

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- /Note:/ Consider using 'providerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caProviderARNs :: Lens.Lens' CreateAuthorizer (Lude.Maybe [Lude.Text])
caProviderARNs = Lens.lens (providerARNs :: CreateAuthorizer -> Lude.Maybe [Lude.Text]) (\s a -> s {providerARNs = a} :: CreateAuthorizer)
{-# DEPRECATED caProviderARNs "Use generic-lens or generic-optics with 'providerARNs' instead." #-}

-- | [Required] The name of the authorizer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateAuthorizer Lude.Text
caName = Lens.lens (name :: CreateAuthorizer -> Lude.Text) (\s a -> s {name = a} :: CreateAuthorizer)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | [Required] The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caRestAPIId :: Lens.Lens' CreateAuthorizer Lude.Text
caRestAPIId = Lens.lens (restAPIId :: CreateAuthorizer -> Lude.Text) (\s a -> s {restAPIId = a} :: CreateAuthorizer)
{-# DEPRECATED caRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

-- | The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- /Note:/ Consider using 'authorizerResultTtlInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerResultTtlInSeconds :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Int)
caAuthorizerResultTtlInSeconds = Lens.lens (authorizerResultTtlInSeconds :: CreateAuthorizer -> Lude.Maybe Lude.Int) (\s a -> s {authorizerResultTtlInSeconds = a} :: CreateAuthorizer)
{-# DEPRECATED caAuthorizerResultTtlInSeconds "Use generic-lens or generic-optics with 'authorizerResultTtlInSeconds' instead." #-}

-- | Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthType :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Text)
caAuthType = Lens.lens (authType :: CreateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {authType = a} :: CreateAuthorizer)
{-# DEPRECATED caAuthType "Use generic-lens or generic-optics with 'authType' instead." #-}

-- | [Required] The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caType :: Lens.Lens' CreateAuthorizer AuthorizerType
caType = Lens.lens (type' :: CreateAuthorizer -> AuthorizerType) (\s a -> s {type' = a} :: CreateAuthorizer)
{-# DEPRECATED caType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The identity source for which authorization is requested.
--
--     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
--
--     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
--
--
-- /Note:/ Consider using 'identitySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caIdentitySource :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Text)
caIdentitySource = Lens.lens (identitySource :: CreateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {identitySource = a} :: CreateAuthorizer)
{-# DEPRECATED caIdentitySource "Use generic-lens or generic-optics with 'identitySource' instead." #-}

-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- /Note:/ Consider using 'authorizerCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthorizerCredentials :: Lens.Lens' CreateAuthorizer (Lude.Maybe Lude.Text)
caAuthorizerCredentials = Lens.lens (authorizerCredentials :: CreateAuthorizer -> Lude.Maybe Lude.Text) (\s a -> s {authorizerCredentials = a} :: CreateAuthorizer)
{-# DEPRECATED caAuthorizerCredentials "Use generic-lens or generic-optics with 'authorizerCredentials' instead." #-}

instance Lude.AWSRequest CreateAuthorizer where
  type Rs CreateAuthorizer = Authorizer
  request = Req.postJSON apiGatewayService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders CreateAuthorizer where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          ["Accept" Lude.=# ("application/json" :: Lude.ByteString)]
      )

instance Lude.ToJSON CreateAuthorizer where
  toJSON CreateAuthorizer' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("authorizerUri" Lude..=) Lude.<$> authorizerURI,
            ("identityValidationExpression" Lude..=)
              Lude.<$> identityValidationExpression,
            ("providerARNs" Lude..=) Lude.<$> providerARNs,
            Lude.Just ("name" Lude..= name),
            ("authorizerResultTtlInSeconds" Lude..=)
              Lude.<$> authorizerResultTtlInSeconds,
            ("authType" Lude..=) Lude.<$> authType,
            Lude.Just ("type" Lude..= type'),
            ("identitySource" Lude..=) Lude.<$> identitySource,
            ("authorizerCredentials" Lude..=) Lude.<$> authorizerCredentials
          ]
      )

instance Lude.ToPath CreateAuthorizer where
  toPath CreateAuthorizer' {..} =
    Lude.mconcat ["/restapis/", Lude.toBS restAPIId, "/authorizers"]

instance Lude.ToQuery CreateAuthorizer where
  toQuery = Lude.const Lude.mempty
