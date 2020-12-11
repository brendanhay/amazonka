-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.Authorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.Authorizer
  ( Authorizer (..),

    -- * Smart constructor
    mkAuthorizer,

    -- * Lenses
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

import Network.AWS.APIGateway.Types.AuthorizerType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an authorization layer for methods. If enabled on a method, API Gateway will activate the authorizer when a client calls the method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer>
--
-- /See:/ 'mkAuthorizer' smart constructor.
data Authorizer = Authorizer'
  { authorizerURI ::
      Lude.Maybe Lude.Text,
    identityValidationExpression :: Lude.Maybe Lude.Text,
    providerARNs :: Lude.Maybe [Lude.Text],
    name :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    authorizerResultTtlInSeconds :: Lude.Maybe Lude.Int,
    authType :: Lude.Maybe Lude.Text,
    type' :: Lude.Maybe AuthorizerType,
    identitySource :: Lude.Maybe Lude.Text,
    authorizerCredentials :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Authorizer' with the minimum fields required to make a request.
--
-- * 'authType' - Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
-- * 'authorizerCredentials' - Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
-- * 'authorizerResultTtlInSeconds' - The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
-- * 'authorizerURI' - Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
-- * 'id' - The identifier for the authorizer resource.
-- * 'identitySource' - The identity source for which authorization is requested.
--
--     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
--
--     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
--
-- * 'identityValidationExpression' - A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
-- * 'name' - [Required] The name of the authorizer.
-- * 'providerARNs' - A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
-- * 'type'' - The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
mkAuthorizer ::
  Authorizer
mkAuthorizer =
  Authorizer'
    { authorizerURI = Lude.Nothing,
      identityValidationExpression = Lude.Nothing,
      providerARNs = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing,
      authorizerResultTtlInSeconds = Lude.Nothing,
      authType = Lude.Nothing,
      type' = Lude.Nothing,
      identitySource = Lude.Nothing,
      authorizerCredentials = Lude.Nothing
    }

-- | Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
--
-- /Note:/ Consider using 'authorizerURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthorizerURI :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aAuthorizerURI = Lens.lens (authorizerURI :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {authorizerURI = a} :: Authorizer)
{-# DEPRECATED aAuthorizerURI "Use generic-lens or generic-optics with 'authorizerURI' instead." #-}

-- | A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
--
-- /Note:/ Consider using 'identityValidationExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIdentityValidationExpression :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aIdentityValidationExpression = Lens.lens (identityValidationExpression :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {identityValidationExpression = a} :: Authorizer)
{-# DEPRECATED aIdentityValidationExpression "Use generic-lens or generic-optics with 'identityValidationExpression' instead." #-}

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- /Note:/ Consider using 'providerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProviderARNs :: Lens.Lens' Authorizer (Lude.Maybe [Lude.Text])
aProviderARNs = Lens.lens (providerARNs :: Authorizer -> Lude.Maybe [Lude.Text]) (\s a -> s {providerARNs = a} :: Authorizer)
{-# DEPRECATED aProviderARNs "Use generic-lens or generic-optics with 'providerARNs' instead." #-}

-- | [Required] The name of the authorizer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aName = Lens.lens (name :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Authorizer)
{-# DEPRECATED aName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier for the authorizer resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aId = Lens.lens (id :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: Authorizer)
{-# DEPRECATED aId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- /Note:/ Consider using 'authorizerResultTtlInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthorizerResultTtlInSeconds :: Lens.Lens' Authorizer (Lude.Maybe Lude.Int)
aAuthorizerResultTtlInSeconds = Lens.lens (authorizerResultTtlInSeconds :: Authorizer -> Lude.Maybe Lude.Int) (\s a -> s {authorizerResultTtlInSeconds = a} :: Authorizer)
{-# DEPRECATED aAuthorizerResultTtlInSeconds "Use generic-lens or generic-optics with 'authorizerResultTtlInSeconds' instead." #-}

-- | Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthType :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aAuthType = Lens.lens (authType :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {authType = a} :: Authorizer)
{-# DEPRECATED aAuthType "Use generic-lens or generic-optics with 'authType' instead." #-}

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Authorizer (Lude.Maybe AuthorizerType)
aType = Lens.lens (type' :: Authorizer -> Lude.Maybe AuthorizerType) (\s a -> s {type' = a} :: Authorizer)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The identity source for which authorization is requested.
--
--     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
--
--     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
--
--
-- /Note:/ Consider using 'identitySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIdentitySource :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aIdentitySource = Lens.lens (identitySource :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {identitySource = a} :: Authorizer)
{-# DEPRECATED aIdentitySource "Use generic-lens or generic-optics with 'identitySource' instead." #-}

-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- /Note:/ Consider using 'authorizerCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthorizerCredentials :: Lens.Lens' Authorizer (Lude.Maybe Lude.Text)
aAuthorizerCredentials = Lens.lens (authorizerCredentials :: Authorizer -> Lude.Maybe Lude.Text) (\s a -> s {authorizerCredentials = a} :: Authorizer)
{-# DEPRECATED aAuthorizerCredentials "Use generic-lens or generic-optics with 'authorizerCredentials' instead." #-}

instance Lude.FromJSON Authorizer where
  parseJSON =
    Lude.withObject
      "Authorizer"
      ( \x ->
          Authorizer'
            Lude.<$> (x Lude..:? "authorizerUri")
            Lude.<*> (x Lude..:? "identityValidationExpression")
            Lude.<*> (x Lude..:? "providerARNs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "authorizerResultTtlInSeconds")
            Lude.<*> (x Lude..:? "authType")
            Lude.<*> (x Lude..:? "type")
            Lude.<*> (x Lude..:? "identitySource")
            Lude.<*> (x Lude..:? "authorizerCredentials")
      )
