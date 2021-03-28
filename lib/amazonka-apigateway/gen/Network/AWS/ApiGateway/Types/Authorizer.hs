{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ApiGateway.Types.Authorizer
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ApiGateway.Types.Authorizer
  ( Authorizer (..)
  -- * Smart constructor
  , mkAuthorizer
  -- * Lenses
  , aAuthType
  , aAuthorizerCredentials
  , aAuthorizerResultTtlInSeconds
  , aAuthorizerUri
  , aId
  , aIdentitySource
  , aIdentityValidationExpression
  , aName
  , aProviderARNs
  , aType
  ) where

import qualified Network.AWS.ApiGateway.Types.AuthorizerType as Types
import qualified Network.AWS.ApiGateway.Types.ProviderARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an authorization layer for methods. If enabled on a method, API Gateway will activate the authorizer when a client calls the method.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-use-lambda-authorizer.html Use Lambda Function as Authorizer> <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-integrate-with-cognito.html Use Cognito User Pool as Authorizer> 
--
-- /See:/ 'mkAuthorizer' smart constructor.
data Authorizer = Authorizer'
  { authType :: Core.Maybe Core.Text
    -- ^ Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
  , authorizerCredentials :: Core.Maybe Core.Text
    -- ^ Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
  , authorizerResultTtlInSeconds :: Core.Maybe Core.Int
    -- ^ The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
  , authorizerUri :: Core.Maybe Core.Text
    -- ^ Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
  , id :: Core.Maybe Core.Text
    -- ^ The identifier for the authorizer resource.
  , identitySource :: Core.Maybe Core.Text
    -- ^ The identity source for which authorization is requested. 
--
--     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
--
--     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
--
  , identityValidationExpression :: Core.Maybe Core.Text
    -- ^ A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
  , name :: Core.Maybe Core.Text
    -- ^ [Required] The name of the authorizer.
  , providerARNs :: Core.Maybe [Types.ProviderARN]
    -- ^ A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
  , type' :: Core.Maybe Types.AuthorizerType
    -- ^ The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Authorizer' value with any optional fields omitted.
mkAuthorizer
    :: Authorizer
mkAuthorizer
  = Authorizer'{authType = Core.Nothing,
                authorizerCredentials = Core.Nothing,
                authorizerResultTtlInSeconds = Core.Nothing,
                authorizerUri = Core.Nothing, id = Core.Nothing,
                identitySource = Core.Nothing,
                identityValidationExpression = Core.Nothing, name = Core.Nothing,
                providerARNs = Core.Nothing, type' = Core.Nothing}

-- | Optional customer-defined field, used in OpenAPI imports and exports without functional impact.
--
-- /Note:/ Consider using 'authType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthType :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aAuthType = Lens.field @"authType"
{-# INLINEABLE aAuthType #-}
{-# DEPRECATED authType "Use generic-lens or generic-optics with 'authType' instead"  #-}

-- | Specifies the required credentials as an IAM role for API Gateway to invoke the authorizer. To specify an IAM role for API Gateway to assume, use the role's Amazon Resource Name (ARN). To use resource-based permissions on the Lambda function, specify null.
--
-- /Note:/ Consider using 'authorizerCredentials' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthorizerCredentials :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aAuthorizerCredentials = Lens.field @"authorizerCredentials"
{-# INLINEABLE aAuthorizerCredentials #-}
{-# DEPRECATED authorizerCredentials "Use generic-lens or generic-optics with 'authorizerCredentials' instead"  #-}

-- | The TTL in seconds of cached authorizer results. If it equals 0, authorization caching is disabled. If it is greater than 0, API Gateway will cache authorizer responses. If this field is not set, the default value is 300. The maximum value is 3600, or 1 hour.
--
-- /Note:/ Consider using 'authorizerResultTtlInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthorizerResultTtlInSeconds :: Lens.Lens' Authorizer (Core.Maybe Core.Int)
aAuthorizerResultTtlInSeconds = Lens.field @"authorizerResultTtlInSeconds"
{-# INLINEABLE aAuthorizerResultTtlInSeconds #-}
{-# DEPRECATED authorizerResultTtlInSeconds "Use generic-lens or generic-optics with 'authorizerResultTtlInSeconds' instead"  #-}

-- | Specifies the authorizer's Uniform Resource Identifier (URI). For @TOKEN@ or @REQUEST@ authorizers, this must be a well-formed Lambda function URI, for example, @arn:aws:apigateway:us-west-2:lambda:path/2015-03-31/functions/arn:aws:lambda:us-west-2:{account_id}:function:{lambda_function_name}/invocations@ . In general, the URI has this form @arn:aws:apigateway:{region}:lambda:path/{service_api}@ , where @{region}@ is the same as the region hosting the Lambda function, @path@ indicates that the remaining substring in the URI should be treated as the path to the resource, including the initial @/@ . For Lambda functions, this is usually of the form @/2015-03-31/functions/[FunctionARN]/invocations@ .
--
-- /Note:/ Consider using 'authorizerUri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAuthorizerUri :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aAuthorizerUri = Lens.field @"authorizerUri"
{-# INLINEABLE aAuthorizerUri #-}
{-# DEPRECATED authorizerUri "Use generic-lens or generic-optics with 'authorizerUri' instead"  #-}

-- | The identifier for the authorizer resource.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aId :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aId = Lens.field @"id"
{-# INLINEABLE aId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The identity source for which authorization is requested. 
--
--     * For a @TOKEN@ or @COGNITO_USER_POOLS@ authorizer, this is required and specifies the request header mapping expression for the custom header holding the authorization token submitted by the client. For example, if the token header name is @Auth@ , the header mapping expression is @method.request.header.Auth@ .
--
--     * For the @REQUEST@ authorizer, this is required when authorization caching is enabled. The value is a comma-separated string of one or more mapping expressions of the specified request parameters. For example, if an @Auth@ header, a @Name@ query string parameter are defined as identity sources, this value is @method.request.header.Auth, method.request.querystring.Name@ . These parameters will be used to derive the authorization caching key and to perform runtime validation of the @REQUEST@ authorizer by verifying all of the identity-related request parameters are present, not null and non-empty. Only when this is true does the authorizer invoke the authorizer Lambda function, otherwise, it returns a 401 Unauthorized response without calling the Lambda function. The valid value is a string of comma-separated mapping expressions of the specified request parameters. When the authorization caching is not enabled, this property is optional.
--
--
--
-- /Note:/ Consider using 'identitySource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIdentitySource :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aIdentitySource = Lens.field @"identitySource"
{-# INLINEABLE aIdentitySource #-}
{-# DEPRECATED identitySource "Use generic-lens or generic-optics with 'identitySource' instead"  #-}

-- | A validation expression for the incoming identity token. For @TOKEN@ authorizers, this value is a regular expression. For @COGNITO_USER_POOLS@ authorizers, API Gateway will match the @aud@ field of the incoming token from the client against the specified regular expression. It will invoke the authorizer's Lambda function when there is a match. Otherwise, it will return a 401 Unauthorized response without calling the Lambda function. The validation expression does not apply to the @REQUEST@ authorizer.
--
-- /Note:/ Consider using 'identityValidationExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aIdentityValidationExpression :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aIdentityValidationExpression = Lens.field @"identityValidationExpression"
{-# INLINEABLE aIdentityValidationExpression #-}
{-# DEPRECATED identityValidationExpression "Use generic-lens or generic-optics with 'identityValidationExpression' instead"  #-}

-- | [Required] The name of the authorizer.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aName :: Lens.Lens' Authorizer (Core.Maybe Core.Text)
aName = Lens.field @"name"
{-# INLINEABLE aName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A list of the Amazon Cognito user pool ARNs for the @COGNITO_USER_POOLS@ authorizer. Each element is of this format: @arn:aws:cognito-idp:{region}:{account_id}:userpool/{user_pool_id}@ . For a @TOKEN@ or @REQUEST@ authorizer, this is not defined.
--
-- /Note:/ Consider using 'providerARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aProviderARNs :: Lens.Lens' Authorizer (Core.Maybe [Types.ProviderARN])
aProviderARNs = Lens.field @"providerARNs"
{-# INLINEABLE aProviderARNs #-}
{-# DEPRECATED providerARNs "Use generic-lens or generic-optics with 'providerARNs' instead"  #-}

-- | The authorizer type. Valid values are @TOKEN@ for a Lambda function using a single authorization token submitted in a custom header, @REQUEST@ for a Lambda function using incoming request parameters, and @COGNITO_USER_POOLS@ for using an Amazon Cognito user pool.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Authorizer (Core.Maybe Types.AuthorizerType)
aType = Lens.field @"type'"
{-# INLINEABLE aType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

instance Core.FromJSON Authorizer where
        parseJSON
          = Core.withObject "Authorizer" Core.$
              \ x ->
                Authorizer' Core.<$>
                  (x Core..:? "authType") Core.<*> x Core..:? "authorizerCredentials"
                    Core.<*> x Core..:? "authorizerResultTtlInSeconds"
                    Core.<*> x Core..:? "authorizerUri"
                    Core.<*> x Core..:? "id"
                    Core.<*> x Core..:? "identitySource"
                    Core.<*> x Core..:? "identityValidationExpression"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "providerARNs"
                    Core.<*> x Core..:? "type"
