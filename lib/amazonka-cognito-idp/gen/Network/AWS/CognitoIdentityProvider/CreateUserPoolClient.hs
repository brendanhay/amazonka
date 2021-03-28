{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user pool client.
module Network.AWS.CognitoIdentityProvider.CreateUserPoolClient
    (
    -- * Creating a request
      CreateUserPoolClient (..)
    , mkCreateUserPoolClient
    -- ** Request lenses
    , cupcUserPoolId
    , cupcClientName
    , cupcAccessTokenValidity
    , cupcAllowedOAuthFlows
    , cupcAllowedOAuthFlowsUserPoolClient
    , cupcAllowedOAuthScopes
    , cupcAnalyticsConfiguration
    , cupcCallbackURLs
    , cupcDefaultRedirectURI
    , cupcExplicitAuthFlows
    , cupcGenerateSecret
    , cupcIdTokenValidity
    , cupcLogoutURLs
    , cupcPreventUserExistenceErrors
    , cupcReadAttributes
    , cupcRefreshTokenValidity
    , cupcSupportedIdentityProviders
    , cupcTokenValidityUnits
    , cupcWriteAttributes

    -- * Destructuring the response
    , CreateUserPoolClientResponse (..)
    , mkCreateUserPoolClientResponse
    -- ** Response lenses
    , cupcrrsUserPoolClient
    , cupcrrsResponseStatus
    ) where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create a user pool client.
--
-- /See:/ 'mkCreateUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
  { userPoolId :: Types.UserPoolId
    -- ^ The user pool ID for the user pool where you want to create a user pool client.
  , clientName :: Types.ClientNameType
    -- ^ The client name for the user pool client you would like to create.
  , accessTokenValidity :: Core.Maybe Core.Natural
    -- ^ The time limit, between 5 minutes and 1 day, after which the access token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
  , allowedOAuthFlows :: Core.Maybe [Types.OAuthFlowType]
    -- ^ The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
  , allowedOAuthFlowsUserPoolClient :: Core.Maybe Core.Bool
    -- ^ Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
  , allowedOAuthScopes :: Core.Maybe [Types.ScopeType]
    -- ^ The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
  , analyticsConfiguration :: Core.Maybe Types.AnalyticsConfigurationType
    -- ^ The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
  , callbackURLs :: Core.Maybe [Types.RedirectUrlType]
    -- ^ A list of allowed redirect (callback) URLs for the identity providers.
--
-- A redirect URI must:
--
--     * Be an absolute URI.
--
--
--     * Be registered with the authorization server.
--
--
--     * Not include a fragment component.
--
--
-- See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> .
-- Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only.
-- App callback URLs such as myapp://example are also supported.
  , defaultRedirectURI :: Core.Maybe Types.RedirectUrlType
    -- ^ The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- A redirect URI must:
--
--     * Be an absolute URI.
--
--
--     * Be registered with the authorization server.
--
--
--     * Not include a fragment component.
--
--
-- See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> .
-- Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only.
-- App callback URLs such as myapp://example are also supported.
  , explicitAuthFlows :: Core.Maybe [Types.ExplicitAuthFlowsType]
    -- ^ The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix.
--
-- Valid values include:
--
--     * @ALLOW_ADMIN_USER_PASSWORD_AUTH@ : Enable admin based user password authentication flow @ADMIN_USER_PASSWORD_AUTH@ . This setting replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication flow, Cognito receives the password in the request instead of using the SRP (Secure Remote Password protocol) protocol to verify passwords.
--
--
--     * @ALLOW_CUSTOM_AUTH@ : Enable Lambda trigger based authentication.
--
--
--     * @ALLOW_USER_PASSWORD_AUTH@ : Enable user password-based authentication. In this flow, Cognito receives the password in the request instead of using the SRP protocol to verify passwords.
--
--
--     * @ALLOW_USER_SRP_AUTH@ : Enable SRP based authentication.
--
--
--     * @ALLOW_REFRESH_TOKEN_AUTH@ : Enable authflow to refresh tokens.
--
--
  , generateSecret :: Core.Maybe Core.Bool
    -- ^ Boolean to specify whether you want to generate a secret for the user pool client being created.
  , idTokenValidity :: Core.Maybe Core.Natural
    -- ^ The time limit, between 5 minutes and 1 day, after which the ID token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
  , logoutURLs :: Core.Maybe [Types.RedirectUrlType]
    -- ^ A list of allowed logout URLs for the identity providers.
  , preventUserExistenceErrors :: Core.Maybe Types.PreventUserExistenceErrorTypes
    -- ^ Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool.
--
-- Valid values include:
--
--     * @ENABLED@ - This prevents user existence-related errors.
--
--
--     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
--
--
  , readAttributes :: Core.Maybe [Types.ClientPermissionType]
    -- ^ The read attributes.
  , refreshTokenValidity :: Core.Maybe Core.Natural
    -- ^ The time limit, in days, after which the refresh token is no longer valid and cannot be used.
  , supportedIdentityProviders :: Core.Maybe [Types.ProviderNameType]
    -- ^ A list of provider names for the identity providers that are supported on this client. The following are supported: @COGNITO@ , @Facebook@ , @Google@ and @LoginWithAmazon@ .
  , tokenValidityUnits :: Core.Maybe Types.TokenValidityUnitsType
    -- ^ The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
  , writeAttributes :: Core.Maybe [Types.ClientPermissionType]
    -- ^ The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an identity provider, this array must include all attributes that are mapped to identity provider attributes. Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If your app client lacks write access to a mapped attribute, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserPoolClient' value with any optional fields omitted.
mkCreateUserPoolClient
    :: Types.UserPoolId -- ^ 'userPoolId'
    -> Types.ClientNameType -- ^ 'clientName'
    -> CreateUserPoolClient
mkCreateUserPoolClient userPoolId clientName
  = CreateUserPoolClient'{userPoolId, clientName,
                          accessTokenValidity = Core.Nothing,
                          allowedOAuthFlows = Core.Nothing,
                          allowedOAuthFlowsUserPoolClient = Core.Nothing,
                          allowedOAuthScopes = Core.Nothing,
                          analyticsConfiguration = Core.Nothing, callbackURLs = Core.Nothing,
                          defaultRedirectURI = Core.Nothing,
                          explicitAuthFlows = Core.Nothing, generateSecret = Core.Nothing,
                          idTokenValidity = Core.Nothing, logoutURLs = Core.Nothing,
                          preventUserExistenceErrors = Core.Nothing,
                          readAttributes = Core.Nothing, refreshTokenValidity = Core.Nothing,
                          supportedIdentityProviders = Core.Nothing,
                          tokenValidityUnits = Core.Nothing, writeAttributes = Core.Nothing}

-- | The user pool ID for the user pool where you want to create a user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcUserPoolId :: Lens.Lens' CreateUserPoolClient Types.UserPoolId
cupcUserPoolId = Lens.field @"userPoolId"
{-# INLINEABLE cupcUserPoolId #-}
{-# DEPRECATED userPoolId "Use generic-lens or generic-optics with 'userPoolId' instead"  #-}

-- | The client name for the user pool client you would like to create.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcClientName :: Lens.Lens' CreateUserPoolClient Types.ClientNameType
cupcClientName = Lens.field @"clientName"
{-# INLINEABLE cupcClientName #-}
{-# DEPRECATED clientName "Use generic-lens or generic-optics with 'clientName' instead"  #-}

-- | The time limit, between 5 minutes and 1 day, after which the access token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
--
-- /Note:/ Consider using 'accessTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAccessTokenValidity :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Natural)
cupcAccessTokenValidity = Lens.field @"accessTokenValidity"
{-# INLINEABLE cupcAccessTokenValidity #-}
{-# DEPRECATED accessTokenValidity "Use generic-lens or generic-optics with 'accessTokenValidity' instead"  #-}

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- /Note:/ Consider using 'allowedOAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAllowedOAuthFlows :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.OAuthFlowType])
cupcAllowedOAuthFlows = Lens.field @"allowedOAuthFlows"
{-# INLINEABLE cupcAllowedOAuthFlows #-}
{-# DEPRECATED allowedOAuthFlows "Use generic-lens or generic-optics with 'allowedOAuthFlows' instead"  #-}

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- /Note:/ Consider using 'allowedOAuthFlowsUserPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAllowedOAuthFlowsUserPoolClient :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Bool)
cupcAllowedOAuthFlowsUserPoolClient = Lens.field @"allowedOAuthFlowsUserPoolClient"
{-# INLINEABLE cupcAllowedOAuthFlowsUserPoolClient #-}
{-# DEPRECATED allowedOAuthFlowsUserPoolClient "Use generic-lens or generic-optics with 'allowedOAuthFlowsUserPoolClient' instead"  #-}

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- /Note:/ Consider using 'allowedOAuthScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAllowedOAuthScopes :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.ScopeType])
cupcAllowedOAuthScopes = Lens.field @"allowedOAuthScopes"
{-# INLINEABLE cupcAllowedOAuthScopes #-}
{-# DEPRECATED allowedOAuthScopes "Use generic-lens or generic-optics with 'allowedOAuthScopes' instead"  #-}

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAnalyticsConfiguration :: Lens.Lens' CreateUserPoolClient (Core.Maybe Types.AnalyticsConfigurationType)
cupcAnalyticsConfiguration = Lens.field @"analyticsConfiguration"
{-# INLINEABLE cupcAnalyticsConfiguration #-}
{-# DEPRECATED analyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead"  #-}

-- | A list of allowed redirect (callback) URLs for the identity providers.
--
-- A redirect URI must:
--
--     * Be an absolute URI.
--
--
--     * Be registered with the authorization server.
--
--
--     * Not include a fragment component.
--
--
-- See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> .
-- Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only.
-- App callback URLs such as myapp://example are also supported.
--
-- /Note:/ Consider using 'callbackURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcCallbackURLs :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.RedirectUrlType])
cupcCallbackURLs = Lens.field @"callbackURLs"
{-# INLINEABLE cupcCallbackURLs #-}
{-# DEPRECATED callbackURLs "Use generic-lens or generic-optics with 'callbackURLs' instead"  #-}

-- | The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- A redirect URI must:
--
--     * Be an absolute URI.
--
--
--     * Be registered with the authorization server.
--
--
--     * Not include a fragment component.
--
--
-- See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> .
-- Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only.
-- App callback URLs such as myapp://example are also supported.
--
-- /Note:/ Consider using 'defaultRedirectURI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcDefaultRedirectURI :: Lens.Lens' CreateUserPoolClient (Core.Maybe Types.RedirectUrlType)
cupcDefaultRedirectURI = Lens.field @"defaultRedirectURI"
{-# INLINEABLE cupcDefaultRedirectURI #-}
{-# DEPRECATED defaultRedirectURI "Use generic-lens or generic-optics with 'defaultRedirectURI' instead"  #-}

-- | The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix.
--
-- Valid values include:
--
--     * @ALLOW_ADMIN_USER_PASSWORD_AUTH@ : Enable admin based user password authentication flow @ADMIN_USER_PASSWORD_AUTH@ . This setting replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication flow, Cognito receives the password in the request instead of using the SRP (Secure Remote Password protocol) protocol to verify passwords.
--
--
--     * @ALLOW_CUSTOM_AUTH@ : Enable Lambda trigger based authentication.
--
--
--     * @ALLOW_USER_PASSWORD_AUTH@ : Enable user password-based authentication. In this flow, Cognito receives the password in the request instead of using the SRP protocol to verify passwords.
--
--
--     * @ALLOW_USER_SRP_AUTH@ : Enable SRP based authentication.
--
--
--     * @ALLOW_REFRESH_TOKEN_AUTH@ : Enable authflow to refresh tokens.
--
--
--
-- /Note:/ Consider using 'explicitAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcExplicitAuthFlows :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.ExplicitAuthFlowsType])
cupcExplicitAuthFlows = Lens.field @"explicitAuthFlows"
{-# INLINEABLE cupcExplicitAuthFlows #-}
{-# DEPRECATED explicitAuthFlows "Use generic-lens or generic-optics with 'explicitAuthFlows' instead"  #-}

-- | Boolean to specify whether you want to generate a secret for the user pool client being created.
--
-- /Note:/ Consider using 'generateSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcGenerateSecret :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Bool)
cupcGenerateSecret = Lens.field @"generateSecret"
{-# INLINEABLE cupcGenerateSecret #-}
{-# DEPRECATED generateSecret "Use generic-lens or generic-optics with 'generateSecret' instead"  #-}

-- | The time limit, between 5 minutes and 1 day, after which the ID token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
--
-- /Note:/ Consider using 'idTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcIdTokenValidity :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Natural)
cupcIdTokenValidity = Lens.field @"idTokenValidity"
{-# INLINEABLE cupcIdTokenValidity #-}
{-# DEPRECATED idTokenValidity "Use generic-lens or generic-optics with 'idTokenValidity' instead"  #-}

-- | A list of allowed logout URLs for the identity providers.
--
-- /Note:/ Consider using 'logoutURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcLogoutURLs :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.RedirectUrlType])
cupcLogoutURLs = Lens.field @"logoutURLs"
{-# INLINEABLE cupcLogoutURLs #-}
{-# DEPRECATED logoutURLs "Use generic-lens or generic-optics with 'logoutURLs' instead"  #-}

-- | Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool.
--
-- Valid values include:
--
--     * @ENABLED@ - This prevents user existence-related errors.
--
--
--     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
--
--
--
-- /Note:/ Consider using 'preventUserExistenceErrors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcPreventUserExistenceErrors :: Lens.Lens' CreateUserPoolClient (Core.Maybe Types.PreventUserExistenceErrorTypes)
cupcPreventUserExistenceErrors = Lens.field @"preventUserExistenceErrors"
{-# INLINEABLE cupcPreventUserExistenceErrors #-}
{-# DEPRECATED preventUserExistenceErrors "Use generic-lens or generic-optics with 'preventUserExistenceErrors' instead"  #-}

-- | The read attributes.
--
-- /Note:/ Consider using 'readAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcReadAttributes :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.ClientPermissionType])
cupcReadAttributes = Lens.field @"readAttributes"
{-# INLINEABLE cupcReadAttributes #-}
{-# DEPRECATED readAttributes "Use generic-lens or generic-optics with 'readAttributes' instead"  #-}

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'refreshTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcRefreshTokenValidity :: Lens.Lens' CreateUserPoolClient (Core.Maybe Core.Natural)
cupcRefreshTokenValidity = Lens.field @"refreshTokenValidity"
{-# INLINEABLE cupcRefreshTokenValidity #-}
{-# DEPRECATED refreshTokenValidity "Use generic-lens or generic-optics with 'refreshTokenValidity' instead"  #-}

-- | A list of provider names for the identity providers that are supported on this client. The following are supported: @COGNITO@ , @Facebook@ , @Google@ and @LoginWithAmazon@ .
--
-- /Note:/ Consider using 'supportedIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcSupportedIdentityProviders :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.ProviderNameType])
cupcSupportedIdentityProviders = Lens.field @"supportedIdentityProviders"
{-# INLINEABLE cupcSupportedIdentityProviders #-}
{-# DEPRECATED supportedIdentityProviders "Use generic-lens or generic-optics with 'supportedIdentityProviders' instead"  #-}

-- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
--
-- /Note:/ Consider using 'tokenValidityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcTokenValidityUnits :: Lens.Lens' CreateUserPoolClient (Core.Maybe Types.TokenValidityUnitsType)
cupcTokenValidityUnits = Lens.field @"tokenValidityUnits"
{-# INLINEABLE cupcTokenValidityUnits #-}
{-# DEPRECATED tokenValidityUnits "Use generic-lens or generic-optics with 'tokenValidityUnits' instead"  #-}

-- | The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an identity provider, this array must include all attributes that are mapped to identity provider attributes. Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If your app client lacks write access to a mapped attribute, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
--
-- /Note:/ Consider using 'writeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcWriteAttributes :: Lens.Lens' CreateUserPoolClient (Core.Maybe [Types.ClientPermissionType])
cupcWriteAttributes = Lens.field @"writeAttributes"
{-# INLINEABLE cupcWriteAttributes #-}
{-# DEPRECATED writeAttributes "Use generic-lens or generic-optics with 'writeAttributes' instead"  #-}

instance Core.ToQuery CreateUserPoolClient where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateUserPoolClient where
        toHeaders CreateUserPoolClient{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSCognitoIdentityProviderService.CreateUserPoolClient")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateUserPoolClient where
        toJSON CreateUserPoolClient{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("UserPoolId" Core..= userPoolId),
                  Core.Just ("ClientName" Core..= clientName),
                  ("AccessTokenValidity" Core..=) Core.<$> accessTokenValidity,
                  ("AllowedOAuthFlows" Core..=) Core.<$> allowedOAuthFlows,
                  ("AllowedOAuthFlowsUserPoolClient" Core..=) Core.<$>
                    allowedOAuthFlowsUserPoolClient,
                  ("AllowedOAuthScopes" Core..=) Core.<$> allowedOAuthScopes,
                  ("AnalyticsConfiguration" Core..=) Core.<$> analyticsConfiguration,
                  ("CallbackURLs" Core..=) Core.<$> callbackURLs,
                  ("DefaultRedirectURI" Core..=) Core.<$> defaultRedirectURI,
                  ("ExplicitAuthFlows" Core..=) Core.<$> explicitAuthFlows,
                  ("GenerateSecret" Core..=) Core.<$> generateSecret,
                  ("IdTokenValidity" Core..=) Core.<$> idTokenValidity,
                  ("LogoutURLs" Core..=) Core.<$> logoutURLs,
                  ("PreventUserExistenceErrors" Core..=) Core.<$>
                    preventUserExistenceErrors,
                  ("ReadAttributes" Core..=) Core.<$> readAttributes,
                  ("RefreshTokenValidity" Core..=) Core.<$> refreshTokenValidity,
                  ("SupportedIdentityProviders" Core..=) Core.<$>
                    supportedIdentityProviders,
                  ("TokenValidityUnits" Core..=) Core.<$> tokenValidityUnits,
                  ("WriteAttributes" Core..=) Core.<$> writeAttributes])

instance Core.AWSRequest CreateUserPoolClient where
        type Rs CreateUserPoolClient = CreateUserPoolClientResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateUserPoolClientResponse' Core.<$>
                   (x Core..:? "UserPoolClient") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the response from the server to create a user pool client.
--
-- /See:/ 'mkCreateUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
  { userPoolClient :: Core.Maybe Types.UserPoolClientType
    -- ^ The user pool client that was just created.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'CreateUserPoolClientResponse' value with any optional fields omitted.
mkCreateUserPoolClientResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateUserPoolClientResponse
mkCreateUserPoolClientResponse responseStatus
  = CreateUserPoolClientResponse'{userPoolClient = Core.Nothing,
                                  responseStatus}

-- | The user pool client that was just created.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcrrsUserPoolClient :: Lens.Lens' CreateUserPoolClientResponse (Core.Maybe Types.UserPoolClientType)
cupcrrsUserPoolClient = Lens.field @"userPoolClient"
{-# INLINEABLE cupcrrsUserPoolClient #-}
{-# DEPRECATED userPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcrrsResponseStatus :: Lens.Lens' CreateUserPoolClientResponse Core.Int
cupcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cupcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
