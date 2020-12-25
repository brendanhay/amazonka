{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
  ( UserPoolClientType (..),

    -- * Smart constructor
    mkUserPoolClientType,

    -- * Lenses
    upctAccessTokenValidity,
    upctAllowedOAuthFlows,
    upctAllowedOAuthFlowsUserPoolClient,
    upctAllowedOAuthScopes,
    upctAnalyticsConfiguration,
    upctCallbackURLs,
    upctClientId,
    upctClientName,
    upctClientSecret,
    upctCreationDate,
    upctDefaultRedirectURI,
    upctExplicitAuthFlows,
    upctIdTokenValidity,
    upctLastModifiedDate,
    upctLogoutURLs,
    upctPreventUserExistenceErrors,
    upctReadAttributes,
    upctRefreshTokenValidity,
    upctSupportedIdentityProviders,
    upctTokenValidityUnits,
    upctUserPoolId,
    upctWriteAttributes,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientId as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientPermissionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ClientSecret as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ProviderNameType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.RedirectUrlType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ScopeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a user pool client.
--
-- /See:/ 'mkUserPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
  { -- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
    accessTokenValidity :: Core.Maybe Core.Natural,
    -- | The allowed OAuth flows.
    --
    -- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
    -- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
    -- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
    allowedOAuthFlows :: Core.Maybe [Types.OAuthFlowType],
    -- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Core.Maybe Core.Bool,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
    allowedOAuthScopes :: Core.Maybe [Types.ScopeType],
    -- | The Amazon Pinpoint analytics configuration for the user pool client.
    analyticsConfiguration :: Core.Maybe Types.AnalyticsConfigurationType,
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
    callbackURLs :: Core.Maybe [Types.RedirectUrlType],
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Maybe Types.ClientId,
    -- | The client name from the user pool request of the client type.
    clientName :: Core.Maybe Types.ClientNameType,
    -- | The client secret from the user pool request of the client type.
    clientSecret :: Core.Maybe Types.ClientSecret,
    -- | The date the user pool client was created.
    creationDate :: Core.Maybe Core.NominalDiffTime,
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
    defaultRedirectURI :: Core.Maybe Types.RedirectUrlType,
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
    explicitAuthFlows :: Core.Maybe [Types.ExplicitAuthFlowsType],
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
    idTokenValidity :: Core.Maybe Core.Natural,
    -- | The date the user pool client was last modified.
    lastModifiedDate :: Core.Maybe Core.NominalDiffTime,
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Core.Maybe [Types.RedirectUrlType],
    -- | Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool.
    --
    -- Valid values include:
    --
    --     * @ENABLED@ - This prevents user existence-related errors.
    --
    --
    --     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
    preventUserExistenceErrors :: Core.Maybe Types.PreventUserExistenceErrorTypes,
    -- | The Read-only attributes.
    readAttributes :: Core.Maybe [Types.ClientPermissionType],
    -- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
    refreshTokenValidity :: Core.Maybe Core.Natural,
    -- | A list of provider names for the identity providers that are supported on this client.
    supportedIdentityProviders :: Core.Maybe [Types.ProviderNameType],
    -- | The time units used to specify the token validity times of their respective token.
    tokenValidityUnits :: Core.Maybe Types.TokenValidityUnitsType,
    -- | The user pool ID for the user pool client.
    userPoolId :: Core.Maybe Types.UserPoolId,
    -- | The writeable attributes.
    writeAttributes :: Core.Maybe [Types.ClientPermissionType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UserPoolClientType' value with any optional fields omitted.
mkUserPoolClientType ::
  UserPoolClientType
mkUserPoolClientType =
  UserPoolClientType'
    { accessTokenValidity = Core.Nothing,
      allowedOAuthFlows = Core.Nothing,
      allowedOAuthFlowsUserPoolClient = Core.Nothing,
      allowedOAuthScopes = Core.Nothing,
      analyticsConfiguration = Core.Nothing,
      callbackURLs = Core.Nothing,
      clientId = Core.Nothing,
      clientName = Core.Nothing,
      clientSecret = Core.Nothing,
      creationDate = Core.Nothing,
      defaultRedirectURI = Core.Nothing,
      explicitAuthFlows = Core.Nothing,
      idTokenValidity = Core.Nothing,
      lastModifiedDate = Core.Nothing,
      logoutURLs = Core.Nothing,
      preventUserExistenceErrors = Core.Nothing,
      readAttributes = Core.Nothing,
      refreshTokenValidity = Core.Nothing,
      supportedIdentityProviders = Core.Nothing,
      tokenValidityUnits = Core.Nothing,
      userPoolId = Core.Nothing,
      writeAttributes = Core.Nothing
    }

-- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'accessTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAccessTokenValidity :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Natural)
upctAccessTokenValidity = Lens.field @"accessTokenValidity"
{-# DEPRECATED upctAccessTokenValidity "Use generic-lens or generic-optics with 'accessTokenValidity' instead." #-}

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- /Note:/ Consider using 'allowedOAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAllowedOAuthFlows :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.OAuthFlowType])
upctAllowedOAuthFlows = Lens.field @"allowedOAuthFlows"
{-# DEPRECATED upctAllowedOAuthFlows "Use generic-lens or generic-optics with 'allowedOAuthFlows' instead." #-}

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- /Note:/ Consider using 'allowedOAuthFlowsUserPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAllowedOAuthFlowsUserPoolClient :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Bool)
upctAllowedOAuthFlowsUserPoolClient = Lens.field @"allowedOAuthFlowsUserPoolClient"
{-# DEPRECATED upctAllowedOAuthFlowsUserPoolClient "Use generic-lens or generic-optics with 'allowedOAuthFlowsUserPoolClient' instead." #-}

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- /Note:/ Consider using 'allowedOAuthScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAllowedOAuthScopes :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.ScopeType])
upctAllowedOAuthScopes = Lens.field @"allowedOAuthScopes"
{-# DEPRECATED upctAllowedOAuthScopes "Use generic-lens or generic-optics with 'allowedOAuthScopes' instead." #-}

-- | The Amazon Pinpoint analytics configuration for the user pool client.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAnalyticsConfiguration :: Lens.Lens' UserPoolClientType (Core.Maybe Types.AnalyticsConfigurationType)
upctAnalyticsConfiguration = Lens.field @"analyticsConfiguration"
{-# DEPRECATED upctAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

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
upctCallbackURLs :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.RedirectUrlType])
upctCallbackURLs = Lens.field @"callbackURLs"
{-# DEPRECATED upctCallbackURLs "Use generic-lens or generic-optics with 'callbackURLs' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctClientId :: Lens.Lens' UserPoolClientType (Core.Maybe Types.ClientId)
upctClientId = Lens.field @"clientId"
{-# DEPRECATED upctClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The client name from the user pool request of the client type.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctClientName :: Lens.Lens' UserPoolClientType (Core.Maybe Types.ClientNameType)
upctClientName = Lens.field @"clientName"
{-# DEPRECATED upctClientName "Use generic-lens or generic-optics with 'clientName' instead." #-}

-- | The client secret from the user pool request of the client type.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctClientSecret :: Lens.Lens' UserPoolClientType (Core.Maybe Types.ClientSecret)
upctClientSecret = Lens.field @"clientSecret"
{-# DEPRECATED upctClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The date the user pool client was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctCreationDate :: Lens.Lens' UserPoolClientType (Core.Maybe Core.NominalDiffTime)
upctCreationDate = Lens.field @"creationDate"
{-# DEPRECATED upctCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

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
upctDefaultRedirectURI :: Lens.Lens' UserPoolClientType (Core.Maybe Types.RedirectUrlType)
upctDefaultRedirectURI = Lens.field @"defaultRedirectURI"
{-# DEPRECATED upctDefaultRedirectURI "Use generic-lens or generic-optics with 'defaultRedirectURI' instead." #-}

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
upctExplicitAuthFlows :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.ExplicitAuthFlowsType])
upctExplicitAuthFlows = Lens.field @"explicitAuthFlows"
{-# DEPRECATED upctExplicitAuthFlows "Use generic-lens or generic-optics with 'explicitAuthFlows' instead." #-}

-- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'idTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctIdTokenValidity :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Natural)
upctIdTokenValidity = Lens.field @"idTokenValidity"
{-# DEPRECATED upctIdTokenValidity "Use generic-lens or generic-optics with 'idTokenValidity' instead." #-}

-- | The date the user pool client was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctLastModifiedDate :: Lens.Lens' UserPoolClientType (Core.Maybe Core.NominalDiffTime)
upctLastModifiedDate = Lens.field @"lastModifiedDate"
{-# DEPRECATED upctLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A list of allowed logout URLs for the identity providers.
--
-- /Note:/ Consider using 'logoutURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctLogoutURLs :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.RedirectUrlType])
upctLogoutURLs = Lens.field @"logoutURLs"
{-# DEPRECATED upctLogoutURLs "Use generic-lens or generic-optics with 'logoutURLs' instead." #-}

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
upctPreventUserExistenceErrors :: Lens.Lens' UserPoolClientType (Core.Maybe Types.PreventUserExistenceErrorTypes)
upctPreventUserExistenceErrors = Lens.field @"preventUserExistenceErrors"
{-# DEPRECATED upctPreventUserExistenceErrors "Use generic-lens or generic-optics with 'preventUserExistenceErrors' instead." #-}

-- | The Read-only attributes.
--
-- /Note:/ Consider using 'readAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctReadAttributes :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.ClientPermissionType])
upctReadAttributes = Lens.field @"readAttributes"
{-# DEPRECATED upctReadAttributes "Use generic-lens or generic-optics with 'readAttributes' instead." #-}

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'refreshTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctRefreshTokenValidity :: Lens.Lens' UserPoolClientType (Core.Maybe Core.Natural)
upctRefreshTokenValidity = Lens.field @"refreshTokenValidity"
{-# DEPRECATED upctRefreshTokenValidity "Use generic-lens or generic-optics with 'refreshTokenValidity' instead." #-}

-- | A list of provider names for the identity providers that are supported on this client.
--
-- /Note:/ Consider using 'supportedIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctSupportedIdentityProviders :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.ProviderNameType])
upctSupportedIdentityProviders = Lens.field @"supportedIdentityProviders"
{-# DEPRECATED upctSupportedIdentityProviders "Use generic-lens or generic-optics with 'supportedIdentityProviders' instead." #-}

-- | The time units used to specify the token validity times of their respective token.
--
-- /Note:/ Consider using 'tokenValidityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctTokenValidityUnits :: Lens.Lens' UserPoolClientType (Core.Maybe Types.TokenValidityUnitsType)
upctTokenValidityUnits = Lens.field @"tokenValidityUnits"
{-# DEPRECATED upctTokenValidityUnits "Use generic-lens or generic-optics with 'tokenValidityUnits' instead." #-}

-- | The user pool ID for the user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctUserPoolId :: Lens.Lens' UserPoolClientType (Core.Maybe Types.UserPoolId)
upctUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED upctUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The writeable attributes.
--
-- /Note:/ Consider using 'writeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctWriteAttributes :: Lens.Lens' UserPoolClientType (Core.Maybe [Types.ClientPermissionType])
upctWriteAttributes = Lens.field @"writeAttributes"
{-# DEPRECATED upctWriteAttributes "Use generic-lens or generic-optics with 'writeAttributes' instead." #-}

instance Core.FromJSON UserPoolClientType where
  parseJSON =
    Core.withObject "UserPoolClientType" Core.$
      \x ->
        UserPoolClientType'
          Core.<$> (x Core..:? "AccessTokenValidity")
          Core.<*> (x Core..:? "AllowedOAuthFlows")
          Core.<*> (x Core..:? "AllowedOAuthFlowsUserPoolClient")
          Core.<*> (x Core..:? "AllowedOAuthScopes")
          Core.<*> (x Core..:? "AnalyticsConfiguration")
          Core.<*> (x Core..:? "CallbackURLs")
          Core.<*> (x Core..:? "ClientId")
          Core.<*> (x Core..:? "ClientName")
          Core.<*> (x Core..:? "ClientSecret")
          Core.<*> (x Core..:? "CreationDate")
          Core.<*> (x Core..:? "DefaultRedirectURI")
          Core.<*> (x Core..:? "ExplicitAuthFlows")
          Core.<*> (x Core..:? "IdTokenValidity")
          Core.<*> (x Core..:? "LastModifiedDate")
          Core.<*> (x Core..:? "LogoutURLs")
          Core.<*> (x Core..:? "PreventUserExistenceErrors")
          Core.<*> (x Core..:? "ReadAttributes")
          Core.<*> (x Core..:? "RefreshTokenValidity")
          Core.<*> (x Core..:? "SupportedIdentityProviders")
          Core.<*> (x Core..:? "TokenValidityUnits")
          Core.<*> (x Core..:? "UserPoolId")
          Core.<*> (x Core..:? "WriteAttributes")
