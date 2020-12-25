{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user pool app client with the specified attributes. You can get a list of the current user pool app client settings using <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_DescribeUserPoolClient.html DescribeUserPoolClient> .
--
-- /Important:/ If you don't provide a value for an attribute, it will be set to the default value.
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
  ( -- * Creating a request
    UpdateUserPoolClient (..),
    mkUpdateUserPoolClient,

    -- ** Request lenses
    uupcUserPoolId,
    uupcClientId,
    uupcAccessTokenValidity,
    uupcAllowedOAuthFlows,
    uupcAllowedOAuthFlowsUserPoolClient,
    uupcAllowedOAuthScopes,
    uupcAnalyticsConfiguration,
    uupcCallbackURLs,
    uupcClientName,
    uupcDefaultRedirectURI,
    uupcExplicitAuthFlows,
    uupcIdTokenValidity,
    uupcLogoutURLs,
    uupcPreventUserExistenceErrors,
    uupcReadAttributes,
    uupcRefreshTokenValidity,
    uupcSupportedIdentityProviders,
    uupcTokenValidityUnits,
    uupcWriteAttributes,

    -- * Destructuring the response
    UpdateUserPoolClientResponse (..),
    mkUpdateUserPoolClientResponse,

    -- ** Response lenses
    uupcrrsUserPoolClient,
    uupcrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user pool client.
--
-- /See:/ 'mkUpdateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { -- | The user pool ID for the user pool where you want to update the user pool client.
    userPoolId :: Types.UserPoolId,
    -- | The ID of the client associated with the user pool.
    clientId :: Types.ClientIdType,
    -- | The time limit, after which the access token is no longer valid and cannot be used.
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
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
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
    -- | The client name from the update user pool client request.
    clientName :: Core.Maybe Types.ClientNameType,
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
    -- | The time limit, after which the ID token is no longer valid and cannot be used.
    idTokenValidity :: Core.Maybe Core.Natural,
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
    -- | The read-only attributes of the user pool.
    readAttributes :: Core.Maybe [Types.ClientPermissionType],
    -- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
    refreshTokenValidity :: Core.Maybe Core.Natural,
    -- | A list of provider names for the identity providers that are supported on this client.
    supportedIdentityProviders :: Core.Maybe [Types.ProviderNameType],
    -- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
    tokenValidityUnits :: Core.Maybe Types.TokenValidityUnitsType,
    -- | The writeable attributes of the user pool.
    writeAttributes :: Core.Maybe [Types.ClientPermissionType]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserPoolClient' value with any optional fields omitted.
mkUpdateUserPoolClient ::
  -- | 'userPoolId'
  Types.UserPoolId ->
  -- | 'clientId'
  Types.ClientIdType ->
  UpdateUserPoolClient
mkUpdateUserPoolClient userPoolId clientId =
  UpdateUserPoolClient'
    { userPoolId,
      clientId,
      accessTokenValidity = Core.Nothing,
      allowedOAuthFlows = Core.Nothing,
      allowedOAuthFlowsUserPoolClient = Core.Nothing,
      allowedOAuthScopes = Core.Nothing,
      analyticsConfiguration = Core.Nothing,
      callbackURLs = Core.Nothing,
      clientName = Core.Nothing,
      defaultRedirectURI = Core.Nothing,
      explicitAuthFlows = Core.Nothing,
      idTokenValidity = Core.Nothing,
      logoutURLs = Core.Nothing,
      preventUserExistenceErrors = Core.Nothing,
      readAttributes = Core.Nothing,
      refreshTokenValidity = Core.Nothing,
      supportedIdentityProviders = Core.Nothing,
      tokenValidityUnits = Core.Nothing,
      writeAttributes = Core.Nothing
    }

-- | The user pool ID for the user pool where you want to update the user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcUserPoolId :: Lens.Lens' UpdateUserPoolClient Types.UserPoolId
uupcUserPoolId = Lens.field @"userPoolId"
{-# DEPRECATED uupcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcClientId :: Lens.Lens' UpdateUserPoolClient Types.ClientIdType
uupcClientId = Lens.field @"clientId"
{-# DEPRECATED uupcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

-- | The time limit, after which the access token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'accessTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAccessTokenValidity :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Natural)
uupcAccessTokenValidity = Lens.field @"accessTokenValidity"
{-# DEPRECATED uupcAccessTokenValidity "Use generic-lens or generic-optics with 'accessTokenValidity' instead." #-}

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- /Note:/ Consider using 'allowedOAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAllowedOAuthFlows :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.OAuthFlowType])
uupcAllowedOAuthFlows = Lens.field @"allowedOAuthFlows"
{-# DEPRECATED uupcAllowedOAuthFlows "Use generic-lens or generic-optics with 'allowedOAuthFlows' instead." #-}

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- /Note:/ Consider using 'allowedOAuthFlowsUserPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAllowedOAuthFlowsUserPoolClient :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Bool)
uupcAllowedOAuthFlowsUserPoolClient = Lens.field @"allowedOAuthFlowsUserPoolClient"
{-# DEPRECATED uupcAllowedOAuthFlowsUserPoolClient "Use generic-lens or generic-optics with 'allowedOAuthFlowsUserPoolClient' instead." #-}

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- /Note:/ Consider using 'allowedOAuthScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAllowedOAuthScopes :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.ScopeType])
uupcAllowedOAuthScopes = Lens.field @"allowedOAuthScopes"
{-# DEPRECATED uupcAllowedOAuthScopes "Use generic-lens or generic-optics with 'allowedOAuthScopes' instead." #-}

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAnalyticsConfiguration :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Types.AnalyticsConfigurationType)
uupcAnalyticsConfiguration = Lens.field @"analyticsConfiguration"
{-# DEPRECATED uupcAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

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
uupcCallbackURLs :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.RedirectUrlType])
uupcCallbackURLs = Lens.field @"callbackURLs"
{-# DEPRECATED uupcCallbackURLs "Use generic-lens or generic-optics with 'callbackURLs' instead." #-}

-- | The client name from the update user pool client request.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcClientName :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Types.ClientNameType)
uupcClientName = Lens.field @"clientName"
{-# DEPRECATED uupcClientName "Use generic-lens or generic-optics with 'clientName' instead." #-}

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
uupcDefaultRedirectURI :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Types.RedirectUrlType)
uupcDefaultRedirectURI = Lens.field @"defaultRedirectURI"
{-# DEPRECATED uupcDefaultRedirectURI "Use generic-lens or generic-optics with 'defaultRedirectURI' instead." #-}

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
uupcExplicitAuthFlows :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.ExplicitAuthFlowsType])
uupcExplicitAuthFlows = Lens.field @"explicitAuthFlows"
{-# DEPRECATED uupcExplicitAuthFlows "Use generic-lens or generic-optics with 'explicitAuthFlows' instead." #-}

-- | The time limit, after which the ID token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'idTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcIdTokenValidity :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Natural)
uupcIdTokenValidity = Lens.field @"idTokenValidity"
{-# DEPRECATED uupcIdTokenValidity "Use generic-lens or generic-optics with 'idTokenValidity' instead." #-}

-- | A list of allowed logout URLs for the identity providers.
--
-- /Note:/ Consider using 'logoutURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcLogoutURLs :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.RedirectUrlType])
uupcLogoutURLs = Lens.field @"logoutURLs"
{-# DEPRECATED uupcLogoutURLs "Use generic-lens or generic-optics with 'logoutURLs' instead." #-}

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
uupcPreventUserExistenceErrors :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Types.PreventUserExistenceErrorTypes)
uupcPreventUserExistenceErrors = Lens.field @"preventUserExistenceErrors"
{-# DEPRECATED uupcPreventUserExistenceErrors "Use generic-lens or generic-optics with 'preventUserExistenceErrors' instead." #-}

-- | The read-only attributes of the user pool.
--
-- /Note:/ Consider using 'readAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcReadAttributes :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.ClientPermissionType])
uupcReadAttributes = Lens.field @"readAttributes"
{-# DEPRECATED uupcReadAttributes "Use generic-lens or generic-optics with 'readAttributes' instead." #-}

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'refreshTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcRefreshTokenValidity :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Natural)
uupcRefreshTokenValidity = Lens.field @"refreshTokenValidity"
{-# DEPRECATED uupcRefreshTokenValidity "Use generic-lens or generic-optics with 'refreshTokenValidity' instead." #-}

-- | A list of provider names for the identity providers that are supported on this client.
--
-- /Note:/ Consider using 'supportedIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcSupportedIdentityProviders :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.ProviderNameType])
uupcSupportedIdentityProviders = Lens.field @"supportedIdentityProviders"
{-# DEPRECATED uupcSupportedIdentityProviders "Use generic-lens or generic-optics with 'supportedIdentityProviders' instead." #-}

-- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
--
-- /Note:/ Consider using 'tokenValidityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcTokenValidityUnits :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Types.TokenValidityUnitsType)
uupcTokenValidityUnits = Lens.field @"tokenValidityUnits"
{-# DEPRECATED uupcTokenValidityUnits "Use generic-lens or generic-optics with 'tokenValidityUnits' instead." #-}

-- | The writeable attributes of the user pool.
--
-- /Note:/ Consider using 'writeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcWriteAttributes :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Types.ClientPermissionType])
uupcWriteAttributes = Lens.field @"writeAttributes"
{-# DEPRECATED uupcWriteAttributes "Use generic-lens or generic-optics with 'writeAttributes' instead." #-}

instance Core.FromJSON UpdateUserPoolClient where
  toJSON UpdateUserPoolClient {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientId" Core..= clientId),
            ("AccessTokenValidity" Core..=) Core.<$> accessTokenValidity,
            ("AllowedOAuthFlows" Core..=) Core.<$> allowedOAuthFlows,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Core.<$> allowedOAuthFlowsUserPoolClient,
            ("AllowedOAuthScopes" Core..=) Core.<$> allowedOAuthScopes,
            ("AnalyticsConfiguration" Core..=) Core.<$> analyticsConfiguration,
            ("CallbackURLs" Core..=) Core.<$> callbackURLs,
            ("ClientName" Core..=) Core.<$> clientName,
            ("DefaultRedirectURI" Core..=) Core.<$> defaultRedirectURI,
            ("ExplicitAuthFlows" Core..=) Core.<$> explicitAuthFlows,
            ("IdTokenValidity" Core..=) Core.<$> idTokenValidity,
            ("LogoutURLs" Core..=) Core.<$> logoutURLs,
            ("PreventUserExistenceErrors" Core..=)
              Core.<$> preventUserExistenceErrors,
            ("ReadAttributes" Core..=) Core.<$> readAttributes,
            ("RefreshTokenValidity" Core..=) Core.<$> refreshTokenValidity,
            ("SupportedIdentityProviders" Core..=)
              Core.<$> supportedIdentityProviders,
            ("TokenValidityUnits" Core..=) Core.<$> tokenValidityUnits,
            ("WriteAttributes" Core..=) Core.<$> writeAttributes
          ]
      )

instance Core.AWSRequest UpdateUserPoolClient where
  type Rs UpdateUserPoolClient = UpdateUserPoolClientResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.UpdateUserPoolClient"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserPoolClientResponse'
            Core.<$> (x Core..:? "UserPoolClient")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server to the request to update the user pool client.
--
-- /See:/ 'mkUpdateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { -- | The user pool client value from the response from the server when an update user pool client request is made.
    userPoolClient :: Core.Maybe Types.UserPoolClientType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'UpdateUserPoolClientResponse' value with any optional fields omitted.
mkUpdateUserPoolClientResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateUserPoolClientResponse
mkUpdateUserPoolClientResponse responseStatus =
  UpdateUserPoolClientResponse'
    { userPoolClient = Core.Nothing,
      responseStatus
    }

-- | The user pool client value from the response from the server when an update user pool client request is made.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcrrsUserPoolClient :: Lens.Lens' UpdateUserPoolClientResponse (Core.Maybe Types.UserPoolClientType)
uupcrrsUserPoolClient = Lens.field @"userPoolClient"
{-# DEPRECATED uupcrrsUserPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcrrsResponseStatus :: Lens.Lens' UpdateUserPoolClientResponse Core.Int
uupcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uupcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
