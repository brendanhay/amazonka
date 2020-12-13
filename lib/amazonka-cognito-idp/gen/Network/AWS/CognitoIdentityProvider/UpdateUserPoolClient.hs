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
    uupcRefreshTokenValidity,
    uupcClientId,
    uupcExplicitAuthFlows,
    uupcSupportedIdentityProviders,
    uupcLogoutURLs,
    uupcAllowedOAuthFlowsUserPoolClient,
    uupcUserPoolId,
    uupcIdTokenValidity,
    uupcTokenValidityUnits,
    uupcDefaultRedirectURI,
    uupcWriteAttributes,
    uupcPreventUserExistenceErrors,
    uupcAccessTokenValidity,
    uupcReadAttributes,
    uupcAllowedOAuthScopes,
    uupcAllowedOAuthFlows,
    uupcAnalyticsConfiguration,
    uupcClientName,
    uupcCallbackURLs,

    -- * Destructuring the response
    UpdateUserPoolClientResponse (..),
    mkUpdateUserPoolClientResponse,

    -- ** Response lenses
    uupcrsUserPoolClient,
    uupcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to update the user pool client.
--
-- /See:/ 'mkUpdateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { -- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
    refreshTokenValidity :: Lude.Maybe Lude.Natural,
    -- | The ID of the client associated with the user pool.
    clientId :: Lude.Sensitive Lude.Text,
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
    explicitAuthFlows :: Lude.Maybe [ExplicitAuthFlowsType],
    -- | A list of provider names for the identity providers that are supported on this client.
    supportedIdentityProviders :: Lude.Maybe [Lude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Lude.Maybe [Lude.Text],
    -- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Lude.Maybe Lude.Bool,
    -- | The user pool ID for the user pool where you want to update the user pool client.
    userPoolId :: Lude.Text,
    -- | The time limit, after which the ID token is no longer valid and cannot be used.
    idTokenValidity :: Lude.Maybe Lude.Natural,
    -- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
    tokenValidityUnits :: Lude.Maybe TokenValidityUnitsType,
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
    defaultRedirectURI :: Lude.Maybe Lude.Text,
    -- | The writeable attributes of the user pool.
    writeAttributes :: Lude.Maybe [Lude.Text],
    -- | Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool.
    --
    -- Valid values include:
    --
    --     * @ENABLED@ - This prevents user existence-related errors.
    --
    --
    --     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
    preventUserExistenceErrors :: Lude.Maybe PreventUserExistenceErrorTypes,
    -- | The time limit, after which the access token is no longer valid and cannot be used.
    accessTokenValidity :: Lude.Maybe Lude.Natural,
    -- | The read-only attributes of the user pool.
    readAttributes :: Lude.Maybe [Lude.Text],
    -- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
    allowedOAuthScopes :: Lude.Maybe [Lude.Text],
    -- | The allowed OAuth flows.
    --
    -- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
    -- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
    -- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
    allowedOAuthFlows :: Lude.Maybe [OAuthFlowType],
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
    analyticsConfiguration :: Lude.Maybe AnalyticsConfigurationType,
    -- | The client name from the update user pool client request.
    clientName :: Lude.Maybe Lude.Text,
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
    callbackURLs :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPoolClient' with the minimum fields required to make a request.
--
-- * 'refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer valid and cannot be used.
-- * 'clientId' - The ID of the client associated with the user pool.
-- * 'explicitAuthFlows' - The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix.
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
-- * 'supportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client.
-- * 'logoutURLs' - A list of allowed logout URLs for the identity providers.
-- * 'allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
-- * 'userPoolId' - The user pool ID for the user pool where you want to update the user pool client.
-- * 'idTokenValidity' - The time limit, after which the ID token is no longer valid and cannot be used.
-- * 'tokenValidityUnits' - The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
-- * 'defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
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
-- * 'writeAttributes' - The writeable attributes of the user pool.
-- * 'preventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool.
--
-- Valid values include:
--
--     * @ENABLED@ - This prevents user existence-related errors.
--
--
--     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
--
--
-- * 'accessTokenValidity' - The time limit, after which the access token is no longer valid and cannot be used.
-- * 'readAttributes' - The read-only attributes of the user pool.
-- * 'allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
-- * 'allowedOAuthFlows' - The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
-- * 'analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
-- * 'clientName' - The client name from the update user pool client request.
-- * 'callbackURLs' - A list of allowed redirect (callback) URLs for the identity providers.
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
mkUpdateUserPoolClient ::
  -- | 'clientId'
  Lude.Sensitive Lude.Text ->
  -- | 'userPoolId'
  Lude.Text ->
  UpdateUserPoolClient
mkUpdateUserPoolClient pClientId_ pUserPoolId_ =
  UpdateUserPoolClient'
    { refreshTokenValidity = Lude.Nothing,
      clientId = pClientId_,
      explicitAuthFlows = Lude.Nothing,
      supportedIdentityProviders = Lude.Nothing,
      logoutURLs = Lude.Nothing,
      allowedOAuthFlowsUserPoolClient = Lude.Nothing,
      userPoolId = pUserPoolId_,
      idTokenValidity = Lude.Nothing,
      tokenValidityUnits = Lude.Nothing,
      defaultRedirectURI = Lude.Nothing,
      writeAttributes = Lude.Nothing,
      preventUserExistenceErrors = Lude.Nothing,
      accessTokenValidity = Lude.Nothing,
      readAttributes = Lude.Nothing,
      allowedOAuthScopes = Lude.Nothing,
      allowedOAuthFlows = Lude.Nothing,
      analyticsConfiguration = Lude.Nothing,
      clientName = Lude.Nothing,
      callbackURLs = Lude.Nothing
    }

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'refreshTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcRefreshTokenValidity :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe Lude.Natural)
uupcRefreshTokenValidity = Lens.lens (refreshTokenValidity :: UpdateUserPoolClient -> Lude.Maybe Lude.Natural) (\s a -> s {refreshTokenValidity = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcRefreshTokenValidity "Use generic-lens or generic-optics with 'refreshTokenValidity' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcClientId :: Lens.Lens' UpdateUserPoolClient (Lude.Sensitive Lude.Text)
uupcClientId = Lens.lens (clientId :: UpdateUserPoolClient -> Lude.Sensitive Lude.Text) (\s a -> s {clientId = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

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
uupcExplicitAuthFlows :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [ExplicitAuthFlowsType])
uupcExplicitAuthFlows = Lens.lens (explicitAuthFlows :: UpdateUserPoolClient -> Lude.Maybe [ExplicitAuthFlowsType]) (\s a -> s {explicitAuthFlows = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcExplicitAuthFlows "Use generic-lens or generic-optics with 'explicitAuthFlows' instead." #-}

-- | A list of provider names for the identity providers that are supported on this client.
--
-- /Note:/ Consider using 'supportedIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcSupportedIdentityProviders :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [Lude.Text])
uupcSupportedIdentityProviders = Lens.lens (supportedIdentityProviders :: UpdateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedIdentityProviders = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcSupportedIdentityProviders "Use generic-lens or generic-optics with 'supportedIdentityProviders' instead." #-}

-- | A list of allowed logout URLs for the identity providers.
--
-- /Note:/ Consider using 'logoutURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcLogoutURLs :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [Lude.Text])
uupcLogoutURLs = Lens.lens (logoutURLs :: UpdateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {logoutURLs = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcLogoutURLs "Use generic-lens or generic-optics with 'logoutURLs' instead." #-}

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- /Note:/ Consider using 'allowedOAuthFlowsUserPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAllowedOAuthFlowsUserPoolClient :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe Lude.Bool)
uupcAllowedOAuthFlowsUserPoolClient = Lens.lens (allowedOAuthFlowsUserPoolClient :: UpdateUserPoolClient -> Lude.Maybe Lude.Bool) (\s a -> s {allowedOAuthFlowsUserPoolClient = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcAllowedOAuthFlowsUserPoolClient "Use generic-lens or generic-optics with 'allowedOAuthFlowsUserPoolClient' instead." #-}

-- | The user pool ID for the user pool where you want to update the user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcUserPoolId :: Lens.Lens' UpdateUserPoolClient Lude.Text
uupcUserPoolId = Lens.lens (userPoolId :: UpdateUserPoolClient -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The time limit, after which the ID token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'idTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcIdTokenValidity :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe Lude.Natural)
uupcIdTokenValidity = Lens.lens (idTokenValidity :: UpdateUserPoolClient -> Lude.Maybe Lude.Natural) (\s a -> s {idTokenValidity = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcIdTokenValidity "Use generic-lens or generic-optics with 'idTokenValidity' instead." #-}

-- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
--
-- /Note:/ Consider using 'tokenValidityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcTokenValidityUnits :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe TokenValidityUnitsType)
uupcTokenValidityUnits = Lens.lens (tokenValidityUnits :: UpdateUserPoolClient -> Lude.Maybe TokenValidityUnitsType) (\s a -> s {tokenValidityUnits = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcTokenValidityUnits "Use generic-lens or generic-optics with 'tokenValidityUnits' instead." #-}

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
uupcDefaultRedirectURI :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe Lude.Text)
uupcDefaultRedirectURI = Lens.lens (defaultRedirectURI :: UpdateUserPoolClient -> Lude.Maybe Lude.Text) (\s a -> s {defaultRedirectURI = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcDefaultRedirectURI "Use generic-lens or generic-optics with 'defaultRedirectURI' instead." #-}

-- | The writeable attributes of the user pool.
--
-- /Note:/ Consider using 'writeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcWriteAttributes :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [Lude.Text])
uupcWriteAttributes = Lens.lens (writeAttributes :: UpdateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {writeAttributes = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcWriteAttributes "Use generic-lens or generic-optics with 'writeAttributes' instead." #-}

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
uupcPreventUserExistenceErrors :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe PreventUserExistenceErrorTypes)
uupcPreventUserExistenceErrors = Lens.lens (preventUserExistenceErrors :: UpdateUserPoolClient -> Lude.Maybe PreventUserExistenceErrorTypes) (\s a -> s {preventUserExistenceErrors = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcPreventUserExistenceErrors "Use generic-lens or generic-optics with 'preventUserExistenceErrors' instead." #-}

-- | The time limit, after which the access token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'accessTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAccessTokenValidity :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe Lude.Natural)
uupcAccessTokenValidity = Lens.lens (accessTokenValidity :: UpdateUserPoolClient -> Lude.Maybe Lude.Natural) (\s a -> s {accessTokenValidity = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcAccessTokenValidity "Use generic-lens or generic-optics with 'accessTokenValidity' instead." #-}

-- | The read-only attributes of the user pool.
--
-- /Note:/ Consider using 'readAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcReadAttributes :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [Lude.Text])
uupcReadAttributes = Lens.lens (readAttributes :: UpdateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {readAttributes = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcReadAttributes "Use generic-lens or generic-optics with 'readAttributes' instead." #-}

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- /Note:/ Consider using 'allowedOAuthScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAllowedOAuthScopes :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [Lude.Text])
uupcAllowedOAuthScopes = Lens.lens (allowedOAuthScopes :: UpdateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedOAuthScopes = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcAllowedOAuthScopes "Use generic-lens or generic-optics with 'allowedOAuthScopes' instead." #-}

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- /Note:/ Consider using 'allowedOAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAllowedOAuthFlows :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [OAuthFlowType])
uupcAllowedOAuthFlows = Lens.lens (allowedOAuthFlows :: UpdateUserPoolClient -> Lude.Maybe [OAuthFlowType]) (\s a -> s {allowedOAuthFlows = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcAllowedOAuthFlows "Use generic-lens or generic-optics with 'allowedOAuthFlows' instead." #-}

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcAnalyticsConfiguration :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe AnalyticsConfigurationType)
uupcAnalyticsConfiguration = Lens.lens (analyticsConfiguration :: UpdateUserPoolClient -> Lude.Maybe AnalyticsConfigurationType) (\s a -> s {analyticsConfiguration = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

-- | The client name from the update user pool client request.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcClientName :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe Lude.Text)
uupcClientName = Lens.lens (clientName :: UpdateUserPoolClient -> Lude.Maybe Lude.Text) (\s a -> s {clientName = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcClientName "Use generic-lens or generic-optics with 'clientName' instead." #-}

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
uupcCallbackURLs :: Lens.Lens' UpdateUserPoolClient (Lude.Maybe [Lude.Text])
uupcCallbackURLs = Lens.lens (callbackURLs :: UpdateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {callbackURLs = a} :: UpdateUserPoolClient)
{-# DEPRECATED uupcCallbackURLs "Use generic-lens or generic-optics with 'callbackURLs' instead." #-}

instance Lude.AWSRequest UpdateUserPoolClient where
  type Rs UpdateUserPoolClient = UpdateUserPoolClientResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateUserPoolClientResponse'
            Lude.<$> (x Lude..?> "UserPoolClient")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUserPoolClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolClient" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserPoolClient where
  toJSON UpdateUserPoolClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RefreshTokenValidity" Lude..=) Lude.<$> refreshTokenValidity,
            Lude.Just ("ClientId" Lude..= clientId),
            ("ExplicitAuthFlows" Lude..=) Lude.<$> explicitAuthFlows,
            ("SupportedIdentityProviders" Lude..=)
              Lude.<$> supportedIdentityProviders,
            ("LogoutURLs" Lude..=) Lude.<$> logoutURLs,
            ("AllowedOAuthFlowsUserPoolClient" Lude..=)
              Lude.<$> allowedOAuthFlowsUserPoolClient,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            ("IdTokenValidity" Lude..=) Lude.<$> idTokenValidity,
            ("TokenValidityUnits" Lude..=) Lude.<$> tokenValidityUnits,
            ("DefaultRedirectURI" Lude..=) Lude.<$> defaultRedirectURI,
            ("WriteAttributes" Lude..=) Lude.<$> writeAttributes,
            ("PreventUserExistenceErrors" Lude..=)
              Lude.<$> preventUserExistenceErrors,
            ("AccessTokenValidity" Lude..=) Lude.<$> accessTokenValidity,
            ("ReadAttributes" Lude..=) Lude.<$> readAttributes,
            ("AllowedOAuthScopes" Lude..=) Lude.<$> allowedOAuthScopes,
            ("AllowedOAuthFlows" Lude..=) Lude.<$> allowedOAuthFlows,
            ("AnalyticsConfiguration" Lude..=) Lude.<$> analyticsConfiguration,
            ("ClientName" Lude..=) Lude.<$> clientName,
            ("CallbackURLs" Lude..=) Lude.<$> callbackURLs
          ]
      )

instance Lude.ToPath UpdateUserPoolClient where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserPoolClient where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to the request to update the user pool client.
--
-- /See:/ 'mkUpdateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { -- | The user pool client value from the response from the server when an update user pool client request is made.
    userPoolClient :: Lude.Maybe UserPoolClientType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- * 'userPoolClient' - The user pool client value from the response from the server when an update user pool client request is made.
-- * 'responseStatus' - The response status code.
mkUpdateUserPoolClientResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserPoolClientResponse
mkUpdateUserPoolClientResponse pResponseStatus_ =
  UpdateUserPoolClientResponse'
    { userPoolClient = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user pool client value from the response from the server when an update user pool client request is made.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcrsUserPoolClient :: Lens.Lens' UpdateUserPoolClientResponse (Lude.Maybe UserPoolClientType)
uupcrsUserPoolClient = Lens.lens (userPoolClient :: UpdateUserPoolClientResponse -> Lude.Maybe UserPoolClientType) (\s a -> s {userPoolClient = a} :: UpdateUserPoolClientResponse)
{-# DEPRECATED uupcrsUserPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupcrsResponseStatus :: Lens.Lens' UpdateUserPoolClientResponse Lude.Int
uupcrsResponseStatus = Lens.lens (responseStatus :: UpdateUserPoolClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserPoolClientResponse)
{-# DEPRECATED uupcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
