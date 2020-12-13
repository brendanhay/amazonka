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
    upctRefreshTokenValidity,
    upctClientId,
    upctExplicitAuthFlows,
    upctClientSecret,
    upctLastModifiedDate,
    upctSupportedIdentityProviders,
    upctLogoutURLs,
    upctAllowedOAuthFlowsUserPoolClient,
    upctUserPoolId,
    upctIdTokenValidity,
    upctTokenValidityUnits,
    upctDefaultRedirectURI,
    upctWriteAttributes,
    upctPreventUserExistenceErrors,
    upctAccessTokenValidity,
    upctCreationDate,
    upctReadAttributes,
    upctAllowedOAuthScopes,
    upctAllowedOAuthFlows,
    upctAnalyticsConfiguration,
    upctClientName,
    upctCallbackURLs,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a user pool client.
--
-- /See:/ 'mkUserPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
  { -- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
    refreshTokenValidity :: Lude.Maybe Lude.Natural,
    -- | The ID of the client associated with the user pool.
    clientId :: Lude.Maybe (Lude.Sensitive Lude.Text),
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
    -- | The client secret from the user pool request of the client type.
    clientSecret :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The date the user pool client was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | A list of provider names for the identity providers that are supported on this client.
    supportedIdentityProviders :: Lude.Maybe [Lude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Lude.Maybe [Lude.Text],
    -- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Lude.Maybe Lude.Bool,
    -- | The user pool ID for the user pool client.
    userPoolId :: Lude.Maybe Lude.Text,
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
    idTokenValidity :: Lude.Maybe Lude.Natural,
    -- | The time units used to specify the token validity times of their respective token.
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
    -- | The writeable attributes.
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
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
    accessTokenValidity :: Lude.Maybe Lude.Natural,
    -- | The date the user pool client was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The Read-only attributes.
    readAttributes :: Lude.Maybe [Lude.Text],
    -- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
    allowedOAuthScopes :: Lude.Maybe [Lude.Text],
    -- | The allowed OAuth flows.
    --
    -- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
    -- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
    -- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
    allowedOAuthFlows :: Lude.Maybe [OAuthFlowType],
    -- | The Amazon Pinpoint analytics configuration for the user pool client.
    analyticsConfiguration :: Lude.Maybe AnalyticsConfigurationType,
    -- | The client name from the user pool request of the client type.
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

-- | Creates a value of 'UserPoolClientType' with the minimum fields required to make a request.
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
-- * 'clientSecret' - The client secret from the user pool request of the client type.
-- * 'lastModifiedDate' - The date the user pool client was last modified.
-- * 'supportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client.
-- * 'logoutURLs' - A list of allowed logout URLs for the identity providers.
-- * 'allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
-- * 'userPoolId' - The user pool ID for the user pool client.
-- * 'idTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
-- * 'tokenValidityUnits' - The time units used to specify the token validity times of their respective token.
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
-- * 'writeAttributes' - The writeable attributes.
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
-- * 'accessTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
-- * 'creationDate' - The date the user pool client was created.
-- * 'readAttributes' - The Read-only attributes.
-- * 'allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
-- * 'allowedOAuthFlows' - The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
-- * 'analyticsConfiguration' - The Amazon Pinpoint analytics configuration for the user pool client.
-- * 'clientName' - The client name from the user pool request of the client type.
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
mkUserPoolClientType ::
  UserPoolClientType
mkUserPoolClientType =
  UserPoolClientType'
    { refreshTokenValidity = Lude.Nothing,
      clientId = Lude.Nothing,
      explicitAuthFlows = Lude.Nothing,
      clientSecret = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      supportedIdentityProviders = Lude.Nothing,
      logoutURLs = Lude.Nothing,
      allowedOAuthFlowsUserPoolClient = Lude.Nothing,
      userPoolId = Lude.Nothing,
      idTokenValidity = Lude.Nothing,
      tokenValidityUnits = Lude.Nothing,
      defaultRedirectURI = Lude.Nothing,
      writeAttributes = Lude.Nothing,
      preventUserExistenceErrors = Lude.Nothing,
      accessTokenValidity = Lude.Nothing,
      creationDate = Lude.Nothing,
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
upctRefreshTokenValidity :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Natural)
upctRefreshTokenValidity = Lens.lens (refreshTokenValidity :: UserPoolClientType -> Lude.Maybe Lude.Natural) (\s a -> s {refreshTokenValidity = a} :: UserPoolClientType)
{-# DEPRECATED upctRefreshTokenValidity "Use generic-lens or generic-optics with 'refreshTokenValidity' instead." #-}

-- | The ID of the client associated with the user pool.
--
-- /Note:/ Consider using 'clientId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctClientId :: Lens.Lens' UserPoolClientType (Lude.Maybe (Lude.Sensitive Lude.Text))
upctClientId = Lens.lens (clientId :: UserPoolClientType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientId = a} :: UserPoolClientType)
{-# DEPRECATED upctClientId "Use generic-lens or generic-optics with 'clientId' instead." #-}

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
upctExplicitAuthFlows :: Lens.Lens' UserPoolClientType (Lude.Maybe [ExplicitAuthFlowsType])
upctExplicitAuthFlows = Lens.lens (explicitAuthFlows :: UserPoolClientType -> Lude.Maybe [ExplicitAuthFlowsType]) (\s a -> s {explicitAuthFlows = a} :: UserPoolClientType)
{-# DEPRECATED upctExplicitAuthFlows "Use generic-lens or generic-optics with 'explicitAuthFlows' instead." #-}

-- | The client secret from the user pool request of the client type.
--
-- /Note:/ Consider using 'clientSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctClientSecret :: Lens.Lens' UserPoolClientType (Lude.Maybe (Lude.Sensitive Lude.Text))
upctClientSecret = Lens.lens (clientSecret :: UserPoolClientType -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {clientSecret = a} :: UserPoolClientType)
{-# DEPRECATED upctClientSecret "Use generic-lens or generic-optics with 'clientSecret' instead." #-}

-- | The date the user pool client was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctLastModifiedDate :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Timestamp)
upctLastModifiedDate = Lens.lens (lastModifiedDate :: UserPoolClientType -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: UserPoolClientType)
{-# DEPRECATED upctLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | A list of provider names for the identity providers that are supported on this client.
--
-- /Note:/ Consider using 'supportedIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctSupportedIdentityProviders :: Lens.Lens' UserPoolClientType (Lude.Maybe [Lude.Text])
upctSupportedIdentityProviders = Lens.lens (supportedIdentityProviders :: UserPoolClientType -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedIdentityProviders = a} :: UserPoolClientType)
{-# DEPRECATED upctSupportedIdentityProviders "Use generic-lens or generic-optics with 'supportedIdentityProviders' instead." #-}

-- | A list of allowed logout URLs for the identity providers.
--
-- /Note:/ Consider using 'logoutURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctLogoutURLs :: Lens.Lens' UserPoolClientType (Lude.Maybe [Lude.Text])
upctLogoutURLs = Lens.lens (logoutURLs :: UserPoolClientType -> Lude.Maybe [Lude.Text]) (\s a -> s {logoutURLs = a} :: UserPoolClientType)
{-# DEPRECATED upctLogoutURLs "Use generic-lens or generic-optics with 'logoutURLs' instead." #-}

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- /Note:/ Consider using 'allowedOAuthFlowsUserPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAllowedOAuthFlowsUserPoolClient :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Bool)
upctAllowedOAuthFlowsUserPoolClient = Lens.lens (allowedOAuthFlowsUserPoolClient :: UserPoolClientType -> Lude.Maybe Lude.Bool) (\s a -> s {allowedOAuthFlowsUserPoolClient = a} :: UserPoolClientType)
{-# DEPRECATED upctAllowedOAuthFlowsUserPoolClient "Use generic-lens or generic-optics with 'allowedOAuthFlowsUserPoolClient' instead." #-}

-- | The user pool ID for the user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctUserPoolId :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Text)
upctUserPoolId = Lens.lens (userPoolId :: UserPoolClientType -> Lude.Maybe Lude.Text) (\s a -> s {userPoolId = a} :: UserPoolClientType)
{-# DEPRECATED upctUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'idTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctIdTokenValidity :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Natural)
upctIdTokenValidity = Lens.lens (idTokenValidity :: UserPoolClientType -> Lude.Maybe Lude.Natural) (\s a -> s {idTokenValidity = a} :: UserPoolClientType)
{-# DEPRECATED upctIdTokenValidity "Use generic-lens or generic-optics with 'idTokenValidity' instead." #-}

-- | The time units used to specify the token validity times of their respective token.
--
-- /Note:/ Consider using 'tokenValidityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctTokenValidityUnits :: Lens.Lens' UserPoolClientType (Lude.Maybe TokenValidityUnitsType)
upctTokenValidityUnits = Lens.lens (tokenValidityUnits :: UserPoolClientType -> Lude.Maybe TokenValidityUnitsType) (\s a -> s {tokenValidityUnits = a} :: UserPoolClientType)
{-# DEPRECATED upctTokenValidityUnits "Use generic-lens or generic-optics with 'tokenValidityUnits' instead." #-}

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
upctDefaultRedirectURI :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Text)
upctDefaultRedirectURI = Lens.lens (defaultRedirectURI :: UserPoolClientType -> Lude.Maybe Lude.Text) (\s a -> s {defaultRedirectURI = a} :: UserPoolClientType)
{-# DEPRECATED upctDefaultRedirectURI "Use generic-lens or generic-optics with 'defaultRedirectURI' instead." #-}

-- | The writeable attributes.
--
-- /Note:/ Consider using 'writeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctWriteAttributes :: Lens.Lens' UserPoolClientType (Lude.Maybe [Lude.Text])
upctWriteAttributes = Lens.lens (writeAttributes :: UserPoolClientType -> Lude.Maybe [Lude.Text]) (\s a -> s {writeAttributes = a} :: UserPoolClientType)
{-# DEPRECATED upctWriteAttributes "Use generic-lens or generic-optics with 'writeAttributes' instead." #-}

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
upctPreventUserExistenceErrors :: Lens.Lens' UserPoolClientType (Lude.Maybe PreventUserExistenceErrorTypes)
upctPreventUserExistenceErrors = Lens.lens (preventUserExistenceErrors :: UserPoolClientType -> Lude.Maybe PreventUserExistenceErrorTypes) (\s a -> s {preventUserExistenceErrors = a} :: UserPoolClientType)
{-# DEPRECATED upctPreventUserExistenceErrors "Use generic-lens or generic-optics with 'preventUserExistenceErrors' instead." #-}

-- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'accessTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAccessTokenValidity :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Natural)
upctAccessTokenValidity = Lens.lens (accessTokenValidity :: UserPoolClientType -> Lude.Maybe Lude.Natural) (\s a -> s {accessTokenValidity = a} :: UserPoolClientType)
{-# DEPRECATED upctAccessTokenValidity "Use generic-lens or generic-optics with 'accessTokenValidity' instead." #-}

-- | The date the user pool client was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctCreationDate :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Timestamp)
upctCreationDate = Lens.lens (creationDate :: UserPoolClientType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UserPoolClientType)
{-# DEPRECATED upctCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The Read-only attributes.
--
-- /Note:/ Consider using 'readAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctReadAttributes :: Lens.Lens' UserPoolClientType (Lude.Maybe [Lude.Text])
upctReadAttributes = Lens.lens (readAttributes :: UserPoolClientType -> Lude.Maybe [Lude.Text]) (\s a -> s {readAttributes = a} :: UserPoolClientType)
{-# DEPRECATED upctReadAttributes "Use generic-lens or generic-optics with 'readAttributes' instead." #-}

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- /Note:/ Consider using 'allowedOAuthScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAllowedOAuthScopes :: Lens.Lens' UserPoolClientType (Lude.Maybe [Lude.Text])
upctAllowedOAuthScopes = Lens.lens (allowedOAuthScopes :: UserPoolClientType -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedOAuthScopes = a} :: UserPoolClientType)
{-# DEPRECATED upctAllowedOAuthScopes "Use generic-lens or generic-optics with 'allowedOAuthScopes' instead." #-}

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- /Note:/ Consider using 'allowedOAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAllowedOAuthFlows :: Lens.Lens' UserPoolClientType (Lude.Maybe [OAuthFlowType])
upctAllowedOAuthFlows = Lens.lens (allowedOAuthFlows :: UserPoolClientType -> Lude.Maybe [OAuthFlowType]) (\s a -> s {allowedOAuthFlows = a} :: UserPoolClientType)
{-# DEPRECATED upctAllowedOAuthFlows "Use generic-lens or generic-optics with 'allowedOAuthFlows' instead." #-}

-- | The Amazon Pinpoint analytics configuration for the user pool client.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctAnalyticsConfiguration :: Lens.Lens' UserPoolClientType (Lude.Maybe AnalyticsConfigurationType)
upctAnalyticsConfiguration = Lens.lens (analyticsConfiguration :: UserPoolClientType -> Lude.Maybe AnalyticsConfigurationType) (\s a -> s {analyticsConfiguration = a} :: UserPoolClientType)
{-# DEPRECATED upctAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

-- | The client name from the user pool request of the client type.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upctClientName :: Lens.Lens' UserPoolClientType (Lude.Maybe Lude.Text)
upctClientName = Lens.lens (clientName :: UserPoolClientType -> Lude.Maybe Lude.Text) (\s a -> s {clientName = a} :: UserPoolClientType)
{-# DEPRECATED upctClientName "Use generic-lens or generic-optics with 'clientName' instead." #-}

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
upctCallbackURLs :: Lens.Lens' UserPoolClientType (Lude.Maybe [Lude.Text])
upctCallbackURLs = Lens.lens (callbackURLs :: UserPoolClientType -> Lude.Maybe [Lude.Text]) (\s a -> s {callbackURLs = a} :: UserPoolClientType)
{-# DEPRECATED upctCallbackURLs "Use generic-lens or generic-optics with 'callbackURLs' instead." #-}

instance Lude.FromJSON UserPoolClientType where
  parseJSON =
    Lude.withObject
      "UserPoolClientType"
      ( \x ->
          UserPoolClientType'
            Lude.<$> (x Lude..:? "RefreshTokenValidity")
            Lude.<*> (x Lude..:? "ClientId")
            Lude.<*> (x Lude..:? "ExplicitAuthFlows" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "ClientSecret")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "SupportedIdentityProviders" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "LogoutURLs" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AllowedOAuthFlowsUserPoolClient")
            Lude.<*> (x Lude..:? "UserPoolId")
            Lude.<*> (x Lude..:? "IdTokenValidity")
            Lude.<*> (x Lude..:? "TokenValidityUnits")
            Lude.<*> (x Lude..:? "DefaultRedirectURI")
            Lude.<*> (x Lude..:? "WriteAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "PreventUserExistenceErrors")
            Lude.<*> (x Lude..:? "AccessTokenValidity")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "ReadAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AllowedOAuthScopes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AllowedOAuthFlows" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AnalyticsConfiguration")
            Lude.<*> (x Lude..:? "ClientName")
            Lude.<*> (x Lude..:? "CallbackURLs" Lude..!= Lude.mempty)
      )
