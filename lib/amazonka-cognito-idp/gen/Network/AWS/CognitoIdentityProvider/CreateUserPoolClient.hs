{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateUserPoolClient (..),
    mkCreateUserPoolClient,

    -- ** Request lenses
    cupcRefreshTokenValidity,
    cupcExplicitAuthFlows,
    cupcSupportedIdentityProviders,
    cupcLogoutURLs,
    cupcAllowedOAuthFlowsUserPoolClient,
    cupcGenerateSecret,
    cupcIdTokenValidity,
    cupcTokenValidityUnits,
    cupcDefaultRedirectURI,
    cupcWriteAttributes,
    cupcPreventUserExistenceErrors,
    cupcAccessTokenValidity,
    cupcReadAttributes,
    cupcAllowedOAuthScopes,
    cupcAllowedOAuthFlows,
    cupcAnalyticsConfiguration,
    cupcCallbackURLs,
    cupcUserPoolId,
    cupcClientName,

    -- * Destructuring the response
    CreateUserPoolClientResponse (..),
    mkCreateUserPoolClientResponse,

    -- ** Response lenses
    cupcrsUserPoolClient,
    cupcrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to create a user pool client.
--
-- /See:/ 'mkCreateUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
  { refreshTokenValidity ::
      Lude.Maybe Lude.Natural,
    explicitAuthFlows ::
      Lude.Maybe [ExplicitAuthFlowsType],
    supportedIdentityProviders ::
      Lude.Maybe [Lude.Text],
    logoutURLs :: Lude.Maybe [Lude.Text],
    allowedOAuthFlowsUserPoolClient ::
      Lude.Maybe Lude.Bool,
    generateSecret :: Lude.Maybe Lude.Bool,
    idTokenValidity :: Lude.Maybe Lude.Natural,
    tokenValidityUnits ::
      Lude.Maybe TokenValidityUnitsType,
    defaultRedirectURI :: Lude.Maybe Lude.Text,
    writeAttributes :: Lude.Maybe [Lude.Text],
    preventUserExistenceErrors ::
      Lude.Maybe PreventUserExistenceErrorTypes,
    accessTokenValidity :: Lude.Maybe Lude.Natural,
    readAttributes :: Lude.Maybe [Lude.Text],
    allowedOAuthScopes :: Lude.Maybe [Lude.Text],
    allowedOAuthFlows :: Lude.Maybe [OAuthFlowType],
    analyticsConfiguration ::
      Lude.Maybe AnalyticsConfigurationType,
    callbackURLs :: Lude.Maybe [Lude.Text],
    userPoolId :: Lude.Text,
    clientName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserPoolClient' with the minimum fields required to make a request.
--
-- * 'accessTokenValidity' - The time limit, between 5 minutes and 1 day, after which the access token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
-- * 'allowedOAuthFlows' - The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
-- * 'allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
-- * 'allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
-- * 'analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
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
-- * 'clientName' - The client name for the user pool client you would like to create.
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
-- * 'generateSecret' - Boolean to specify whether you want to generate a secret for the user pool client being created.
-- * 'idTokenValidity' - The time limit, between 5 minutes and 1 day, after which the ID token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
-- * 'logoutURLs' - A list of allowed logout URLs for the identity providers.
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
-- * 'readAttributes' - The read attributes.
-- * 'refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer valid and cannot be used.
-- * 'supportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client. The following are supported: @COGNITO@ , @Facebook@ , @Google@ and @LoginWithAmazon@ .
-- * 'tokenValidityUnits' - The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
-- * 'userPoolId' - The user pool ID for the user pool where you want to create a user pool client.
-- * 'writeAttributes' - The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an identity provider, this array must include all attributes that are mapped to identity provider attributes. Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If your app client lacks write access to a mapped attribute, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
mkCreateUserPoolClient ::
  -- | 'userPoolId'
  Lude.Text ->
  -- | 'clientName'
  Lude.Text ->
  CreateUserPoolClient
mkCreateUserPoolClient pUserPoolId_ pClientName_ =
  CreateUserPoolClient'
    { refreshTokenValidity = Lude.Nothing,
      explicitAuthFlows = Lude.Nothing,
      supportedIdentityProviders = Lude.Nothing,
      logoutURLs = Lude.Nothing,
      allowedOAuthFlowsUserPoolClient = Lude.Nothing,
      generateSecret = Lude.Nothing,
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
      callbackURLs = Lude.Nothing,
      userPoolId = pUserPoolId_,
      clientName = pClientName_
    }

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- /Note:/ Consider using 'refreshTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcRefreshTokenValidity :: Lens.Lens' CreateUserPoolClient (Lude.Maybe Lude.Natural)
cupcRefreshTokenValidity = Lens.lens (refreshTokenValidity :: CreateUserPoolClient -> Lude.Maybe Lude.Natural) (\s a -> s {refreshTokenValidity = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcRefreshTokenValidity "Use generic-lens or generic-optics with 'refreshTokenValidity' instead." #-}

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
cupcExplicitAuthFlows :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [ExplicitAuthFlowsType])
cupcExplicitAuthFlows = Lens.lens (explicitAuthFlows :: CreateUserPoolClient -> Lude.Maybe [ExplicitAuthFlowsType]) (\s a -> s {explicitAuthFlows = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcExplicitAuthFlows "Use generic-lens or generic-optics with 'explicitAuthFlows' instead." #-}

-- | A list of provider names for the identity providers that are supported on this client. The following are supported: @COGNITO@ , @Facebook@ , @Google@ and @LoginWithAmazon@ .
--
-- /Note:/ Consider using 'supportedIdentityProviders' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcSupportedIdentityProviders :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [Lude.Text])
cupcSupportedIdentityProviders = Lens.lens (supportedIdentityProviders :: CreateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {supportedIdentityProviders = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcSupportedIdentityProviders "Use generic-lens or generic-optics with 'supportedIdentityProviders' instead." #-}

-- | A list of allowed logout URLs for the identity providers.
--
-- /Note:/ Consider using 'logoutURLs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcLogoutURLs :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [Lude.Text])
cupcLogoutURLs = Lens.lens (logoutURLs :: CreateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {logoutURLs = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcLogoutURLs "Use generic-lens or generic-optics with 'logoutURLs' instead." #-}

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- /Note:/ Consider using 'allowedOAuthFlowsUserPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAllowedOAuthFlowsUserPoolClient :: Lens.Lens' CreateUserPoolClient (Lude.Maybe Lude.Bool)
cupcAllowedOAuthFlowsUserPoolClient = Lens.lens (allowedOAuthFlowsUserPoolClient :: CreateUserPoolClient -> Lude.Maybe Lude.Bool) (\s a -> s {allowedOAuthFlowsUserPoolClient = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcAllowedOAuthFlowsUserPoolClient "Use generic-lens or generic-optics with 'allowedOAuthFlowsUserPoolClient' instead." #-}

-- | Boolean to specify whether you want to generate a secret for the user pool client being created.
--
-- /Note:/ Consider using 'generateSecret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcGenerateSecret :: Lens.Lens' CreateUserPoolClient (Lude.Maybe Lude.Bool)
cupcGenerateSecret = Lens.lens (generateSecret :: CreateUserPoolClient -> Lude.Maybe Lude.Bool) (\s a -> s {generateSecret = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcGenerateSecret "Use generic-lens or generic-optics with 'generateSecret' instead." #-}

-- | The time limit, between 5 minutes and 1 day, after which the ID token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
--
-- /Note:/ Consider using 'idTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcIdTokenValidity :: Lens.Lens' CreateUserPoolClient (Lude.Maybe Lude.Natural)
cupcIdTokenValidity = Lens.lens (idTokenValidity :: CreateUserPoolClient -> Lude.Maybe Lude.Natural) (\s a -> s {idTokenValidity = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcIdTokenValidity "Use generic-lens or generic-optics with 'idTokenValidity' instead." #-}

-- | The units in which the validity times are represented in. Default for RefreshToken is days, and default for ID and access tokens are hours.
--
-- /Note:/ Consider using 'tokenValidityUnits' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcTokenValidityUnits :: Lens.Lens' CreateUserPoolClient (Lude.Maybe TokenValidityUnitsType)
cupcTokenValidityUnits = Lens.lens (tokenValidityUnits :: CreateUserPoolClient -> Lude.Maybe TokenValidityUnitsType) (\s a -> s {tokenValidityUnits = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcTokenValidityUnits "Use generic-lens or generic-optics with 'tokenValidityUnits' instead." #-}

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
cupcDefaultRedirectURI :: Lens.Lens' CreateUserPoolClient (Lude.Maybe Lude.Text)
cupcDefaultRedirectURI = Lens.lens (defaultRedirectURI :: CreateUserPoolClient -> Lude.Maybe Lude.Text) (\s a -> s {defaultRedirectURI = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcDefaultRedirectURI "Use generic-lens or generic-optics with 'defaultRedirectURI' instead." #-}

-- | The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an identity provider, this array must include all attributes that are mapped to identity provider attributes. Amazon Cognito updates mapped attributes when users sign in to your application through an identity provider. If your app client lacks write access to a mapped attribute, Amazon Cognito throws an error when it attempts to update the attribute. For more information, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying Identity Provider Attribute Mappings for Your User Pool> .
--
-- /Note:/ Consider using 'writeAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcWriteAttributes :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [Lude.Text])
cupcWriteAttributes = Lens.lens (writeAttributes :: CreateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {writeAttributes = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcWriteAttributes "Use generic-lens or generic-optics with 'writeAttributes' instead." #-}

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
cupcPreventUserExistenceErrors :: Lens.Lens' CreateUserPoolClient (Lude.Maybe PreventUserExistenceErrorTypes)
cupcPreventUserExistenceErrors = Lens.lens (preventUserExistenceErrors :: CreateUserPoolClient -> Lude.Maybe PreventUserExistenceErrorTypes) (\s a -> s {preventUserExistenceErrors = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcPreventUserExistenceErrors "Use generic-lens or generic-optics with 'preventUserExistenceErrors' instead." #-}

-- | The time limit, between 5 minutes and 1 day, after which the access token is no longer valid and cannot be used. This value will be overridden if you have entered a value in TokenValidityUnits.
--
-- /Note:/ Consider using 'accessTokenValidity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAccessTokenValidity :: Lens.Lens' CreateUserPoolClient (Lude.Maybe Lude.Natural)
cupcAccessTokenValidity = Lens.lens (accessTokenValidity :: CreateUserPoolClient -> Lude.Maybe Lude.Natural) (\s a -> s {accessTokenValidity = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcAccessTokenValidity "Use generic-lens or generic-optics with 'accessTokenValidity' instead." #-}

-- | The read attributes.
--
-- /Note:/ Consider using 'readAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcReadAttributes :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [Lude.Text])
cupcReadAttributes = Lens.lens (readAttributes :: CreateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {readAttributes = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcReadAttributes "Use generic-lens or generic-optics with 'readAttributes' instead." #-}

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- /Note:/ Consider using 'allowedOAuthScopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAllowedOAuthScopes :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [Lude.Text])
cupcAllowedOAuthScopes = Lens.lens (allowedOAuthScopes :: CreateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedOAuthScopes = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcAllowedOAuthScopes "Use generic-lens or generic-optics with 'allowedOAuthScopes' instead." #-}

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint.
-- Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
-- Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- /Note:/ Consider using 'allowedOAuthFlows' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAllowedOAuthFlows :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [OAuthFlowType])
cupcAllowedOAuthFlows = Lens.lens (allowedOAuthFlows :: CreateUserPoolClient -> Lude.Maybe [OAuthFlowType]) (\s a -> s {allowedOAuthFlows = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcAllowedOAuthFlows "Use generic-lens or generic-optics with 'allowedOAuthFlows' instead." #-}

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- /Note:/ Consider using 'analyticsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcAnalyticsConfiguration :: Lens.Lens' CreateUserPoolClient (Lude.Maybe AnalyticsConfigurationType)
cupcAnalyticsConfiguration = Lens.lens (analyticsConfiguration :: CreateUserPoolClient -> Lude.Maybe AnalyticsConfigurationType) (\s a -> s {analyticsConfiguration = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcAnalyticsConfiguration "Use generic-lens or generic-optics with 'analyticsConfiguration' instead." #-}

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
cupcCallbackURLs :: Lens.Lens' CreateUserPoolClient (Lude.Maybe [Lude.Text])
cupcCallbackURLs = Lens.lens (callbackURLs :: CreateUserPoolClient -> Lude.Maybe [Lude.Text]) (\s a -> s {callbackURLs = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcCallbackURLs "Use generic-lens or generic-optics with 'callbackURLs' instead." #-}

-- | The user pool ID for the user pool where you want to create a user pool client.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcUserPoolId :: Lens.Lens' CreateUserPoolClient Lude.Text
cupcUserPoolId = Lens.lens (userPoolId :: CreateUserPoolClient -> Lude.Text) (\s a -> s {userPoolId = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

-- | The client name for the user pool client you would like to create.
--
-- /Note:/ Consider using 'clientName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcClientName :: Lens.Lens' CreateUserPoolClient Lude.Text
cupcClientName = Lens.lens (clientName :: CreateUserPoolClient -> Lude.Text) (\s a -> s {clientName = a} :: CreateUserPoolClient)
{-# DEPRECATED cupcClientName "Use generic-lens or generic-optics with 'clientName' instead." #-}

instance Lude.AWSRequest CreateUserPoolClient where
  type Rs CreateUserPoolClient = CreateUserPoolClientResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserPoolClientResponse'
            Lude.<$> (x Lude..?> "UserPoolClient")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserPoolClient where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolClient" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserPoolClient where
  toJSON CreateUserPoolClient' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RefreshTokenValidity" Lude..=) Lude.<$> refreshTokenValidity,
            ("ExplicitAuthFlows" Lude..=) Lude.<$> explicitAuthFlows,
            ("SupportedIdentityProviders" Lude..=)
              Lude.<$> supportedIdentityProviders,
            ("LogoutURLs" Lude..=) Lude.<$> logoutURLs,
            ("AllowedOAuthFlowsUserPoolClient" Lude..=)
              Lude.<$> allowedOAuthFlowsUserPoolClient,
            ("GenerateSecret" Lude..=) Lude.<$> generateSecret,
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
            ("CallbackURLs" Lude..=) Lude.<$> callbackURLs,
            Lude.Just ("UserPoolId" Lude..= userPoolId),
            Lude.Just ("ClientName" Lude..= clientName)
          ]
      )

instance Lude.ToPath CreateUserPoolClient where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserPoolClient where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server to create a user pool client.
--
-- /See:/ 'mkCreateUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
  { userPoolClient ::
      Lude.Maybe UserPoolClientType,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'userPoolClient' - The user pool client that was just created.
mkCreateUserPoolClientResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserPoolClientResponse
mkCreateUserPoolClientResponse pResponseStatus_ =
  CreateUserPoolClientResponse'
    { userPoolClient = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The user pool client that was just created.
--
-- /Note:/ Consider using 'userPoolClient' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcrsUserPoolClient :: Lens.Lens' CreateUserPoolClientResponse (Lude.Maybe UserPoolClientType)
cupcrsUserPoolClient = Lens.lens (userPoolClient :: CreateUserPoolClientResponse -> Lude.Maybe UserPoolClientType) (\s a -> s {userPoolClient = a} :: CreateUserPoolClientResponse)
{-# DEPRECATED cupcrsUserPoolClient "Use generic-lens or generic-optics with 'userPoolClient' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupcrsResponseStatus :: Lens.Lens' CreateUserPoolClientResponse Lude.Int
cupcrsResponseStatus = Lens.lens (responseStatus :: CreateUserPoolClientResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserPoolClientResponse)
{-# DEPRECATED cupcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
