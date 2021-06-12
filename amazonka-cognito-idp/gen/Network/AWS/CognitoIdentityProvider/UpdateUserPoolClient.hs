{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user pool app client with the specified
-- attributes. You can get a list of the current user pool app client
-- settings using
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_DescribeUserPoolClient.html DescribeUserPoolClient>.
--
-- If you don\'t provide a value for an attribute, it will be set to the
-- default value.
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
  ( -- * Creating a Request
    UpdateUserPoolClient (..),
    newUpdateUserPoolClient,

    -- * Request Lenses
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_clientName,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_writeAttributes,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_callbackURLs,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_userPoolId,
    updateUserPoolClient_clientId,

    -- * Destructuring the Response
    UpdateUserPoolClientResponse (..),
    newUpdateUserPoolClientResponse,

    -- * Response Lenses
    updateUserPoolClientResponse_userPoolClient,
    updateUserPoolClientResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user pool client.
--
-- /See:/ 'newUpdateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Core.Maybe Core.Natural,
    -- | The time limit, after which the ID token is no longer valid and cannot
    -- be used.
    idTokenValidity :: Core.Maybe Core.Natural,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Core.Maybe [Core.Text],
    -- | The client name from the update user pool client request.
    clientName :: Core.Maybe Core.Text,
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for
    -- this user pool.
    --
    -- In regions where Pinpoint is not available, Cognito User Pools only
    -- supports sending events to Amazon Pinpoint projects in us-east-1. In
    -- regions where Pinpoint is available, Cognito User Pools will support
    -- sending events to Amazon Pinpoint projects within that same region.
    analyticsConfiguration :: Core.Maybe AnalyticsConfigurationType,
    -- | The read-only attributes of the user pool.
    readAttributes :: Core.Maybe [Core.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Core.Maybe [Core.Text],
    -- | The writeable attributes of the user pool.
    writeAttributes :: Core.Maybe [Core.Text],
    -- | A list of provider names for the identity providers that are supported
    -- on this client.
    supportedIdentityProviders :: Core.Maybe [Core.Text],
    -- | The authentication flows that are supported by the user pool clients.
    -- Flow names without the @ALLOW_@ prefix are deprecated in favor of new
    -- names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix
    -- cannot be used along with values without @ALLOW_@ prefix.
    --
    -- Valid values include:
    --
    -- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
    --     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
    --     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
    --     flow, Cognito receives the password in the request instead of using
    --     the SRP (Secure Remote Password protocol) protocol to verify
    --     passwords.
    --
    -- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
    --
    -- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
    --     authentication. In this flow, Cognito receives the password in the
    --     request instead of using the SRP protocol to verify passwords.
    --
    -- -   @ALLOW_USER_SRP_AUTH@: Enable SRP based authentication.
    --
    -- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
    explicitAuthFlows :: Core.Maybe [ExplicitAuthFlowsType],
    -- | The default redirect URI. Must be in the @CallbackURLs@ list.
    --
    -- A redirect URI must:
    --
    -- -   Be an absolute URI.
    --
    -- -   Be registered with the authorization server.
    --
    -- -   Not include a fragment component.
    --
    -- See
    -- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
    --
    -- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
    -- for testing purposes only.
    --
    -- App callback URLs such as myapp:\/\/example are also supported.
    defaultRedirectURI :: Core.Maybe Core.Text,
    -- | The units in which the validity times are represented in. Default for
    -- RefreshToken is days, and default for ID and access tokens are hours.
    tokenValidityUnits :: Core.Maybe TokenValidityUnitsType,
    -- | A list of allowed redirect (callback) URLs for the identity providers.
    --
    -- A redirect URI must:
    --
    -- -   Be an absolute URI.
    --
    -- -   Be registered with the authorization server.
    --
    -- -   Not include a fragment component.
    --
    -- See
    -- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
    --
    -- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
    -- for testing purposes only.
    --
    -- App callback URLs such as myapp:\/\/example are also supported.
    callbackURLs :: Core.Maybe [Core.Text],
    -- | The allowed OAuth flows.
    --
    -- Set to @code@ to initiate a code grant flow, which provides an
    -- authorization code as the response. This code can be exchanged for
    -- access tokens with the token endpoint.
    --
    -- Set to @implicit@ to specify that the client should get the access token
    -- (and, optionally, ID token, based on scopes) directly.
    --
    -- Set to @client_credentials@ to specify that the client should get the
    -- access token (and, optionally, ID token, based on scopes) from the token
    -- endpoint using a combination of client and client_secret.
    allowedOAuthFlows :: Core.Maybe [OAuthFlowType],
    -- | The time limit, after which the access token is no longer valid and
    -- cannot be used.
    accessTokenValidity :: Core.Maybe Core.Natural,
    -- | Use this setting to choose which errors and responses are returned by
    -- Cognito APIs during authentication, account confirmation, and password
    -- recovery when the user does not exist in the user pool. When set to
    -- @ENABLED@ and the user does not exist, authentication returns an error
    -- indicating either the username or password was incorrect, and account
    -- confirmation and password recovery return a response indicating a code
    -- was sent to a simulated destination. When set to @LEGACY@, those APIs
    -- will return a @UserNotFoundException@ exception if the user does not
    -- exist in the user pool.
    --
    -- Valid values include:
    --
    -- -   @ENABLED@ - This prevents user existence-related errors.
    --
    -- -   @LEGACY@ - This represents the old behavior of Cognito where user
    --     existence related errors are not prevented.
    --
    -- After February 15th 2020, the value of @PreventUserExistenceErrors@ will
    -- default to @ENABLED@ for newly created user pool clients if no value is
    -- provided.
    preventUserExistenceErrors :: Core.Maybe PreventUserExistenceErrorTypes,
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Core.Maybe Core.Bool,
    -- | The user pool ID for the user pool where you want to update the user
    -- pool client.
    userPoolId :: Core.Text,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserPoolClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshTokenValidity', 'updateUserPoolClient_refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
--
-- 'idTokenValidity', 'updateUserPoolClient_idTokenValidity' - The time limit, after which the ID token is no longer valid and cannot
-- be used.
--
-- 'allowedOAuthScopes', 'updateUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
--
-- 'clientName', 'updateUserPoolClient_clientName' - The client name from the update user pool client request.
--
-- 'analyticsConfiguration', 'updateUserPoolClient_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
--
-- 'readAttributes', 'updateUserPoolClient_readAttributes' - The read-only attributes of the user pool.
--
-- 'logoutURLs', 'updateUserPoolClient_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'writeAttributes', 'updateUserPoolClient_writeAttributes' - The writeable attributes of the user pool.
--
-- 'supportedIdentityProviders', 'updateUserPoolClient_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client.
--
-- 'explicitAuthFlows', 'updateUserPoolClient_explicitAuthFlows' - The authentication flows that are supported by the user pool clients.
-- Flow names without the @ALLOW_@ prefix are deprecated in favor of new
-- names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix
-- cannot be used along with values without @ALLOW_@ prefix.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, Cognito receives the password in the request instead of using
--     the SRP (Secure Remote Password protocol) protocol to verify
--     passwords.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Cognito receives the password in the
--     request instead of using the SRP protocol to verify passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
--
-- 'defaultRedirectURI', 'updateUserPoolClient_defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
--
-- 'tokenValidityUnits', 'updateUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
--
-- 'callbackURLs', 'updateUserPoolClient_callbackURLs' - A list of allowed redirect (callback) URLs for the identity providers.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
--
-- 'allowedOAuthFlows', 'updateUserPoolClient_allowedOAuthFlows' - The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an
-- authorization code as the response. This code can be exchanged for
-- access tokens with the token endpoint.
--
-- Set to @implicit@ to specify that the client should get the access token
-- (and, optionally, ID token, based on scopes) directly.
--
-- Set to @client_credentials@ to specify that the client should get the
-- access token (and, optionally, ID token, based on scopes) from the token
-- endpoint using a combination of client and client_secret.
--
-- 'accessTokenValidity', 'updateUserPoolClient_accessTokenValidity' - The time limit, after which the access token is no longer valid and
-- cannot be used.
--
-- 'preventUserExistenceErrors', 'updateUserPoolClient_preventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by
-- Cognito APIs during authentication, account confirmation, and password
-- recovery when the user does not exist in the user pool. When set to
-- @ENABLED@ and the user does not exist, authentication returns an error
-- indicating either the username or password was incorrect, and account
-- confirmation and password recovery return a response indicating a code
-- was sent to a simulated destination. When set to @LEGACY@, those APIs
-- will return a @UserNotFoundException@ exception if the user does not
-- exist in the user pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the old behavior of Cognito where user
--     existence related errors are not prevented.
--
-- After February 15th 2020, the value of @PreventUserExistenceErrors@ will
-- default to @ENABLED@ for newly created user pool clients if no value is
-- provided.
--
-- 'allowedOAuthFlowsUserPoolClient', 'updateUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
--
-- 'userPoolId', 'updateUserPoolClient_userPoolId' - The user pool ID for the user pool where you want to update the user
-- pool client.
--
-- 'clientId', 'updateUserPoolClient_clientId' - The ID of the client associated with the user pool.
newUpdateUserPoolClient ::
  -- | 'userPoolId'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
  UpdateUserPoolClient
newUpdateUserPoolClient pUserPoolId_ pClientId_ =
  UpdateUserPoolClient'
    { refreshTokenValidity =
        Core.Nothing,
      idTokenValidity = Core.Nothing,
      allowedOAuthScopes = Core.Nothing,
      clientName = Core.Nothing,
      analyticsConfiguration = Core.Nothing,
      readAttributes = Core.Nothing,
      logoutURLs = Core.Nothing,
      writeAttributes = Core.Nothing,
      supportedIdentityProviders = Core.Nothing,
      explicitAuthFlows = Core.Nothing,
      defaultRedirectURI = Core.Nothing,
      tokenValidityUnits = Core.Nothing,
      callbackURLs = Core.Nothing,
      allowedOAuthFlows = Core.Nothing,
      accessTokenValidity = Core.Nothing,
      preventUserExistenceErrors = Core.Nothing,
      allowedOAuthFlowsUserPoolClient = Core.Nothing,
      userPoolId = pUserPoolId_,
      clientId = Core._Sensitive Lens.# pClientId_
    }

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
updateUserPoolClient_refreshTokenValidity :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Natural)
updateUserPoolClient_refreshTokenValidity = Lens.lens (\UpdateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: UpdateUserPoolClient)

-- | The time limit, after which the ID token is no longer valid and cannot
-- be used.
updateUserPoolClient_idTokenValidity :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Natural)
updateUserPoolClient_idTokenValidity = Lens.lens (\UpdateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {idTokenValidity = a} :: UpdateUserPoolClient)

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- AWS are: @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
updateUserPoolClient_allowedOAuthScopes :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Core.Text])
updateUserPoolClient_allowedOAuthScopes = Lens.lens (\UpdateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The client name from the update user pool client request.
updateUserPoolClient_clientName :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Text)
updateUserPoolClient_clientName = Lens.lens (\UpdateUserPoolClient' {clientName} -> clientName) (\s@UpdateUserPoolClient' {} a -> s {clientName = a} :: UpdateUserPoolClient)

-- | The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
updateUserPoolClient_analyticsConfiguration :: Lens.Lens' UpdateUserPoolClient (Core.Maybe AnalyticsConfigurationType)
updateUserPoolClient_analyticsConfiguration = Lens.lens (\UpdateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@UpdateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: UpdateUserPoolClient)

-- | The read-only attributes of the user pool.
updateUserPoolClient_readAttributes :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Core.Text])
updateUserPoolClient_readAttributes = Lens.lens (\UpdateUserPoolClient' {readAttributes} -> readAttributes) (\s@UpdateUserPoolClient' {} a -> s {readAttributes = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | A list of allowed logout URLs for the identity providers.
updateUserPoolClient_logoutURLs :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Core.Text])
updateUserPoolClient_logoutURLs = Lens.lens (\UpdateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@UpdateUserPoolClient' {} a -> s {logoutURLs = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The writeable attributes of the user pool.
updateUserPoolClient_writeAttributes :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Core.Text])
updateUserPoolClient_writeAttributes = Lens.lens (\UpdateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@UpdateUserPoolClient' {} a -> s {writeAttributes = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | A list of provider names for the identity providers that are supported
-- on this client.
updateUserPoolClient_supportedIdentityProviders :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Core.Text])
updateUserPoolClient_supportedIdentityProviders = Lens.lens (\UpdateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UpdateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The authentication flows that are supported by the user pool clients.
-- Flow names without the @ALLOW_@ prefix are deprecated in favor of new
-- names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix
-- cannot be used along with values without @ALLOW_@ prefix.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, Cognito receives the password in the request instead of using
--     the SRP (Secure Remote Password protocol) protocol to verify
--     passwords.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Cognito receives the password in the
--     request instead of using the SRP protocol to verify passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
updateUserPoolClient_explicitAuthFlows :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [ExplicitAuthFlowsType])
updateUserPoolClient_explicitAuthFlows = Lens.lens (\UpdateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
updateUserPoolClient_defaultRedirectURI :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Text)
updateUserPoolClient_defaultRedirectURI = Lens.lens (\UpdateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@UpdateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: UpdateUserPoolClient)

-- | The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
updateUserPoolClient_tokenValidityUnits :: Lens.Lens' UpdateUserPoolClient (Core.Maybe TokenValidityUnitsType)
updateUserPoolClient_tokenValidityUnits = Lens.lens (\UpdateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@UpdateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: UpdateUserPoolClient)

-- | A list of allowed redirect (callback) URLs for the identity providers.
--
-- A redirect URI must:
--
-- -   Be an absolute URI.
--
-- -   Be registered with the authorization server.
--
-- -   Not include a fragment component.
--
-- See
-- <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint>.
--
-- Amazon Cognito requires HTTPS over HTTP except for http:\/\/localhost
-- for testing purposes only.
--
-- App callback URLs such as myapp:\/\/example are also supported.
updateUserPoolClient_callbackURLs :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [Core.Text])
updateUserPoolClient_callbackURLs = Lens.lens (\UpdateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@UpdateUserPoolClient' {} a -> s {callbackURLs = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The allowed OAuth flows.
--
-- Set to @code@ to initiate a code grant flow, which provides an
-- authorization code as the response. This code can be exchanged for
-- access tokens with the token endpoint.
--
-- Set to @implicit@ to specify that the client should get the access token
-- (and, optionally, ID token, based on scopes) directly.
--
-- Set to @client_credentials@ to specify that the client should get the
-- access token (and, optionally, ID token, based on scopes) from the token
-- endpoint using a combination of client and client_secret.
updateUserPoolClient_allowedOAuthFlows :: Lens.Lens' UpdateUserPoolClient (Core.Maybe [OAuthFlowType])
updateUserPoolClient_allowedOAuthFlows = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: UpdateUserPoolClient) Core.. Lens.mapping Lens._Coerce

-- | The time limit, after which the access token is no longer valid and
-- cannot be used.
updateUserPoolClient_accessTokenValidity :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Natural)
updateUserPoolClient_accessTokenValidity = Lens.lens (\UpdateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {accessTokenValidity = a} :: UpdateUserPoolClient)

-- | Use this setting to choose which errors and responses are returned by
-- Cognito APIs during authentication, account confirmation, and password
-- recovery when the user does not exist in the user pool. When set to
-- @ENABLED@ and the user does not exist, authentication returns an error
-- indicating either the username or password was incorrect, and account
-- confirmation and password recovery return a response indicating a code
-- was sent to a simulated destination. When set to @LEGACY@, those APIs
-- will return a @UserNotFoundException@ exception if the user does not
-- exist in the user pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the old behavior of Cognito where user
--     existence related errors are not prevented.
--
-- After February 15th 2020, the value of @PreventUserExistenceErrors@ will
-- default to @ENABLED@ for newly created user pool clients if no value is
-- provided.
updateUserPoolClient_preventUserExistenceErrors :: Lens.Lens' UpdateUserPoolClient (Core.Maybe PreventUserExistenceErrorTypes)
updateUserPoolClient_preventUserExistenceErrors = Lens.lens (\UpdateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UpdateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: UpdateUserPoolClient)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
updateUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UpdateUserPoolClient (Core.Maybe Core.Bool)
updateUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UpdateUserPoolClient)

-- | The user pool ID for the user pool where you want to update the user
-- pool client.
updateUserPoolClient_userPoolId :: Lens.Lens' UpdateUserPoolClient Core.Text
updateUserPoolClient_userPoolId = Lens.lens (\UpdateUserPoolClient' {userPoolId} -> userPoolId) (\s@UpdateUserPoolClient' {} a -> s {userPoolId = a} :: UpdateUserPoolClient)

-- | The ID of the client associated with the user pool.
updateUserPoolClient_clientId :: Lens.Lens' UpdateUserPoolClient Core.Text
updateUserPoolClient_clientId = Lens.lens (\UpdateUserPoolClient' {clientId} -> clientId) (\s@UpdateUserPoolClient' {} a -> s {clientId = a} :: UpdateUserPoolClient) Core.. Core._Sensitive

instance Core.AWSRequest UpdateUserPoolClient where
  type
    AWSResponse UpdateUserPoolClient =
      UpdateUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserPoolClientResponse'
            Core.<$> (x Core..?> "UserPoolClient")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateUserPoolClient

instance Core.NFData UpdateUserPoolClient

instance Core.ToHeaders UpdateUserPoolClient where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolClient" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUserPoolClient where
  toJSON UpdateUserPoolClient' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RefreshTokenValidity" Core..=)
              Core.<$> refreshTokenValidity,
            ("IdTokenValidity" Core..=) Core.<$> idTokenValidity,
            ("AllowedOAuthScopes" Core..=)
              Core.<$> allowedOAuthScopes,
            ("ClientName" Core..=) Core.<$> clientName,
            ("AnalyticsConfiguration" Core..=)
              Core.<$> analyticsConfiguration,
            ("ReadAttributes" Core..=) Core.<$> readAttributes,
            ("LogoutURLs" Core..=) Core.<$> logoutURLs,
            ("WriteAttributes" Core..=) Core.<$> writeAttributes,
            ("SupportedIdentityProviders" Core..=)
              Core.<$> supportedIdentityProviders,
            ("ExplicitAuthFlows" Core..=)
              Core.<$> explicitAuthFlows,
            ("DefaultRedirectURI" Core..=)
              Core.<$> defaultRedirectURI,
            ("TokenValidityUnits" Core..=)
              Core.<$> tokenValidityUnits,
            ("CallbackURLs" Core..=) Core.<$> callbackURLs,
            ("AllowedOAuthFlows" Core..=)
              Core.<$> allowedOAuthFlows,
            ("AccessTokenValidity" Core..=)
              Core.<$> accessTokenValidity,
            ("PreventUserExistenceErrors" Core..=)
              Core.<$> preventUserExistenceErrors,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Core.<$> allowedOAuthFlowsUserPoolClient,
            Core.Just ("UserPoolId" Core..= userPoolId),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.ToPath UpdateUserPoolClient where
  toPath = Core.const "/"

instance Core.ToQuery UpdateUserPoolClient where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server to the request to update the
-- user pool client.
--
-- /See:/ 'newUpdateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { -- | The user pool client value from the response from the server when an
    -- update user pool client request is made.
    userPoolClient :: Core.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolClient', 'updateUserPoolClientResponse_userPoolClient' - The user pool client value from the response from the server when an
-- update user pool client request is made.
--
-- 'httpStatus', 'updateUserPoolClientResponse_httpStatus' - The response's http status code.
newUpdateUserPoolClientResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateUserPoolClientResponse
newUpdateUserPoolClientResponse pHttpStatus_ =
  UpdateUserPoolClientResponse'
    { userPoolClient =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client value from the response from the server when an
-- update user pool client request is made.
updateUserPoolClientResponse_userPoolClient :: Lens.Lens' UpdateUserPoolClientResponse (Core.Maybe UserPoolClientType)
updateUserPoolClientResponse_userPoolClient = Lens.lens (\UpdateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@UpdateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: UpdateUserPoolClientResponse)

-- | The response's http status code.
updateUserPoolClientResponse_httpStatus :: Lens.Lens' UpdateUserPoolClientResponse Core.Int
updateUserPoolClientResponse_httpStatus = Lens.lens (\UpdateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@UpdateUserPoolClientResponse' {} a -> s {httpStatus = a} :: UpdateUserPoolClientResponse)

instance Core.NFData UpdateUserPoolClientResponse
