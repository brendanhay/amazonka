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
--
-- You can also use this operation to enable token revocation for user pool
-- clients. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
  ( -- * Creating a Request
    UpdateUserPoolClient (..),
    newUpdateUserPoolClient,

    -- * Request Lenses
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_enableTokenRevocation,
    updateUserPoolClient_writeAttributes,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_clientName,
    updateUserPoolClient_callbackURLs,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user pool client.
--
-- /See:/ 'newUpdateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Prelude.Maybe Prelude.Natural,
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
    explicitAuthFlows :: Prelude.Maybe [ExplicitAuthFlowsType],
    -- | A list of provider names for the identity providers that are supported
    -- on this client.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
    -- | The time limit, after which the ID token is no longer valid and cannot
    -- be used.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The units in which the validity times are represented in. Default for
    -- RefreshToken is days, and default for ID and access tokens are hours.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
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
    defaultRedirectURI :: Prelude.Maybe Prelude.Text,
    -- | Enables or disables token revocation. For more information about
    -- revoking tokens, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
    enableTokenRevocation :: Prelude.Maybe Prelude.Bool,
    -- | The writeable attributes of the user pool.
    writeAttributes :: Prelude.Maybe [Prelude.Text],
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
    preventUserExistenceErrors :: Prelude.Maybe PreventUserExistenceErrorTypes,
    -- | The time limit, after which the access token is no longer valid and
    -- cannot be used.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The read-only attributes of the user pool.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
    -- created in Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
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
    allowedOAuthFlows :: Prelude.Maybe [OAuthFlowType],
    -- | The Amazon Pinpoint analytics configuration for collecting metrics for
    -- this user pool.
    --
    -- In regions where Pinpoint is not available, Cognito User Pools only
    -- supports sending events to Amazon Pinpoint projects in us-east-1. In
    -- regions where Pinpoint is available, Cognito User Pools will support
    -- sending events to Amazon Pinpoint projects within that same region.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
    -- | The client name from the update user pool client request.
    clientName :: Prelude.Maybe Prelude.Text,
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
    callbackURLs :: Prelude.Maybe [Prelude.Text],
    -- | The user pool ID for the user pool where you want to update the user
    -- pool client.
    userPoolId :: Prelude.Text,
    -- | The ID of the client associated with the user pool.
    clientId :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'supportedIdentityProviders', 'updateUserPoolClient_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client.
--
-- 'logoutURLs', 'updateUserPoolClient_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'allowedOAuthFlowsUserPoolClient', 'updateUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
--
-- 'idTokenValidity', 'updateUserPoolClient_idTokenValidity' - The time limit, after which the ID token is no longer valid and cannot
-- be used.
--
-- 'tokenValidityUnits', 'updateUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
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
-- 'enableTokenRevocation', 'updateUserPoolClient_enableTokenRevocation' - Enables or disables token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- 'writeAttributes', 'updateUserPoolClient_writeAttributes' - The writeable attributes of the user pool.
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
-- 'accessTokenValidity', 'updateUserPoolClient_accessTokenValidity' - The time limit, after which the access token is no longer valid and
-- cannot be used.
--
-- 'readAttributes', 'updateUserPoolClient_readAttributes' - The read-only attributes of the user pool.
--
-- 'allowedOAuthScopes', 'updateUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
-- created in Resource Servers are also supported.
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
-- 'analyticsConfiguration', 'updateUserPoolClient_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
--
-- 'clientName', 'updateUserPoolClient_clientName' - The client name from the update user pool client request.
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
-- 'userPoolId', 'updateUserPoolClient_userPoolId' - The user pool ID for the user pool where you want to update the user
-- pool client.
--
-- 'clientId', 'updateUserPoolClient_clientId' - The ID of the client associated with the user pool.
newUpdateUserPoolClient ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  UpdateUserPoolClient
newUpdateUserPoolClient pUserPoolId_ pClientId_ =
  UpdateUserPoolClient'
    { refreshTokenValidity =
        Prelude.Nothing,
      explicitAuthFlows = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      enableTokenRevocation = Prelude.Nothing,
      writeAttributes = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      accessTokenValidity = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      clientName = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      clientId = Core._Sensitive Lens.# pClientId_
    }

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
updateUserPoolClient_refreshTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_refreshTokenValidity = Lens.lens (\UpdateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: UpdateUserPoolClient)

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
updateUserPoolClient_explicitAuthFlows :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [ExplicitAuthFlowsType])
updateUserPoolClient_explicitAuthFlows = Lens.lens (\UpdateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | A list of provider names for the identity providers that are supported
-- on this client.
updateUserPoolClient_supportedIdentityProviders :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_supportedIdentityProviders = Lens.lens (\UpdateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UpdateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | A list of allowed logout URLs for the identity providers.
updateUserPoolClient_logoutURLs :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_logoutURLs = Lens.lens (\UpdateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@UpdateUserPoolClient' {} a -> s {logoutURLs = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
updateUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Bool)
updateUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UpdateUserPoolClient)

-- | The time limit, after which the ID token is no longer valid and cannot
-- be used.
updateUserPoolClient_idTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_idTokenValidity = Lens.lens (\UpdateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {idTokenValidity = a} :: UpdateUserPoolClient)

-- | The units in which the validity times are represented in. Default for
-- RefreshToken is days, and default for ID and access tokens are hours.
updateUserPoolClient_tokenValidityUnits :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe TokenValidityUnitsType)
updateUserPoolClient_tokenValidityUnits = Lens.lens (\UpdateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@UpdateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: UpdateUserPoolClient)

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
updateUserPoolClient_defaultRedirectURI :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Text)
updateUserPoolClient_defaultRedirectURI = Lens.lens (\UpdateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@UpdateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: UpdateUserPoolClient)

-- | Enables or disables token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
updateUserPoolClient_enableTokenRevocation :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Bool)
updateUserPoolClient_enableTokenRevocation = Lens.lens (\UpdateUserPoolClient' {enableTokenRevocation} -> enableTokenRevocation) (\s@UpdateUserPoolClient' {} a -> s {enableTokenRevocation = a} :: UpdateUserPoolClient)

-- | The writeable attributes of the user pool.
updateUserPoolClient_writeAttributes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_writeAttributes = Lens.lens (\UpdateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@UpdateUserPoolClient' {} a -> s {writeAttributes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

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
updateUserPoolClient_preventUserExistenceErrors :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe PreventUserExistenceErrorTypes)
updateUserPoolClient_preventUserExistenceErrors = Lens.lens (\UpdateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UpdateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: UpdateUserPoolClient)

-- | The time limit, after which the access token is no longer valid and
-- cannot be used.
updateUserPoolClient_accessTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_accessTokenValidity = Lens.lens (\UpdateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {accessTokenValidity = a} :: UpdateUserPoolClient)

-- | The read-only attributes of the user pool.
updateUserPoolClient_readAttributes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_readAttributes = Lens.lens (\UpdateUserPoolClient' {readAttributes} -> readAttributes) (\s@UpdateUserPoolClient' {} a -> s {readAttributes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
-- created in Resource Servers are also supported.
updateUserPoolClient_allowedOAuthScopes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_allowedOAuthScopes = Lens.lens (\UpdateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

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
updateUserPoolClient_allowedOAuthFlows :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [OAuthFlowType])
updateUserPoolClient_allowedOAuthFlows = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Pinpoint analytics configuration for collecting metrics for
-- this user pool.
--
-- In regions where Pinpoint is not available, Cognito User Pools only
-- supports sending events to Amazon Pinpoint projects in us-east-1. In
-- regions where Pinpoint is available, Cognito User Pools will support
-- sending events to Amazon Pinpoint projects within that same region.
updateUserPoolClient_analyticsConfiguration :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe AnalyticsConfigurationType)
updateUserPoolClient_analyticsConfiguration = Lens.lens (\UpdateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@UpdateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: UpdateUserPoolClient)

-- | The client name from the update user pool client request.
updateUserPoolClient_clientName :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Text)
updateUserPoolClient_clientName = Lens.lens (\UpdateUserPoolClient' {clientName} -> clientName) (\s@UpdateUserPoolClient' {} a -> s {clientName = a} :: UpdateUserPoolClient)

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
updateUserPoolClient_callbackURLs :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_callbackURLs = Lens.lens (\UpdateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@UpdateUserPoolClient' {} a -> s {callbackURLs = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where you want to update the user
-- pool client.
updateUserPoolClient_userPoolId :: Lens.Lens' UpdateUserPoolClient Prelude.Text
updateUserPoolClient_userPoolId = Lens.lens (\UpdateUserPoolClient' {userPoolId} -> userPoolId) (\s@UpdateUserPoolClient' {} a -> s {userPoolId = a} :: UpdateUserPoolClient)

-- | The ID of the client associated with the user pool.
updateUserPoolClient_clientId :: Lens.Lens' UpdateUserPoolClient Prelude.Text
updateUserPoolClient_clientId = Lens.lens (\UpdateUserPoolClient' {clientId} -> clientId) (\s@UpdateUserPoolClient' {} a -> s {clientId = a} :: UpdateUserPoolClient) Prelude.. Core._Sensitive

instance Core.AWSRequest UpdateUserPoolClient where
  type
    AWSResponse UpdateUserPoolClient =
      UpdateUserPoolClientResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserPoolClientResponse'
            Prelude.<$> (x Core..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserPoolClient

instance Prelude.NFData UpdateUserPoolClient

instance Core.ToHeaders UpdateUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateUserPoolClient where
  toJSON UpdateUserPoolClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RefreshTokenValidity" Core..=)
              Prelude.<$> refreshTokenValidity,
            ("ExplicitAuthFlows" Core..=)
              Prelude.<$> explicitAuthFlows,
            ("SupportedIdentityProviders" Core..=)
              Prelude.<$> supportedIdentityProviders,
            ("LogoutURLs" Core..=) Prelude.<$> logoutURLs,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Prelude.<$> allowedOAuthFlowsUserPoolClient,
            ("IdTokenValidity" Core..=)
              Prelude.<$> idTokenValidity,
            ("TokenValidityUnits" Core..=)
              Prelude.<$> tokenValidityUnits,
            ("DefaultRedirectURI" Core..=)
              Prelude.<$> defaultRedirectURI,
            ("EnableTokenRevocation" Core..=)
              Prelude.<$> enableTokenRevocation,
            ("WriteAttributes" Core..=)
              Prelude.<$> writeAttributes,
            ("PreventUserExistenceErrors" Core..=)
              Prelude.<$> preventUserExistenceErrors,
            ("AccessTokenValidity" Core..=)
              Prelude.<$> accessTokenValidity,
            ("ReadAttributes" Core..=)
              Prelude.<$> readAttributes,
            ("AllowedOAuthScopes" Core..=)
              Prelude.<$> allowedOAuthScopes,
            ("AllowedOAuthFlows" Core..=)
              Prelude.<$> allowedOAuthFlows,
            ("AnalyticsConfiguration" Core..=)
              Prelude.<$> analyticsConfiguration,
            ("ClientName" Core..=) Prelude.<$> clientName,
            ("CallbackURLs" Core..=) Prelude.<$> callbackURLs,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("ClientId" Core..= clientId)
          ]
      )

instance Core.ToPath UpdateUserPoolClient where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to update the
-- user pool client.
--
-- /See:/ 'newUpdateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { -- | The user pool client value from the response from the server when an
    -- update user pool client request is made.
    userPoolClient :: Prelude.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateUserPoolClientResponse
newUpdateUserPoolClientResponse pHttpStatus_ =
  UpdateUserPoolClientResponse'
    { userPoolClient =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client value from the response from the server when an
-- update user pool client request is made.
updateUserPoolClientResponse_userPoolClient :: Lens.Lens' UpdateUserPoolClientResponse (Prelude.Maybe UserPoolClientType)
updateUserPoolClientResponse_userPoolClient = Lens.lens (\UpdateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@UpdateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: UpdateUserPoolClientResponse)

-- | The response's http status code.
updateUserPoolClientResponse_httpStatus :: Lens.Lens' UpdateUserPoolClientResponse Prelude.Int
updateUserPoolClientResponse_httpStatus = Lens.lens (\UpdateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@UpdateUserPoolClientResponse' {} a -> s {httpStatus = a} :: UpdateUserPoolClientResponse)

instance Prelude.NFData UpdateUserPoolClientResponse
