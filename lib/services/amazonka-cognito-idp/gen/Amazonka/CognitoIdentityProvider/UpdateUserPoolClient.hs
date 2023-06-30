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
-- Module      : Amazonka.CognitoIdentityProvider.UpdateUserPoolClient
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CognitoIdentityProvider.UpdateUserPoolClient
  ( -- * Creating a Request
    UpdateUserPoolClient (..),
    newUpdateUserPoolClient,

    -- * Request Lenses
    updateUserPoolClient_accessTokenValidity,
    updateUserPoolClient_allowedOAuthFlows,
    updateUserPoolClient_allowedOAuthFlowsUserPoolClient,
    updateUserPoolClient_allowedOAuthScopes,
    updateUserPoolClient_analyticsConfiguration,
    updateUserPoolClient_authSessionValidity,
    updateUserPoolClient_callbackURLs,
    updateUserPoolClient_clientName,
    updateUserPoolClient_defaultRedirectURI,
    updateUserPoolClient_enablePropagateAdditionalUserContextData,
    updateUserPoolClient_enableTokenRevocation,
    updateUserPoolClient_explicitAuthFlows,
    updateUserPoolClient_idTokenValidity,
    updateUserPoolClient_logoutURLs,
    updateUserPoolClient_preventUserExistenceErrors,
    updateUserPoolClient_readAttributes,
    updateUserPoolClient_refreshTokenValidity,
    updateUserPoolClient_supportedIdentityProviders,
    updateUserPoolClient_tokenValidityUnits,
    updateUserPoolClient_writeAttributes,
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

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to update the user pool client.
--
-- /See:/ 'newUpdateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { -- | The access token time limit. After this limit expires, your user can\'t
    -- use their access token. To specify the time unit for
    -- @AccessTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set a
    -- @TokenValidityUnits@ value in your API request.
    --
    -- For example, when you set @AccessTokenValidity@ to @10@ and
    -- @TokenValidityUnits@ to @hours@, your user can authorize access with
    -- their access token for 10 hours.
    --
    -- The default time unit for @AccessTokenValidity@ in an API request is
    -- hours. /Valid range/ is displayed below in seconds.
    --
    -- If you don\'t specify otherwise in the configuration of your app client,
    -- your access tokens are valid for one hour.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The allowed OAuth flows.
    --
    -- [code]
    --     Use a code grant flow, which provides an authorization code as the
    --     response. This code can be exchanged for access tokens with the
    --     @\/oauth2\/token@ endpoint.
    --
    -- [implicit]
    --     Issue the access token (and, optionally, ID token, based on scopes)
    --     directly to your user.
    --
    -- [client_credentials]
    --     Issue the access token from the @\/oauth2\/token@ endpoint directly
    --     to a non-person user using a combination of the client ID and client
    --     secret.
    allowedOAuthFlows :: Prelude.Maybe [OAuthFlowType],
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Amazon Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are @phone@,
    -- @email@, @openid@, and @profile@. Possible values provided by Amazon Web
    -- Services are @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Pinpoint analytics configuration necessary to collect metrics
    -- for this user pool.
    --
    -- In Amazon Web Services Regions where Amazon Pinpoint isn\'t available,
    -- user pools only support sending events to Amazon Pinpoint projects in
    -- us-east-1. In Regions where Amazon Pinpoint is available, user pools
    -- support sending events to Amazon Pinpoint projects within that same
    -- Region.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
    -- | Amazon Cognito creates a session token for each API request in an
    -- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
    -- of that session token. Your user pool native user must respond to each
    -- authentication challenge before the session expires.
    authSessionValidity :: Prelude.Maybe Prelude.Natural,
    -- | A list of allowed redirect (callback) URLs for the IdPs.
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
    -- App callback URLs such as @myapp:\/\/example@ are also supported.
    callbackURLs :: Prelude.Maybe [Prelude.Text],
    -- | The client name from the update user pool client request.
    clientName :: Prelude.Maybe Prelude.Text,
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
    -- Amazon Cognito requires HTTPS over HTTP except for @http:\/\/localhost@
    -- for testing purposes only.
    --
    -- App callback URLs such as @myapp:\/\/example@ are also supported.
    defaultRedirectURI :: Prelude.Maybe Prelude.Text,
    -- | Activates the propagation of additional user context data. For more
    -- information about propagation of user context data, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-advanced-security.html Adding advanced security to a user pool>.
    -- If you don’t include this parameter, you can\'t send device fingerprint
    -- information, including source IP address, to Amazon Cognito advanced
    -- security. You can only activate
    -- @EnablePropagateAdditionalUserContextData@ in an app client that has a
    -- client secret.
    enablePropagateAdditionalUserContextData :: Prelude.Maybe Prelude.Bool,
    -- | Activates or deactivates token revocation. For more information about
    -- revoking tokens, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
    enableTokenRevocation :: Prelude.Maybe Prelude.Bool,
    -- | The authentication flows that you want your user pool client to support.
    -- For each app client in your user pool, you can sign in your users with
    -- any combination of one or more flows, including with a user name and
    -- Secure Remote Password (SRP), a user name and password, or a custom
    -- authentication process that you define with Lambda functions.
    --
    -- If you don\'t specify a value for @ExplicitAuthFlows@, your user client
    -- supports @ALLOW_REFRESH_TOKEN_AUTH@, @ALLOW_USER_SRP_AUTH@, and
    -- @ALLOW_CUSTOM_AUTH@.
    --
    -- Valid values include:
    --
    -- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
    --     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
    --     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
    --     flow, your app passes a user name and password to Amazon Cognito in
    --     the request, instead of using the Secure Remote Password (SRP)
    --     protocol to securely transmit the password.
    --
    -- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
    --
    -- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
    --     authentication. In this flow, Amazon Cognito receives the password
    --     in the request instead of using the SRP protocol to verify
    --     passwords.
    --
    -- -   @ALLOW_USER_SRP_AUTH@: Enable SRP-based authentication.
    --
    -- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
    --
    -- In some environments, you will see the values @ADMIN_NO_SRP_AUTH@,
    -- @CUSTOM_AUTH_FLOW_ONLY@, or @USER_PASSWORD_AUTH@. You can\'t assign
    -- these legacy @ExplicitAuthFlows@ values to user pool clients at the same
    -- time as values that begin with @ALLOW_@, like @ALLOW_USER_SRP_AUTH@.
    explicitAuthFlows :: Prelude.Maybe [ExplicitAuthFlowsType],
    -- | The ID token time limit. After this limit expires, your user can\'t use
    -- their ID token. To specify the time unit for @IdTokenValidity@ as
    -- @seconds@, @minutes@, @hours@, or @days@, set a @TokenValidityUnits@
    -- value in your API request.
    --
    -- For example, when you set @IdTokenValidity@ as @10@ and
    -- @TokenValidityUnits@ as @hours@, your user can authenticate their
    -- session with their ID token for 10 hours.
    --
    -- The default time unit for @AccessTokenValidity@ in an API request is
    -- hours. /Valid range/ is displayed below in seconds.
    --
    -- If you don\'t specify otherwise in the configuration of your app client,
    -- your ID tokens are valid for one hour.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | A list of allowed logout URLs for the IdPs.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | Errors and responses that you want Amazon Cognito APIs to return during
    -- authentication, account confirmation, and password recovery when the
    -- user doesn\'t exist in the user pool. When set to @ENABLED@ and the user
    -- doesn\'t exist, authentication returns an error indicating either the
    -- username or password was incorrect. Account confirmation and password
    -- recovery return a response indicating a code was sent to a simulated
    -- destination. When set to @LEGACY@, those APIs return a
    -- @UserNotFoundException@ exception if the user doesn\'t exist in the user
    -- pool.
    --
    -- Valid values include:
    --
    -- -   @ENABLED@ - This prevents user existence-related errors.
    --
    -- -   @LEGACY@ - This represents the early behavior of Amazon Cognito
    --     where user existence related errors aren\'t prevented.
    preventUserExistenceErrors :: Prelude.Maybe PreventUserExistenceErrorTypes,
    -- | The read-only attributes of the user pool.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The refresh token time limit. After this limit expires, your user can\'t
    -- use their refresh token. To specify the time unit for
    -- @RefreshTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set
    -- a @TokenValidityUnits@ value in your API request.
    --
    -- For example, when you set @RefreshTokenValidity@ as @10@ and
    -- @TokenValidityUnits@ as @days@, your user can refresh their session and
    -- retrieve new access and ID tokens for 10 days.
    --
    -- The default time unit for @RefreshTokenValidity@ in an API request is
    -- days. You can\'t set @RefreshTokenValidity@ to 0. If you do, Amazon
    -- Cognito overrides the value with the default value of 30 days. /Valid
    -- range/ is displayed below in seconds.
    --
    -- If you don\'t specify otherwise in the configuration of your app client,
    -- your refresh tokens are valid for 30 days.
    refreshTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | A list of provider names for the IdPs that this client supports. The
    -- following are supported: @COGNITO@, @Facebook@, @Google@,
    -- @SignInWithApple@, @LoginWithAmazon@, and the names of your own SAML and
    -- OIDC providers.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The units in which the validity times are represented. The default unit
    -- for RefreshToken is days, and the default for ID and access tokens is
    -- hours.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
    -- | The writeable attributes of the user pool.
    writeAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The user pool ID for the user pool where you want to update the user
    -- pool client.
    userPoolId :: Prelude.Text,
    -- | The ID of the client associated with the user pool.
    clientId :: Data.Sensitive Prelude.Text
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
-- 'accessTokenValidity', 'updateUserPoolClient_accessTokenValidity' - The access token time limit. After this limit expires, your user can\'t
-- use their access token. To specify the time unit for
-- @AccessTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set a
-- @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @AccessTokenValidity@ to @10@ and
-- @TokenValidityUnits@ to @hours@, your user can authorize access with
-- their access token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your access tokens are valid for one hour.
--
-- 'allowedOAuthFlows', 'updateUserPoolClient_allowedOAuthFlows' - The allowed OAuth flows.
--
-- [code]
--     Use a code grant flow, which provides an authorization code as the
--     response. This code can be exchanged for access tokens with the
--     @\/oauth2\/token@ endpoint.
--
-- [implicit]
--     Issue the access token (and, optionally, ID token, based on scopes)
--     directly to your user.
--
-- [client_credentials]
--     Issue the access token from the @\/oauth2\/token@ endpoint directly
--     to a non-person user using a combination of the client ID and client
--     secret.
--
-- 'allowedOAuthFlowsUserPoolClient', 'updateUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Amazon Cognito user pools.
--
-- 'allowedOAuthScopes', 'updateUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are @phone@,
-- @email@, @openid@, and @profile@. Possible values provided by Amazon Web
-- Services are @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
--
-- 'analyticsConfiguration', 'updateUserPoolClient_analyticsConfiguration' - The Amazon Pinpoint analytics configuration necessary to collect metrics
-- for this user pool.
--
-- In Amazon Web Services Regions where Amazon Pinpoint isn\'t available,
-- user pools only support sending events to Amazon Pinpoint projects in
-- us-east-1. In Regions where Amazon Pinpoint is available, user pools
-- support sending events to Amazon Pinpoint projects within that same
-- Region.
--
-- 'authSessionValidity', 'updateUserPoolClient_authSessionValidity' - Amazon Cognito creates a session token for each API request in an
-- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
-- of that session token. Your user pool native user must respond to each
-- authentication challenge before the session expires.
--
-- 'callbackURLs', 'updateUserPoolClient_callbackURLs' - A list of allowed redirect (callback) URLs for the IdPs.
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
-- App callback URLs such as @myapp:\/\/example@ are also supported.
--
-- 'clientName', 'updateUserPoolClient_clientName' - The client name from the update user pool client request.
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
-- Amazon Cognito requires HTTPS over HTTP except for @http:\/\/localhost@
-- for testing purposes only.
--
-- App callback URLs such as @myapp:\/\/example@ are also supported.
--
-- 'enablePropagateAdditionalUserContextData', 'updateUserPoolClient_enablePropagateAdditionalUserContextData' - Activates the propagation of additional user context data. For more
-- information about propagation of user context data, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-advanced-security.html Adding advanced security to a user pool>.
-- If you don’t include this parameter, you can\'t send device fingerprint
-- information, including source IP address, to Amazon Cognito advanced
-- security. You can only activate
-- @EnablePropagateAdditionalUserContextData@ in an app client that has a
-- client secret.
--
-- 'enableTokenRevocation', 'updateUserPoolClient_enableTokenRevocation' - Activates or deactivates token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- 'explicitAuthFlows', 'updateUserPoolClient_explicitAuthFlows' - The authentication flows that you want your user pool client to support.
-- For each app client in your user pool, you can sign in your users with
-- any combination of one or more flows, including with a user name and
-- Secure Remote Password (SRP), a user name and password, or a custom
-- authentication process that you define with Lambda functions.
--
-- If you don\'t specify a value for @ExplicitAuthFlows@, your user client
-- supports @ALLOW_REFRESH_TOKEN_AUTH@, @ALLOW_USER_SRP_AUTH@, and
-- @ALLOW_CUSTOM_AUTH@.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, your app passes a user name and password to Amazon Cognito in
--     the request, instead of using the Secure Remote Password (SRP)
--     protocol to securely transmit the password.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Amazon Cognito receives the password
--     in the request instead of using the SRP protocol to verify
--     passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP-based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
--
-- In some environments, you will see the values @ADMIN_NO_SRP_AUTH@,
-- @CUSTOM_AUTH_FLOW_ONLY@, or @USER_PASSWORD_AUTH@. You can\'t assign
-- these legacy @ExplicitAuthFlows@ values to user pool clients at the same
-- time as values that begin with @ALLOW_@, like @ALLOW_USER_SRP_AUTH@.
--
-- 'idTokenValidity', 'updateUserPoolClient_idTokenValidity' - The ID token time limit. After this limit expires, your user can\'t use
-- their ID token. To specify the time unit for @IdTokenValidity@ as
-- @seconds@, @minutes@, @hours@, or @days@, set a @TokenValidityUnits@
-- value in your API request.
--
-- For example, when you set @IdTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @hours@, your user can authenticate their
-- session with their ID token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your ID tokens are valid for one hour.
--
-- 'logoutURLs', 'updateUserPoolClient_logoutURLs' - A list of allowed logout URLs for the IdPs.
--
-- 'preventUserExistenceErrors', 'updateUserPoolClient_preventUserExistenceErrors' - Errors and responses that you want Amazon Cognito APIs to return during
-- authentication, account confirmation, and password recovery when the
-- user doesn\'t exist in the user pool. When set to @ENABLED@ and the user
-- doesn\'t exist, authentication returns an error indicating either the
-- username or password was incorrect. Account confirmation and password
-- recovery return a response indicating a code was sent to a simulated
-- destination. When set to @LEGACY@, those APIs return a
-- @UserNotFoundException@ exception if the user doesn\'t exist in the user
-- pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the early behavior of Amazon Cognito
--     where user existence related errors aren\'t prevented.
--
-- 'readAttributes', 'updateUserPoolClient_readAttributes' - The read-only attributes of the user pool.
--
-- 'refreshTokenValidity', 'updateUserPoolClient_refreshTokenValidity' - The refresh token time limit. After this limit expires, your user can\'t
-- use their refresh token. To specify the time unit for
-- @RefreshTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set
-- a @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @RefreshTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @days@, your user can refresh their session and
-- retrieve new access and ID tokens for 10 days.
--
-- The default time unit for @RefreshTokenValidity@ in an API request is
-- days. You can\'t set @RefreshTokenValidity@ to 0. If you do, Amazon
-- Cognito overrides the value with the default value of 30 days. /Valid
-- range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your refresh tokens are valid for 30 days.
--
-- 'supportedIdentityProviders', 'updateUserPoolClient_supportedIdentityProviders' - A list of provider names for the IdPs that this client supports. The
-- following are supported: @COGNITO@, @Facebook@, @Google@,
-- @SignInWithApple@, @LoginWithAmazon@, and the names of your own SAML and
-- OIDC providers.
--
-- 'tokenValidityUnits', 'updateUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented. The default unit
-- for RefreshToken is days, and the default for ID and access tokens is
-- hours.
--
-- 'writeAttributes', 'updateUserPoolClient_writeAttributes' - The writeable attributes of the user pool.
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
    { accessTokenValidity =
        Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      authSessionValidity = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      clientName = Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      enablePropagateAdditionalUserContextData =
        Prelude.Nothing,
      enableTokenRevocation = Prelude.Nothing,
      explicitAuthFlows = Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      refreshTokenValidity = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      writeAttributes = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      clientId = Data._Sensitive Lens.# pClientId_
    }

-- | The access token time limit. After this limit expires, your user can\'t
-- use their access token. To specify the time unit for
-- @AccessTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set a
-- @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @AccessTokenValidity@ to @10@ and
-- @TokenValidityUnits@ to @hours@, your user can authorize access with
-- their access token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your access tokens are valid for one hour.
updateUserPoolClient_accessTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_accessTokenValidity = Lens.lens (\UpdateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {accessTokenValidity = a} :: UpdateUserPoolClient)

-- | The allowed OAuth flows.
--
-- [code]
--     Use a code grant flow, which provides an authorization code as the
--     response. This code can be exchanged for access tokens with the
--     @\/oauth2\/token@ endpoint.
--
-- [implicit]
--     Issue the access token (and, optionally, ID token, based on scopes)
--     directly to your user.
--
-- [client_credentials]
--     Issue the access token from the @\/oauth2\/token@ endpoint directly
--     to a non-person user using a combination of the client ID and client
--     secret.
updateUserPoolClient_allowedOAuthFlows :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [OAuthFlowType])
updateUserPoolClient_allowedOAuthFlows = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Amazon Cognito user pools.
updateUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Bool)
updateUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\UpdateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UpdateUserPoolClient)

-- | The allowed OAuth scopes. Possible values provided by OAuth are @phone@,
-- @email@, @openid@, and @profile@. Possible values provided by Amazon Web
-- Services are @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
updateUserPoolClient_allowedOAuthScopes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_allowedOAuthScopes = Lens.lens (\UpdateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UpdateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Pinpoint analytics configuration necessary to collect metrics
-- for this user pool.
--
-- In Amazon Web Services Regions where Amazon Pinpoint isn\'t available,
-- user pools only support sending events to Amazon Pinpoint projects in
-- us-east-1. In Regions where Amazon Pinpoint is available, user pools
-- support sending events to Amazon Pinpoint projects within that same
-- Region.
updateUserPoolClient_analyticsConfiguration :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe AnalyticsConfigurationType)
updateUserPoolClient_analyticsConfiguration = Lens.lens (\UpdateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@UpdateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: UpdateUserPoolClient)

-- | Amazon Cognito creates a session token for each API request in an
-- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
-- of that session token. Your user pool native user must respond to each
-- authentication challenge before the session expires.
updateUserPoolClient_authSessionValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_authSessionValidity = Lens.lens (\UpdateUserPoolClient' {authSessionValidity} -> authSessionValidity) (\s@UpdateUserPoolClient' {} a -> s {authSessionValidity = a} :: UpdateUserPoolClient)

-- | A list of allowed redirect (callback) URLs for the IdPs.
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
-- App callback URLs such as @myapp:\/\/example@ are also supported.
updateUserPoolClient_callbackURLs :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_callbackURLs = Lens.lens (\UpdateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@UpdateUserPoolClient' {} a -> s {callbackURLs = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The client name from the update user pool client request.
updateUserPoolClient_clientName :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Text)
updateUserPoolClient_clientName = Lens.lens (\UpdateUserPoolClient' {clientName} -> clientName) (\s@UpdateUserPoolClient' {} a -> s {clientName = a} :: UpdateUserPoolClient)

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
-- Amazon Cognito requires HTTPS over HTTP except for @http:\/\/localhost@
-- for testing purposes only.
--
-- App callback URLs such as @myapp:\/\/example@ are also supported.
updateUserPoolClient_defaultRedirectURI :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Text)
updateUserPoolClient_defaultRedirectURI = Lens.lens (\UpdateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@UpdateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: UpdateUserPoolClient)

-- | Activates the propagation of additional user context data. For more
-- information about propagation of user context data, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-advanced-security.html Adding advanced security to a user pool>.
-- If you don’t include this parameter, you can\'t send device fingerprint
-- information, including source IP address, to Amazon Cognito advanced
-- security. You can only activate
-- @EnablePropagateAdditionalUserContextData@ in an app client that has a
-- client secret.
updateUserPoolClient_enablePropagateAdditionalUserContextData :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Bool)
updateUserPoolClient_enablePropagateAdditionalUserContextData = Lens.lens (\UpdateUserPoolClient' {enablePropagateAdditionalUserContextData} -> enablePropagateAdditionalUserContextData) (\s@UpdateUserPoolClient' {} a -> s {enablePropagateAdditionalUserContextData = a} :: UpdateUserPoolClient)

-- | Activates or deactivates token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
updateUserPoolClient_enableTokenRevocation :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Bool)
updateUserPoolClient_enableTokenRevocation = Lens.lens (\UpdateUserPoolClient' {enableTokenRevocation} -> enableTokenRevocation) (\s@UpdateUserPoolClient' {} a -> s {enableTokenRevocation = a} :: UpdateUserPoolClient)

-- | The authentication flows that you want your user pool client to support.
-- For each app client in your user pool, you can sign in your users with
-- any combination of one or more flows, including with a user name and
-- Secure Remote Password (SRP), a user name and password, or a custom
-- authentication process that you define with Lambda functions.
--
-- If you don\'t specify a value for @ExplicitAuthFlows@, your user client
-- supports @ALLOW_REFRESH_TOKEN_AUTH@, @ALLOW_USER_SRP_AUTH@, and
-- @ALLOW_CUSTOM_AUTH@.
--
-- Valid values include:
--
-- -   @ALLOW_ADMIN_USER_PASSWORD_AUTH@: Enable admin based user password
--     authentication flow @ADMIN_USER_PASSWORD_AUTH@. This setting
--     replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication
--     flow, your app passes a user name and password to Amazon Cognito in
--     the request, instead of using the Secure Remote Password (SRP)
--     protocol to securely transmit the password.
--
-- -   @ALLOW_CUSTOM_AUTH@: Enable Lambda trigger based authentication.
--
-- -   @ALLOW_USER_PASSWORD_AUTH@: Enable user password-based
--     authentication. In this flow, Amazon Cognito receives the password
--     in the request instead of using the SRP protocol to verify
--     passwords.
--
-- -   @ALLOW_USER_SRP_AUTH@: Enable SRP-based authentication.
--
-- -   @ALLOW_REFRESH_TOKEN_AUTH@: Enable authflow to refresh tokens.
--
-- In some environments, you will see the values @ADMIN_NO_SRP_AUTH@,
-- @CUSTOM_AUTH_FLOW_ONLY@, or @USER_PASSWORD_AUTH@. You can\'t assign
-- these legacy @ExplicitAuthFlows@ values to user pool clients at the same
-- time as values that begin with @ALLOW_@, like @ALLOW_USER_SRP_AUTH@.
updateUserPoolClient_explicitAuthFlows :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [ExplicitAuthFlowsType])
updateUserPoolClient_explicitAuthFlows = Lens.lens (\UpdateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@UpdateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The ID token time limit. After this limit expires, your user can\'t use
-- their ID token. To specify the time unit for @IdTokenValidity@ as
-- @seconds@, @minutes@, @hours@, or @days@, set a @TokenValidityUnits@
-- value in your API request.
--
-- For example, when you set @IdTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @hours@, your user can authenticate their
-- session with their ID token for 10 hours.
--
-- The default time unit for @AccessTokenValidity@ in an API request is
-- hours. /Valid range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your ID tokens are valid for one hour.
updateUserPoolClient_idTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_idTokenValidity = Lens.lens (\UpdateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {idTokenValidity = a} :: UpdateUserPoolClient)

-- | A list of allowed logout URLs for the IdPs.
updateUserPoolClient_logoutURLs :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_logoutURLs = Lens.lens (\UpdateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@UpdateUserPoolClient' {} a -> s {logoutURLs = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | Errors and responses that you want Amazon Cognito APIs to return during
-- authentication, account confirmation, and password recovery when the
-- user doesn\'t exist in the user pool. When set to @ENABLED@ and the user
-- doesn\'t exist, authentication returns an error indicating either the
-- username or password was incorrect. Account confirmation and password
-- recovery return a response indicating a code was sent to a simulated
-- destination. When set to @LEGACY@, those APIs return a
-- @UserNotFoundException@ exception if the user doesn\'t exist in the user
-- pool.
--
-- Valid values include:
--
-- -   @ENABLED@ - This prevents user existence-related errors.
--
-- -   @LEGACY@ - This represents the early behavior of Amazon Cognito
--     where user existence related errors aren\'t prevented.
updateUserPoolClient_preventUserExistenceErrors :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe PreventUserExistenceErrorTypes)
updateUserPoolClient_preventUserExistenceErrors = Lens.lens (\UpdateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UpdateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: UpdateUserPoolClient)

-- | The read-only attributes of the user pool.
updateUserPoolClient_readAttributes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_readAttributes = Lens.lens (\UpdateUserPoolClient' {readAttributes} -> readAttributes) (\s@UpdateUserPoolClient' {} a -> s {readAttributes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The refresh token time limit. After this limit expires, your user can\'t
-- use their refresh token. To specify the time unit for
-- @RefreshTokenValidity@ as @seconds@, @minutes@, @hours@, or @days@, set
-- a @TokenValidityUnits@ value in your API request.
--
-- For example, when you set @RefreshTokenValidity@ as @10@ and
-- @TokenValidityUnits@ as @days@, your user can refresh their session and
-- retrieve new access and ID tokens for 10 days.
--
-- The default time unit for @RefreshTokenValidity@ in an API request is
-- days. You can\'t set @RefreshTokenValidity@ to 0. If you do, Amazon
-- Cognito overrides the value with the default value of 30 days. /Valid
-- range/ is displayed below in seconds.
--
-- If you don\'t specify otherwise in the configuration of your app client,
-- your refresh tokens are valid for 30 days.
updateUserPoolClient_refreshTokenValidity :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe Prelude.Natural)
updateUserPoolClient_refreshTokenValidity = Lens.lens (\UpdateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@UpdateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: UpdateUserPoolClient)

-- | A list of provider names for the IdPs that this client supports. The
-- following are supported: @COGNITO@, @Facebook@, @Google@,
-- @SignInWithApple@, @LoginWithAmazon@, and the names of your own SAML and
-- OIDC providers.
updateUserPoolClient_supportedIdentityProviders :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_supportedIdentityProviders = Lens.lens (\UpdateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UpdateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The units in which the validity times are represented. The default unit
-- for RefreshToken is days, and the default for ID and access tokens is
-- hours.
updateUserPoolClient_tokenValidityUnits :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe TokenValidityUnitsType)
updateUserPoolClient_tokenValidityUnits = Lens.lens (\UpdateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@UpdateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: UpdateUserPoolClient)

-- | The writeable attributes of the user pool.
updateUserPoolClient_writeAttributes :: Lens.Lens' UpdateUserPoolClient (Prelude.Maybe [Prelude.Text])
updateUserPoolClient_writeAttributes = Lens.lens (\UpdateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@UpdateUserPoolClient' {} a -> s {writeAttributes = a} :: UpdateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where you want to update the user
-- pool client.
updateUserPoolClient_userPoolId :: Lens.Lens' UpdateUserPoolClient Prelude.Text
updateUserPoolClient_userPoolId = Lens.lens (\UpdateUserPoolClient' {userPoolId} -> userPoolId) (\s@UpdateUserPoolClient' {} a -> s {userPoolId = a} :: UpdateUserPoolClient)

-- | The ID of the client associated with the user pool.
updateUserPoolClient_clientId :: Lens.Lens' UpdateUserPoolClient Prelude.Text
updateUserPoolClient_clientId = Lens.lens (\UpdateUserPoolClient' {clientId} -> clientId) (\s@UpdateUserPoolClient' {} a -> s {clientId = a} :: UpdateUserPoolClient) Prelude.. Data._Sensitive

instance Core.AWSRequest UpdateUserPoolClient where
  type
    AWSResponse UpdateUserPoolClient =
      UpdateUserPoolClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateUserPoolClientResponse'
            Prelude.<$> (x Data..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateUserPoolClient where
  hashWithSalt _salt UpdateUserPoolClient' {..} =
    _salt
      `Prelude.hashWithSalt` accessTokenValidity
      `Prelude.hashWithSalt` allowedOAuthFlows
      `Prelude.hashWithSalt` allowedOAuthFlowsUserPoolClient
      `Prelude.hashWithSalt` allowedOAuthScopes
      `Prelude.hashWithSalt` analyticsConfiguration
      `Prelude.hashWithSalt` authSessionValidity
      `Prelude.hashWithSalt` callbackURLs
      `Prelude.hashWithSalt` clientName
      `Prelude.hashWithSalt` defaultRedirectURI
      `Prelude.hashWithSalt` enablePropagateAdditionalUserContextData
      `Prelude.hashWithSalt` enableTokenRevocation
      `Prelude.hashWithSalt` explicitAuthFlows
      `Prelude.hashWithSalt` idTokenValidity
      `Prelude.hashWithSalt` logoutURLs
      `Prelude.hashWithSalt` preventUserExistenceErrors
      `Prelude.hashWithSalt` readAttributes
      `Prelude.hashWithSalt` refreshTokenValidity
      `Prelude.hashWithSalt` supportedIdentityProviders
      `Prelude.hashWithSalt` tokenValidityUnits
      `Prelude.hashWithSalt` writeAttributes
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` clientId

instance Prelude.NFData UpdateUserPoolClient where
  rnf UpdateUserPoolClient' {..} =
    Prelude.rnf accessTokenValidity
      `Prelude.seq` Prelude.rnf allowedOAuthFlows
      `Prelude.seq` Prelude.rnf allowedOAuthFlowsUserPoolClient
      `Prelude.seq` Prelude.rnf allowedOAuthScopes
      `Prelude.seq` Prelude.rnf analyticsConfiguration
      `Prelude.seq` Prelude.rnf authSessionValidity
      `Prelude.seq` Prelude.rnf callbackURLs
      `Prelude.seq` Prelude.rnf clientName
      `Prelude.seq` Prelude.rnf defaultRedirectURI
      `Prelude.seq` Prelude.rnf
        enablePropagateAdditionalUserContextData
      `Prelude.seq` Prelude.rnf enableTokenRevocation
      `Prelude.seq` Prelude.rnf explicitAuthFlows
      `Prelude.seq` Prelude.rnf idTokenValidity
      `Prelude.seq` Prelude.rnf logoutURLs
      `Prelude.seq` Prelude.rnf preventUserExistenceErrors
      `Prelude.seq` Prelude.rnf readAttributes
      `Prelude.seq` Prelude.rnf refreshTokenValidity
      `Prelude.seq` Prelude.rnf
        supportedIdentityProviders
      `Prelude.seq` Prelude.rnf tokenValidityUnits
      `Prelude.seq` Prelude.rnf writeAttributes
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf clientId

instance Data.ToHeaders UpdateUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.UpdateUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateUserPoolClient where
  toJSON UpdateUserPoolClient' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessTokenValidity" Data..=)
              Prelude.<$> accessTokenValidity,
            ("AllowedOAuthFlows" Data..=)
              Prelude.<$> allowedOAuthFlows,
            ("AllowedOAuthFlowsUserPoolClient" Data..=)
              Prelude.<$> allowedOAuthFlowsUserPoolClient,
            ("AllowedOAuthScopes" Data..=)
              Prelude.<$> allowedOAuthScopes,
            ("AnalyticsConfiguration" Data..=)
              Prelude.<$> analyticsConfiguration,
            ("AuthSessionValidity" Data..=)
              Prelude.<$> authSessionValidity,
            ("CallbackURLs" Data..=) Prelude.<$> callbackURLs,
            ("ClientName" Data..=) Prelude.<$> clientName,
            ("DefaultRedirectURI" Data..=)
              Prelude.<$> defaultRedirectURI,
            ("EnablePropagateAdditionalUserContextData" Data..=)
              Prelude.<$> enablePropagateAdditionalUserContextData,
            ("EnableTokenRevocation" Data..=)
              Prelude.<$> enableTokenRevocation,
            ("ExplicitAuthFlows" Data..=)
              Prelude.<$> explicitAuthFlows,
            ("IdTokenValidity" Data..=)
              Prelude.<$> idTokenValidity,
            ("LogoutURLs" Data..=) Prelude.<$> logoutURLs,
            ("PreventUserExistenceErrors" Data..=)
              Prelude.<$> preventUserExistenceErrors,
            ("ReadAttributes" Data..=)
              Prelude.<$> readAttributes,
            ("RefreshTokenValidity" Data..=)
              Prelude.<$> refreshTokenValidity,
            ("SupportedIdentityProviders" Data..=)
              Prelude.<$> supportedIdentityProviders,
            ("TokenValidityUnits" Data..=)
              Prelude.<$> tokenValidityUnits,
            ("WriteAttributes" Data..=)
              Prelude.<$> writeAttributes,
            Prelude.Just ("UserPoolId" Data..= userPoolId),
            Prelude.Just ("ClientId" Data..= clientId)
          ]
      )

instance Data.ToPath UpdateUserPoolClient where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to the request to update the
-- user pool client.
--
-- /See:/ 'newUpdateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { -- | The user pool client value from the response from the server when you
    -- request to update the user pool client.
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
-- 'userPoolClient', 'updateUserPoolClientResponse_userPoolClient' - The user pool client value from the response from the server when you
-- request to update the user pool client.
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

-- | The user pool client value from the response from the server when you
-- request to update the user pool client.
updateUserPoolClientResponse_userPoolClient :: Lens.Lens' UpdateUserPoolClientResponse (Prelude.Maybe UserPoolClientType)
updateUserPoolClientResponse_userPoolClient = Lens.lens (\UpdateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@UpdateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: UpdateUserPoolClientResponse)

-- | The response's http status code.
updateUserPoolClientResponse_httpStatus :: Lens.Lens' UpdateUserPoolClientResponse Prelude.Int
updateUserPoolClientResponse_httpStatus = Lens.lens (\UpdateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@UpdateUserPoolClientResponse' {} a -> s {httpStatus = a} :: UpdateUserPoolClientResponse)

instance Prelude.NFData UpdateUserPoolClientResponse where
  rnf UpdateUserPoolClientResponse' {..} =
    Prelude.rnf userPoolClient
      `Prelude.seq` Prelude.rnf httpStatus
