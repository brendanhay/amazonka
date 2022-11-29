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
-- Module      : Amazonka.CognitoIdentityProvider.CreateUserPoolClient
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the user pool client.
--
-- When you create a new user pool client, token revocation is
-- automatically activated. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
module Amazonka.CognitoIdentityProvider.CreateUserPoolClient
  ( -- * Creating a Request
    CreateUserPoolClient (..),
    newCreateUserPoolClient,

    -- * Request Lenses
    createUserPoolClient_authSessionValidity,
    createUserPoolClient_defaultRedirectURI,
    createUserPoolClient_accessTokenValidity,
    createUserPoolClient_explicitAuthFlows,
    createUserPoolClient_callbackURLs,
    createUserPoolClient_allowedOAuthScopes,
    createUserPoolClient_idTokenValidity,
    createUserPoolClient_allowedOAuthFlowsUserPoolClient,
    createUserPoolClient_refreshTokenValidity,
    createUserPoolClient_analyticsConfiguration,
    createUserPoolClient_preventUserExistenceErrors,
    createUserPoolClient_enablePropagateAdditionalUserContextData,
    createUserPoolClient_tokenValidityUnits,
    createUserPoolClient_generateSecret,
    createUserPoolClient_enableTokenRevocation,
    createUserPoolClient_allowedOAuthFlows,
    createUserPoolClient_writeAttributes,
    createUserPoolClient_logoutURLs,
    createUserPoolClient_supportedIdentityProviders,
    createUserPoolClient_readAttributes,
    createUserPoolClient_userPoolId,
    createUserPoolClient_clientName,

    -- * Destructuring the Response
    CreateUserPoolClientResponse (..),
    newCreateUserPoolClientResponse,

    -- * Response Lenses
    createUserPoolClientResponse_userPoolClient,
    createUserPoolClientResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to create a user pool client.
--
-- /See:/ 'newCreateUserPoolClient' smart constructor.
data CreateUserPoolClient = CreateUserPoolClient'
  { -- | Amazon Cognito creates a session token for each API request in an
    -- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
    -- of that session token. Your user pool native user must respond to each
    -- authentication challenge before the session expires.
    authSessionValidity :: Prelude.Maybe Prelude.Natural,
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
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
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
    -- App callback URLs such as myapp:\/\/example are also supported.
    callbackURLs :: Prelude.Maybe [Prelude.Text],
    -- | The allowed OAuth scopes. Possible values provided by OAuth are @phone@,
    -- @email@, @openid@, and @profile@. Possible values provided by Amazon Web
    -- Services are @aws.cognito.signin.user.admin@. Custom scopes created in
    -- Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
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
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Amazon Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
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
    -- | The user pool analytics configuration for collecting metrics and sending
    -- them to your Amazon Pinpoint campaign.
    --
    -- In Amazon Web Services Regions where Amazon Pinpoint isn\'t available,
    -- user pools only support sending events to Amazon Pinpoint projects in
    -- Amazon Web Services Region us-east-1. In Regions where Amazon Pinpoint
    -- is available, user pools support sending events to Amazon Pinpoint
    -- projects within that same Region.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
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
    -- | Activates the propagation of additional user context data. For more
    -- information about propagation of user context data, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-advanced-security.html Adding advanced security to a user pool>.
    -- If you don’t include this parameter, you can\'t send device fingerprint
    -- information, including source IP address, to Amazon Cognito advanced
    -- security. You can only activate
    -- @EnablePropagateAdditionalUserContextData@ in an app client that has a
    -- client secret.
    enablePropagateAdditionalUserContextData :: Prelude.Maybe Prelude.Bool,
    -- | The units in which the validity times are represented. The default unit
    -- for RefreshToken is days, and default for ID and access tokens are
    -- hours.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
    -- | Boolean to specify whether you want to generate a secret for the user
    -- pool client being created.
    generateSecret :: Prelude.Maybe Prelude.Bool,
    -- | Activates or deactivates token revocation. For more information about
    -- revoking tokens, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
    --
    -- If you don\'t include this parameter, token revocation is automatically
    -- activated for the new user pool client.
    enableTokenRevocation :: Prelude.Maybe Prelude.Bool,
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
    -- | The user pool attributes that the app client can write to.
    --
    -- If your app client allows users to sign in through an IdP, this array
    -- must include all attributes that you have mapped to IdP attributes.
    -- Amazon Cognito updates mapped attributes when users sign in to your
    -- application through an IdP. If your app client does not have write
    -- access to a mapped attribute, Amazon Cognito throws an error when it
    -- tries to update the attribute. For more information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying IdP Attribute Mappings for Your user pool>.
    writeAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of allowed logout URLs for the IdPs.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | A list of provider names for the identity providers (IdPs) that are
    -- supported on this client. The following are supported: @COGNITO@,
    -- @Facebook@, @Google@, @SignInWithApple@, and @LoginWithAmazon@. You can
    -- also specify the names that you configured for the SAML and OIDC IdPs in
    -- your user pool, for example @MySAMLIdP@ or @MyOIDCIdP@.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The read attributes.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The user pool ID for the user pool where you want to create a user pool
    -- client.
    userPoolId :: Prelude.Text,
    -- | The client name for the user pool client you would like to create.
    clientName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserPoolClient' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authSessionValidity', 'createUserPoolClient_authSessionValidity' - Amazon Cognito creates a session token for each API request in an
-- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
-- of that session token. Your user pool native user must respond to each
-- authentication challenge before the session expires.
--
-- 'defaultRedirectURI', 'createUserPoolClient_defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
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
-- 'accessTokenValidity', 'createUserPoolClient_accessTokenValidity' - The access token time limit. After this limit expires, your user can\'t
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
-- 'explicitAuthFlows', 'createUserPoolClient_explicitAuthFlows' - The authentication flows that you want your user pool client to support.
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
-- 'callbackURLs', 'createUserPoolClient_callbackURLs' - A list of allowed redirect (callback) URLs for the IdPs.
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
-- 'allowedOAuthScopes', 'createUserPoolClient_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are @phone@,
-- @email@, @openid@, and @profile@. Possible values provided by Amazon Web
-- Services are @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
--
-- 'idTokenValidity', 'createUserPoolClient_idTokenValidity' - The ID token time limit. After this limit expires, your user can\'t use
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
-- 'allowedOAuthFlowsUserPoolClient', 'createUserPoolClient_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Amazon Cognito user pools.
--
-- 'refreshTokenValidity', 'createUserPoolClient_refreshTokenValidity' - The refresh token time limit. After this limit expires, your user can\'t
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
-- 'analyticsConfiguration', 'createUserPoolClient_analyticsConfiguration' - The user pool analytics configuration for collecting metrics and sending
-- them to your Amazon Pinpoint campaign.
--
-- In Amazon Web Services Regions where Amazon Pinpoint isn\'t available,
-- user pools only support sending events to Amazon Pinpoint projects in
-- Amazon Web Services Region us-east-1. In Regions where Amazon Pinpoint
-- is available, user pools support sending events to Amazon Pinpoint
-- projects within that same Region.
--
-- 'preventUserExistenceErrors', 'createUserPoolClient_preventUserExistenceErrors' - Errors and responses that you want Amazon Cognito APIs to return during
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
-- 'enablePropagateAdditionalUserContextData', 'createUserPoolClient_enablePropagateAdditionalUserContextData' - Activates the propagation of additional user context data. For more
-- information about propagation of user context data, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-advanced-security.html Adding advanced security to a user pool>.
-- If you don’t include this parameter, you can\'t send device fingerprint
-- information, including source IP address, to Amazon Cognito advanced
-- security. You can only activate
-- @EnablePropagateAdditionalUserContextData@ in an app client that has a
-- client secret.
--
-- 'tokenValidityUnits', 'createUserPoolClient_tokenValidityUnits' - The units in which the validity times are represented. The default unit
-- for RefreshToken is days, and default for ID and access tokens are
-- hours.
--
-- 'generateSecret', 'createUserPoolClient_generateSecret' - Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
--
-- 'enableTokenRevocation', 'createUserPoolClient_enableTokenRevocation' - Activates or deactivates token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- If you don\'t include this parameter, token revocation is automatically
-- activated for the new user pool client.
--
-- 'allowedOAuthFlows', 'createUserPoolClient_allowedOAuthFlows' - The allowed OAuth flows.
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
-- 'writeAttributes', 'createUserPoolClient_writeAttributes' - The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an IdP, this array
-- must include all attributes that you have mapped to IdP attributes.
-- Amazon Cognito updates mapped attributes when users sign in to your
-- application through an IdP. If your app client does not have write
-- access to a mapped attribute, Amazon Cognito throws an error when it
-- tries to update the attribute. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying IdP Attribute Mappings for Your user pool>.
--
-- 'logoutURLs', 'createUserPoolClient_logoutURLs' - A list of allowed logout URLs for the IdPs.
--
-- 'supportedIdentityProviders', 'createUserPoolClient_supportedIdentityProviders' - A list of provider names for the identity providers (IdPs) that are
-- supported on this client. The following are supported: @COGNITO@,
-- @Facebook@, @Google@, @SignInWithApple@, and @LoginWithAmazon@. You can
-- also specify the names that you configured for the SAML and OIDC IdPs in
-- your user pool, for example @MySAMLIdP@ or @MyOIDCIdP@.
--
-- 'readAttributes', 'createUserPoolClient_readAttributes' - The read attributes.
--
-- 'userPoolId', 'createUserPoolClient_userPoolId' - The user pool ID for the user pool where you want to create a user pool
-- client.
--
-- 'clientName', 'createUserPoolClient_clientName' - The client name for the user pool client you would like to create.
newCreateUserPoolClient ::
  -- | 'userPoolId'
  Prelude.Text ->
  -- | 'clientName'
  Prelude.Text ->
  CreateUserPoolClient
newCreateUserPoolClient pUserPoolId_ pClientName_ =
  CreateUserPoolClient'
    { authSessionValidity =
        Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      accessTokenValidity = Prelude.Nothing,
      explicitAuthFlows = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      refreshTokenValidity = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing,
      enablePropagateAdditionalUserContextData =
        Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      generateSecret = Prelude.Nothing,
      enableTokenRevocation = Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      writeAttributes = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      userPoolId = pUserPoolId_,
      clientName = pClientName_
    }

-- | Amazon Cognito creates a session token for each API request in an
-- authentication flow. @AuthSessionValidity@ is the duration, in minutes,
-- of that session token. Your user pool native user must respond to each
-- authentication challenge before the session expires.
createUserPoolClient_authSessionValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_authSessionValidity = Lens.lens (\CreateUserPoolClient' {authSessionValidity} -> authSessionValidity) (\s@CreateUserPoolClient' {} a -> s {authSessionValidity = a} :: CreateUserPoolClient)

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
createUserPoolClient_defaultRedirectURI :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Text)
createUserPoolClient_defaultRedirectURI = Lens.lens (\CreateUserPoolClient' {defaultRedirectURI} -> defaultRedirectURI) (\s@CreateUserPoolClient' {} a -> s {defaultRedirectURI = a} :: CreateUserPoolClient)

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
createUserPoolClient_accessTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_accessTokenValidity = Lens.lens (\CreateUserPoolClient' {accessTokenValidity} -> accessTokenValidity) (\s@CreateUserPoolClient' {} a -> s {accessTokenValidity = a} :: CreateUserPoolClient)

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
createUserPoolClient_explicitAuthFlows :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [ExplicitAuthFlowsType])
createUserPoolClient_explicitAuthFlows = Lens.lens (\CreateUserPoolClient' {explicitAuthFlows} -> explicitAuthFlows) (\s@CreateUserPoolClient' {} a -> s {explicitAuthFlows = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

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
-- App callback URLs such as myapp:\/\/example are also supported.
createUserPoolClient_callbackURLs :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_callbackURLs = Lens.lens (\CreateUserPoolClient' {callbackURLs} -> callbackURLs) (\s@CreateUserPoolClient' {} a -> s {callbackURLs = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The allowed OAuth scopes. Possible values provided by OAuth are @phone@,
-- @email@, @openid@, and @profile@. Possible values provided by Amazon Web
-- Services are @aws.cognito.signin.user.admin@. Custom scopes created in
-- Resource Servers are also supported.
createUserPoolClient_allowedOAuthScopes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_allowedOAuthScopes = Lens.lens (\CreateUserPoolClient' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthScopes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

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
createUserPoolClient_idTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_idTokenValidity = Lens.lens (\CreateUserPoolClient' {idTokenValidity} -> idTokenValidity) (\s@CreateUserPoolClient' {} a -> s {idTokenValidity = a} :: CreateUserPoolClient)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Amazon Cognito user pools.
createUserPoolClient_allowedOAuthFlowsUserPoolClient :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_allowedOAuthFlowsUserPoolClient = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: CreateUserPoolClient)

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
createUserPoolClient_refreshTokenValidity :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Natural)
createUserPoolClient_refreshTokenValidity = Lens.lens (\CreateUserPoolClient' {refreshTokenValidity} -> refreshTokenValidity) (\s@CreateUserPoolClient' {} a -> s {refreshTokenValidity = a} :: CreateUserPoolClient)

-- | The user pool analytics configuration for collecting metrics and sending
-- them to your Amazon Pinpoint campaign.
--
-- In Amazon Web Services Regions where Amazon Pinpoint isn\'t available,
-- user pools only support sending events to Amazon Pinpoint projects in
-- Amazon Web Services Region us-east-1. In Regions where Amazon Pinpoint
-- is available, user pools support sending events to Amazon Pinpoint
-- projects within that same Region.
createUserPoolClient_analyticsConfiguration :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe AnalyticsConfigurationType)
createUserPoolClient_analyticsConfiguration = Lens.lens (\CreateUserPoolClient' {analyticsConfiguration} -> analyticsConfiguration) (\s@CreateUserPoolClient' {} a -> s {analyticsConfiguration = a} :: CreateUserPoolClient)

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
createUserPoolClient_preventUserExistenceErrors :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe PreventUserExistenceErrorTypes)
createUserPoolClient_preventUserExistenceErrors = Lens.lens (\CreateUserPoolClient' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@CreateUserPoolClient' {} a -> s {preventUserExistenceErrors = a} :: CreateUserPoolClient)

-- | Activates the propagation of additional user context data. For more
-- information about propagation of user context data, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pool-settings-advanced-security.html Adding advanced security to a user pool>.
-- If you don’t include this parameter, you can\'t send device fingerprint
-- information, including source IP address, to Amazon Cognito advanced
-- security. You can only activate
-- @EnablePropagateAdditionalUserContextData@ in an app client that has a
-- client secret.
createUserPoolClient_enablePropagateAdditionalUserContextData :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_enablePropagateAdditionalUserContextData = Lens.lens (\CreateUserPoolClient' {enablePropagateAdditionalUserContextData} -> enablePropagateAdditionalUserContextData) (\s@CreateUserPoolClient' {} a -> s {enablePropagateAdditionalUserContextData = a} :: CreateUserPoolClient)

-- | The units in which the validity times are represented. The default unit
-- for RefreshToken is days, and default for ID and access tokens are
-- hours.
createUserPoolClient_tokenValidityUnits :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe TokenValidityUnitsType)
createUserPoolClient_tokenValidityUnits = Lens.lens (\CreateUserPoolClient' {tokenValidityUnits} -> tokenValidityUnits) (\s@CreateUserPoolClient' {} a -> s {tokenValidityUnits = a} :: CreateUserPoolClient)

-- | Boolean to specify whether you want to generate a secret for the user
-- pool client being created.
createUserPoolClient_generateSecret :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_generateSecret = Lens.lens (\CreateUserPoolClient' {generateSecret} -> generateSecret) (\s@CreateUserPoolClient' {} a -> s {generateSecret = a} :: CreateUserPoolClient)

-- | Activates or deactivates token revocation. For more information about
-- revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- If you don\'t include this parameter, token revocation is automatically
-- activated for the new user pool client.
createUserPoolClient_enableTokenRevocation :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe Prelude.Bool)
createUserPoolClient_enableTokenRevocation = Lens.lens (\CreateUserPoolClient' {enableTokenRevocation} -> enableTokenRevocation) (\s@CreateUserPoolClient' {} a -> s {enableTokenRevocation = a} :: CreateUserPoolClient)

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
createUserPoolClient_allowedOAuthFlows :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [OAuthFlowType])
createUserPoolClient_allowedOAuthFlows = Lens.lens (\CreateUserPoolClient' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@CreateUserPoolClient' {} a -> s {allowedOAuthFlows = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The user pool attributes that the app client can write to.
--
-- If your app client allows users to sign in through an IdP, this array
-- must include all attributes that you have mapped to IdP attributes.
-- Amazon Cognito updates mapped attributes when users sign in to your
-- application through an IdP. If your app client does not have write
-- access to a mapped attribute, Amazon Cognito throws an error when it
-- tries to update the attribute. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-specifying-attribute-mapping.html Specifying IdP Attribute Mappings for Your user pool>.
createUserPoolClient_writeAttributes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_writeAttributes = Lens.lens (\CreateUserPoolClient' {writeAttributes} -> writeAttributes) (\s@CreateUserPoolClient' {} a -> s {writeAttributes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | A list of allowed logout URLs for the IdPs.
createUserPoolClient_logoutURLs :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_logoutURLs = Lens.lens (\CreateUserPoolClient' {logoutURLs} -> logoutURLs) (\s@CreateUserPoolClient' {} a -> s {logoutURLs = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | A list of provider names for the identity providers (IdPs) that are
-- supported on this client. The following are supported: @COGNITO@,
-- @Facebook@, @Google@, @SignInWithApple@, and @LoginWithAmazon@. You can
-- also specify the names that you configured for the SAML and OIDC IdPs in
-- your user pool, for example @MySAMLIdP@ or @MyOIDCIdP@.
createUserPoolClient_supportedIdentityProviders :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_supportedIdentityProviders = Lens.lens (\CreateUserPoolClient' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@CreateUserPoolClient' {} a -> s {supportedIdentityProviders = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The read attributes.
createUserPoolClient_readAttributes :: Lens.Lens' CreateUserPoolClient (Prelude.Maybe [Prelude.Text])
createUserPoolClient_readAttributes = Lens.lens (\CreateUserPoolClient' {readAttributes} -> readAttributes) (\s@CreateUserPoolClient' {} a -> s {readAttributes = a} :: CreateUserPoolClient) Prelude.. Lens.mapping Lens.coerced

-- | The user pool ID for the user pool where you want to create a user pool
-- client.
createUserPoolClient_userPoolId :: Lens.Lens' CreateUserPoolClient Prelude.Text
createUserPoolClient_userPoolId = Lens.lens (\CreateUserPoolClient' {userPoolId} -> userPoolId) (\s@CreateUserPoolClient' {} a -> s {userPoolId = a} :: CreateUserPoolClient)

-- | The client name for the user pool client you would like to create.
createUserPoolClient_clientName :: Lens.Lens' CreateUserPoolClient Prelude.Text
createUserPoolClient_clientName = Lens.lens (\CreateUserPoolClient' {clientName} -> clientName) (\s@CreateUserPoolClient' {} a -> s {clientName = a} :: CreateUserPoolClient)

instance Core.AWSRequest CreateUserPoolClient where
  type
    AWSResponse CreateUserPoolClient =
      CreateUserPoolClientResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolClientResponse'
            Prelude.<$> (x Core..?> "UserPoolClient")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPoolClient where
  hashWithSalt _salt CreateUserPoolClient' {..} =
    _salt `Prelude.hashWithSalt` authSessionValidity
      `Prelude.hashWithSalt` defaultRedirectURI
      `Prelude.hashWithSalt` accessTokenValidity
      `Prelude.hashWithSalt` explicitAuthFlows
      `Prelude.hashWithSalt` callbackURLs
      `Prelude.hashWithSalt` allowedOAuthScopes
      `Prelude.hashWithSalt` idTokenValidity
      `Prelude.hashWithSalt` allowedOAuthFlowsUserPoolClient
      `Prelude.hashWithSalt` refreshTokenValidity
      `Prelude.hashWithSalt` analyticsConfiguration
      `Prelude.hashWithSalt` preventUserExistenceErrors
      `Prelude.hashWithSalt` enablePropagateAdditionalUserContextData
      `Prelude.hashWithSalt` tokenValidityUnits
      `Prelude.hashWithSalt` generateSecret
      `Prelude.hashWithSalt` enableTokenRevocation
      `Prelude.hashWithSalt` allowedOAuthFlows
      `Prelude.hashWithSalt` writeAttributes
      `Prelude.hashWithSalt` logoutURLs
      `Prelude.hashWithSalt` supportedIdentityProviders
      `Prelude.hashWithSalt` readAttributes
      `Prelude.hashWithSalt` userPoolId
      `Prelude.hashWithSalt` clientName

instance Prelude.NFData CreateUserPoolClient where
  rnf CreateUserPoolClient' {..} =
    Prelude.rnf authSessionValidity
      `Prelude.seq` Prelude.rnf defaultRedirectURI
      `Prelude.seq` Prelude.rnf accessTokenValidity
      `Prelude.seq` Prelude.rnf explicitAuthFlows
      `Prelude.seq` Prelude.rnf callbackURLs
      `Prelude.seq` Prelude.rnf allowedOAuthScopes
      `Prelude.seq` Prelude.rnf idTokenValidity
      `Prelude.seq` Prelude.rnf allowedOAuthFlowsUserPoolClient
      `Prelude.seq` Prelude.rnf refreshTokenValidity
      `Prelude.seq` Prelude.rnf analyticsConfiguration
      `Prelude.seq` Prelude.rnf preventUserExistenceErrors
      `Prelude.seq` Prelude.rnf
        enablePropagateAdditionalUserContextData
      `Prelude.seq` Prelude.rnf tokenValidityUnits
      `Prelude.seq` Prelude.rnf generateSecret
      `Prelude.seq` Prelude.rnf enableTokenRevocation
      `Prelude.seq` Prelude.rnf allowedOAuthFlows
      `Prelude.seq` Prelude.rnf writeAttributes
      `Prelude.seq` Prelude.rnf logoutURLs
      `Prelude.seq` Prelude.rnf
        supportedIdentityProviders
      `Prelude.seq` Prelude.rnf readAttributes
      `Prelude.seq` Prelude.rnf userPoolId
      `Prelude.seq` Prelude.rnf clientName

instance Core.ToHeaders CreateUserPoolClient where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateUserPoolClient" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUserPoolClient where
  toJSON CreateUserPoolClient' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AuthSessionValidity" Core..=)
              Prelude.<$> authSessionValidity,
            ("DefaultRedirectURI" Core..=)
              Prelude.<$> defaultRedirectURI,
            ("AccessTokenValidity" Core..=)
              Prelude.<$> accessTokenValidity,
            ("ExplicitAuthFlows" Core..=)
              Prelude.<$> explicitAuthFlows,
            ("CallbackURLs" Core..=) Prelude.<$> callbackURLs,
            ("AllowedOAuthScopes" Core..=)
              Prelude.<$> allowedOAuthScopes,
            ("IdTokenValidity" Core..=)
              Prelude.<$> idTokenValidity,
            ("AllowedOAuthFlowsUserPoolClient" Core..=)
              Prelude.<$> allowedOAuthFlowsUserPoolClient,
            ("RefreshTokenValidity" Core..=)
              Prelude.<$> refreshTokenValidity,
            ("AnalyticsConfiguration" Core..=)
              Prelude.<$> analyticsConfiguration,
            ("PreventUserExistenceErrors" Core..=)
              Prelude.<$> preventUserExistenceErrors,
            ("EnablePropagateAdditionalUserContextData" Core..=)
              Prelude.<$> enablePropagateAdditionalUserContextData,
            ("TokenValidityUnits" Core..=)
              Prelude.<$> tokenValidityUnits,
            ("GenerateSecret" Core..=)
              Prelude.<$> generateSecret,
            ("EnableTokenRevocation" Core..=)
              Prelude.<$> enableTokenRevocation,
            ("AllowedOAuthFlows" Core..=)
              Prelude.<$> allowedOAuthFlows,
            ("WriteAttributes" Core..=)
              Prelude.<$> writeAttributes,
            ("LogoutURLs" Core..=) Prelude.<$> logoutURLs,
            ("SupportedIdentityProviders" Core..=)
              Prelude.<$> supportedIdentityProviders,
            ("ReadAttributes" Core..=)
              Prelude.<$> readAttributes,
            Prelude.Just ("UserPoolId" Core..= userPoolId),
            Prelude.Just ("ClientName" Core..= clientName)
          ]
      )

instance Core.ToPath CreateUserPoolClient where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUserPoolClient where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server to create a user pool client.
--
-- /See:/ 'newCreateUserPoolClientResponse' smart constructor.
data CreateUserPoolClientResponse = CreateUserPoolClientResponse'
  { -- | The user pool client that was just created.
    userPoolClient :: Prelude.Maybe UserPoolClientType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserPoolClientResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolClient', 'createUserPoolClientResponse_userPoolClient' - The user pool client that was just created.
--
-- 'httpStatus', 'createUserPoolClientResponse_httpStatus' - The response's http status code.
newCreateUserPoolClientResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserPoolClientResponse
newCreateUserPoolClientResponse pHttpStatus_ =
  CreateUserPoolClientResponse'
    { userPoolClient =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The user pool client that was just created.
createUserPoolClientResponse_userPoolClient :: Lens.Lens' CreateUserPoolClientResponse (Prelude.Maybe UserPoolClientType)
createUserPoolClientResponse_userPoolClient = Lens.lens (\CreateUserPoolClientResponse' {userPoolClient} -> userPoolClient) (\s@CreateUserPoolClientResponse' {} a -> s {userPoolClient = a} :: CreateUserPoolClientResponse)

-- | The response's http status code.
createUserPoolClientResponse_httpStatus :: Lens.Lens' CreateUserPoolClientResponse Prelude.Int
createUserPoolClientResponse_httpStatus = Lens.lens (\CreateUserPoolClientResponse' {httpStatus} -> httpStatus) (\s@CreateUserPoolClientResponse' {} a -> s {httpStatus = a} :: CreateUserPoolClientResponse)

instance Prelude.NFData CreateUserPoolClientResponse where
  rnf CreateUserPoolClientResponse' {..} =
    Prelude.rnf userPoolClient
      `Prelude.seq` Prelude.rnf httpStatus
