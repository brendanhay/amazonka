{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType where

import Network.AWS.CognitoIdentityProvider.Types.AnalyticsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.ExplicitAuthFlowsType
import Network.AWS.CognitoIdentityProvider.Types.OAuthFlowType
import Network.AWS.CognitoIdentityProvider.Types.PreventUserExistenceErrorTypes
import Network.AWS.CognitoIdentityProvider.Types.TokenValidityUnitsType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a user pool client.
--
-- /See:/ 'newUserPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
  { -- | The date the user pool client was last modified.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The client secret from the user pool request of the client type.
    clientSecret :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours,
    -- after which the refresh token is no longer valid and cannot be used.
    idTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The time limit, in days, after which the refresh token is no longer
    -- valid and cannot be used.
    refreshTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the client associated with the user pool.
    clientId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The client name from the user pool request of the client type.
    clientName :: Prelude.Maybe Prelude.Text,
    -- | The allowed OAuth scopes. Possible values provided by OAuth are:
    -- @phone@, @email@, @openid@, and @profile@. Possible values provided by
    -- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
    -- created in Resource Servers are also supported.
    allowedOAuthScopes :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Pinpoint analytics configuration for the user pool client.
    --
    -- Cognito User Pools only supports sending events to Amazon Pinpoint
    -- projects in the US East (N. Virginia) us-east-1 Region, regardless of
    -- the region in which the user pool resides.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfigurationType,
    -- | The user pool ID for the user pool client.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The date the user pool client was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The Read-only attributes.
    readAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of provider names for the identity providers that are supported
    -- on this client.
    supportedIdentityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The writeable attributes.
    writeAttributes :: Prelude.Maybe [Prelude.Text],
    -- | A list of allowed logout URLs for the identity providers.
    logoutURLs :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether token revocation is enabled for the user pool client.
    -- When you create a new user pool client, token revocation is enabled by
    -- default. For more information about revoking tokens, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
    enableTokenRevocation :: Prelude.Maybe Prelude.Bool,
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
    -- | The time units used to specify the token validity times of their
    -- respective token.
    tokenValidityUnits :: Prelude.Maybe TokenValidityUnitsType,
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
    -- | The time limit, specified by tokenValidityUnits, defaulting to hours,
    -- after which the access token is no longer valid and cannot be used.
    accessTokenValidity :: Prelude.Maybe Prelude.Natural,
    -- | Set to true if the client is allowed to follow the OAuth protocol when
    -- interacting with Cognito user pools.
    allowedOAuthFlowsUserPoolClient :: Prelude.Maybe Prelude.Bool,
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
    preventUserExistenceErrors :: Prelude.Maybe PreventUserExistenceErrorTypes
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolClientType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'userPoolClientType_lastModifiedDate' - The date the user pool client was last modified.
--
-- 'clientSecret', 'userPoolClientType_clientSecret' - The client secret from the user pool request of the client type.
--
-- 'idTokenValidity', 'userPoolClientType_idTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the refresh token is no longer valid and cannot be used.
--
-- 'refreshTokenValidity', 'userPoolClientType_refreshTokenValidity' - The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
--
-- 'clientId', 'userPoolClientType_clientId' - The ID of the client associated with the user pool.
--
-- 'clientName', 'userPoolClientType_clientName' - The client name from the user pool request of the client type.
--
-- 'allowedOAuthScopes', 'userPoolClientType_allowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
-- created in Resource Servers are also supported.
--
-- 'analyticsConfiguration', 'userPoolClientType_analyticsConfiguration' - The Amazon Pinpoint analytics configuration for the user pool client.
--
-- Cognito User Pools only supports sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the region in which the user pool resides.
--
-- 'userPoolId', 'userPoolClientType_userPoolId' - The user pool ID for the user pool client.
--
-- 'creationDate', 'userPoolClientType_creationDate' - The date the user pool client was created.
--
-- 'readAttributes', 'userPoolClientType_readAttributes' - The Read-only attributes.
--
-- 'supportedIdentityProviders', 'userPoolClientType_supportedIdentityProviders' - A list of provider names for the identity providers that are supported
-- on this client.
--
-- 'writeAttributes', 'userPoolClientType_writeAttributes' - The writeable attributes.
--
-- 'logoutURLs', 'userPoolClientType_logoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- 'enableTokenRevocation', 'userPoolClientType_enableTokenRevocation' - Indicates whether token revocation is enabled for the user pool client.
-- When you create a new user pool client, token revocation is enabled by
-- default. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
--
-- 'defaultRedirectURI', 'userPoolClientType_defaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
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
-- 'explicitAuthFlows', 'userPoolClientType_explicitAuthFlows' - The authentication flows that are supported by the user pool clients.
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
-- 'tokenValidityUnits', 'userPoolClientType_tokenValidityUnits' - The time units used to specify the token validity times of their
-- respective token.
--
-- 'callbackURLs', 'userPoolClientType_callbackURLs' - A list of allowed redirect (callback) URLs for the identity providers.
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
-- 'allowedOAuthFlows', 'userPoolClientType_allowedOAuthFlows' - The allowed OAuth flows.
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
-- 'accessTokenValidity', 'userPoolClientType_accessTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the access token is no longer valid and cannot be used.
--
-- 'allowedOAuthFlowsUserPoolClient', 'userPoolClientType_allowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
--
-- 'preventUserExistenceErrors', 'userPoolClientType_preventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by
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
newUserPoolClientType ::
  UserPoolClientType
newUserPoolClientType =
  UserPoolClientType'
    { lastModifiedDate =
        Prelude.Nothing,
      clientSecret = Prelude.Nothing,
      idTokenValidity = Prelude.Nothing,
      refreshTokenValidity = Prelude.Nothing,
      clientId = Prelude.Nothing,
      clientName = Prelude.Nothing,
      allowedOAuthScopes = Prelude.Nothing,
      analyticsConfiguration = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      readAttributes = Prelude.Nothing,
      supportedIdentityProviders = Prelude.Nothing,
      writeAttributes = Prelude.Nothing,
      logoutURLs = Prelude.Nothing,
      enableTokenRevocation = Prelude.Nothing,
      defaultRedirectURI = Prelude.Nothing,
      explicitAuthFlows = Prelude.Nothing,
      tokenValidityUnits = Prelude.Nothing,
      callbackURLs = Prelude.Nothing,
      allowedOAuthFlows = Prelude.Nothing,
      accessTokenValidity = Prelude.Nothing,
      allowedOAuthFlowsUserPoolClient = Prelude.Nothing,
      preventUserExistenceErrors = Prelude.Nothing
    }

-- | The date the user pool client was last modified.
userPoolClientType_lastModifiedDate :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.UTCTime)
userPoolClientType_lastModifiedDate = Lens.lens (\UserPoolClientType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolClientType' {} a -> s {lastModifiedDate = a} :: UserPoolClientType) Prelude.. Lens.mapping Core._Time

-- | The client secret from the user pool request of the client type.
userPoolClientType_clientSecret :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_clientSecret = Lens.lens (\UserPoolClientType' {clientSecret} -> clientSecret) (\s@UserPoolClientType' {} a -> s {clientSecret = a} :: UserPoolClientType) Prelude.. Lens.mapping Core._Sensitive

-- | The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the refresh token is no longer valid and cannot be used.
userPoolClientType_idTokenValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_idTokenValidity = Lens.lens (\UserPoolClientType' {idTokenValidity} -> idTokenValidity) (\s@UserPoolClientType' {} a -> s {idTokenValidity = a} :: UserPoolClientType)

-- | The time limit, in days, after which the refresh token is no longer
-- valid and cannot be used.
userPoolClientType_refreshTokenValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_refreshTokenValidity = Lens.lens (\UserPoolClientType' {refreshTokenValidity} -> refreshTokenValidity) (\s@UserPoolClientType' {} a -> s {refreshTokenValidity = a} :: UserPoolClientType)

-- | The ID of the client associated with the user pool.
userPoolClientType_clientId :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_clientId = Lens.lens (\UserPoolClientType' {clientId} -> clientId) (\s@UserPoolClientType' {} a -> s {clientId = a} :: UserPoolClientType) Prelude.. Lens.mapping Core._Sensitive

-- | The client name from the user pool request of the client type.
userPoolClientType_clientName :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_clientName = Lens.lens (\UserPoolClientType' {clientName} -> clientName) (\s@UserPoolClientType' {} a -> s {clientName = a} :: UserPoolClientType)

-- | The allowed OAuth scopes. Possible values provided by OAuth are:
-- @phone@, @email@, @openid@, and @profile@. Possible values provided by
-- Amazon Web Services are: @aws.cognito.signin.user.admin@. Custom scopes
-- created in Resource Servers are also supported.
userPoolClientType_allowedOAuthScopes :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_allowedOAuthScopes = Lens.lens (\UserPoolClientType' {allowedOAuthScopes} -> allowedOAuthScopes) (\s@UserPoolClientType' {} a -> s {allowedOAuthScopes = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Pinpoint analytics configuration for the user pool client.
--
-- Cognito User Pools only supports sending events to Amazon Pinpoint
-- projects in the US East (N. Virginia) us-east-1 Region, regardless of
-- the region in which the user pool resides.
userPoolClientType_analyticsConfiguration :: Lens.Lens' UserPoolClientType (Prelude.Maybe AnalyticsConfigurationType)
userPoolClientType_analyticsConfiguration = Lens.lens (\UserPoolClientType' {analyticsConfiguration} -> analyticsConfiguration) (\s@UserPoolClientType' {} a -> s {analyticsConfiguration = a} :: UserPoolClientType)

-- | The user pool ID for the user pool client.
userPoolClientType_userPoolId :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_userPoolId = Lens.lens (\UserPoolClientType' {userPoolId} -> userPoolId) (\s@UserPoolClientType' {} a -> s {userPoolId = a} :: UserPoolClientType)

-- | The date the user pool client was created.
userPoolClientType_creationDate :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.UTCTime)
userPoolClientType_creationDate = Lens.lens (\UserPoolClientType' {creationDate} -> creationDate) (\s@UserPoolClientType' {} a -> s {creationDate = a} :: UserPoolClientType) Prelude.. Lens.mapping Core._Time

-- | The Read-only attributes.
userPoolClientType_readAttributes :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_readAttributes = Lens.lens (\UserPoolClientType' {readAttributes} -> readAttributes) (\s@UserPoolClientType' {} a -> s {readAttributes = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | A list of provider names for the identity providers that are supported
-- on this client.
userPoolClientType_supportedIdentityProviders :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_supportedIdentityProviders = Lens.lens (\UserPoolClientType' {supportedIdentityProviders} -> supportedIdentityProviders) (\s@UserPoolClientType' {} a -> s {supportedIdentityProviders = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | The writeable attributes.
userPoolClientType_writeAttributes :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_writeAttributes = Lens.lens (\UserPoolClientType' {writeAttributes} -> writeAttributes) (\s@UserPoolClientType' {} a -> s {writeAttributes = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | A list of allowed logout URLs for the identity providers.
userPoolClientType_logoutURLs :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_logoutURLs = Lens.lens (\UserPoolClientType' {logoutURLs} -> logoutURLs) (\s@UserPoolClientType' {} a -> s {logoutURLs = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether token revocation is enabled for the user pool client.
-- When you create a new user pool client, token revocation is enabled by
-- default. For more information about revoking tokens, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_RevokeToken.html RevokeToken>.
userPoolClientType_enableTokenRevocation :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Bool)
userPoolClientType_enableTokenRevocation = Lens.lens (\UserPoolClientType' {enableTokenRevocation} -> enableTokenRevocation) (\s@UserPoolClientType' {} a -> s {enableTokenRevocation = a} :: UserPoolClientType)

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
userPoolClientType_defaultRedirectURI :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Text)
userPoolClientType_defaultRedirectURI = Lens.lens (\UserPoolClientType' {defaultRedirectURI} -> defaultRedirectURI) (\s@UserPoolClientType' {} a -> s {defaultRedirectURI = a} :: UserPoolClientType)

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
userPoolClientType_explicitAuthFlows :: Lens.Lens' UserPoolClientType (Prelude.Maybe [ExplicitAuthFlowsType])
userPoolClientType_explicitAuthFlows = Lens.lens (\UserPoolClientType' {explicitAuthFlows} -> explicitAuthFlows) (\s@UserPoolClientType' {} a -> s {explicitAuthFlows = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | The time units used to specify the token validity times of their
-- respective token.
userPoolClientType_tokenValidityUnits :: Lens.Lens' UserPoolClientType (Prelude.Maybe TokenValidityUnitsType)
userPoolClientType_tokenValidityUnits = Lens.lens (\UserPoolClientType' {tokenValidityUnits} -> tokenValidityUnits) (\s@UserPoolClientType' {} a -> s {tokenValidityUnits = a} :: UserPoolClientType)

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
userPoolClientType_callbackURLs :: Lens.Lens' UserPoolClientType (Prelude.Maybe [Prelude.Text])
userPoolClientType_callbackURLs = Lens.lens (\UserPoolClientType' {callbackURLs} -> callbackURLs) (\s@UserPoolClientType' {} a -> s {callbackURLs = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

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
userPoolClientType_allowedOAuthFlows :: Lens.Lens' UserPoolClientType (Prelude.Maybe [OAuthFlowType])
userPoolClientType_allowedOAuthFlows = Lens.lens (\UserPoolClientType' {allowedOAuthFlows} -> allowedOAuthFlows) (\s@UserPoolClientType' {} a -> s {allowedOAuthFlows = a} :: UserPoolClientType) Prelude.. Lens.mapping Lens._Coerce

-- | The time limit, specified by tokenValidityUnits, defaulting to hours,
-- after which the access token is no longer valid and cannot be used.
userPoolClientType_accessTokenValidity :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Natural)
userPoolClientType_accessTokenValidity = Lens.lens (\UserPoolClientType' {accessTokenValidity} -> accessTokenValidity) (\s@UserPoolClientType' {} a -> s {accessTokenValidity = a} :: UserPoolClientType)

-- | Set to true if the client is allowed to follow the OAuth protocol when
-- interacting with Cognito user pools.
userPoolClientType_allowedOAuthFlowsUserPoolClient :: Lens.Lens' UserPoolClientType (Prelude.Maybe Prelude.Bool)
userPoolClientType_allowedOAuthFlowsUserPoolClient = Lens.lens (\UserPoolClientType' {allowedOAuthFlowsUserPoolClient} -> allowedOAuthFlowsUserPoolClient) (\s@UserPoolClientType' {} a -> s {allowedOAuthFlowsUserPoolClient = a} :: UserPoolClientType)

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
userPoolClientType_preventUserExistenceErrors :: Lens.Lens' UserPoolClientType (Prelude.Maybe PreventUserExistenceErrorTypes)
userPoolClientType_preventUserExistenceErrors = Lens.lens (\UserPoolClientType' {preventUserExistenceErrors} -> preventUserExistenceErrors) (\s@UserPoolClientType' {} a -> s {preventUserExistenceErrors = a} :: UserPoolClientType)

instance Core.FromJSON UserPoolClientType where
  parseJSON =
    Core.withObject
      "UserPoolClientType"
      ( \x ->
          UserPoolClientType'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "ClientSecret")
            Prelude.<*> (x Core..:? "IdTokenValidity")
            Prelude.<*> (x Core..:? "RefreshTokenValidity")
            Prelude.<*> (x Core..:? "ClientId")
            Prelude.<*> (x Core..:? "ClientName")
            Prelude.<*> ( x Core..:? "AllowedOAuthScopes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AnalyticsConfiguration")
            Prelude.<*> (x Core..:? "UserPoolId")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "ReadAttributes" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "SupportedIdentityProviders"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "WriteAttributes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "LogoutURLs" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EnableTokenRevocation")
            Prelude.<*> (x Core..:? "DefaultRedirectURI")
            Prelude.<*> ( x Core..:? "ExplicitAuthFlows"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "TokenValidityUnits")
            Prelude.<*> (x Core..:? "CallbackURLs" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AllowedOAuthFlows"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AccessTokenValidity")
            Prelude.<*> (x Core..:? "AllowedOAuthFlowsUserPoolClient")
            Prelude.<*> (x Core..:? "PreventUserExistenceErrors")
      )

instance Prelude.Hashable UserPoolClientType

instance Prelude.NFData UserPoolClientType
