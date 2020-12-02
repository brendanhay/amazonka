{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientType
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a user pool client.
--
--
--
-- /See:/ 'userPoolClientType' smart constructor.
data UserPoolClientType = UserPoolClientType'
  { _upctRefreshTokenValidity ::
      !(Maybe Nat),
    _upctClientId :: !(Maybe (Sensitive Text)),
    _upctExplicitAuthFlows ::
      !(Maybe [ExplicitAuthFlowsType]),
    _upctClientSecret :: !(Maybe (Sensitive Text)),
    _upctLastModifiedDate :: !(Maybe POSIX),
    _upctSupportedIdentityProviders :: !(Maybe [Text]),
    _upctLogoutURLs :: !(Maybe [Text]),
    _upctAllowedOAuthFlowsUserPoolClient :: !(Maybe Bool),
    _upctUserPoolId :: !(Maybe Text),
    _upctIdTokenValidity :: !(Maybe Nat),
    _upctTokenValidityUnits ::
      !(Maybe TokenValidityUnitsType),
    _upctDefaultRedirectURI :: !(Maybe Text),
    _upctWriteAttributes :: !(Maybe [Text]),
    _upctPreventUserExistenceErrors ::
      !(Maybe PreventUserExistenceErrorTypes),
    _upctAccessTokenValidity :: !(Maybe Nat),
    _upctCreationDate :: !(Maybe POSIX),
    _upctReadAttributes :: !(Maybe [Text]),
    _upctAllowedOAuthScopes :: !(Maybe [Text]),
    _upctAllowedOAuthFlows :: !(Maybe [OAuthFlowType]),
    _upctAnalyticsConfiguration ::
      !(Maybe AnalyticsConfigurationType),
    _upctClientName :: !(Maybe Text),
    _upctCallbackURLs :: !(Maybe [Text])
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPoolClientType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'upctRefreshTokenValidity' - The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- * 'upctClientId' - The ID of the client associated with the user pool.
--
-- * 'upctExplicitAuthFlows' - The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix. Valid values include:     * @ALLOW_ADMIN_USER_PASSWORD_AUTH@ : Enable admin based user password authentication flow @ADMIN_USER_PASSWORD_AUTH@ . This setting replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication flow, Cognito receives the password in the request instead of using the SRP (Secure Remote Password protocol) protocol to verify passwords.     * @ALLOW_CUSTOM_AUTH@ : Enable Lambda trigger based authentication.     * @ALLOW_USER_PASSWORD_AUTH@ : Enable user password-based authentication. In this flow, Cognito receives the password in the request instead of using the SRP protocol to verify passwords.     * @ALLOW_USER_SRP_AUTH@ : Enable SRP based authentication.     * @ALLOW_REFRESH_TOKEN_AUTH@ : Enable authflow to refresh tokens.
--
-- * 'upctClientSecret' - The client secret from the user pool request of the client type.
--
-- * 'upctLastModifiedDate' - The date the user pool client was last modified.
--
-- * 'upctSupportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client.
--
-- * 'upctLogoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- * 'upctAllowedOAuthFlowsUserPoolClient' - Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- * 'upctUserPoolId' - The user pool ID for the user pool client.
--
-- * 'upctIdTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
--
-- * 'upctTokenValidityUnits' - The time units used to specify the token validity times of their respective token.
--
-- * 'upctDefaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
--
-- * 'upctWriteAttributes' - The writeable attributes.
--
-- * 'upctPreventUserExistenceErrors' - Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool. Valid values include:     * @ENABLED@ - This prevents user existence-related errors.     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
--
-- * 'upctAccessTokenValidity' - The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
--
-- * 'upctCreationDate' - The date the user pool client was created.
--
-- * 'upctReadAttributes' - The Read-only attributes.
--
-- * 'upctAllowedOAuthScopes' - The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
--
-- * 'upctAllowedOAuthFlows' - The allowed OAuth flows. Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly. Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
--
-- * 'upctAnalyticsConfiguration' - The Amazon Pinpoint analytics configuration for the user pool client.
--
-- * 'upctClientName' - The client name from the user pool request of the client type.
--
-- * 'upctCallbackURLs' - A list of allowed redirect (callback) URLs for the identity providers. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
userPoolClientType ::
  UserPoolClientType
userPoolClientType =
  UserPoolClientType'
    { _upctRefreshTokenValidity = Nothing,
      _upctClientId = Nothing,
      _upctExplicitAuthFlows = Nothing,
      _upctClientSecret = Nothing,
      _upctLastModifiedDate = Nothing,
      _upctSupportedIdentityProviders = Nothing,
      _upctLogoutURLs = Nothing,
      _upctAllowedOAuthFlowsUserPoolClient = Nothing,
      _upctUserPoolId = Nothing,
      _upctIdTokenValidity = Nothing,
      _upctTokenValidityUnits = Nothing,
      _upctDefaultRedirectURI = Nothing,
      _upctWriteAttributes = Nothing,
      _upctPreventUserExistenceErrors = Nothing,
      _upctAccessTokenValidity = Nothing,
      _upctCreationDate = Nothing,
      _upctReadAttributes = Nothing,
      _upctAllowedOAuthScopes = Nothing,
      _upctAllowedOAuthFlows = Nothing,
      _upctAnalyticsConfiguration = Nothing,
      _upctClientName = Nothing,
      _upctCallbackURLs = Nothing
    }

-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
upctRefreshTokenValidity :: Lens' UserPoolClientType (Maybe Natural)
upctRefreshTokenValidity = lens _upctRefreshTokenValidity (\s a -> s {_upctRefreshTokenValidity = a}) . mapping _Nat

-- | The ID of the client associated with the user pool.
upctClientId :: Lens' UserPoolClientType (Maybe Text)
upctClientId = lens _upctClientId (\s a -> s {_upctClientId = a}) . mapping _Sensitive

-- | The authentication flows that are supported by the user pool clients. Flow names without the @ALLOW_@ prefix are deprecated in favor of new names with the @ALLOW_@ prefix. Note that values with @ALLOW_@ prefix cannot be used along with values without @ALLOW_@ prefix. Valid values include:     * @ALLOW_ADMIN_USER_PASSWORD_AUTH@ : Enable admin based user password authentication flow @ADMIN_USER_PASSWORD_AUTH@ . This setting replaces the @ADMIN_NO_SRP_AUTH@ setting. With this authentication flow, Cognito receives the password in the request instead of using the SRP (Secure Remote Password protocol) protocol to verify passwords.     * @ALLOW_CUSTOM_AUTH@ : Enable Lambda trigger based authentication.     * @ALLOW_USER_PASSWORD_AUTH@ : Enable user password-based authentication. In this flow, Cognito receives the password in the request instead of using the SRP protocol to verify passwords.     * @ALLOW_USER_SRP_AUTH@ : Enable SRP based authentication.     * @ALLOW_REFRESH_TOKEN_AUTH@ : Enable authflow to refresh tokens.
upctExplicitAuthFlows :: Lens' UserPoolClientType [ExplicitAuthFlowsType]
upctExplicitAuthFlows = lens _upctExplicitAuthFlows (\s a -> s {_upctExplicitAuthFlows = a}) . _Default . _Coerce

-- | The client secret from the user pool request of the client type.
upctClientSecret :: Lens' UserPoolClientType (Maybe Text)
upctClientSecret = lens _upctClientSecret (\s a -> s {_upctClientSecret = a}) . mapping _Sensitive

-- | The date the user pool client was last modified.
upctLastModifiedDate :: Lens' UserPoolClientType (Maybe UTCTime)
upctLastModifiedDate = lens _upctLastModifiedDate (\s a -> s {_upctLastModifiedDate = a}) . mapping _Time

-- | A list of provider names for the identity providers that are supported on this client.
upctSupportedIdentityProviders :: Lens' UserPoolClientType [Text]
upctSupportedIdentityProviders = lens _upctSupportedIdentityProviders (\s a -> s {_upctSupportedIdentityProviders = a}) . _Default . _Coerce

-- | A list of allowed logout URLs for the identity providers.
upctLogoutURLs :: Lens' UserPoolClientType [Text]
upctLogoutURLs = lens _upctLogoutURLs (\s a -> s {_upctLogoutURLs = a}) . _Default . _Coerce

-- | Set to true if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
upctAllowedOAuthFlowsUserPoolClient :: Lens' UserPoolClientType (Maybe Bool)
upctAllowedOAuthFlowsUserPoolClient = lens _upctAllowedOAuthFlowsUserPoolClient (\s a -> s {_upctAllowedOAuthFlowsUserPoolClient = a})

-- | The user pool ID for the user pool client.
upctUserPoolId :: Lens' UserPoolClientType (Maybe Text)
upctUserPoolId = lens _upctUserPoolId (\s a -> s {_upctUserPoolId = a})

-- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the refresh token is no longer valid and cannot be used.
upctIdTokenValidity :: Lens' UserPoolClientType (Maybe Natural)
upctIdTokenValidity = lens _upctIdTokenValidity (\s a -> s {_upctIdTokenValidity = a}) . mapping _Nat

-- | The time units used to specify the token validity times of their respective token.
upctTokenValidityUnits :: Lens' UserPoolClientType (Maybe TokenValidityUnitsType)
upctTokenValidityUnits = lens _upctTokenValidityUnits (\s a -> s {_upctTokenValidityUnits = a})

-- | The default redirect URI. Must be in the @CallbackURLs@ list. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
upctDefaultRedirectURI :: Lens' UserPoolClientType (Maybe Text)
upctDefaultRedirectURI = lens _upctDefaultRedirectURI (\s a -> s {_upctDefaultRedirectURI = a})

-- | The writeable attributes.
upctWriteAttributes :: Lens' UserPoolClientType [Text]
upctWriteAttributes = lens _upctWriteAttributes (\s a -> s {_upctWriteAttributes = a}) . _Default . _Coerce

-- | Use this setting to choose which errors and responses are returned by Cognito APIs during authentication, account confirmation, and password recovery when the user does not exist in the user pool. When set to @ENABLED@ and the user does not exist, authentication returns an error indicating either the username or password was incorrect, and account confirmation and password recovery return a response indicating a code was sent to a simulated destination. When set to @LEGACY@ , those APIs will return a @UserNotFoundException@ exception if the user does not exist in the user pool. Valid values include:     * @ENABLED@ - This prevents user existence-related errors.     * @LEGACY@ - This represents the old behavior of Cognito where user existence related errors are not prevented.
upctPreventUserExistenceErrors :: Lens' UserPoolClientType (Maybe PreventUserExistenceErrorTypes)
upctPreventUserExistenceErrors = lens _upctPreventUserExistenceErrors (\s a -> s {_upctPreventUserExistenceErrors = a})

-- | The time limit, specified by tokenValidityUnits, defaulting to hours, after which the access token is no longer valid and cannot be used.
upctAccessTokenValidity :: Lens' UserPoolClientType (Maybe Natural)
upctAccessTokenValidity = lens _upctAccessTokenValidity (\s a -> s {_upctAccessTokenValidity = a}) . mapping _Nat

-- | The date the user pool client was created.
upctCreationDate :: Lens' UserPoolClientType (Maybe UTCTime)
upctCreationDate = lens _upctCreationDate (\s a -> s {_upctCreationDate = a}) . mapping _Time

-- | The Read-only attributes.
upctReadAttributes :: Lens' UserPoolClientType [Text]
upctReadAttributes = lens _upctReadAttributes (\s a -> s {_upctReadAttributes = a}) . _Default . _Coerce

-- | The allowed OAuth scopes. Possible values provided by OAuth are: @phone@ , @email@ , @openid@ , and @profile@ . Possible values provided by AWS are: @aws.cognito.signin.user.admin@ . Custom scopes created in Resource Servers are also supported.
upctAllowedOAuthScopes :: Lens' UserPoolClientType [Text]
upctAllowedOAuthScopes = lens _upctAllowedOAuthScopes (\s a -> s {_upctAllowedOAuthScopes = a}) . _Default . _Coerce

-- | The allowed OAuth flows. Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @implicit@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly. Set to @client_credentials@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) from the token endpoint using a combination of client and client_secret.
upctAllowedOAuthFlows :: Lens' UserPoolClientType [OAuthFlowType]
upctAllowedOAuthFlows = lens _upctAllowedOAuthFlows (\s a -> s {_upctAllowedOAuthFlows = a}) . _Default . _Coerce

-- | The Amazon Pinpoint analytics configuration for the user pool client.
upctAnalyticsConfiguration :: Lens' UserPoolClientType (Maybe AnalyticsConfigurationType)
upctAnalyticsConfiguration = lens _upctAnalyticsConfiguration (\s a -> s {_upctAnalyticsConfiguration = a})

-- | The client name from the user pool request of the client type.
upctClientName :: Lens' UserPoolClientType (Maybe Text)
upctClientName = lens _upctClientName (\s a -> s {_upctClientName = a})

-- | A list of allowed redirect (callback) URLs for the identity providers. A redirect URI must:     * Be an absolute URI.     * Be registered with the authorization server.     * Not include a fragment component. See <https://tools.ietf.org/html/rfc6749#section-3.1.2 OAuth 2.0 - Redirection Endpoint> . Amazon Cognito requires HTTPS over HTTP except for http://localhost for testing purposes only. App callback URLs such as myapp://example are also supported.
upctCallbackURLs :: Lens' UserPoolClientType [Text]
upctCallbackURLs = lens _upctCallbackURLs (\s a -> s {_upctCallbackURLs = a}) . _Default . _Coerce

instance FromJSON UserPoolClientType where
  parseJSON =
    withObject
      "UserPoolClientType"
      ( \x ->
          UserPoolClientType'
            <$> (x .:? "RefreshTokenValidity")
            <*> (x .:? "ClientId")
            <*> (x .:? "ExplicitAuthFlows" .!= mempty)
            <*> (x .:? "ClientSecret")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "SupportedIdentityProviders" .!= mempty)
            <*> (x .:? "LogoutURLs" .!= mempty)
            <*> (x .:? "AllowedOAuthFlowsUserPoolClient")
            <*> (x .:? "UserPoolId")
            <*> (x .:? "IdTokenValidity")
            <*> (x .:? "TokenValidityUnits")
            <*> (x .:? "DefaultRedirectURI")
            <*> (x .:? "WriteAttributes" .!= mempty)
            <*> (x .:? "PreventUserExistenceErrors")
            <*> (x .:? "AccessTokenValidity")
            <*> (x .:? "CreationDate")
            <*> (x .:? "ReadAttributes" .!= mempty)
            <*> (x .:? "AllowedOAuthScopes" .!= mempty)
            <*> (x .:? "AllowedOAuthFlows" .!= mempty)
            <*> (x .:? "AnalyticsConfiguration")
            <*> (x .:? "ClientName")
            <*> (x .:? "CallbackURLs" .!= mempty)
      )

instance Hashable UserPoolClientType

instance NFData UserPoolClientType
