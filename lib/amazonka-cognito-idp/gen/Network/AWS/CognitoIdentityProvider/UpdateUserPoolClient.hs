{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows the developer to update the specified user pool client and password policy.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateUserPoolClient
    (
    -- * Creating a Request
      updateUserPoolClient
    , UpdateUserPoolClient
    -- * Request Lenses
    , uupcRefreshTokenValidity
    , uupcExplicitAuthFlows
    , uupcSupportedIdentityProviders
    , uupcLogoutURLs
    , uupcAllowedOAuthFlowsUserPoolClient
    , uupcDefaultRedirectURI
    , uupcWriteAttributes
    , uupcReadAttributes
    , uupcAllowedOAuthScopes
    , uupcAllowedOAuthFlows
    , uupcAnalyticsConfiguration
    , uupcClientName
    , uupcCallbackURLs
    , uupcUserPoolId
    , uupcClientId

    -- * Destructuring the Response
    , updateUserPoolClientResponse
    , UpdateUserPoolClientResponse
    -- * Response Lenses
    , uupcrsUserPoolClient
    , uupcrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to update the user pool client.
--
--
--
-- /See:/ 'updateUserPoolClient' smart constructor.
data UpdateUserPoolClient = UpdateUserPoolClient'
  { _uupcRefreshTokenValidity            :: !(Maybe Nat)
  , _uupcExplicitAuthFlows               :: !(Maybe [ExplicitAuthFlowsType])
  , _uupcSupportedIdentityProviders      :: !(Maybe [Text])
  , _uupcLogoutURLs                      :: !(Maybe [Text])
  , _uupcAllowedOAuthFlowsUserPoolClient :: !(Maybe Bool)
  , _uupcDefaultRedirectURI              :: !(Maybe Text)
  , _uupcWriteAttributes                 :: !(Maybe [Text])
  , _uupcReadAttributes                  :: !(Maybe [Text])
  , _uupcAllowedOAuthScopes              :: !(Maybe [Text])
  , _uupcAllowedOAuthFlows               :: !(Maybe [OAuthFlowType])
  , _uupcAnalyticsConfiguration          :: !(Maybe AnalyticsConfigurationType)
  , _uupcClientName                      :: !(Maybe Text)
  , _uupcCallbackURLs                    :: !(Maybe [Text])
  , _uupcUserPoolId                      :: !Text
  , _uupcClientId                        :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPoolClient' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupcRefreshTokenValidity' - The time limit, in days, after which the refresh token is no longer valid and cannot be used.
--
-- * 'uupcExplicitAuthFlows' - Explicit authentication flows.
--
-- * 'uupcSupportedIdentityProviders' - A list of provider names for the identity providers that are supported on this client.
--
-- * 'uupcLogoutURLs' - A list of allowed logout URLs for the identity providers.
--
-- * 'uupcAllowedOAuthFlowsUserPoolClient' - Set to TRUE if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
--
-- * 'uupcDefaultRedirectURI' - The default redirect URI. Must be in the @CallbackURLs@ list.
--
-- * 'uupcWriteAttributes' - The writeable attributes of the user pool.
--
-- * 'uupcReadAttributes' - The read-only attributes of the user pool.
--
-- * 'uupcAllowedOAuthScopes' - A list of allowed @OAuth@ scopes. Currently supported values are @"phone"@ , @"email"@ , @"openid"@ , and @"Cognito"@ .
--
-- * 'uupcAllowedOAuthFlows' - Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @token@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
--
-- * 'uupcAnalyticsConfiguration' - The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
--
-- * 'uupcClientName' - The client name from the update user pool client request.
--
-- * 'uupcCallbackURLs' - A list of allowed callback URLs for the identity providers.
--
-- * 'uupcUserPoolId' - The user pool ID for the user pool where you want to update the user pool client.
--
-- * 'uupcClientId' - The ID of the client associated with the user pool.
updateUserPoolClient
    :: Text -- ^ 'uupcUserPoolId'
    -> Text -- ^ 'uupcClientId'
    -> UpdateUserPoolClient
updateUserPoolClient pUserPoolId_ pClientId_ =
  UpdateUserPoolClient'
    { _uupcRefreshTokenValidity = Nothing
    , _uupcExplicitAuthFlows = Nothing
    , _uupcSupportedIdentityProviders = Nothing
    , _uupcLogoutURLs = Nothing
    , _uupcAllowedOAuthFlowsUserPoolClient = Nothing
    , _uupcDefaultRedirectURI = Nothing
    , _uupcWriteAttributes = Nothing
    , _uupcReadAttributes = Nothing
    , _uupcAllowedOAuthScopes = Nothing
    , _uupcAllowedOAuthFlows = Nothing
    , _uupcAnalyticsConfiguration = Nothing
    , _uupcClientName = Nothing
    , _uupcCallbackURLs = Nothing
    , _uupcUserPoolId = pUserPoolId_
    , _uupcClientId = _Sensitive # pClientId_
    }


-- | The time limit, in days, after which the refresh token is no longer valid and cannot be used.
uupcRefreshTokenValidity :: Lens' UpdateUserPoolClient (Maybe Natural)
uupcRefreshTokenValidity = lens _uupcRefreshTokenValidity (\ s a -> s{_uupcRefreshTokenValidity = a}) . mapping _Nat

-- | Explicit authentication flows.
uupcExplicitAuthFlows :: Lens' UpdateUserPoolClient [ExplicitAuthFlowsType]
uupcExplicitAuthFlows = lens _uupcExplicitAuthFlows (\ s a -> s{_uupcExplicitAuthFlows = a}) . _Default . _Coerce

-- | A list of provider names for the identity providers that are supported on this client.
uupcSupportedIdentityProviders :: Lens' UpdateUserPoolClient [Text]
uupcSupportedIdentityProviders = lens _uupcSupportedIdentityProviders (\ s a -> s{_uupcSupportedIdentityProviders = a}) . _Default . _Coerce

-- | A list of allowed logout URLs for the identity providers.
uupcLogoutURLs :: Lens' UpdateUserPoolClient [Text]
uupcLogoutURLs = lens _uupcLogoutURLs (\ s a -> s{_uupcLogoutURLs = a}) . _Default . _Coerce

-- | Set to TRUE if the client is allowed to follow the OAuth protocol when interacting with Cognito user pools.
uupcAllowedOAuthFlowsUserPoolClient :: Lens' UpdateUserPoolClient (Maybe Bool)
uupcAllowedOAuthFlowsUserPoolClient = lens _uupcAllowedOAuthFlowsUserPoolClient (\ s a -> s{_uupcAllowedOAuthFlowsUserPoolClient = a})

-- | The default redirect URI. Must be in the @CallbackURLs@ list.
uupcDefaultRedirectURI :: Lens' UpdateUserPoolClient (Maybe Text)
uupcDefaultRedirectURI = lens _uupcDefaultRedirectURI (\ s a -> s{_uupcDefaultRedirectURI = a})

-- | The writeable attributes of the user pool.
uupcWriteAttributes :: Lens' UpdateUserPoolClient [Text]
uupcWriteAttributes = lens _uupcWriteAttributes (\ s a -> s{_uupcWriteAttributes = a}) . _Default . _Coerce

-- | The read-only attributes of the user pool.
uupcReadAttributes :: Lens' UpdateUserPoolClient [Text]
uupcReadAttributes = lens _uupcReadAttributes (\ s a -> s{_uupcReadAttributes = a}) . _Default . _Coerce

-- | A list of allowed @OAuth@ scopes. Currently supported values are @"phone"@ , @"email"@ , @"openid"@ , and @"Cognito"@ .
uupcAllowedOAuthScopes :: Lens' UpdateUserPoolClient [Text]
uupcAllowedOAuthScopes = lens _uupcAllowedOAuthScopes (\ s a -> s{_uupcAllowedOAuthScopes = a}) . _Default . _Coerce

-- | Set to @code@ to initiate a code grant flow, which provides an authorization code as the response. This code can be exchanged for access tokens with the token endpoint. Set to @token@ to specify that the client should get the access token (and, optionally, ID token, based on scopes) directly.
uupcAllowedOAuthFlows :: Lens' UpdateUserPoolClient [OAuthFlowType]
uupcAllowedOAuthFlows = lens _uupcAllowedOAuthFlows (\ s a -> s{_uupcAllowedOAuthFlows = a}) . _Default . _Coerce

-- | The Amazon Pinpoint analytics configuration for collecting metrics for this user pool.
uupcAnalyticsConfiguration :: Lens' UpdateUserPoolClient (Maybe AnalyticsConfigurationType)
uupcAnalyticsConfiguration = lens _uupcAnalyticsConfiguration (\ s a -> s{_uupcAnalyticsConfiguration = a})

-- | The client name from the update user pool client request.
uupcClientName :: Lens' UpdateUserPoolClient (Maybe Text)
uupcClientName = lens _uupcClientName (\ s a -> s{_uupcClientName = a})

-- | A list of allowed callback URLs for the identity providers.
uupcCallbackURLs :: Lens' UpdateUserPoolClient [Text]
uupcCallbackURLs = lens _uupcCallbackURLs (\ s a -> s{_uupcCallbackURLs = a}) . _Default . _Coerce

-- | The user pool ID for the user pool where you want to update the user pool client.
uupcUserPoolId :: Lens' UpdateUserPoolClient Text
uupcUserPoolId = lens _uupcUserPoolId (\ s a -> s{_uupcUserPoolId = a})

-- | The ID of the client associated with the user pool.
uupcClientId :: Lens' UpdateUserPoolClient Text
uupcClientId = lens _uupcClientId (\ s a -> s{_uupcClientId = a}) . _Sensitive

instance AWSRequest UpdateUserPoolClient where
        type Rs UpdateUserPoolClient =
             UpdateUserPoolClientResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 UpdateUserPoolClientResponse' <$>
                   (x .?> "UserPoolClient") <*> (pure (fromEnum s)))

instance Hashable UpdateUserPoolClient where

instance NFData UpdateUserPoolClient where

instance ToHeaders UpdateUserPoolClient where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateUserPoolClient"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserPoolClient where
        toJSON UpdateUserPoolClient'{..}
          = object
              (catMaybes
                 [("RefreshTokenValidity" .=) <$>
                    _uupcRefreshTokenValidity,
                  ("ExplicitAuthFlows" .=) <$> _uupcExplicitAuthFlows,
                  ("SupportedIdentityProviders" .=) <$>
                    _uupcSupportedIdentityProviders,
                  ("LogoutURLs" .=) <$> _uupcLogoutURLs,
                  ("AllowedOAuthFlowsUserPoolClient" .=) <$>
                    _uupcAllowedOAuthFlowsUserPoolClient,
                  ("DefaultRedirectURI" .=) <$>
                    _uupcDefaultRedirectURI,
                  ("WriteAttributes" .=) <$> _uupcWriteAttributes,
                  ("ReadAttributes" .=) <$> _uupcReadAttributes,
                  ("AllowedOAuthScopes" .=) <$>
                    _uupcAllowedOAuthScopes,
                  ("AllowedOAuthFlows" .=) <$> _uupcAllowedOAuthFlows,
                  ("AnalyticsConfiguration" .=) <$>
                    _uupcAnalyticsConfiguration,
                  ("ClientName" .=) <$> _uupcClientName,
                  ("CallbackURLs" .=) <$> _uupcCallbackURLs,
                  Just ("UserPoolId" .= _uupcUserPoolId),
                  Just ("ClientId" .= _uupcClientId)])

instance ToPath UpdateUserPoolClient where
        toPath = const "/"

instance ToQuery UpdateUserPoolClient where
        toQuery = const mempty

-- | Represents the response from the server to the request to update the user pool client.
--
--
--
-- /See:/ 'updateUserPoolClientResponse' smart constructor.
data UpdateUserPoolClientResponse = UpdateUserPoolClientResponse'
  { _uupcrsUserPoolClient :: !(Maybe UserPoolClientType)
  , _uupcrsResponseStatus :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPoolClientResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupcrsUserPoolClient' - The user pool client value from the response from the server when an update user pool client request is made.
--
-- * 'uupcrsResponseStatus' - -- | The response status code.
updateUserPoolClientResponse
    :: Int -- ^ 'uupcrsResponseStatus'
    -> UpdateUserPoolClientResponse
updateUserPoolClientResponse pResponseStatus_ =
  UpdateUserPoolClientResponse'
    {_uupcrsUserPoolClient = Nothing, _uupcrsResponseStatus = pResponseStatus_}


-- | The user pool client value from the response from the server when an update user pool client request is made.
uupcrsUserPoolClient :: Lens' UpdateUserPoolClientResponse (Maybe UserPoolClientType)
uupcrsUserPoolClient = lens _uupcrsUserPoolClient (\ s a -> s{_uupcrsUserPoolClient = a})

-- | -- | The response status code.
uupcrsResponseStatus :: Lens' UpdateUserPoolClientResponse Int
uupcrsResponseStatus = lens _uupcrsResponseStatus (\ s a -> s{_uupcrsResponseStatus = a})

instance NFData UpdateUserPoolClientResponse where
