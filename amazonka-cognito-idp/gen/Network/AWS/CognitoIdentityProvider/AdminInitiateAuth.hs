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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow, as an administrator.
--
--
module Network.AWS.CognitoIdentityProvider.AdminInitiateAuth
    (
    -- * Creating a Request
      adminInitiateAuth
    , AdminInitiateAuth
    -- * Request Lenses
    , aiaClientMetadata
    , aiaAuthParameters
    , aiaUserPoolId
    , aiaClientId
    , aiaAuthFlow

    -- * Destructuring the Response
    , adminInitiateAuthResponse
    , AdminInitiateAuthResponse
    -- * Response Lenses
    , aiarsChallengeName
    , aiarsChallengeParameters
    , aiarsAuthenticationResult
    , aiarsSession
    , aiarsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Initiates the authorization request, as an administrator.
--
--
--
-- /See:/ 'adminInitiateAuth' smart constructor.
data AdminInitiateAuth = AdminInitiateAuth'
    { _aiaClientMetadata :: !(Maybe (Map Text Text))
    , _aiaAuthParameters :: !(Maybe (Map Text Text))
    , _aiaUserPoolId     :: !Text
    , _aiaClientId       :: !(Sensitive Text)
    , _aiaAuthFlow       :: !AuthFlowType
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdminInitiateAuth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiaClientMetadata' - The client app metadata.
--
-- * 'aiaAuthParameters' - The authentication parameters.
--
-- * 'aiaUserPoolId' - The ID of the Amazon Cognito user pool.
--
-- * 'aiaClientId' - The client app ID.
--
-- * 'aiaAuthFlow' - The authentication flow.
adminInitiateAuth
    :: Text -- ^ 'aiaUserPoolId'
    -> Text -- ^ 'aiaClientId'
    -> AuthFlowType -- ^ 'aiaAuthFlow'
    -> AdminInitiateAuth
adminInitiateAuth pUserPoolId_ pClientId_ pAuthFlow_ =
    AdminInitiateAuth'
    { _aiaClientMetadata = Nothing
    , _aiaAuthParameters = Nothing
    , _aiaUserPoolId = pUserPoolId_
    , _aiaClientId = _Sensitive # pClientId_
    , _aiaAuthFlow = pAuthFlow_
    }

-- | The client app metadata.
aiaClientMetadata :: Lens' AdminInitiateAuth (HashMap Text Text)
aiaClientMetadata = lens _aiaClientMetadata (\ s a -> s{_aiaClientMetadata = a}) . _Default . _Map;

-- | The authentication parameters.
aiaAuthParameters :: Lens' AdminInitiateAuth (HashMap Text Text)
aiaAuthParameters = lens _aiaAuthParameters (\ s a -> s{_aiaAuthParameters = a}) . _Default . _Map;

-- | The ID of the Amazon Cognito user pool.
aiaUserPoolId :: Lens' AdminInitiateAuth Text
aiaUserPoolId = lens _aiaUserPoolId (\ s a -> s{_aiaUserPoolId = a});

-- | The client app ID.
aiaClientId :: Lens' AdminInitiateAuth Text
aiaClientId = lens _aiaClientId (\ s a -> s{_aiaClientId = a}) . _Sensitive;

-- | The authentication flow.
aiaAuthFlow :: Lens' AdminInitiateAuth AuthFlowType
aiaAuthFlow = lens _aiaAuthFlow (\ s a -> s{_aiaAuthFlow = a});

instance AWSRequest AdminInitiateAuth where
        type Rs AdminInitiateAuth = AdminInitiateAuthResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminInitiateAuthResponse' <$>
                   (x .?> "ChallengeName") <*>
                     (x .?> "ChallengeParameters" .!@ mempty)
                     <*> (x .?> "AuthenticationResult")
                     <*> (x .?> "Session")
                     <*> (pure (fromEnum s)))

instance Hashable AdminInitiateAuth

instance NFData AdminInitiateAuth

instance ToHeaders AdminInitiateAuth where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminInitiateAuth"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminInitiateAuth where
        toJSON AdminInitiateAuth'{..}
          = object
              (catMaybes
                 [("ClientMetadata" .=) <$> _aiaClientMetadata,
                  ("AuthParameters" .=) <$> _aiaAuthParameters,
                  Just ("UserPoolId" .= _aiaUserPoolId),
                  Just ("ClientId" .= _aiaClientId),
                  Just ("AuthFlow" .= _aiaAuthFlow)])

instance ToPath AdminInitiateAuth where
        toPath = const "/"

instance ToQuery AdminInitiateAuth where
        toQuery = const mempty

-- | Initiates the authentication response, as an administrator.
--
--
--
-- /See:/ 'adminInitiateAuthResponse' smart constructor.
data AdminInitiateAuthResponse = AdminInitiateAuthResponse'
    { _aiarsChallengeName        :: !(Maybe ChallengeNameType)
    , _aiarsChallengeParameters  :: !(Maybe (Map Text Text))
    , _aiarsAuthenticationResult :: !(Maybe AuthenticationResultType)
    , _aiarsSession              :: !(Maybe Text)
    , _aiarsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AdminInitiateAuthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'aiarsChallengeName' - The name of the challenge.
--
-- * 'aiarsChallengeParameters' - The challenge parameters.
--
-- * 'aiarsAuthenticationResult' - The result of the authentication response.
--
-- * 'aiarsSession' - The session.
--
-- * 'aiarsResponseStatus' - -- | The response status code.
adminInitiateAuthResponse
    :: Int -- ^ 'aiarsResponseStatus'
    -> AdminInitiateAuthResponse
adminInitiateAuthResponse pResponseStatus_ =
    AdminInitiateAuthResponse'
    { _aiarsChallengeName = Nothing
    , _aiarsChallengeParameters = Nothing
    , _aiarsAuthenticationResult = Nothing
    , _aiarsSession = Nothing
    , _aiarsResponseStatus = pResponseStatus_
    }

-- | The name of the challenge.
aiarsChallengeName :: Lens' AdminInitiateAuthResponse (Maybe ChallengeNameType)
aiarsChallengeName = lens _aiarsChallengeName (\ s a -> s{_aiarsChallengeName = a});

-- | The challenge parameters.
aiarsChallengeParameters :: Lens' AdminInitiateAuthResponse (HashMap Text Text)
aiarsChallengeParameters = lens _aiarsChallengeParameters (\ s a -> s{_aiarsChallengeParameters = a}) . _Default . _Map;

-- | The result of the authentication response.
aiarsAuthenticationResult :: Lens' AdminInitiateAuthResponse (Maybe AuthenticationResultType)
aiarsAuthenticationResult = lens _aiarsAuthenticationResult (\ s a -> s{_aiarsAuthenticationResult = a});

-- | The session.
aiarsSession :: Lens' AdminInitiateAuthResponse (Maybe Text)
aiarsSession = lens _aiarsSession (\ s a -> s{_aiarsSession = a});

-- | -- | The response status code.
aiarsResponseStatus :: Lens' AdminInitiateAuthResponse Int
aiarsResponseStatus = lens _aiarsResponseStatus (\ s a -> s{_aiarsResponseStatus = a});

instance NFData AdminInitiateAuthResponse
