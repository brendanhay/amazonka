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
-- Module      : Network.AWS.CognitoIdentityProvider.InitiateAuth
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Initiates the authentication flow.
--
--
module Network.AWS.CognitoIdentityProvider.InitiateAuth
    (
    -- * Creating a Request
      initiateAuth
    , InitiateAuth
    -- * Request Lenses
    , iaClientMetadata
    , iaAuthParameters
    , iaAuthFlow
    , iaClientId

    -- * Destructuring the Response
    , initiateAuthResponse
    , InitiateAuthResponse
    -- * Response Lenses
    , iarsChallengeName
    , iarsChallengeParameters
    , iarsAuthenticationResult
    , iarsSession
    , iarsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Initiates the authentication request.
--
--
--
-- /See:/ 'initiateAuth' smart constructor.
data InitiateAuth = InitiateAuth'
    { _iaClientMetadata :: !(Maybe (Map Text Text))
    , _iaAuthParameters :: !(Maybe (Map Text Text))
    , _iaAuthFlow       :: !AuthFlowType
    , _iaClientId       :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InitiateAuth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iaClientMetadata' - The client app's metadata.
--
-- * 'iaAuthParameters' - The authentication parameters.
--
-- * 'iaAuthFlow' - The authentication flow.
--
-- * 'iaClientId' - The client ID.
initiateAuth
    :: AuthFlowType -- ^ 'iaAuthFlow'
    -> Text -- ^ 'iaClientId'
    -> InitiateAuth
initiateAuth pAuthFlow_ pClientId_ =
    InitiateAuth'
    { _iaClientMetadata = Nothing
    , _iaAuthParameters = Nothing
    , _iaAuthFlow = pAuthFlow_
    , _iaClientId = _Sensitive # pClientId_
    }

-- | The client app's metadata.
iaClientMetadata :: Lens' InitiateAuth (HashMap Text Text)
iaClientMetadata = lens _iaClientMetadata (\ s a -> s{_iaClientMetadata = a}) . _Default . _Map;

-- | The authentication parameters.
iaAuthParameters :: Lens' InitiateAuth (HashMap Text Text)
iaAuthParameters = lens _iaAuthParameters (\ s a -> s{_iaAuthParameters = a}) . _Default . _Map;

-- | The authentication flow.
iaAuthFlow :: Lens' InitiateAuth AuthFlowType
iaAuthFlow = lens _iaAuthFlow (\ s a -> s{_iaAuthFlow = a});

-- | The client ID.
iaClientId :: Lens' InitiateAuth Text
iaClientId = lens _iaClientId (\ s a -> s{_iaClientId = a}) . _Sensitive;

instance AWSRequest InitiateAuth where
        type Rs InitiateAuth = InitiateAuthResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 InitiateAuthResponse' <$>
                   (x .?> "ChallengeName") <*>
                     (x .?> "ChallengeParameters" .!@ mempty)
                     <*> (x .?> "AuthenticationResult")
                     <*> (x .?> "Session")
                     <*> (pure (fromEnum s)))

instance Hashable InitiateAuth

instance NFData InitiateAuth

instance ToHeaders InitiateAuth where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.InitiateAuth" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON InitiateAuth where
        toJSON InitiateAuth'{..}
          = object
              (catMaybes
                 [("ClientMetadata" .=) <$> _iaClientMetadata,
                  ("AuthParameters" .=) <$> _iaAuthParameters,
                  Just ("AuthFlow" .= _iaAuthFlow),
                  Just ("ClientId" .= _iaClientId)])

instance ToPath InitiateAuth where
        toPath = const "/"

instance ToQuery InitiateAuth where
        toQuery = const mempty

-- | Initiates the authentication response.
--
--
--
-- /See:/ 'initiateAuthResponse' smart constructor.
data InitiateAuthResponse = InitiateAuthResponse'
    { _iarsChallengeName        :: !(Maybe ChallengeNameType)
    , _iarsChallengeParameters  :: !(Maybe (Map Text Text))
    , _iarsAuthenticationResult :: !(Maybe AuthenticationResultType)
    , _iarsSession              :: !(Maybe Text)
    , _iarsResponseStatus       :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'InitiateAuthResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'iarsChallengeName' - The name of the challenge.
--
-- * 'iarsChallengeParameters' - The challenge parameters.
--
-- * 'iarsAuthenticationResult' - Undocumented member.
--
-- * 'iarsSession' - The session.
--
-- * 'iarsResponseStatus' - -- | The response status code.
initiateAuthResponse
    :: Int -- ^ 'iarsResponseStatus'
    -> InitiateAuthResponse
initiateAuthResponse pResponseStatus_ =
    InitiateAuthResponse'
    { _iarsChallengeName = Nothing
    , _iarsChallengeParameters = Nothing
    , _iarsAuthenticationResult = Nothing
    , _iarsSession = Nothing
    , _iarsResponseStatus = pResponseStatus_
    }

-- | The name of the challenge.
iarsChallengeName :: Lens' InitiateAuthResponse (Maybe ChallengeNameType)
iarsChallengeName = lens _iarsChallengeName (\ s a -> s{_iarsChallengeName = a});

-- | The challenge parameters.
iarsChallengeParameters :: Lens' InitiateAuthResponse (HashMap Text Text)
iarsChallengeParameters = lens _iarsChallengeParameters (\ s a -> s{_iarsChallengeParameters = a}) . _Default . _Map;

-- | Undocumented member.
iarsAuthenticationResult :: Lens' InitiateAuthResponse (Maybe AuthenticationResultType)
iarsAuthenticationResult = lens _iarsAuthenticationResult (\ s a -> s{_iarsAuthenticationResult = a});

-- | The session.
iarsSession :: Lens' InitiateAuthResponse (Maybe Text)
iarsSession = lens _iarsSession (\ s a -> s{_iarsSession = a});

-- | -- | The response status code.
iarsResponseStatus :: Lens' InitiateAuthResponse Int
iarsResponseStatus = lens _iarsResponseStatus (\ s a -> s{_iarsResponseStatus = a});

instance NFData InitiateAuthResponse
