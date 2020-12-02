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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Responds to an authentication challenge, as an administrator.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminRespondToAuthChallenge
    (
    -- * Creating a Request
      adminRespondToAuthChallenge
    , AdminRespondToAuthChallenge
    -- * Request Lenses
    , artacContextData
    , artacAnalyticsMetadata
    , artacChallengeResponses
    , artacSession
    , artacUserPoolId
    , artacClientId
    , artacChallengeName

    -- * Destructuring the Response
    , adminRespondToAuthChallengeResponse
    , AdminRespondToAuthChallengeResponse
    -- * Response Lenses
    , artacrsChallengeName
    , artacrsChallengeParameters
    , artacrsAuthenticationResult
    , artacrsSession
    , artacrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to respond to the authentication challenge, as an administrator.
--
--
--
-- /See:/ 'adminRespondToAuthChallenge' smart constructor.
data AdminRespondToAuthChallenge = AdminRespondToAuthChallenge'
  { _artacContextData        :: !(Maybe ContextDataType)
  , _artacAnalyticsMetadata  :: !(Maybe AnalyticsMetadataType)
  , _artacChallengeResponses :: !(Maybe (Map Text Text))
  , _artacSession            :: !(Maybe Text)
  , _artacUserPoolId         :: !Text
  , _artacClientId           :: !(Sensitive Text)
  , _artacChallengeName      :: !ChallengeNameType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminRespondToAuthChallenge' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artacContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- * 'artacAnalyticsMetadata' - The analytics metadata for collecting Amazon Pinpoint metrics for @AdminRespondToAuthChallenge@ calls.
--
-- * 'artacChallengeResponses' - The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).     * @ADMIN_NO_SRP_AUTH@ : @PASSWORD@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).      * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).  The value of the @USERNAME@ attribute must be the user's actual username, not an alias (such as email address or phone number). To make this easier, the @AdminInitiateAuth@ response includes the actual username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you specified an alias in your call to @AdminInitiateAuth@ .
--
-- * 'artacSession' - The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- * 'artacUserPoolId' - The ID of the Amazon Cognito user pool.
--
-- * 'artacClientId' - The app client ID.
--
-- * 'artacChallengeName' - The challenge name. For more information, see .
adminRespondToAuthChallenge
    :: Text -- ^ 'artacUserPoolId'
    -> Text -- ^ 'artacClientId'
    -> ChallengeNameType -- ^ 'artacChallengeName'
    -> AdminRespondToAuthChallenge
adminRespondToAuthChallenge pUserPoolId_ pClientId_ pChallengeName_ =
  AdminRespondToAuthChallenge'
    { _artacContextData = Nothing
    , _artacAnalyticsMetadata = Nothing
    , _artacChallengeResponses = Nothing
    , _artacSession = Nothing
    , _artacUserPoolId = pUserPoolId_
    , _artacClientId = _Sensitive # pClientId_
    , _artacChallengeName = pChallengeName_
    }


-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
artacContextData :: Lens' AdminRespondToAuthChallenge (Maybe ContextDataType)
artacContextData = lens _artacContextData (\ s a -> s{_artacContextData = a})

-- | The analytics metadata for collecting Amazon Pinpoint metrics for @AdminRespondToAuthChallenge@ calls.
artacAnalyticsMetadata :: Lens' AdminRespondToAuthChallenge (Maybe AnalyticsMetadataType)
artacAnalyticsMetadata = lens _artacAnalyticsMetadata (\ s a -> s{_artacAnalyticsMetadata = a})

-- | The challenge responses. These are inputs corresponding to the value of @ChallengeName@ , for example:     * @SMS_MFA@ : @SMS_MFA_CODE@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).     * @PASSWORD_VERIFIER@ : @PASSWORD_CLAIM_SIGNATURE@ , @PASSWORD_CLAIM_SECRET_BLOCK@ , @TIMESTAMP@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).     * @ADMIN_NO_SRP_AUTH@ : @PASSWORD@ , @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).      * @NEW_PASSWORD_REQUIRED@ : @NEW_PASSWORD@ , any other required attributes, @USERNAME@ , @SECRET_HASH@ (if app client is configured with client secret).  The value of the @USERNAME@ attribute must be the user's actual username, not an alias (such as email address or phone number). To make this easier, the @AdminInitiateAuth@ response includes the actual username value in the @USERNAMEUSER_ID_FOR_SRP@ attribute, even if you specified an alias in your call to @AdminInitiateAuth@ .
artacChallengeResponses :: Lens' AdminRespondToAuthChallenge (HashMap Text Text)
artacChallengeResponses = lens _artacChallengeResponses (\ s a -> s{_artacChallengeResponses = a}) . _Default . _Map

-- | The session which should be passed both ways in challenge-response calls to the service. If @InitiateAuth@ or @RespondToAuthChallenge@ API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
artacSession :: Lens' AdminRespondToAuthChallenge (Maybe Text)
artacSession = lens _artacSession (\ s a -> s{_artacSession = a})

-- | The ID of the Amazon Cognito user pool.
artacUserPoolId :: Lens' AdminRespondToAuthChallenge Text
artacUserPoolId = lens _artacUserPoolId (\ s a -> s{_artacUserPoolId = a})

-- | The app client ID.
artacClientId :: Lens' AdminRespondToAuthChallenge Text
artacClientId = lens _artacClientId (\ s a -> s{_artacClientId = a}) . _Sensitive

-- | The challenge name. For more information, see .
artacChallengeName :: Lens' AdminRespondToAuthChallenge ChallengeNameType
artacChallengeName = lens _artacChallengeName (\ s a -> s{_artacChallengeName = a})

instance AWSRequest AdminRespondToAuthChallenge where
        type Rs AdminRespondToAuthChallenge =
             AdminRespondToAuthChallengeResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminRespondToAuthChallengeResponse' <$>
                   (x .?> "ChallengeName") <*>
                     (x .?> "ChallengeParameters" .!@ mempty)
                     <*> (x .?> "AuthenticationResult")
                     <*> (x .?> "Session")
                     <*> (pure (fromEnum s)))

instance Hashable AdminRespondToAuthChallenge where

instance NFData AdminRespondToAuthChallenge where

instance ToHeaders AdminRespondToAuthChallenge where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminRespondToAuthChallenge"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminRespondToAuthChallenge where
        toJSON AdminRespondToAuthChallenge'{..}
          = object
              (catMaybes
                 [("ContextData" .=) <$> _artacContextData,
                  ("AnalyticsMetadata" .=) <$> _artacAnalyticsMetadata,
                  ("ChallengeResponses" .=) <$>
                    _artacChallengeResponses,
                  ("Session" .=) <$> _artacSession,
                  Just ("UserPoolId" .= _artacUserPoolId),
                  Just ("ClientId" .= _artacClientId),
                  Just ("ChallengeName" .= _artacChallengeName)])

instance ToPath AdminRespondToAuthChallenge where
        toPath = const "/"

instance ToQuery AdminRespondToAuthChallenge where
        toQuery = const mempty

-- | Responds to the authentication challenge, as an administrator.
--
--
--
-- /See:/ 'adminRespondToAuthChallengeResponse' smart constructor.
data AdminRespondToAuthChallengeResponse = AdminRespondToAuthChallengeResponse'
  { _artacrsChallengeName        :: !(Maybe ChallengeNameType)
  , _artacrsChallengeParameters  :: !(Maybe (Map Text Text))
  , _artacrsAuthenticationResult :: !(Maybe AuthenticationResultType)
  , _artacrsSession              :: !(Maybe Text)
  , _artacrsResponseStatus       :: !Int
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminRespondToAuthChallengeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artacrsChallengeName' - The name of the challenge. For more information, see .
--
-- * 'artacrsChallengeParameters' - The challenge parameters. For more information, see .
--
-- * 'artacrsAuthenticationResult' - The result returned by the server in response to the authentication request.
--
-- * 'artacrsSession' - The session which should be passed both ways in challenge-response calls to the service. If the or API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
--
-- * 'artacrsResponseStatus' - -- | The response status code.
adminRespondToAuthChallengeResponse
    :: Int -- ^ 'artacrsResponseStatus'
    -> AdminRespondToAuthChallengeResponse
adminRespondToAuthChallengeResponse pResponseStatus_ =
  AdminRespondToAuthChallengeResponse'
    { _artacrsChallengeName = Nothing
    , _artacrsChallengeParameters = Nothing
    , _artacrsAuthenticationResult = Nothing
    , _artacrsSession = Nothing
    , _artacrsResponseStatus = pResponseStatus_
    }


-- | The name of the challenge. For more information, see .
artacrsChallengeName :: Lens' AdminRespondToAuthChallengeResponse (Maybe ChallengeNameType)
artacrsChallengeName = lens _artacrsChallengeName (\ s a -> s{_artacrsChallengeName = a})

-- | The challenge parameters. For more information, see .
artacrsChallengeParameters :: Lens' AdminRespondToAuthChallengeResponse (HashMap Text Text)
artacrsChallengeParameters = lens _artacrsChallengeParameters (\ s a -> s{_artacrsChallengeParameters = a}) . _Default . _Map

-- | The result returned by the server in response to the authentication request.
artacrsAuthenticationResult :: Lens' AdminRespondToAuthChallengeResponse (Maybe AuthenticationResultType)
artacrsAuthenticationResult = lens _artacrsAuthenticationResult (\ s a -> s{_artacrsAuthenticationResult = a})

-- | The session which should be passed both ways in challenge-response calls to the service. If the or API call determines that the caller needs to go through another challenge, they return a session with other challenge parameters. This session should be passed as it is to the next @RespondToAuthChallenge@ API call.
artacrsSession :: Lens' AdminRespondToAuthChallengeResponse (Maybe Text)
artacrsSession = lens _artacrsSession (\ s a -> s{_artacrsSession = a})

-- | -- | The response status code.
artacrsResponseStatus :: Lens' AdminRespondToAuthChallengeResponse Int
artacrsResponseStatus = lens _artacrsResponseStatus (\ s a -> s{_artacrsResponseStatus = a})

instance NFData AdminRespondToAuthChallengeResponse
         where
