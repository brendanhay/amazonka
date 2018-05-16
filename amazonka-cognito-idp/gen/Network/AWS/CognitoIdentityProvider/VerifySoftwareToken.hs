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
-- Module      : Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this API to register a user's entered TOTP code and mark the user's software token MFA status as "verified" if successful,
--
--
module Network.AWS.CognitoIdentityProvider.VerifySoftwareToken
    (
    -- * Creating a Request
      verifySoftwareToken
    , VerifySoftwareToken
    -- * Request Lenses
    , vstAccessToken
    , vstFriendlyDeviceName
    , vstSession
    , vstUserCode

    -- * Destructuring the Response
    , verifySoftwareTokenResponse
    , VerifySoftwareTokenResponse
    -- * Response Lenses
    , vstrsStatus
    , vstrsSession
    , vstrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'verifySoftwareToken' smart constructor.
data VerifySoftwareToken = VerifySoftwareToken'
  { _vstAccessToken        :: !(Maybe (Sensitive Text))
  , _vstFriendlyDeviceName :: !(Maybe Text)
  , _vstSession            :: !(Maybe Text)
  , _vstUserCode           :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifySoftwareToken' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vstAccessToken' - The access token.
--
-- * 'vstFriendlyDeviceName' - The friendly device name.
--
-- * 'vstSession' - The session which should be passed both ways in challenge-response calls to the service.
--
-- * 'vstUserCode' - The one time password computed using the secret code returned by
verifySoftwareToken
    :: Text -- ^ 'vstUserCode'
    -> VerifySoftwareToken
verifySoftwareToken pUserCode_ =
  VerifySoftwareToken'
    { _vstAccessToken = Nothing
    , _vstFriendlyDeviceName = Nothing
    , _vstSession = Nothing
    , _vstUserCode = pUserCode_
    }


-- | The access token.
vstAccessToken :: Lens' VerifySoftwareToken (Maybe Text)
vstAccessToken = lens _vstAccessToken (\ s a -> s{_vstAccessToken = a}) . mapping _Sensitive

-- | The friendly device name.
vstFriendlyDeviceName :: Lens' VerifySoftwareToken (Maybe Text)
vstFriendlyDeviceName = lens _vstFriendlyDeviceName (\ s a -> s{_vstFriendlyDeviceName = a})

-- | The session which should be passed both ways in challenge-response calls to the service.
vstSession :: Lens' VerifySoftwareToken (Maybe Text)
vstSession = lens _vstSession (\ s a -> s{_vstSession = a})

-- | The one time password computed using the secret code returned by
vstUserCode :: Lens' VerifySoftwareToken Text
vstUserCode = lens _vstUserCode (\ s a -> s{_vstUserCode = a})

instance AWSRequest VerifySoftwareToken where
        type Rs VerifySoftwareToken =
             VerifySoftwareTokenResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 VerifySoftwareTokenResponse' <$>
                   (x .?> "Status") <*> (x .?> "Session") <*>
                     (pure (fromEnum s)))

instance Hashable VerifySoftwareToken where

instance NFData VerifySoftwareToken where

instance ToHeaders VerifySoftwareToken where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.VerifySoftwareToken"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON VerifySoftwareToken where
        toJSON VerifySoftwareToken'{..}
          = object
              (catMaybes
                 [("AccessToken" .=) <$> _vstAccessToken,
                  ("FriendlyDeviceName" .=) <$> _vstFriendlyDeviceName,
                  ("Session" .=) <$> _vstSession,
                  Just ("UserCode" .= _vstUserCode)])

instance ToPath VerifySoftwareToken where
        toPath = const "/"

instance ToQuery VerifySoftwareToken where
        toQuery = const mempty

-- | /See:/ 'verifySoftwareTokenResponse' smart constructor.
data VerifySoftwareTokenResponse = VerifySoftwareTokenResponse'
  { _vstrsStatus         :: !(Maybe VerifySoftwareTokenResponseType)
  , _vstrsSession        :: !(Maybe Text)
  , _vstrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VerifySoftwareTokenResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vstrsStatus' - The status of the verify software token.
--
-- * 'vstrsSession' - The session which should be passed both ways in challenge-response calls to the service.
--
-- * 'vstrsResponseStatus' - -- | The response status code.
verifySoftwareTokenResponse
    :: Int -- ^ 'vstrsResponseStatus'
    -> VerifySoftwareTokenResponse
verifySoftwareTokenResponse pResponseStatus_ =
  VerifySoftwareTokenResponse'
    { _vstrsStatus = Nothing
    , _vstrsSession = Nothing
    , _vstrsResponseStatus = pResponseStatus_
    }


-- | The status of the verify software token.
vstrsStatus :: Lens' VerifySoftwareTokenResponse (Maybe VerifySoftwareTokenResponseType)
vstrsStatus = lens _vstrsStatus (\ s a -> s{_vstrsStatus = a})

-- | The session which should be passed both ways in challenge-response calls to the service.
vstrsSession :: Lens' VerifySoftwareTokenResponse (Maybe Text)
vstrsSession = lens _vstrsSession (\ s a -> s{_vstrsSession = a})

-- | -- | The response status code.
vstrsResponseStatus :: Lens' VerifySoftwareTokenResponse Int
vstrsResponseStatus = lens _vstrsResponseStatus (\ s a -> s{_vstrsResponseStatus = a})

instance NFData VerifySoftwareTokenResponse where
