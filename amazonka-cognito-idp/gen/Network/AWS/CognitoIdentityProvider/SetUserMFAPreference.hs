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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user's multi-factor authentication (MFA) method preference.
--
--
module Network.AWS.CognitoIdentityProvider.SetUserMFAPreference
    (
    -- * Creating a Request
      setUserMFAPreference
    , SetUserMFAPreference
    -- * Request Lenses
    , sumpSMSMFASettings
    , sumpSoftwareTokenMFASettings
    , sumpAccessToken

    -- * Destructuring the Response
    , setUserMFAPreferenceResponse
    , SetUserMFAPreferenceResponse
    -- * Response Lenses
    , sumprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setUserMFAPreference' smart constructor.
data SetUserMFAPreference = SetUserMFAPreference'
  { _sumpSMSMFASettings           :: !(Maybe SMSMFASettingsType)
  , _sumpSoftwareTokenMFASettings :: !(Maybe SoftwareTokenMFASettingsType)
  , _sumpAccessToken              :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetUserMFAPreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumpSMSMFASettings' - The SMS text message multi-factor authentication (MFA) settings.
--
-- * 'sumpSoftwareTokenMFASettings' - The time-based one-time password software token MFA settings.
--
-- * 'sumpAccessToken' - The access token.
setUserMFAPreference
    :: Text -- ^ 'sumpAccessToken'
    -> SetUserMFAPreference
setUserMFAPreference pAccessToken_ =
  SetUserMFAPreference'
    { _sumpSMSMFASettings = Nothing
    , _sumpSoftwareTokenMFASettings = Nothing
    , _sumpAccessToken = _Sensitive # pAccessToken_
    }


-- | The SMS text message multi-factor authentication (MFA) settings.
sumpSMSMFASettings :: Lens' SetUserMFAPreference (Maybe SMSMFASettingsType)
sumpSMSMFASettings = lens _sumpSMSMFASettings (\ s a -> s{_sumpSMSMFASettings = a})

-- | The time-based one-time password software token MFA settings.
sumpSoftwareTokenMFASettings :: Lens' SetUserMFAPreference (Maybe SoftwareTokenMFASettingsType)
sumpSoftwareTokenMFASettings = lens _sumpSoftwareTokenMFASettings (\ s a -> s{_sumpSoftwareTokenMFASettings = a})

-- | The access token.
sumpAccessToken :: Lens' SetUserMFAPreference Text
sumpAccessToken = lens _sumpAccessToken (\ s a -> s{_sumpAccessToken = a}) . _Sensitive

instance AWSRequest SetUserMFAPreference where
        type Rs SetUserMFAPreference =
             SetUserMFAPreferenceResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 SetUserMFAPreferenceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable SetUserMFAPreference where

instance NFData SetUserMFAPreference where

instance ToHeaders SetUserMFAPreference where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.SetUserMFAPreference"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetUserMFAPreference where
        toJSON SetUserMFAPreference'{..}
          = object
              (catMaybes
                 [("SMSMfaSettings" .=) <$> _sumpSMSMFASettings,
                  ("SoftwareTokenMfaSettings" .=) <$>
                    _sumpSoftwareTokenMFASettings,
                  Just ("AccessToken" .= _sumpAccessToken)])

instance ToPath SetUserMFAPreference where
        toPath = const "/"

instance ToQuery SetUserMFAPreference where
        toQuery = const mempty

-- | /See:/ 'setUserMFAPreferenceResponse' smart constructor.
newtype SetUserMFAPreferenceResponse = SetUserMFAPreferenceResponse'
  { _sumprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetUserMFAPreferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sumprsResponseStatus' - -- | The response status code.
setUserMFAPreferenceResponse
    :: Int -- ^ 'sumprsResponseStatus'
    -> SetUserMFAPreferenceResponse
setUserMFAPreferenceResponse pResponseStatus_ =
  SetUserMFAPreferenceResponse' {_sumprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
sumprsResponseStatus :: Lens' SetUserMFAPreferenceResponse Int
sumprsResponseStatus = lens _sumprsResponseStatus (\ s a -> s{_sumprsResponseStatus = a})

instance NFData SetUserMFAPreferenceResponse where
