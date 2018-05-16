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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user's multi-factor authentication (MFA) preference.
--
--
module Network.AWS.CognitoIdentityProvider.AdminSetUserMFAPreference
    (
    -- * Creating a Request
      adminSetUserMFAPreference
    , AdminSetUserMFAPreference
    -- * Request Lenses
    , asumpSMSMFASettings
    , asumpSoftwareTokenMFASettings
    , asumpUsername
    , asumpUserPoolId

    -- * Destructuring the Response
    , adminSetUserMFAPreferenceResponse
    , AdminSetUserMFAPreferenceResponse
    -- * Response Lenses
    , asumprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'adminSetUserMFAPreference' smart constructor.
data AdminSetUserMFAPreference = AdminSetUserMFAPreference'
  { _asumpSMSMFASettings           :: !(Maybe SMSMFASettingsType)
  , _asumpSoftwareTokenMFASettings :: !(Maybe SoftwareTokenMFASettingsType)
  , _asumpUsername                 :: !(Sensitive Text)
  , _asumpUserPoolId               :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminSetUserMFAPreference' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asumpSMSMFASettings' - The SMS text message MFA settings.
--
-- * 'asumpSoftwareTokenMFASettings' - The time-based one-time password software token MFA settings.
--
-- * 'asumpUsername' - The user pool username or alias.
--
-- * 'asumpUserPoolId' - The user pool ID.
adminSetUserMFAPreference
    :: Text -- ^ 'asumpUsername'
    -> Text -- ^ 'asumpUserPoolId'
    -> AdminSetUserMFAPreference
adminSetUserMFAPreference pUsername_ pUserPoolId_ =
  AdminSetUserMFAPreference'
    { _asumpSMSMFASettings = Nothing
    , _asumpSoftwareTokenMFASettings = Nothing
    , _asumpUsername = _Sensitive # pUsername_
    , _asumpUserPoolId = pUserPoolId_
    }


-- | The SMS text message MFA settings.
asumpSMSMFASettings :: Lens' AdminSetUserMFAPreference (Maybe SMSMFASettingsType)
asumpSMSMFASettings = lens _asumpSMSMFASettings (\ s a -> s{_asumpSMSMFASettings = a})

-- | The time-based one-time password software token MFA settings.
asumpSoftwareTokenMFASettings :: Lens' AdminSetUserMFAPreference (Maybe SoftwareTokenMFASettingsType)
asumpSoftwareTokenMFASettings = lens _asumpSoftwareTokenMFASettings (\ s a -> s{_asumpSoftwareTokenMFASettings = a})

-- | The user pool username or alias.
asumpUsername :: Lens' AdminSetUserMFAPreference Text
asumpUsername = lens _asumpUsername (\ s a -> s{_asumpUsername = a}) . _Sensitive

-- | The user pool ID.
asumpUserPoolId :: Lens' AdminSetUserMFAPreference Text
asumpUserPoolId = lens _asumpUserPoolId (\ s a -> s{_asumpUserPoolId = a})

instance AWSRequest AdminSetUserMFAPreference where
        type Rs AdminSetUserMFAPreference =
             AdminSetUserMFAPreferenceResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminSetUserMFAPreferenceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminSetUserMFAPreference where

instance NFData AdminSetUserMFAPreference where

instance ToHeaders AdminSetUserMFAPreference where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminSetUserMFAPreference"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminSetUserMFAPreference where
        toJSON AdminSetUserMFAPreference'{..}
          = object
              (catMaybes
                 [("SMSMfaSettings" .=) <$> _asumpSMSMFASettings,
                  ("SoftwareTokenMfaSettings" .=) <$>
                    _asumpSoftwareTokenMFASettings,
                  Just ("Username" .= _asumpUsername),
                  Just ("UserPoolId" .= _asumpUserPoolId)])

instance ToPath AdminSetUserMFAPreference where
        toPath = const "/"

instance ToQuery AdminSetUserMFAPreference where
        toQuery = const mempty

-- | /See:/ 'adminSetUserMFAPreferenceResponse' smart constructor.
newtype AdminSetUserMFAPreferenceResponse = AdminSetUserMFAPreferenceResponse'
  { _asumprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminSetUserMFAPreferenceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asumprsResponseStatus' - -- | The response status code.
adminSetUserMFAPreferenceResponse
    :: Int -- ^ 'asumprsResponseStatus'
    -> AdminSetUserMFAPreferenceResponse
adminSetUserMFAPreferenceResponse pResponseStatus_ =
  AdminSetUserMFAPreferenceResponse' {_asumprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
asumprsResponseStatus :: Lens' AdminSetUserMFAPreferenceResponse Int
asumprsResponseStatus = lens _asumprsResponseStatus (\ s a -> s{_asumprsResponseStatus = a})

instance NFData AdminSetUserMFAPreferenceResponse
         where
