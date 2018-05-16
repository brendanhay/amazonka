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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets all the user settings for a specified user name. Works on any user.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
    (
    -- * Creating a Request
      adminSetUserSettings
    , AdminSetUserSettings
    -- * Request Lenses
    , asusUserPoolId
    , asusUsername
    , asusMFAOptions

    -- * Destructuring the Response
    , adminSetUserSettingsResponse
    , AdminSetUserSettingsResponse
    -- * Response Lenses
    , asusrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to set user settings as an administrator.
--
--
--
-- /See:/ 'adminSetUserSettings' smart constructor.
data AdminSetUserSettings = AdminSetUserSettings'
  { _asusUserPoolId :: !Text
  , _asusUsername   :: !(Sensitive Text)
  , _asusMFAOptions :: ![MFAOptionType]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminSetUserSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asusUserPoolId' - The user pool ID for the user pool where you want to set the user's settings, such as MFA options.
--
-- * 'asusUsername' - The user name of the user for whom you wish to set user settings.
--
-- * 'asusMFAOptions' - Specifies the options for MFA (e.g., email or phone number).
adminSetUserSettings
    :: Text -- ^ 'asusUserPoolId'
    -> Text -- ^ 'asusUsername'
    -> AdminSetUserSettings
adminSetUserSettings pUserPoolId_ pUsername_ =
  AdminSetUserSettings'
    { _asusUserPoolId = pUserPoolId_
    , _asusUsername = _Sensitive # pUsername_
    , _asusMFAOptions = mempty
    }


-- | The user pool ID for the user pool where you want to set the user's settings, such as MFA options.
asusUserPoolId :: Lens' AdminSetUserSettings Text
asusUserPoolId = lens _asusUserPoolId (\ s a -> s{_asusUserPoolId = a})

-- | The user name of the user for whom you wish to set user settings.
asusUsername :: Lens' AdminSetUserSettings Text
asusUsername = lens _asusUsername (\ s a -> s{_asusUsername = a}) . _Sensitive

-- | Specifies the options for MFA (e.g., email or phone number).
asusMFAOptions :: Lens' AdminSetUserSettings [MFAOptionType]
asusMFAOptions = lens _asusMFAOptions (\ s a -> s{_asusMFAOptions = a}) . _Coerce

instance AWSRequest AdminSetUserSettings where
        type Rs AdminSetUserSettings =
             AdminSetUserSettingsResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminSetUserSettingsResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminSetUserSettings where

instance NFData AdminSetUserSettings where

instance ToHeaders AdminSetUserSettings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminSetUserSettings"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminSetUserSettings where
        toJSON AdminSetUserSettings'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _asusUserPoolId),
                  Just ("Username" .= _asusUsername),
                  Just ("MFAOptions" .= _asusMFAOptions)])

instance ToPath AdminSetUserSettings where
        toPath = const "/"

instance ToQuery AdminSetUserSettings where
        toQuery = const mempty

-- | Represents the response from the server to set user settings as an administrator.
--
--
--
-- /See:/ 'adminSetUserSettingsResponse' smart constructor.
newtype AdminSetUserSettingsResponse = AdminSetUserSettingsResponse'
  { _asusrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminSetUserSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asusrsResponseStatus' - -- | The response status code.
adminSetUserSettingsResponse
    :: Int -- ^ 'asusrsResponseStatus'
    -> AdminSetUserSettingsResponse
adminSetUserSettingsResponse pResponseStatus_ =
  AdminSetUserSettingsResponse' {_asusrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
asusrsResponseStatus :: Lens' AdminSetUserSettingsResponse Int
asusrsResponseStatus = lens _asusrsResponseStatus (\ s a -> s{_asusrsResponseStatus = a})

instance NFData AdminSetUserSettingsResponse where
