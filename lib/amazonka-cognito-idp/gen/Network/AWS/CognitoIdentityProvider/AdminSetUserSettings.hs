{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /This action is no longer supported./ You can use it to configure only SMS MFA. You can't use it to configure TOTP software token MFA. To configure either type of MFA, use <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_AdminSetUserMFAPreference.html AdminSetUserMFAPreference> instead.
module Network.AWS.CognitoIdentityProvider.AdminSetUserSettings
  ( -- * Creating a Request
    adminSetUserSettings,
    AdminSetUserSettings,

    -- * Request Lenses
    asusUserPoolId,
    asusUsername,
    asusMFAOptions,

    -- * Destructuring the Response
    adminSetUserSettingsResponse,
    AdminSetUserSettingsResponse,

    -- * Response Lenses
    asusrsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | You can use this parameter to set an MFA configuration that uses the SMS delivery medium.
--
--
--
-- /See:/ 'adminSetUserSettings' smart constructor.
data AdminSetUserSettings = AdminSetUserSettings'
  { _asusUserPoolId ::
      !Text,
    _asusUsername :: !(Sensitive Text),
    _asusMFAOptions :: ![MFAOptionType]
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminSetUserSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asusUserPoolId' - The ID of the user pool that contains the user that you are setting options for.
--
-- * 'asusUsername' - The user name of the user that you are setting options for.
--
-- * 'asusMFAOptions' - You can use this parameter only to set an SMS configuration that uses SMS for delivery.
adminSetUserSettings ::
  -- | 'asusUserPoolId'
  Text ->
  -- | 'asusUsername'
  Text ->
  AdminSetUserSettings
adminSetUserSettings pUserPoolId_ pUsername_ =
  AdminSetUserSettings'
    { _asusUserPoolId = pUserPoolId_,
      _asusUsername = _Sensitive # pUsername_,
      _asusMFAOptions = mempty
    }

-- | The ID of the user pool that contains the user that you are setting options for.
asusUserPoolId :: Lens' AdminSetUserSettings Text
asusUserPoolId = lens _asusUserPoolId (\s a -> s {_asusUserPoolId = a})

-- | The user name of the user that you are setting options for.
asusUsername :: Lens' AdminSetUserSettings Text
asusUsername = lens _asusUsername (\s a -> s {_asusUsername = a}) . _Sensitive

-- | You can use this parameter only to set an SMS configuration that uses SMS for delivery.
asusMFAOptions :: Lens' AdminSetUserSettings [MFAOptionType]
asusMFAOptions = lens _asusMFAOptions (\s a -> s {_asusMFAOptions = a}) . _Coerce

instance AWSRequest AdminSetUserSettings where
  type Rs AdminSetUserSettings = AdminSetUserSettingsResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveEmpty
      (\s h x -> AdminSetUserSettingsResponse' <$> (pure (fromEnum s)))

instance Hashable AdminSetUserSettings

instance NFData AdminSetUserSettings

instance ToHeaders AdminSetUserSettings where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AWSCognitoIdentityProviderService.AdminSetUserSettings" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON AdminSetUserSettings where
  toJSON AdminSetUserSettings' {..} =
    object
      ( catMaybes
          [ Just ("UserPoolId" .= _asusUserPoolId),
            Just ("Username" .= _asusUsername),
            Just ("MFAOptions" .= _asusMFAOptions)
          ]
      )

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
  { _asusrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AdminSetUserSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asusrsResponseStatus' - -- | The response status code.
adminSetUserSettingsResponse ::
  -- | 'asusrsResponseStatus'
  Int ->
  AdminSetUserSettingsResponse
adminSetUserSettingsResponse pResponseStatus_ =
  AdminSetUserSettingsResponse'
    { _asusrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
asusrsResponseStatus :: Lens' AdminSetUserSettingsResponse Int
asusrsResponseStatus = lens _asusrsResponseStatus (\s a -> s {_asusrsResponseStatus = a})

instance NFData AdminSetUserSettingsResponse
