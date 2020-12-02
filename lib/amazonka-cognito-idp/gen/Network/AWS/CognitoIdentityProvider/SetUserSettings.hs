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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserSettings
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the user settings like multi-factor authentication (MFA). If MFA is to be removed for a particular attribute pass the attribute with code delivery as null. If null list is passed, all MFA options are removed.
--
--
module Network.AWS.CognitoIdentityProvider.SetUserSettings
    (
    -- * Creating a Request
      setUserSettings
    , SetUserSettings
    -- * Request Lenses
    , susAccessToken
    , susMFAOptions

    -- * Destructuring the Response
    , setUserSettingsResponse
    , SetUserSettingsResponse
    -- * Response Lenses
    , susrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to set user settings.
--
--
--
-- /See:/ 'setUserSettings' smart constructor.
data SetUserSettings = SetUserSettings'
  { _susAccessToken :: !(Sensitive Text)
  , _susMFAOptions  :: ![MFAOptionType]
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetUserSettings' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'susAccessToken' - The access token for the set user settings request.
--
-- * 'susMFAOptions' - Specifies the options for MFA (e.g., email or phone number).
setUserSettings
    :: Text -- ^ 'susAccessToken'
    -> SetUserSettings
setUserSettings pAccessToken_ =
  SetUserSettings'
    {_susAccessToken = _Sensitive # pAccessToken_, _susMFAOptions = mempty}


-- | The access token for the set user settings request.
susAccessToken :: Lens' SetUserSettings Text
susAccessToken = lens _susAccessToken (\ s a -> s{_susAccessToken = a}) . _Sensitive

-- | Specifies the options for MFA (e.g., email or phone number).
susMFAOptions :: Lens' SetUserSettings [MFAOptionType]
susMFAOptions = lens _susMFAOptions (\ s a -> s{_susMFAOptions = a}) . _Coerce

instance AWSRequest SetUserSettings where
        type Rs SetUserSettings = SetUserSettingsResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 SetUserSettingsResponse' <$> (pure (fromEnum s)))

instance Hashable SetUserSettings where

instance NFData SetUserSettings where

instance ToHeaders SetUserSettings where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.SetUserSettings"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetUserSettings where
        toJSON SetUserSettings'{..}
          = object
              (catMaybes
                 [Just ("AccessToken" .= _susAccessToken),
                  Just ("MFAOptions" .= _susMFAOptions)])

instance ToPath SetUserSettings where
        toPath = const "/"

instance ToQuery SetUserSettings where
        toQuery = const mempty

-- | The response from the server for a set user settings request.
--
--
--
-- /See:/ 'setUserSettingsResponse' smart constructor.
newtype SetUserSettingsResponse = SetUserSettingsResponse'
  { _susrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetUserSettingsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'susrsResponseStatus' - -- | The response status code.
setUserSettingsResponse
    :: Int -- ^ 'susrsResponseStatus'
    -> SetUserSettingsResponse
setUserSettingsResponse pResponseStatus_ =
  SetUserSettingsResponse' {_susrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
susrsResponseStatus :: Lens' SetUserSettingsResponse Int
susrsResponseStatus = lens _susrsResponseStatus (\ s a -> s{_susrsResponseStatus = a})

instance NFData SetUserSettingsResponse where
