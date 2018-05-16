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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserPoolMFAConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user pool multi-factor authentication (MFA) configuration.
--
--
module Network.AWS.CognitoIdentityProvider.GetUserPoolMFAConfig
    (
    -- * Creating a Request
      getUserPoolMFAConfig
    , GetUserPoolMFAConfig
    -- * Request Lenses
    , gupmcUserPoolId

    -- * Destructuring the Response
    , getUserPoolMFAConfigResponse
    , GetUserPoolMFAConfigResponse
    -- * Response Lenses
    , gupmcrsSmsMFAConfiguration
    , gupmcrsSoftwareTokenMFAConfiguration
    , gupmcrsMFAConfiguration
    , gupmcrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUserPoolMFAConfig' smart constructor.
newtype GetUserPoolMFAConfig = GetUserPoolMFAConfig'
  { _gupmcUserPoolId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserPoolMFAConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gupmcUserPoolId' - The user pool ID.
getUserPoolMFAConfig
    :: Text -- ^ 'gupmcUserPoolId'
    -> GetUserPoolMFAConfig
getUserPoolMFAConfig pUserPoolId_ =
  GetUserPoolMFAConfig' {_gupmcUserPoolId = pUserPoolId_}


-- | The user pool ID.
gupmcUserPoolId :: Lens' GetUserPoolMFAConfig Text
gupmcUserPoolId = lens _gupmcUserPoolId (\ s a -> s{_gupmcUserPoolId = a})

instance AWSRequest GetUserPoolMFAConfig where
        type Rs GetUserPoolMFAConfig =
             GetUserPoolMFAConfigResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetUserPoolMFAConfigResponse' <$>
                   (x .?> "SmsMfaConfiguration") <*>
                     (x .?> "SoftwareTokenMfaConfiguration")
                     <*> (x .?> "MfaConfiguration")
                     <*> (pure (fromEnum s)))

instance Hashable GetUserPoolMFAConfig where

instance NFData GetUserPoolMFAConfig where

instance ToHeaders GetUserPoolMFAConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetUserPoolMfaConfig"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUserPoolMFAConfig where
        toJSON GetUserPoolMFAConfig'{..}
          = object
              (catMaybes [Just ("UserPoolId" .= _gupmcUserPoolId)])

instance ToPath GetUserPoolMFAConfig where
        toPath = const "/"

instance ToQuery GetUserPoolMFAConfig where
        toQuery = const mempty

-- | /See:/ 'getUserPoolMFAConfigResponse' smart constructor.
data GetUserPoolMFAConfigResponse = GetUserPoolMFAConfigResponse'
  { _gupmcrsSmsMFAConfiguration           :: !(Maybe SmsMFAConfigType)
  , _gupmcrsSoftwareTokenMFAConfiguration :: !(Maybe SoftwareTokenMFAConfigType)
  , _gupmcrsMFAConfiguration              :: !(Maybe UserPoolMFAType)
  , _gupmcrsResponseStatus                :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserPoolMFAConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gupmcrsSmsMFAConfiguration' - The SMS text message multi-factor (MFA) configuration.
--
-- * 'gupmcrsSoftwareTokenMFAConfiguration' - The software token multi-factor (MFA) configuration.
--
-- * 'gupmcrsMFAConfiguration' - The multi-factor (MFA) configuration.
--
-- * 'gupmcrsResponseStatus' - -- | The response status code.
getUserPoolMFAConfigResponse
    :: Int -- ^ 'gupmcrsResponseStatus'
    -> GetUserPoolMFAConfigResponse
getUserPoolMFAConfigResponse pResponseStatus_ =
  GetUserPoolMFAConfigResponse'
    { _gupmcrsSmsMFAConfiguration = Nothing
    , _gupmcrsSoftwareTokenMFAConfiguration = Nothing
    , _gupmcrsMFAConfiguration = Nothing
    , _gupmcrsResponseStatus = pResponseStatus_
    }


-- | The SMS text message multi-factor (MFA) configuration.
gupmcrsSmsMFAConfiguration :: Lens' GetUserPoolMFAConfigResponse (Maybe SmsMFAConfigType)
gupmcrsSmsMFAConfiguration = lens _gupmcrsSmsMFAConfiguration (\ s a -> s{_gupmcrsSmsMFAConfiguration = a})

-- | The software token multi-factor (MFA) configuration.
gupmcrsSoftwareTokenMFAConfiguration :: Lens' GetUserPoolMFAConfigResponse (Maybe SoftwareTokenMFAConfigType)
gupmcrsSoftwareTokenMFAConfiguration = lens _gupmcrsSoftwareTokenMFAConfiguration (\ s a -> s{_gupmcrsSoftwareTokenMFAConfiguration = a})

-- | The multi-factor (MFA) configuration.
gupmcrsMFAConfiguration :: Lens' GetUserPoolMFAConfigResponse (Maybe UserPoolMFAType)
gupmcrsMFAConfiguration = lens _gupmcrsMFAConfiguration (\ s a -> s{_gupmcrsMFAConfiguration = a})

-- | -- | The response status code.
gupmcrsResponseStatus :: Lens' GetUserPoolMFAConfigResponse Int
gupmcrsResponseStatus = lens _gupmcrsResponseStatus (\ s a -> s{_gupmcrsResponseStatus = a})

instance NFData GetUserPoolMFAConfigResponse where
