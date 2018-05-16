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
-- Module      : Network.AWS.CognitoIdentityProvider.SetUserPoolMFAConfig
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set the user pool MFA configuration.
--
--
module Network.AWS.CognitoIdentityProvider.SetUserPoolMFAConfig
    (
    -- * Creating a Request
      setUserPoolMFAConfig
    , SetUserPoolMFAConfig
    -- * Request Lenses
    , supmcSmsMFAConfiguration
    , supmcSoftwareTokenMFAConfiguration
    , supmcMFAConfiguration
    , supmcUserPoolId

    -- * Destructuring the Response
    , setUserPoolMFAConfigResponse
    , SetUserPoolMFAConfigResponse
    -- * Response Lenses
    , supmcrsSmsMFAConfiguration
    , supmcrsSoftwareTokenMFAConfiguration
    , supmcrsMFAConfiguration
    , supmcrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setUserPoolMFAConfig' smart constructor.
data SetUserPoolMFAConfig = SetUserPoolMFAConfig'
  { _supmcSmsMFAConfiguration           :: !(Maybe SmsMFAConfigType)
  , _supmcSoftwareTokenMFAConfiguration :: !(Maybe SoftwareTokenMFAConfigType)
  , _supmcMFAConfiguration              :: !(Maybe UserPoolMFAType)
  , _supmcUserPoolId                    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetUserPoolMFAConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'supmcSmsMFAConfiguration' - The SMS text message MFA configuration.
--
-- * 'supmcSoftwareTokenMFAConfiguration' - The software token MFA configuration.
--
-- * 'supmcMFAConfiguration' - The MFA configuration.
--
-- * 'supmcUserPoolId' - The user pool ID.
setUserPoolMFAConfig
    :: Text -- ^ 'supmcUserPoolId'
    -> SetUserPoolMFAConfig
setUserPoolMFAConfig pUserPoolId_ =
  SetUserPoolMFAConfig'
    { _supmcSmsMFAConfiguration = Nothing
    , _supmcSoftwareTokenMFAConfiguration = Nothing
    , _supmcMFAConfiguration = Nothing
    , _supmcUserPoolId = pUserPoolId_
    }


-- | The SMS text message MFA configuration.
supmcSmsMFAConfiguration :: Lens' SetUserPoolMFAConfig (Maybe SmsMFAConfigType)
supmcSmsMFAConfiguration = lens _supmcSmsMFAConfiguration (\ s a -> s{_supmcSmsMFAConfiguration = a})

-- | The software token MFA configuration.
supmcSoftwareTokenMFAConfiguration :: Lens' SetUserPoolMFAConfig (Maybe SoftwareTokenMFAConfigType)
supmcSoftwareTokenMFAConfiguration = lens _supmcSoftwareTokenMFAConfiguration (\ s a -> s{_supmcSoftwareTokenMFAConfiguration = a})

-- | The MFA configuration.
supmcMFAConfiguration :: Lens' SetUserPoolMFAConfig (Maybe UserPoolMFAType)
supmcMFAConfiguration = lens _supmcMFAConfiguration (\ s a -> s{_supmcMFAConfiguration = a})

-- | The user pool ID.
supmcUserPoolId :: Lens' SetUserPoolMFAConfig Text
supmcUserPoolId = lens _supmcUserPoolId (\ s a -> s{_supmcUserPoolId = a})

instance AWSRequest SetUserPoolMFAConfig where
        type Rs SetUserPoolMFAConfig =
             SetUserPoolMFAConfigResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 SetUserPoolMFAConfigResponse' <$>
                   (x .?> "SmsMfaConfiguration") <*>
                     (x .?> "SoftwareTokenMfaConfiguration")
                     <*> (x .?> "MfaConfiguration")
                     <*> (pure (fromEnum s)))

instance Hashable SetUserPoolMFAConfig where

instance NFData SetUserPoolMFAConfig where

instance ToHeaders SetUserPoolMFAConfig where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.SetUserPoolMfaConfig"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetUserPoolMFAConfig where
        toJSON SetUserPoolMFAConfig'{..}
          = object
              (catMaybes
                 [("SmsMfaConfiguration" .=) <$>
                    _supmcSmsMFAConfiguration,
                  ("SoftwareTokenMfaConfiguration" .=) <$>
                    _supmcSoftwareTokenMFAConfiguration,
                  ("MfaConfiguration" .=) <$> _supmcMFAConfiguration,
                  Just ("UserPoolId" .= _supmcUserPoolId)])

instance ToPath SetUserPoolMFAConfig where
        toPath = const "/"

instance ToQuery SetUserPoolMFAConfig where
        toQuery = const mempty

-- | /See:/ 'setUserPoolMFAConfigResponse' smart constructor.
data SetUserPoolMFAConfigResponse = SetUserPoolMFAConfigResponse'
  { _supmcrsSmsMFAConfiguration           :: !(Maybe SmsMFAConfigType)
  , _supmcrsSoftwareTokenMFAConfiguration :: !(Maybe SoftwareTokenMFAConfigType)
  , _supmcrsMFAConfiguration              :: !(Maybe UserPoolMFAType)
  , _supmcrsResponseStatus                :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetUserPoolMFAConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'supmcrsSmsMFAConfiguration' - The SMS text message MFA configuration.
--
-- * 'supmcrsSoftwareTokenMFAConfiguration' - The software token MFA configuration.
--
-- * 'supmcrsMFAConfiguration' - The MFA configuration.
--
-- * 'supmcrsResponseStatus' - -- | The response status code.
setUserPoolMFAConfigResponse
    :: Int -- ^ 'supmcrsResponseStatus'
    -> SetUserPoolMFAConfigResponse
setUserPoolMFAConfigResponse pResponseStatus_ =
  SetUserPoolMFAConfigResponse'
    { _supmcrsSmsMFAConfiguration = Nothing
    , _supmcrsSoftwareTokenMFAConfiguration = Nothing
    , _supmcrsMFAConfiguration = Nothing
    , _supmcrsResponseStatus = pResponseStatus_
    }


-- | The SMS text message MFA configuration.
supmcrsSmsMFAConfiguration :: Lens' SetUserPoolMFAConfigResponse (Maybe SmsMFAConfigType)
supmcrsSmsMFAConfiguration = lens _supmcrsSmsMFAConfiguration (\ s a -> s{_supmcrsSmsMFAConfiguration = a})

-- | The software token MFA configuration.
supmcrsSoftwareTokenMFAConfiguration :: Lens' SetUserPoolMFAConfigResponse (Maybe SoftwareTokenMFAConfigType)
supmcrsSoftwareTokenMFAConfiguration = lens _supmcrsSoftwareTokenMFAConfiguration (\ s a -> s{_supmcrsSoftwareTokenMFAConfiguration = a})

-- | The MFA configuration.
supmcrsMFAConfiguration :: Lens' SetUserPoolMFAConfigResponse (Maybe UserPoolMFAType)
supmcrsMFAConfiguration = lens _supmcrsMFAConfiguration (\ s a -> s{_supmcrsMFAConfiguration = a})

-- | -- | The response status code.
supmcrsResponseStatus :: Lens' SetUserPoolMFAConfigResponse Int
supmcrsResponseStatus = lens _supmcrsResponseStatus (\ s a -> s{_supmcrsResponseStatus = a})

instance NFData SetUserPoolMFAConfigResponse where
