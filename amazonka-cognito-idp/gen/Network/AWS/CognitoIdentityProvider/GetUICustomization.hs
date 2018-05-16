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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUICustomization
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the UI Customization information for a particular app client's app UI, if there is something set. If nothing is set for the particular client, but there is an existing pool level customization (app @clientId@ will be @ALL@ ), then that is returned. If nothing is present, then an empty shape is returned.
--
--
module Network.AWS.CognitoIdentityProvider.GetUICustomization
    (
    -- * Creating a Request
      getUICustomization
    , GetUICustomization
    -- * Request Lenses
    , guicClientId
    , guicUserPoolId

    -- * Destructuring the Response
    , getUICustomizationResponse
    , GetUICustomizationResponse
    -- * Response Lenses
    , guicrsResponseStatus
    , guicrsUICustomization
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getUICustomization' smart constructor.
data GetUICustomization = GetUICustomization'
  { _guicClientId   :: !(Maybe (Sensitive Text))
  , _guicUserPoolId :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUICustomization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guicClientId' - The client ID for the client app.
--
-- * 'guicUserPoolId' - The user pool ID for the user pool.
getUICustomization
    :: Text -- ^ 'guicUserPoolId'
    -> GetUICustomization
getUICustomization pUserPoolId_ =
  GetUICustomization' {_guicClientId = Nothing, _guicUserPoolId = pUserPoolId_}


-- | The client ID for the client app.
guicClientId :: Lens' GetUICustomization (Maybe Text)
guicClientId = lens _guicClientId (\ s a -> s{_guicClientId = a}) . mapping _Sensitive

-- | The user pool ID for the user pool.
guicUserPoolId :: Lens' GetUICustomization Text
guicUserPoolId = lens _guicUserPoolId (\ s a -> s{_guicUserPoolId = a})

instance AWSRequest GetUICustomization where
        type Rs GetUICustomization =
             GetUICustomizationResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetUICustomizationResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "UICustomization"))

instance Hashable GetUICustomization where

instance NFData GetUICustomization where

instance ToHeaders GetUICustomization where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetUICustomization"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUICustomization where
        toJSON GetUICustomization'{..}
          = object
              (catMaybes
                 [("ClientId" .=) <$> _guicClientId,
                  Just ("UserPoolId" .= _guicUserPoolId)])

instance ToPath GetUICustomization where
        toPath = const "/"

instance ToQuery GetUICustomization where
        toQuery = const mempty

-- | /See:/ 'getUICustomizationResponse' smart constructor.
data GetUICustomizationResponse = GetUICustomizationResponse'
  { _guicrsResponseStatus  :: !Int
  , _guicrsUICustomization :: !UICustomizationType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUICustomizationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guicrsResponseStatus' - -- | The response status code.
--
-- * 'guicrsUICustomization' - The UI customization information.
getUICustomizationResponse
    :: Int -- ^ 'guicrsResponseStatus'
    -> UICustomizationType -- ^ 'guicrsUICustomization'
    -> GetUICustomizationResponse
getUICustomizationResponse pResponseStatus_ pUICustomization_ =
  GetUICustomizationResponse'
    { _guicrsResponseStatus = pResponseStatus_
    , _guicrsUICustomization = pUICustomization_
    }


-- | -- | The response status code.
guicrsResponseStatus :: Lens' GetUICustomizationResponse Int
guicrsResponseStatus = lens _guicrsResponseStatus (\ s a -> s{_guicrsResponseStatus = a})

-- | The UI customization information.
guicrsUICustomization :: Lens' GetUICustomizationResponse UICustomizationType
guicrsUICustomization = lens _guicrsUICustomization (\ s a -> s{_guicrsUICustomization = a})

instance NFData GetUICustomizationResponse where
