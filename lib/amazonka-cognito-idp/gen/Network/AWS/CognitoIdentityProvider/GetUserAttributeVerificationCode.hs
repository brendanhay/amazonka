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
-- Module      : Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the user attribute verification code for the specified attribute name.
--
--
module Network.AWS.CognitoIdentityProvider.GetUserAttributeVerificationCode
    (
    -- * Creating a Request
      getUserAttributeVerificationCode
    , GetUserAttributeVerificationCode
    -- * Request Lenses
    , guavcAccessToken
    , guavcAttributeName

    -- * Destructuring the Response
    , getUserAttributeVerificationCodeResponse
    , GetUserAttributeVerificationCodeResponse
    -- * Response Lenses
    , guavcrsCodeDeliveryDetails
    , guavcrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get user attribute verification.
--
--
--
-- /See:/ 'getUserAttributeVerificationCode' smart constructor.
data GetUserAttributeVerificationCode = GetUserAttributeVerificationCode'
  { _guavcAccessToken   :: !(Sensitive Text)
  , _guavcAttributeName :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserAttributeVerificationCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guavcAccessToken' - The access token returned by the server response to get the user attribute verification code.
--
-- * 'guavcAttributeName' - The attribute name returned by the server response to get the user attribute verification code.
getUserAttributeVerificationCode
    :: Text -- ^ 'guavcAccessToken'
    -> Text -- ^ 'guavcAttributeName'
    -> GetUserAttributeVerificationCode
getUserAttributeVerificationCode pAccessToken_ pAttributeName_ =
  GetUserAttributeVerificationCode'
    { _guavcAccessToken = _Sensitive # pAccessToken_
    , _guavcAttributeName = pAttributeName_
    }


-- | The access token returned by the server response to get the user attribute verification code.
guavcAccessToken :: Lens' GetUserAttributeVerificationCode Text
guavcAccessToken = lens _guavcAccessToken (\ s a -> s{_guavcAccessToken = a}) . _Sensitive

-- | The attribute name returned by the server response to get the user attribute verification code.
guavcAttributeName :: Lens' GetUserAttributeVerificationCode Text
guavcAttributeName = lens _guavcAttributeName (\ s a -> s{_guavcAttributeName = a})

instance AWSRequest GetUserAttributeVerificationCode
         where
        type Rs GetUserAttributeVerificationCode =
             GetUserAttributeVerificationCodeResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetUserAttributeVerificationCodeResponse' <$>
                   (x .?> "CodeDeliveryDetails") <*>
                     (pure (fromEnum s)))

instance Hashable GetUserAttributeVerificationCode
         where

instance NFData GetUserAttributeVerificationCode
         where

instance ToHeaders GetUserAttributeVerificationCode
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetUserAttributeVerificationCode"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetUserAttributeVerificationCode
         where
        toJSON GetUserAttributeVerificationCode'{..}
          = object
              (catMaybes
                 [Just ("AccessToken" .= _guavcAccessToken),
                  Just ("AttributeName" .= _guavcAttributeName)])

instance ToPath GetUserAttributeVerificationCode
         where
        toPath = const "/"

instance ToQuery GetUserAttributeVerificationCode
         where
        toQuery = const mempty

-- | The verification code response returned by the server response to get the user attribute verification code.
--
--
--
-- /See:/ 'getUserAttributeVerificationCodeResponse' smart constructor.
data GetUserAttributeVerificationCodeResponse = GetUserAttributeVerificationCodeResponse'
  { _guavcrsCodeDeliveryDetails :: !(Maybe CodeDeliveryDetailsType)
  , _guavcrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetUserAttributeVerificationCodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'guavcrsCodeDeliveryDetails' - The code delivery details returned by the server in response to the request to get the user attribute verification code.
--
-- * 'guavcrsResponseStatus' - -- | The response status code.
getUserAttributeVerificationCodeResponse
    :: Int -- ^ 'guavcrsResponseStatus'
    -> GetUserAttributeVerificationCodeResponse
getUserAttributeVerificationCodeResponse pResponseStatus_ =
  GetUserAttributeVerificationCodeResponse'
    { _guavcrsCodeDeliveryDetails = Nothing
    , _guavcrsResponseStatus = pResponseStatus_
    }


-- | The code delivery details returned by the server in response to the request to get the user attribute verification code.
guavcrsCodeDeliveryDetails :: Lens' GetUserAttributeVerificationCodeResponse (Maybe CodeDeliveryDetailsType)
guavcrsCodeDeliveryDetails = lens _guavcrsCodeDeliveryDetails (\ s a -> s{_guavcrsCodeDeliveryDetails = a})

-- | -- | The response status code.
guavcrsResponseStatus :: Lens' GetUserAttributeVerificationCodeResponse Int
guavcrsResponseStatus = lens _guavcrsResponseStatus (\ s a -> s{_guavcrsResponseStatus = a})

instance NFData
           GetUserAttributeVerificationCodeResponse
         where
