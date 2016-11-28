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
-- Module      : Network.AWS.CognitoIdentityProvider.ForgotPassword
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the password for the specified client ID or username.
--
--
module Network.AWS.CognitoIdentityProvider.ForgotPassword
    (
    -- * Creating a Request
      forgotPassword
    , ForgotPassword
    -- * Request Lenses
    , fpSecretHash
    , fpClientId
    , fpUsername

    -- * Destructuring the Response
    , forgotPasswordResponse
    , ForgotPasswordResponse
    -- * Response Lenses
    , fprsCodeDeliveryDetails
    , fprsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to reset a user's password.
--
--
--
-- /See:/ 'forgotPassword' smart constructor.
data ForgotPassword = ForgotPassword'
    { _fpSecretHash :: !(Maybe (Sensitive Text))
    , _fpClientId   :: !(Sensitive Text)
    , _fpUsername   :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ForgotPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fpSecretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- * 'fpClientId' - The ID of the client associated with the user pool.
--
-- * 'fpUsername' - The user name of the user for whom you want to enter a code to retrieve a forgotten password.
forgotPassword
    :: Text -- ^ 'fpClientId'
    -> Text -- ^ 'fpUsername'
    -> ForgotPassword
forgotPassword pClientId_ pUsername_ =
    ForgotPassword'
    { _fpSecretHash = Nothing
    , _fpClientId = _Sensitive # pClientId_
    , _fpUsername = _Sensitive # pUsername_
    }

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
fpSecretHash :: Lens' ForgotPassword (Maybe Text)
fpSecretHash = lens _fpSecretHash (\ s a -> s{_fpSecretHash = a}) . mapping _Sensitive;

-- | The ID of the client associated with the user pool.
fpClientId :: Lens' ForgotPassword Text
fpClientId = lens _fpClientId (\ s a -> s{_fpClientId = a}) . _Sensitive;

-- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
fpUsername :: Lens' ForgotPassword Text
fpUsername = lens _fpUsername (\ s a -> s{_fpUsername = a}) . _Sensitive;

instance AWSRequest ForgotPassword where
        type Rs ForgotPassword = ForgotPasswordResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ForgotPasswordResponse' <$>
                   (x .?> "CodeDeliveryDetails") <*>
                     (pure (fromEnum s)))

instance Hashable ForgotPassword

instance NFData ForgotPassword

instance ToHeaders ForgotPassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ForgotPassword"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ForgotPassword where
        toJSON ForgotPassword'{..}
          = object
              (catMaybes
                 [("SecretHash" .=) <$> _fpSecretHash,
                  Just ("ClientId" .= _fpClientId),
                  Just ("Username" .= _fpUsername)])

instance ToPath ForgotPassword where
        toPath = const "/"

instance ToQuery ForgotPassword where
        toQuery = const mempty

-- | Respresents the response from the server regarding the request to reset a password.
--
--
--
-- /See:/ 'forgotPasswordResponse' smart constructor.
data ForgotPasswordResponse = ForgotPasswordResponse'
    { _fprsCodeDeliveryDetails :: !(Maybe CodeDeliveryDetailsType)
    , _fprsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ForgotPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fprsCodeDeliveryDetails' - Undocumented member.
--
-- * 'fprsResponseStatus' - -- | The response status code.
forgotPasswordResponse
    :: Int -- ^ 'fprsResponseStatus'
    -> ForgotPasswordResponse
forgotPasswordResponse pResponseStatus_ =
    ForgotPasswordResponse'
    { _fprsCodeDeliveryDetails = Nothing
    , _fprsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
fprsCodeDeliveryDetails :: Lens' ForgotPasswordResponse (Maybe CodeDeliveryDetailsType)
fprsCodeDeliveryDetails = lens _fprsCodeDeliveryDetails (\ s a -> s{_fprsCodeDeliveryDetails = a});

-- | -- | The response status code.
fprsResponseStatus :: Lens' ForgotPasswordResponse Int
fprsResponseStatus = lens _fprsResponseStatus (\ s a -> s{_fprsResponseStatus = a});

instance NFData ForgotPasswordResponse
