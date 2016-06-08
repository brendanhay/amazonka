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
-- Module      : Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Resends the confirmation (for confirmation of registration) to a specific user in the user pool.
module Network.AWS.CognitoIdentityProvider.ResendConfirmationCode
    (
    -- * Creating a Request
      resendConfirmationCode
    , ResendConfirmationCode
    -- * Request Lenses
    , rccSecretHash
    , rccClientId
    , rccUsername

    -- * Destructuring the Response
    , resendConfirmationCodeResponse
    , ResendConfirmationCodeResponse
    -- * Response Lenses
    , rccrsCodeDeliveryDetails
    , rccrsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to resend the confirmation code.
--
-- /See:/ 'resendConfirmationCode' smart constructor.
data ResendConfirmationCode = ResendConfirmationCode'
    { _rccSecretHash :: !(Maybe (Sensitive Text))
    , _rccClientId   :: !(Sensitive Text)
    , _rccUsername   :: !(Sensitive Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResendConfirmationCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccSecretHash'
--
-- * 'rccClientId'
--
-- * 'rccUsername'
resendConfirmationCode
    :: Text -- ^ 'rccClientId'
    -> Text -- ^ 'rccUsername'
    -> ResendConfirmationCode
resendConfirmationCode pClientId_ pUsername_ =
    ResendConfirmationCode'
    { _rccSecretHash = Nothing
    , _rccClientId = _Sensitive # pClientId_
    , _rccUsername = _Sensitive # pUsername_
    }

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
rccSecretHash :: Lens' ResendConfirmationCode (Maybe Text)
rccSecretHash = lens _rccSecretHash (\ s a -> s{_rccSecretHash = a}) . mapping _Sensitive;

-- | The ID of the client associated with the user pool.
rccClientId :: Lens' ResendConfirmationCode Text
rccClientId = lens _rccClientId (\ s a -> s{_rccClientId = a}) . _Sensitive;

-- | The user name of the user to whom you wish to resend a confirmation code.
rccUsername :: Lens' ResendConfirmationCode Text
rccUsername = lens _rccUsername (\ s a -> s{_rccUsername = a}) . _Sensitive;

instance AWSRequest ResendConfirmationCode where
        type Rs ResendConfirmationCode =
             ResendConfirmationCodeResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ResendConfirmationCodeResponse' <$>
                   (x .?> "CodeDeliveryDetails") <*>
                     (pure (fromEnum s)))

instance Hashable ResendConfirmationCode

instance NFData ResendConfirmationCode

instance ToHeaders ResendConfirmationCode where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ResendConfirmationCode"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ResendConfirmationCode where
        toJSON ResendConfirmationCode'{..}
          = object
              (catMaybes
                 [("SecretHash" .=) <$> _rccSecretHash,
                  Just ("ClientId" .= _rccClientId),
                  Just ("Username" .= _rccUsername)])

instance ToPath ResendConfirmationCode where
        toPath = const "/"

instance ToQuery ResendConfirmationCode where
        toQuery = const mempty

-- | The response from the server when the Amazon Cognito service makes the request to resend a confirmation code.
--
-- /See:/ 'resendConfirmationCodeResponse' smart constructor.
data ResendConfirmationCodeResponse = ResendConfirmationCodeResponse'
    { _rccrsCodeDeliveryDetails :: !(Maybe CodeDeliveryDetailsType)
    , _rccrsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ResendConfirmationCodeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rccrsCodeDeliveryDetails'
--
-- * 'rccrsResponseStatus'
resendConfirmationCodeResponse
    :: Int -- ^ 'rccrsResponseStatus'
    -> ResendConfirmationCodeResponse
resendConfirmationCodeResponse pResponseStatus_ =
    ResendConfirmationCodeResponse'
    { _rccrsCodeDeliveryDetails = Nothing
    , _rccrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rccrsCodeDeliveryDetails :: Lens' ResendConfirmationCodeResponse (Maybe CodeDeliveryDetailsType)
rccrsCodeDeliveryDetails = lens _rccrsCodeDeliveryDetails (\ s a -> s{_rccrsCodeDeliveryDetails = a});

-- | The response status code.
rccrsResponseStatus :: Lens' ResendConfirmationCodeResponse Int
rccrsResponseStatus = lens _rccrsResponseStatus (\ s a -> s{_rccrsResponseStatus = a});

instance NFData ResendConfirmationCodeResponse
