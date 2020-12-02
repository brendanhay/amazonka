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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Calling this API causes a message to be sent to the end user with a confirmation code that is required to change the user's password. For the @Username@ parameter, you can use the username or user alias. If a verified phone number exists for the user, the confirmation code is sent to the phone number. Otherwise, if a verified email exists, the confirmation code is sent to the email. If neither a verified phone number nor a verified email exists, @InvalidParameterException@ is thrown. To use the confirmation code for resetting the password, call .
--
--
module Network.AWS.CognitoIdentityProvider.ForgotPassword
    (
    -- * Creating a Request
      forgotPassword
    , ForgotPassword
    -- * Request Lenses
    , fpAnalyticsMetadata
    , fpUserContextData
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

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to reset a user's password.
--
--
--
-- /See:/ 'forgotPassword' smart constructor.
data ForgotPassword = ForgotPassword'
  { _fpAnalyticsMetadata :: !(Maybe AnalyticsMetadataType)
  , _fpUserContextData   :: !(Maybe UserContextDataType)
  , _fpSecretHash        :: !(Maybe (Sensitive Text))
  , _fpClientId          :: !(Sensitive Text)
  , _fpUsername          :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ForgotPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fpAnalyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ForgotPassword@ calls.
--
-- * 'fpUserContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- * 'fpSecretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- * 'fpClientId' - The ID of the client associated with the user pool.
--
-- * 'fpUsername' - The user name of the user for whom you want to enter a code to reset a forgotten password.
forgotPassword
    :: Text -- ^ 'fpClientId'
    -> Text -- ^ 'fpUsername'
    -> ForgotPassword
forgotPassword pClientId_ pUsername_ =
  ForgotPassword'
    { _fpAnalyticsMetadata = Nothing
    , _fpUserContextData = Nothing
    , _fpSecretHash = Nothing
    , _fpClientId = _Sensitive # pClientId_
    , _fpUsername = _Sensitive # pUsername_
    }


-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ForgotPassword@ calls.
fpAnalyticsMetadata :: Lens' ForgotPassword (Maybe AnalyticsMetadataType)
fpAnalyticsMetadata = lens _fpAnalyticsMetadata (\ s a -> s{_fpAnalyticsMetadata = a})

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
fpUserContextData :: Lens' ForgotPassword (Maybe UserContextDataType)
fpUserContextData = lens _fpUserContextData (\ s a -> s{_fpUserContextData = a})

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
fpSecretHash :: Lens' ForgotPassword (Maybe Text)
fpSecretHash = lens _fpSecretHash (\ s a -> s{_fpSecretHash = a}) . mapping _Sensitive

-- | The ID of the client associated with the user pool.
fpClientId :: Lens' ForgotPassword Text
fpClientId = lens _fpClientId (\ s a -> s{_fpClientId = a}) . _Sensitive

-- | The user name of the user for whom you want to enter a code to reset a forgotten password.
fpUsername :: Lens' ForgotPassword Text
fpUsername = lens _fpUsername (\ s a -> s{_fpUsername = a}) . _Sensitive

instance AWSRequest ForgotPassword where
        type Rs ForgotPassword = ForgotPasswordResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ForgotPasswordResponse' <$>
                   (x .?> "CodeDeliveryDetails") <*>
                     (pure (fromEnum s)))

instance Hashable ForgotPassword where

instance NFData ForgotPassword where

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
                 [("AnalyticsMetadata" .=) <$> _fpAnalyticsMetadata,
                  ("UserContextData" .=) <$> _fpUserContextData,
                  ("SecretHash" .=) <$> _fpSecretHash,
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
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ForgotPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fprsCodeDeliveryDetails' - The code delivery details returned by the server in response to the request to reset a password.
--
-- * 'fprsResponseStatus' - -- | The response status code.
forgotPasswordResponse
    :: Int -- ^ 'fprsResponseStatus'
    -> ForgotPasswordResponse
forgotPasswordResponse pResponseStatus_ =
  ForgotPasswordResponse'
    {_fprsCodeDeliveryDetails = Nothing, _fprsResponseStatus = pResponseStatus_}


-- | The code delivery details returned by the server in response to the request to reset a password.
fprsCodeDeliveryDetails :: Lens' ForgotPasswordResponse (Maybe CodeDeliveryDetailsType)
fprsCodeDeliveryDetails = lens _fprsCodeDeliveryDetails (\ s a -> s{_fprsCodeDeliveryDetails = a})

-- | -- | The response status code.
fprsResponseStatus :: Lens' ForgotPasswordResponse Int
fprsResponseStatus = lens _fprsResponseStatus (\ s a -> s{_fprsResponseStatus = a})

instance NFData ForgotPasswordResponse where
