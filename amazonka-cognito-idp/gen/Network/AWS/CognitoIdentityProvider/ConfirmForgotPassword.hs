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
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows a user to enter a confirmation code to reset a forgotten password.
--
--
module Network.AWS.CognitoIdentityProvider.ConfirmForgotPassword
    (
    -- * Creating a Request
      confirmForgotPassword
    , ConfirmForgotPassword
    -- * Request Lenses
    , cfpAnalyticsMetadata
    , cfpUserContextData
    , cfpSecretHash
    , cfpClientId
    , cfpUsername
    , cfpConfirmationCode
    , cfpPassword

    -- * Destructuring the Response
    , confirmForgotPasswordResponse
    , ConfirmForgotPasswordResponse
    -- * Response Lenses
    , cfprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request representing the confirmation for a password reset.
--
--
--
-- /See:/ 'confirmForgotPassword' smart constructor.
data ConfirmForgotPassword = ConfirmForgotPassword'
  { _cfpAnalyticsMetadata :: !(Maybe AnalyticsMetadataType)
  , _cfpUserContextData   :: !(Maybe UserContextDataType)
  , _cfpSecretHash        :: !(Maybe (Sensitive Text))
  , _cfpClientId          :: !(Sensitive Text)
  , _cfpUsername          :: !(Sensitive Text)
  , _cfpConfirmationCode  :: !Text
  , _cfpPassword          :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmForgotPassword' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfpAnalyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
--
-- * 'cfpUserContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- * 'cfpSecretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- * 'cfpClientId' - The app client ID of the app associated with the user pool.
--
-- * 'cfpUsername' - The user name of the user for whom you want to enter a code to retrieve a forgotten password.
--
-- * 'cfpConfirmationCode' - The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see
--
-- * 'cfpPassword' - The password sent by a user's request to retrieve a forgotten password.
confirmForgotPassword
    :: Text -- ^ 'cfpClientId'
    -> Text -- ^ 'cfpUsername'
    -> Text -- ^ 'cfpConfirmationCode'
    -> Text -- ^ 'cfpPassword'
    -> ConfirmForgotPassword
confirmForgotPassword pClientId_ pUsername_ pConfirmationCode_ pPassword_ =
  ConfirmForgotPassword'
    { _cfpAnalyticsMetadata = Nothing
    , _cfpUserContextData = Nothing
    , _cfpSecretHash = Nothing
    , _cfpClientId = _Sensitive # pClientId_
    , _cfpUsername = _Sensitive # pUsername_
    , _cfpConfirmationCode = pConfirmationCode_
    , _cfpPassword = _Sensitive # pPassword_
    }


-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmForgotPassword@ calls.
cfpAnalyticsMetadata :: Lens' ConfirmForgotPassword (Maybe AnalyticsMetadataType)
cfpAnalyticsMetadata = lens _cfpAnalyticsMetadata (\ s a -> s{_cfpAnalyticsMetadata = a})

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
cfpUserContextData :: Lens' ConfirmForgotPassword (Maybe UserContextDataType)
cfpUserContextData = lens _cfpUserContextData (\ s a -> s{_cfpUserContextData = a})

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
cfpSecretHash :: Lens' ConfirmForgotPassword (Maybe Text)
cfpSecretHash = lens _cfpSecretHash (\ s a -> s{_cfpSecretHash = a}) . mapping _Sensitive

-- | The app client ID of the app associated with the user pool.
cfpClientId :: Lens' ConfirmForgotPassword Text
cfpClientId = lens _cfpClientId (\ s a -> s{_cfpClientId = a}) . _Sensitive

-- | The user name of the user for whom you want to enter a code to retrieve a forgotten password.
cfpUsername :: Lens' ConfirmForgotPassword Text
cfpUsername = lens _cfpUsername (\ s a -> s{_cfpUsername = a}) . _Sensitive

-- | The confirmation code sent by a user's request to retrieve a forgotten password. For more information, see
cfpConfirmationCode :: Lens' ConfirmForgotPassword Text
cfpConfirmationCode = lens _cfpConfirmationCode (\ s a -> s{_cfpConfirmationCode = a})

-- | The password sent by a user's request to retrieve a forgotten password.
cfpPassword :: Lens' ConfirmForgotPassword Text
cfpPassword = lens _cfpPassword (\ s a -> s{_cfpPassword = a}) . _Sensitive

instance AWSRequest ConfirmForgotPassword where
        type Rs ConfirmForgotPassword =
             ConfirmForgotPasswordResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 ConfirmForgotPasswordResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ConfirmForgotPassword where

instance NFData ConfirmForgotPassword where

instance ToHeaders ConfirmForgotPassword where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ConfirmForgotPassword"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmForgotPassword where
        toJSON ConfirmForgotPassword'{..}
          = object
              (catMaybes
                 [("AnalyticsMetadata" .=) <$> _cfpAnalyticsMetadata,
                  ("UserContextData" .=) <$> _cfpUserContextData,
                  ("SecretHash" .=) <$> _cfpSecretHash,
                  Just ("ClientId" .= _cfpClientId),
                  Just ("Username" .= _cfpUsername),
                  Just ("ConfirmationCode" .= _cfpConfirmationCode),
                  Just ("Password" .= _cfpPassword)])

instance ToPath ConfirmForgotPassword where
        toPath = const "/"

instance ToQuery ConfirmForgotPassword where
        toQuery = const mempty

-- | The response from the server that results from a user's request to retrieve a forgotten password.
--
--
--
-- /See:/ 'confirmForgotPasswordResponse' smart constructor.
newtype ConfirmForgotPasswordResponse = ConfirmForgotPasswordResponse'
  { _cfprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmForgotPasswordResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cfprsResponseStatus' - -- | The response status code.
confirmForgotPasswordResponse
    :: Int -- ^ 'cfprsResponseStatus'
    -> ConfirmForgotPasswordResponse
confirmForgotPasswordResponse pResponseStatus_ =
  ConfirmForgotPasswordResponse' {_cfprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
cfprsResponseStatus :: Lens' ConfirmForgotPasswordResponse Int
cfprsResponseStatus = lens _cfprsResponseStatus (\ s a -> s{_cfprsResponseStatus = a})

instance NFData ConfirmForgotPasswordResponse where
