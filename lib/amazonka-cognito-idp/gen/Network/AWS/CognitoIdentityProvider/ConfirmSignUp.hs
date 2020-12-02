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
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmSignUp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms registration of a user and handles the existing alias from a previous user.
--
--
module Network.AWS.CognitoIdentityProvider.ConfirmSignUp
    (
    -- * Creating a Request
      confirmSignUp
    , ConfirmSignUp
    -- * Request Lenses
    , csuForceAliasCreation
    , csuAnalyticsMetadata
    , csuUserContextData
    , csuSecretHash
    , csuClientId
    , csuUsername
    , csuConfirmationCode

    -- * Destructuring the Response
    , confirmSignUpResponse
    , ConfirmSignUpResponse
    -- * Response Lenses
    , csursResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to confirm registration of a user.
--
--
--
-- /See:/ 'confirmSignUp' smart constructor.
data ConfirmSignUp = ConfirmSignUp'
  { _csuForceAliasCreation :: !(Maybe Bool)
  , _csuAnalyticsMetadata  :: !(Maybe AnalyticsMetadataType)
  , _csuUserContextData    :: !(Maybe UserContextDataType)
  , _csuSecretHash         :: !(Maybe (Sensitive Text))
  , _csuClientId           :: !(Sensitive Text)
  , _csuUsername           :: !(Sensitive Text)
  , _csuConfirmationCode   :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmSignUp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csuForceAliasCreation' - Boolean to be specified to force user confirmation irrespective of existing alias. By default set to @False@ . If this parameter is set to @True@ and the phone number/email used for sign up confirmation already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user being confirmed. If set to @False@ , the API will throw an __AliasExistsException__ error.
--
-- * 'csuAnalyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmSignUp@ calls.
--
-- * 'csuUserContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- * 'csuSecretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- * 'csuClientId' - The ID of the app client associated with the user pool.
--
-- * 'csuUsername' - The user name of the user whose registration you wish to confirm.
--
-- * 'csuConfirmationCode' - The confirmation code sent by a user's request to confirm registration.
confirmSignUp
    :: Text -- ^ 'csuClientId'
    -> Text -- ^ 'csuUsername'
    -> Text -- ^ 'csuConfirmationCode'
    -> ConfirmSignUp
confirmSignUp pClientId_ pUsername_ pConfirmationCode_ =
  ConfirmSignUp'
    { _csuForceAliasCreation = Nothing
    , _csuAnalyticsMetadata = Nothing
    , _csuUserContextData = Nothing
    , _csuSecretHash = Nothing
    , _csuClientId = _Sensitive # pClientId_
    , _csuUsername = _Sensitive # pUsername_
    , _csuConfirmationCode = pConfirmationCode_
    }


-- | Boolean to be specified to force user confirmation irrespective of existing alias. By default set to @False@ . If this parameter is set to @True@ and the phone number/email used for sign up confirmation already exists as an alias with a different user, the API call will migrate the alias from the previous user to the newly created user being confirmed. If set to @False@ , the API will throw an __AliasExistsException__ error.
csuForceAliasCreation :: Lens' ConfirmSignUp (Maybe Bool)
csuForceAliasCreation = lens _csuForceAliasCreation (\ s a -> s{_csuForceAliasCreation = a})

-- | The Amazon Pinpoint analytics metadata for collecting metrics for @ConfirmSignUp@ calls.
csuAnalyticsMetadata :: Lens' ConfirmSignUp (Maybe AnalyticsMetadataType)
csuAnalyticsMetadata = lens _csuAnalyticsMetadata (\ s a -> s{_csuAnalyticsMetadata = a})

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
csuUserContextData :: Lens' ConfirmSignUp (Maybe UserContextDataType)
csuUserContextData = lens _csuUserContextData (\ s a -> s{_csuUserContextData = a})

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
csuSecretHash :: Lens' ConfirmSignUp (Maybe Text)
csuSecretHash = lens _csuSecretHash (\ s a -> s{_csuSecretHash = a}) . mapping _Sensitive

-- | The ID of the app client associated with the user pool.
csuClientId :: Lens' ConfirmSignUp Text
csuClientId = lens _csuClientId (\ s a -> s{_csuClientId = a}) . _Sensitive

-- | The user name of the user whose registration you wish to confirm.
csuUsername :: Lens' ConfirmSignUp Text
csuUsername = lens _csuUsername (\ s a -> s{_csuUsername = a}) . _Sensitive

-- | The confirmation code sent by a user's request to confirm registration.
csuConfirmationCode :: Lens' ConfirmSignUp Text
csuConfirmationCode = lens _csuConfirmationCode (\ s a -> s{_csuConfirmationCode = a})

instance AWSRequest ConfirmSignUp where
        type Rs ConfirmSignUp = ConfirmSignUpResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 ConfirmSignUpResponse' <$> (pure (fromEnum s)))

instance Hashable ConfirmSignUp where

instance NFData ConfirmSignUp where

instance ToHeaders ConfirmSignUp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ConfirmSignUp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmSignUp where
        toJSON ConfirmSignUp'{..}
          = object
              (catMaybes
                 [("ForceAliasCreation" .=) <$>
                    _csuForceAliasCreation,
                  ("AnalyticsMetadata" .=) <$> _csuAnalyticsMetadata,
                  ("UserContextData" .=) <$> _csuUserContextData,
                  ("SecretHash" .=) <$> _csuSecretHash,
                  Just ("ClientId" .= _csuClientId),
                  Just ("Username" .= _csuUsername),
                  Just ("ConfirmationCode" .= _csuConfirmationCode)])

instance ToPath ConfirmSignUp where
        toPath = const "/"

instance ToQuery ConfirmSignUp where
        toQuery = const mempty

-- | Represents the response from the server for the registration confirmation.
--
--
--
-- /See:/ 'confirmSignUpResponse' smart constructor.
newtype ConfirmSignUpResponse = ConfirmSignUpResponse'
  { _csursResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmSignUpResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csursResponseStatus' - -- | The response status code.
confirmSignUpResponse
    :: Int -- ^ 'csursResponseStatus'
    -> ConfirmSignUpResponse
confirmSignUpResponse pResponseStatus_ =
  ConfirmSignUpResponse' {_csursResponseStatus = pResponseStatus_}


-- | -- | The response status code.
csursResponseStatus :: Lens' ConfirmSignUpResponse Int
csursResponseStatus = lens _csursResponseStatus (\ s a -> s{_csursResponseStatus = a})

instance NFData ConfirmSignUpResponse where
