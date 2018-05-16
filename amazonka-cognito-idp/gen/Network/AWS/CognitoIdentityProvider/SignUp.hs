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
-- Module      : Network.AWS.CognitoIdentityProvider.SignUp
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers the user in the specified user pool and creates a user name, password, and user attributes.
--
--
module Network.AWS.CognitoIdentityProvider.SignUp
    (
    -- * Creating a Request
      signUp
    , SignUp
    -- * Request Lenses
    , suAnalyticsMetadata
    , suUserContextData
    , suUserAttributes
    , suSecretHash
    , suValidationData
    , suClientId
    , suUsername
    , suPassword

    -- * Destructuring the Response
    , signUpResponse
    , SignUpResponse
    -- * Response Lenses
    , sursCodeDeliveryDetails
    , sursResponseStatus
    , sursUserConfirmed
    , sursUserSub
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to register a user.
--
--
--
-- /See:/ 'signUp' smart constructor.
data SignUp = SignUp'
  { _suAnalyticsMetadata :: !(Maybe AnalyticsMetadataType)
  , _suUserContextData   :: !(Maybe UserContextDataType)
  , _suUserAttributes    :: !(Maybe [AttributeType])
  , _suSecretHash        :: !(Maybe (Sensitive Text))
  , _suValidationData    :: !(Maybe [AttributeType])
  , _suClientId          :: !(Sensitive Text)
  , _suUsername          :: !(Sensitive Text)
  , _suPassword          :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SignUp' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'suAnalyticsMetadata' - The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
--
-- * 'suUserContextData' - Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
--
-- * 'suUserAttributes' - An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
--
-- * 'suSecretHash' - A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
--
-- * 'suValidationData' - The validation data in the request to register a user.
--
-- * 'suClientId' - The ID of the client associated with the user pool.
--
-- * 'suUsername' - The user name of the user you wish to register.
--
-- * 'suPassword' - The password of the user you wish to register.
signUp
    :: Text -- ^ 'suClientId'
    -> Text -- ^ 'suUsername'
    -> Text -- ^ 'suPassword'
    -> SignUp
signUp pClientId_ pUsername_ pPassword_ =
  SignUp'
    { _suAnalyticsMetadata = Nothing
    , _suUserContextData = Nothing
    , _suUserAttributes = Nothing
    , _suSecretHash = Nothing
    , _suValidationData = Nothing
    , _suClientId = _Sensitive # pClientId_
    , _suUsername = _Sensitive # pUsername_
    , _suPassword = _Sensitive # pPassword_
    }


-- | The Amazon Pinpoint analytics metadata for collecting metrics for @SignUp@ calls.
suAnalyticsMetadata :: Lens' SignUp (Maybe AnalyticsMetadataType)
suAnalyticsMetadata = lens _suAnalyticsMetadata (\ s a -> s{_suAnalyticsMetadata = a})

-- | Contextual data such as the user's device fingerprint, IP address, or location used for evaluating the risk of an unexpected event by Amazon Cognito advanced security.
suUserContextData :: Lens' SignUp (Maybe UserContextDataType)
suUserContextData = lens _suUserContextData (\ s a -> s{_suUserContextData = a})

-- | An array of name-value pairs representing user attributes. For custom attributes, you must prepend the @custom:@ prefix to the attribute name.
suUserAttributes :: Lens' SignUp [AttributeType]
suUserAttributes = lens _suUserAttributes (\ s a -> s{_suUserAttributes = a}) . _Default . _Coerce

-- | A keyed-hash message authentication code (HMAC) calculated using the secret key of a user pool client and username plus the client ID in the message.
suSecretHash :: Lens' SignUp (Maybe Text)
suSecretHash = lens _suSecretHash (\ s a -> s{_suSecretHash = a}) . mapping _Sensitive

-- | The validation data in the request to register a user.
suValidationData :: Lens' SignUp [AttributeType]
suValidationData = lens _suValidationData (\ s a -> s{_suValidationData = a}) . _Default . _Coerce

-- | The ID of the client associated with the user pool.
suClientId :: Lens' SignUp Text
suClientId = lens _suClientId (\ s a -> s{_suClientId = a}) . _Sensitive

-- | The user name of the user you wish to register.
suUsername :: Lens' SignUp Text
suUsername = lens _suUsername (\ s a -> s{_suUsername = a}) . _Sensitive

-- | The password of the user you wish to register.
suPassword :: Lens' SignUp Text
suPassword = lens _suPassword (\ s a -> s{_suPassword = a}) . _Sensitive

instance AWSRequest SignUp where
        type Rs SignUp = SignUpResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 SignUpResponse' <$>
                   (x .?> "CodeDeliveryDetails") <*> (pure (fromEnum s))
                     <*> (x .:> "UserConfirmed")
                     <*> (x .:> "UserSub"))

instance Hashable SignUp where

instance NFData SignUp where

instance ToHeaders SignUp where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.SignUp" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SignUp where
        toJSON SignUp'{..}
          = object
              (catMaybes
                 [("AnalyticsMetadata" .=) <$> _suAnalyticsMetadata,
                  ("UserContextData" .=) <$> _suUserContextData,
                  ("UserAttributes" .=) <$> _suUserAttributes,
                  ("SecretHash" .=) <$> _suSecretHash,
                  ("ValidationData" .=) <$> _suValidationData,
                  Just ("ClientId" .= _suClientId),
                  Just ("Username" .= _suUsername),
                  Just ("Password" .= _suPassword)])

instance ToPath SignUp where
        toPath = const "/"

instance ToQuery SignUp where
        toQuery = const mempty

-- | The response from the server for a registration request.
--
--
--
-- /See:/ 'signUpResponse' smart constructor.
data SignUpResponse = SignUpResponse'
  { _sursCodeDeliveryDetails :: !(Maybe CodeDeliveryDetailsType)
  , _sursResponseStatus      :: !Int
  , _sursUserConfirmed       :: !Bool
  , _sursUserSub             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SignUpResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sursCodeDeliveryDetails' - The code delivery details returned by the server response to the user registration request.
--
-- * 'sursResponseStatus' - -- | The response status code.
--
-- * 'sursUserConfirmed' - A response from the server indicating that a user registration has been confirmed.
--
-- * 'sursUserSub' - The UUID of the authenticated user. This is not the same as @username@ .
signUpResponse
    :: Int -- ^ 'sursResponseStatus'
    -> Bool -- ^ 'sursUserConfirmed'
    -> Text -- ^ 'sursUserSub'
    -> SignUpResponse
signUpResponse pResponseStatus_ pUserConfirmed_ pUserSub_ =
  SignUpResponse'
    { _sursCodeDeliveryDetails = Nothing
    , _sursResponseStatus = pResponseStatus_
    , _sursUserConfirmed = pUserConfirmed_
    , _sursUserSub = pUserSub_
    }


-- | The code delivery details returned by the server response to the user registration request.
sursCodeDeliveryDetails :: Lens' SignUpResponse (Maybe CodeDeliveryDetailsType)
sursCodeDeliveryDetails = lens _sursCodeDeliveryDetails (\ s a -> s{_sursCodeDeliveryDetails = a})

-- | -- | The response status code.
sursResponseStatus :: Lens' SignUpResponse Int
sursResponseStatus = lens _sursResponseStatus (\ s a -> s{_sursResponseStatus = a})

-- | A response from the server indicating that a user registration has been confirmed.
sursUserConfirmed :: Lens' SignUpResponse Bool
sursUserConfirmed = lens _sursUserConfirmed (\ s a -> s{_sursUserConfirmed = a})

-- | The UUID of the authenticated user. This is not the same as @username@ .
sursUserSub :: Lens' SignUpResponse Text
sursUserSub = lens _sursUserSub (\ s a -> s{_sursUserSub = a})

instance NFData SignUpResponse where
