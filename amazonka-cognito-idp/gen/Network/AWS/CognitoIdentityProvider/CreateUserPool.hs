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
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPool
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Cognito user pool and sets the password policy for the pool.
module Network.AWS.CognitoIdentityProvider.CreateUserPool
    (
    -- * Creating a Request
      createUserPool
    , CreateUserPool
    -- * Request Lenses
    , cupEmailVerificationMessage
    , cupSmsAuthenticationMessage
    , cupEmailVerificationSubject
    , cupAliasAttributes
    , cupEmailConfiguration
    , cupSmsVerificationMessage
    , cupMFAConfiguration
    , cupLambdaConfig
    , cupSmsConfiguration
    , cupDeviceConfiguration
    , cupAutoVerifiedAttributes
    , cupPolicies
    , cupPoolName

    -- * Destructuring the Response
    , createUserPoolResponse
    , CreateUserPoolResponse
    -- * Response Lenses
    , cuprsUserPool
    , cuprsResponseStatus
    ) where

import           Network.AWS.CognitoIdentityProvider.Types
import           Network.AWS.CognitoIdentityProvider.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the request to create a user pool.
--
-- /See:/ 'createUserPool' smart constructor.
data CreateUserPool = CreateUserPool'
    { _cupEmailVerificationMessage :: !(Maybe Text)
    , _cupSmsAuthenticationMessage :: !(Maybe Text)
    , _cupEmailVerificationSubject :: !(Maybe Text)
    , _cupAliasAttributes          :: !(Maybe [AliasAttributeType])
    , _cupEmailConfiguration       :: !(Maybe EmailConfigurationType)
    , _cupSmsVerificationMessage   :: !(Maybe Text)
    , _cupMFAConfiguration         :: !(Maybe UserPoolMFAType)
    , _cupLambdaConfig             :: !(Maybe LambdaConfigType)
    , _cupSmsConfiguration         :: !(Maybe SmsConfigurationType)
    , _cupDeviceConfiguration      :: !(Maybe DeviceConfigurationType)
    , _cupAutoVerifiedAttributes   :: !(Maybe [VerifiedAttributeType])
    , _cupPolicies                 :: !(Maybe UserPoolPolicyType)
    , _cupPoolName                 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateUserPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupEmailVerificationMessage'
--
-- * 'cupSmsAuthenticationMessage'
--
-- * 'cupEmailVerificationSubject'
--
-- * 'cupAliasAttributes'
--
-- * 'cupEmailConfiguration'
--
-- * 'cupSmsVerificationMessage'
--
-- * 'cupMFAConfiguration'
--
-- * 'cupLambdaConfig'
--
-- * 'cupSmsConfiguration'
--
-- * 'cupDeviceConfiguration'
--
-- * 'cupAutoVerifiedAttributes'
--
-- * 'cupPolicies'
--
-- * 'cupPoolName'
createUserPool
    :: Text -- ^ 'cupPoolName'
    -> CreateUserPool
createUserPool pPoolName_ =
    CreateUserPool'
    { _cupEmailVerificationMessage = Nothing
    , _cupSmsAuthenticationMessage = Nothing
    , _cupEmailVerificationSubject = Nothing
    , _cupAliasAttributes = Nothing
    , _cupEmailConfiguration = Nothing
    , _cupSmsVerificationMessage = Nothing
    , _cupMFAConfiguration = Nothing
    , _cupLambdaConfig = Nothing
    , _cupSmsConfiguration = Nothing
    , _cupDeviceConfiguration = Nothing
    , _cupAutoVerifiedAttributes = Nothing
    , _cupPolicies = Nothing
    , _cupPoolName = pPoolName_
    }

-- | A string representing the email verification message.
cupEmailVerificationMessage :: Lens' CreateUserPool (Maybe Text)
cupEmailVerificationMessage = lens _cupEmailVerificationMessage (\ s a -> s{_cupEmailVerificationMessage = a});

-- | A string representing the SMS authentication message.
cupSmsAuthenticationMessage :: Lens' CreateUserPool (Maybe Text)
cupSmsAuthenticationMessage = lens _cupSmsAuthenticationMessage (\ s a -> s{_cupSmsAuthenticationMessage = a});

-- | A string representing the email verification subject.
cupEmailVerificationSubject :: Lens' CreateUserPool (Maybe Text)
cupEmailVerificationSubject = lens _cupEmailVerificationSubject (\ s a -> s{_cupEmailVerificationSubject = a});

-- | Attributes supported as an alias for this user pool. Possible values: __phone_number__, __email__, or __preferred_username__.
cupAliasAttributes :: Lens' CreateUserPool [AliasAttributeType]
cupAliasAttributes = lens _cupAliasAttributes (\ s a -> s{_cupAliasAttributes = a}) . _Default . _Coerce;

-- | The email configuration.
cupEmailConfiguration :: Lens' CreateUserPool (Maybe EmailConfigurationType)
cupEmailConfiguration = lens _cupEmailConfiguration (\ s a -> s{_cupEmailConfiguration = a});

-- | A string representing the SMS verification message.
cupSmsVerificationMessage :: Lens' CreateUserPool (Maybe Text)
cupSmsVerificationMessage = lens _cupSmsVerificationMessage (\ s a -> s{_cupSmsVerificationMessage = a});

-- | Specifies MFA configuration details.
cupMFAConfiguration :: Lens' CreateUserPool (Maybe UserPoolMFAType)
cupMFAConfiguration = lens _cupMFAConfiguration (\ s a -> s{_cupMFAConfiguration = a});

-- | The Lambda trigger configuration information for the new user pool.
cupLambdaConfig :: Lens' CreateUserPool (Maybe LambdaConfigType)
cupLambdaConfig = lens _cupLambdaConfig (\ s a -> s{_cupLambdaConfig = a});

-- | The SMS configuration.
cupSmsConfiguration :: Lens' CreateUserPool (Maybe SmsConfigurationType)
cupSmsConfiguration = lens _cupSmsConfiguration (\ s a -> s{_cupSmsConfiguration = a});

-- | The device configuration.
cupDeviceConfiguration :: Lens' CreateUserPool (Maybe DeviceConfigurationType)
cupDeviceConfiguration = lens _cupDeviceConfiguration (\ s a -> s{_cupDeviceConfiguration = a});

-- | The attributes to be auto-verified. Possible values: __email__, __phone_number__.
cupAutoVerifiedAttributes :: Lens' CreateUserPool [VerifiedAttributeType]
cupAutoVerifiedAttributes = lens _cupAutoVerifiedAttributes (\ s a -> s{_cupAutoVerifiedAttributes = a}) . _Default . _Coerce;

-- | The policies associated with the new user pool.
cupPolicies :: Lens' CreateUserPool (Maybe UserPoolPolicyType)
cupPolicies = lens _cupPolicies (\ s a -> s{_cupPolicies = a});

-- | A string used to name the user pool.
cupPoolName :: Lens' CreateUserPool Text
cupPoolName = lens _cupPoolName (\ s a -> s{_cupPoolName = a});

instance AWSRequest CreateUserPool where
        type Rs CreateUserPool = CreateUserPoolResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 CreateUserPoolResponse' <$>
                   (x .?> "UserPool") <*> (pure (fromEnum s)))

instance Hashable CreateUserPool

instance NFData CreateUserPool

instance ToHeaders CreateUserPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.CreateUserPool"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateUserPool where
        toJSON CreateUserPool'{..}
          = object
              (catMaybes
                 [("EmailVerificationMessage" .=) <$>
                    _cupEmailVerificationMessage,
                  ("SmsAuthenticationMessage" .=) <$>
                    _cupSmsAuthenticationMessage,
                  ("EmailVerificationSubject" .=) <$>
                    _cupEmailVerificationSubject,
                  ("AliasAttributes" .=) <$> _cupAliasAttributes,
                  ("EmailConfiguration" .=) <$> _cupEmailConfiguration,
                  ("SmsVerificationMessage" .=) <$>
                    _cupSmsVerificationMessage,
                  ("MfaConfiguration" .=) <$> _cupMFAConfiguration,
                  ("LambdaConfig" .=) <$> _cupLambdaConfig,
                  ("SmsConfiguration" .=) <$> _cupSmsConfiguration,
                  ("DeviceConfiguration" .=) <$>
                    _cupDeviceConfiguration,
                  ("AutoVerifiedAttributes" .=) <$>
                    _cupAutoVerifiedAttributes,
                  ("Policies" .=) <$> _cupPolicies,
                  Just ("PoolName" .= _cupPoolName)])

instance ToPath CreateUserPool where
        toPath = const "/"

instance ToQuery CreateUserPool where
        toQuery = const mempty

-- | Represents the response from the server for the request to create a user pool.
--
-- /See:/ 'createUserPoolResponse' smart constructor.
data CreateUserPoolResponse = CreateUserPoolResponse'
    { _cuprsUserPool       :: !(Maybe UserPoolType)
    , _cuprsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateUserPoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuprsUserPool'
--
-- * 'cuprsResponseStatus'
createUserPoolResponse
    :: Int -- ^ 'cuprsResponseStatus'
    -> CreateUserPoolResponse
createUserPoolResponse pResponseStatus_ =
    CreateUserPoolResponse'
    { _cuprsUserPool = Nothing
    , _cuprsResponseStatus = pResponseStatus_
    }

-- | A container for the user pool details.
cuprsUserPool :: Lens' CreateUserPoolResponse (Maybe UserPoolType)
cuprsUserPool = lens _cuprsUserPool (\ s a -> s{_cuprsUserPool = a});

-- | The response status code.
cuprsResponseStatus :: Lens' CreateUserPoolResponse Int
cuprsResponseStatus = lens _cuprsResponseStatus (\ s a -> s{_cuprsResponseStatus = a});

instance NFData CreateUserPoolResponse
