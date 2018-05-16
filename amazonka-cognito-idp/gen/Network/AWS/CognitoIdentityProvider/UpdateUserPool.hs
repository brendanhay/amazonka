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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPool
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user pool with the specified attributes.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateUserPool
    (
    -- * Creating a Request
      updateUserPool
    , UpdateUserPool
    -- * Request Lenses
    , uupUserPoolTags
    , uupVerificationMessageTemplate
    , uupEmailVerificationMessage
    , uupSmsAuthenticationMessage
    , uupUserPoolAddOns
    , uupEmailVerificationSubject
    , uupEmailConfiguration
    , uupSmsVerificationMessage
    , uupMFAConfiguration
    , uupLambdaConfig
    , uupSmsConfiguration
    , uupAdminCreateUserConfig
    , uupDeviceConfiguration
    , uupAutoVerifiedAttributes
    , uupPolicies
    , uupUserPoolId

    -- * Destructuring the Response
    , updateUserPoolResponse
    , UpdateUserPoolResponse
    -- * Response Lenses
    , uuprsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to update the user pool.
--
--
--
-- /See:/ 'updateUserPool' smart constructor.
data UpdateUserPool = UpdateUserPool'
  { _uupUserPoolTags                :: !(Maybe (Map Text Text))
  , _uupVerificationMessageTemplate :: !(Maybe VerificationMessageTemplateType)
  , _uupEmailVerificationMessage    :: !(Maybe Text)
  , _uupSmsAuthenticationMessage    :: !(Maybe Text)
  , _uupUserPoolAddOns              :: !(Maybe UserPoolAddOnsType)
  , _uupEmailVerificationSubject    :: !(Maybe Text)
  , _uupEmailConfiguration          :: !(Maybe EmailConfigurationType)
  , _uupSmsVerificationMessage      :: !(Maybe Text)
  , _uupMFAConfiguration            :: !(Maybe UserPoolMFAType)
  , _uupLambdaConfig                :: !(Maybe LambdaConfigType)
  , _uupSmsConfiguration            :: !(Maybe SmsConfigurationType)
  , _uupAdminCreateUserConfig       :: !(Maybe AdminCreateUserConfigType)
  , _uupDeviceConfiguration         :: !(Maybe DeviceConfigurationType)
  , _uupAutoVerifiedAttributes      :: !(Maybe [VerifiedAttributeType])
  , _uupPolicies                    :: !(Maybe UserPoolPolicyType)
  , _uupUserPoolId                  :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uupUserPoolTags' - The cost allocation tags for the user pool. For more information, see <http://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-cost-allocation-tagging.html Adding Cost Allocation Tags to Your User Pool>
--
-- * 'uupVerificationMessageTemplate' - The template for verification messages.
--
-- * 'uupEmailVerificationMessage' - The contents of the email verification message.
--
-- * 'uupSmsAuthenticationMessage' - The contents of the SMS authentication message.
--
-- * 'uupUserPoolAddOns' - Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
--
-- * 'uupEmailVerificationSubject' - The subject of the email verification message.
--
-- * 'uupEmailConfiguration' - Email configuration.
--
-- * 'uupSmsVerificationMessage' - A container with information about the SMS verification message.
--
-- * 'uupMFAConfiguration' - Can be one of the following values:     * @OFF@ - MFA tokens are not required and cannot be specified during user registration.     * @ON@ - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.     * @OPTIONAL@ - Users have the option when registering to create an MFA token.
--
-- * 'uupLambdaConfig' - The AWS Lambda configuration information from the request to update the user pool.
--
-- * 'uupSmsConfiguration' - SMS configuration.
--
-- * 'uupAdminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- * 'uupDeviceConfiguration' - Device configuration.
--
-- * 'uupAutoVerifiedAttributes' - The attributes that are automatically verified when the Amazon Cognito service makes a request to update user pools.
--
-- * 'uupPolicies' - A container with the policies you wish to update in a user pool.
--
-- * 'uupUserPoolId' - The user pool ID for the user pool you want to update.
updateUserPool
    :: Text -- ^ 'uupUserPoolId'
    -> UpdateUserPool
updateUserPool pUserPoolId_ =
  UpdateUserPool'
    { _uupUserPoolTags = Nothing
    , _uupVerificationMessageTemplate = Nothing
    , _uupEmailVerificationMessage = Nothing
    , _uupSmsAuthenticationMessage = Nothing
    , _uupUserPoolAddOns = Nothing
    , _uupEmailVerificationSubject = Nothing
    , _uupEmailConfiguration = Nothing
    , _uupSmsVerificationMessage = Nothing
    , _uupMFAConfiguration = Nothing
    , _uupLambdaConfig = Nothing
    , _uupSmsConfiguration = Nothing
    , _uupAdminCreateUserConfig = Nothing
    , _uupDeviceConfiguration = Nothing
    , _uupAutoVerifiedAttributes = Nothing
    , _uupPolicies = Nothing
    , _uupUserPoolId = pUserPoolId_
    }


-- | The cost allocation tags for the user pool. For more information, see <http://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-cost-allocation-tagging.html Adding Cost Allocation Tags to Your User Pool>
uupUserPoolTags :: Lens' UpdateUserPool (HashMap Text Text)
uupUserPoolTags = lens _uupUserPoolTags (\ s a -> s{_uupUserPoolTags = a}) . _Default . _Map

-- | The template for verification messages.
uupVerificationMessageTemplate :: Lens' UpdateUserPool (Maybe VerificationMessageTemplateType)
uupVerificationMessageTemplate = lens _uupVerificationMessageTemplate (\ s a -> s{_uupVerificationMessageTemplate = a})

-- | The contents of the email verification message.
uupEmailVerificationMessage :: Lens' UpdateUserPool (Maybe Text)
uupEmailVerificationMessage = lens _uupEmailVerificationMessage (\ s a -> s{_uupEmailVerificationMessage = a})

-- | The contents of the SMS authentication message.
uupSmsAuthenticationMessage :: Lens' UpdateUserPool (Maybe Text)
uupSmsAuthenticationMessage = lens _uupSmsAuthenticationMessage (\ s a -> s{_uupSmsAuthenticationMessage = a})

-- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
uupUserPoolAddOns :: Lens' UpdateUserPool (Maybe UserPoolAddOnsType)
uupUserPoolAddOns = lens _uupUserPoolAddOns (\ s a -> s{_uupUserPoolAddOns = a})

-- | The subject of the email verification message.
uupEmailVerificationSubject :: Lens' UpdateUserPool (Maybe Text)
uupEmailVerificationSubject = lens _uupEmailVerificationSubject (\ s a -> s{_uupEmailVerificationSubject = a})

-- | Email configuration.
uupEmailConfiguration :: Lens' UpdateUserPool (Maybe EmailConfigurationType)
uupEmailConfiguration = lens _uupEmailConfiguration (\ s a -> s{_uupEmailConfiguration = a})

-- | A container with information about the SMS verification message.
uupSmsVerificationMessage :: Lens' UpdateUserPool (Maybe Text)
uupSmsVerificationMessage = lens _uupSmsVerificationMessage (\ s a -> s{_uupSmsVerificationMessage = a})

-- | Can be one of the following values:     * @OFF@ - MFA tokens are not required and cannot be specified during user registration.     * @ON@ - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.     * @OPTIONAL@ - Users have the option when registering to create an MFA token.
uupMFAConfiguration :: Lens' UpdateUserPool (Maybe UserPoolMFAType)
uupMFAConfiguration = lens _uupMFAConfiguration (\ s a -> s{_uupMFAConfiguration = a})

-- | The AWS Lambda configuration information from the request to update the user pool.
uupLambdaConfig :: Lens' UpdateUserPool (Maybe LambdaConfigType)
uupLambdaConfig = lens _uupLambdaConfig (\ s a -> s{_uupLambdaConfig = a})

-- | SMS configuration.
uupSmsConfiguration :: Lens' UpdateUserPool (Maybe SmsConfigurationType)
uupSmsConfiguration = lens _uupSmsConfiguration (\ s a -> s{_uupSmsConfiguration = a})

-- | The configuration for @AdminCreateUser@ requests.
uupAdminCreateUserConfig :: Lens' UpdateUserPool (Maybe AdminCreateUserConfigType)
uupAdminCreateUserConfig = lens _uupAdminCreateUserConfig (\ s a -> s{_uupAdminCreateUserConfig = a})

-- | Device configuration.
uupDeviceConfiguration :: Lens' UpdateUserPool (Maybe DeviceConfigurationType)
uupDeviceConfiguration = lens _uupDeviceConfiguration (\ s a -> s{_uupDeviceConfiguration = a})

-- | The attributes that are automatically verified when the Amazon Cognito service makes a request to update user pools.
uupAutoVerifiedAttributes :: Lens' UpdateUserPool [VerifiedAttributeType]
uupAutoVerifiedAttributes = lens _uupAutoVerifiedAttributes (\ s a -> s{_uupAutoVerifiedAttributes = a}) . _Default . _Coerce

-- | A container with the policies you wish to update in a user pool.
uupPolicies :: Lens' UpdateUserPool (Maybe UserPoolPolicyType)
uupPolicies = lens _uupPolicies (\ s a -> s{_uupPolicies = a})

-- | The user pool ID for the user pool you want to update.
uupUserPoolId :: Lens' UpdateUserPool Text
uupUserPoolId = lens _uupUserPoolId (\ s a -> s{_uupUserPoolId = a})

instance AWSRequest UpdateUserPool where
        type Rs UpdateUserPool = UpdateUserPoolResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateUserPoolResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateUserPool where

instance NFData UpdateUserPool where

instance ToHeaders UpdateUserPool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateUserPool"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateUserPool where
        toJSON UpdateUserPool'{..}
          = object
              (catMaybes
                 [("UserPoolTags" .=) <$> _uupUserPoolTags,
                  ("VerificationMessageTemplate" .=) <$>
                    _uupVerificationMessageTemplate,
                  ("EmailVerificationMessage" .=) <$>
                    _uupEmailVerificationMessage,
                  ("SmsAuthenticationMessage" .=) <$>
                    _uupSmsAuthenticationMessage,
                  ("UserPoolAddOns" .=) <$> _uupUserPoolAddOns,
                  ("EmailVerificationSubject" .=) <$>
                    _uupEmailVerificationSubject,
                  ("EmailConfiguration" .=) <$> _uupEmailConfiguration,
                  ("SmsVerificationMessage" .=) <$>
                    _uupSmsVerificationMessage,
                  ("MfaConfiguration" .=) <$> _uupMFAConfiguration,
                  ("LambdaConfig" .=) <$> _uupLambdaConfig,
                  ("SmsConfiguration" .=) <$> _uupSmsConfiguration,
                  ("AdminCreateUserConfig" .=) <$>
                    _uupAdminCreateUserConfig,
                  ("DeviceConfiguration" .=) <$>
                    _uupDeviceConfiguration,
                  ("AutoVerifiedAttributes" .=) <$>
                    _uupAutoVerifiedAttributes,
                  ("Policies" .=) <$> _uupPolicies,
                  Just ("UserPoolId" .= _uupUserPoolId)])

instance ToPath UpdateUserPool where
        toPath = const "/"

instance ToQuery UpdateUserPool where
        toQuery = const mempty

-- | Represents the response from the server when you make a request to update the user pool.
--
--
--
-- /See:/ 'updateUserPoolResponse' smart constructor.
newtype UpdateUserPoolResponse = UpdateUserPoolResponse'
  { _uuprsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateUserPoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uuprsResponseStatus' - -- | The response status code.
updateUserPoolResponse
    :: Int -- ^ 'uuprsResponseStatus'
    -> UpdateUserPoolResponse
updateUserPoolResponse pResponseStatus_ =
  UpdateUserPoolResponse' {_uuprsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uuprsResponseStatus :: Lens' UpdateUserPoolResponse Int
uuprsResponseStatus = lens _uuprsResponseStatus (\ s a -> s{_uuprsResponseStatus = a})

instance NFData UpdateUserPoolResponse where
