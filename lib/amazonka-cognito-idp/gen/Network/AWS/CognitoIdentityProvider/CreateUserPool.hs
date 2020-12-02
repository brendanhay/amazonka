{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.CreateUserPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Cognito user pool and sets the password policy for the pool.
module Network.AWS.CognitoIdentityProvider.CreateUserPool
  ( -- * Creating a Request
    createUserPool,
    CreateUserPool,

    -- * Request Lenses
    cupUserPoolTags,
    cupVerificationMessageTemplate,
    cupEmailVerificationMessage,
    cupSmsAuthenticationMessage,
    cupUserPoolAddOns,
    cupEmailVerificationSubject,
    cupUsernameAttributes,
    cupAliasAttributes,
    cupSchema,
    cupAccountRecoverySetting,
    cupEmailConfiguration,
    cupSmsVerificationMessage,
    cupMFAConfiguration,
    cupLambdaConfig,
    cupSmsConfiguration,
    cupAdminCreateUserConfig,
    cupDeviceConfiguration,
    cupAutoVerifiedAttributes,
    cupPolicies,
    cupUsernameConfiguration,
    cupPoolName,

    -- * Destructuring the Response
    createUserPoolResponse,
    CreateUserPoolResponse,

    -- * Response Lenses
    cuprsUserPool,
    cuprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to create a user pool.
--
--
--
-- /See:/ 'createUserPool' smart constructor.
data CreateUserPool = CreateUserPool'
  { _cupUserPoolTags ::
      !(Maybe (Map Text (Text))),
    _cupVerificationMessageTemplate ::
      !(Maybe VerificationMessageTemplateType),
    _cupEmailVerificationMessage :: !(Maybe Text),
    _cupSmsAuthenticationMessage :: !(Maybe Text),
    _cupUserPoolAddOns :: !(Maybe UserPoolAddOnsType),
    _cupEmailVerificationSubject :: !(Maybe Text),
    _cupUsernameAttributes :: !(Maybe [UsernameAttributeType]),
    _cupAliasAttributes :: !(Maybe [AliasAttributeType]),
    _cupSchema :: !(Maybe (List1 SchemaAttributeType)),
    _cupAccountRecoverySetting ::
      !(Maybe AccountRecoverySettingType),
    _cupEmailConfiguration :: !(Maybe EmailConfigurationType),
    _cupSmsVerificationMessage :: !(Maybe Text),
    _cupMFAConfiguration :: !(Maybe UserPoolMFAType),
    _cupLambdaConfig :: !(Maybe LambdaConfigType),
    _cupSmsConfiguration :: !(Maybe SmsConfigurationType),
    _cupAdminCreateUserConfig ::
      !(Maybe AdminCreateUserConfigType),
    _cupDeviceConfiguration :: !(Maybe DeviceConfigurationType),
    _cupAutoVerifiedAttributes ::
      !(Maybe [VerifiedAttributeType]),
    _cupPolicies :: !(Maybe UserPoolPolicyType),
    _cupUsernameConfiguration ::
      !(Maybe UsernameConfigurationType),
    _cupPoolName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserPool' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cupUserPoolTags' - The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
--
-- * 'cupVerificationMessageTemplate' - The template for the verification message that the user sees when the app requests permission to access the user's information.
--
-- * 'cupEmailVerificationMessage' - A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- * 'cupSmsAuthenticationMessage' - A string representing the SMS authentication message.
--
-- * 'cupUserPoolAddOns' - Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
--
-- * 'cupEmailVerificationSubject' - A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- * 'cupUsernameAttributes' - Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
--
-- * 'cupAliasAttributes' - Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
--
-- * 'cupSchema' - An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
--
-- * 'cupAccountRecoverySetting' - Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- * 'cupEmailConfiguration' - The email configuration.
--
-- * 'cupSmsVerificationMessage' - A string representing the SMS verification message.
--
-- * 'cupMFAConfiguration' - Specifies MFA configuration details.
--
-- * 'cupLambdaConfig' - The Lambda trigger configuration information for the new user pool.
--
-- * 'cupSmsConfiguration' - The SMS configuration.
--
-- * 'cupAdminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- * 'cupDeviceConfiguration' - The device configuration.
--
-- * 'cupAutoVerifiedAttributes' - The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
--
-- * 'cupPolicies' - The policies associated with the new user pool.
--
-- * 'cupUsernameConfiguration' - You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
--
-- * 'cupPoolName' - A string used to name the user pool.
createUserPool ::
  -- | 'cupPoolName'
  Text ->
  CreateUserPool
createUserPool pPoolName_ =
  CreateUserPool'
    { _cupUserPoolTags = Nothing,
      _cupVerificationMessageTemplate = Nothing,
      _cupEmailVerificationMessage = Nothing,
      _cupSmsAuthenticationMessage = Nothing,
      _cupUserPoolAddOns = Nothing,
      _cupEmailVerificationSubject = Nothing,
      _cupUsernameAttributes = Nothing,
      _cupAliasAttributes = Nothing,
      _cupSchema = Nothing,
      _cupAccountRecoverySetting = Nothing,
      _cupEmailConfiguration = Nothing,
      _cupSmsVerificationMessage = Nothing,
      _cupMFAConfiguration = Nothing,
      _cupLambdaConfig = Nothing,
      _cupSmsConfiguration = Nothing,
      _cupAdminCreateUserConfig = Nothing,
      _cupDeviceConfiguration = Nothing,
      _cupAutoVerifiedAttributes = Nothing,
      _cupPolicies = Nothing,
      _cupUsernameConfiguration = Nothing,
      _cupPoolName = pPoolName_
    }

-- | The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
cupUserPoolTags :: Lens' CreateUserPool (HashMap Text (Text))
cupUserPoolTags = lens _cupUserPoolTags (\s a -> s {_cupUserPoolTags = a}) . _Default . _Map

-- | The template for the verification message that the user sees when the app requests permission to access the user's information.
cupVerificationMessageTemplate :: Lens' CreateUserPool (Maybe VerificationMessageTemplateType)
cupVerificationMessageTemplate = lens _cupVerificationMessageTemplate (\s a -> s {_cupVerificationMessageTemplate = a})

-- | A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
cupEmailVerificationMessage :: Lens' CreateUserPool (Maybe Text)
cupEmailVerificationMessage = lens _cupEmailVerificationMessage (\s a -> s {_cupEmailVerificationMessage = a})

-- | A string representing the SMS authentication message.
cupSmsAuthenticationMessage :: Lens' CreateUserPool (Maybe Text)
cupSmsAuthenticationMessage = lens _cupSmsAuthenticationMessage (\s a -> s {_cupSmsAuthenticationMessage = a})

-- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
cupUserPoolAddOns :: Lens' CreateUserPool (Maybe UserPoolAddOnsType)
cupUserPoolAddOns = lens _cupUserPoolAddOns (\s a -> s {_cupUserPoolAddOns = a})

-- | A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
cupEmailVerificationSubject :: Lens' CreateUserPool (Maybe Text)
cupEmailVerificationSubject = lens _cupEmailVerificationSubject (\s a -> s {_cupEmailVerificationSubject = a})

-- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
cupUsernameAttributes :: Lens' CreateUserPool [UsernameAttributeType]
cupUsernameAttributes = lens _cupUsernameAttributes (\s a -> s {_cupUsernameAttributes = a}) . _Default . _Coerce

-- | Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
cupAliasAttributes :: Lens' CreateUserPool [AliasAttributeType]
cupAliasAttributes = lens _cupAliasAttributes (\s a -> s {_cupAliasAttributes = a}) . _Default . _Coerce

-- | An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
cupSchema :: Lens' CreateUserPool (Maybe (NonEmpty SchemaAttributeType))
cupSchema = lens _cupSchema (\s a -> s {_cupSchema = a}) . mapping _List1

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
cupAccountRecoverySetting :: Lens' CreateUserPool (Maybe AccountRecoverySettingType)
cupAccountRecoverySetting = lens _cupAccountRecoverySetting (\s a -> s {_cupAccountRecoverySetting = a})

-- | The email configuration.
cupEmailConfiguration :: Lens' CreateUserPool (Maybe EmailConfigurationType)
cupEmailConfiguration = lens _cupEmailConfiguration (\s a -> s {_cupEmailConfiguration = a})

-- | A string representing the SMS verification message.
cupSmsVerificationMessage :: Lens' CreateUserPool (Maybe Text)
cupSmsVerificationMessage = lens _cupSmsVerificationMessage (\s a -> s {_cupSmsVerificationMessage = a})

-- | Specifies MFA configuration details.
cupMFAConfiguration :: Lens' CreateUserPool (Maybe UserPoolMFAType)
cupMFAConfiguration = lens _cupMFAConfiguration (\s a -> s {_cupMFAConfiguration = a})

-- | The Lambda trigger configuration information for the new user pool.
cupLambdaConfig :: Lens' CreateUserPool (Maybe LambdaConfigType)
cupLambdaConfig = lens _cupLambdaConfig (\s a -> s {_cupLambdaConfig = a})

-- | The SMS configuration.
cupSmsConfiguration :: Lens' CreateUserPool (Maybe SmsConfigurationType)
cupSmsConfiguration = lens _cupSmsConfiguration (\s a -> s {_cupSmsConfiguration = a})

-- | The configuration for @AdminCreateUser@ requests.
cupAdminCreateUserConfig :: Lens' CreateUserPool (Maybe AdminCreateUserConfigType)
cupAdminCreateUserConfig = lens _cupAdminCreateUserConfig (\s a -> s {_cupAdminCreateUserConfig = a})

-- | The device configuration.
cupDeviceConfiguration :: Lens' CreateUserPool (Maybe DeviceConfigurationType)
cupDeviceConfiguration = lens _cupDeviceConfiguration (\s a -> s {_cupDeviceConfiguration = a})

-- | The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
cupAutoVerifiedAttributes :: Lens' CreateUserPool [VerifiedAttributeType]
cupAutoVerifiedAttributes = lens _cupAutoVerifiedAttributes (\s a -> s {_cupAutoVerifiedAttributes = a}) . _Default . _Coerce

-- | The policies associated with the new user pool.
cupPolicies :: Lens' CreateUserPool (Maybe UserPoolPolicyType)
cupPolicies = lens _cupPolicies (\s a -> s {_cupPolicies = a})

-- | You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
cupUsernameConfiguration :: Lens' CreateUserPool (Maybe UsernameConfigurationType)
cupUsernameConfiguration = lens _cupUsernameConfiguration (\s a -> s {_cupUsernameConfiguration = a})

-- | A string used to name the user pool.
cupPoolName :: Lens' CreateUserPool Text
cupPoolName = lens _cupPoolName (\s a -> s {_cupPoolName = a})

instance AWSRequest CreateUserPool where
  type Rs CreateUserPool = CreateUserPoolResponse
  request = postJSON cognitoIdentityProvider
  response =
    receiveJSON
      ( \s h x ->
          CreateUserPoolResponse'
            <$> (x .?> "UserPool") <*> (pure (fromEnum s))
      )

instance Hashable CreateUserPool

instance NFData CreateUserPool

instance ToHeaders CreateUserPool where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSCognitoIdentityProviderService.CreateUserPool" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateUserPool where
  toJSON CreateUserPool' {..} =
    object
      ( catMaybes
          [ ("UserPoolTags" .=) <$> _cupUserPoolTags,
            ("VerificationMessageTemplate" .=)
              <$> _cupVerificationMessageTemplate,
            ("EmailVerificationMessage" .=) <$> _cupEmailVerificationMessage,
            ("SmsAuthenticationMessage" .=) <$> _cupSmsAuthenticationMessage,
            ("UserPoolAddOns" .=) <$> _cupUserPoolAddOns,
            ("EmailVerificationSubject" .=) <$> _cupEmailVerificationSubject,
            ("UsernameAttributes" .=) <$> _cupUsernameAttributes,
            ("AliasAttributes" .=) <$> _cupAliasAttributes,
            ("Schema" .=) <$> _cupSchema,
            ("AccountRecoverySetting" .=) <$> _cupAccountRecoverySetting,
            ("EmailConfiguration" .=) <$> _cupEmailConfiguration,
            ("SmsVerificationMessage" .=) <$> _cupSmsVerificationMessage,
            ("MfaConfiguration" .=) <$> _cupMFAConfiguration,
            ("LambdaConfig" .=) <$> _cupLambdaConfig,
            ("SmsConfiguration" .=) <$> _cupSmsConfiguration,
            ("AdminCreateUserConfig" .=) <$> _cupAdminCreateUserConfig,
            ("DeviceConfiguration" .=) <$> _cupDeviceConfiguration,
            ("AutoVerifiedAttributes" .=) <$> _cupAutoVerifiedAttributes,
            ("Policies" .=) <$> _cupPolicies,
            ("UsernameConfiguration" .=) <$> _cupUsernameConfiguration,
            Just ("PoolName" .= _cupPoolName)
          ]
      )

instance ToPath CreateUserPool where
  toPath = const "/"

instance ToQuery CreateUserPool where
  toQuery = const mempty

-- | Represents the response from the server for the request to create a user pool.
--
--
--
-- /See:/ 'createUserPoolResponse' smart constructor.
data CreateUserPoolResponse = CreateUserPoolResponse'
  { _cuprsUserPool ::
      !(Maybe UserPoolType),
    _cuprsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateUserPoolResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuprsUserPool' - A container for the user pool details.
--
-- * 'cuprsResponseStatus' - -- | The response status code.
createUserPoolResponse ::
  -- | 'cuprsResponseStatus'
  Int ->
  CreateUserPoolResponse
createUserPoolResponse pResponseStatus_ =
  CreateUserPoolResponse'
    { _cuprsUserPool = Nothing,
      _cuprsResponseStatus = pResponseStatus_
    }

-- | A container for the user pool details.
cuprsUserPool :: Lens' CreateUserPoolResponse (Maybe UserPoolType)
cuprsUserPool = lens _cuprsUserPool (\s a -> s {_cuprsUserPool = a})

-- | -- | The response status code.
cuprsResponseStatus :: Lens' CreateUserPoolResponse Int
cuprsResponseStatus = lens _cuprsResponseStatus (\s a -> s {_cuprsResponseStatus = a})

instance NFData CreateUserPoolResponse
