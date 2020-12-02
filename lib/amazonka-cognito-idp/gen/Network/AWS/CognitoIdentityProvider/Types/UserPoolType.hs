{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolType where

import Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType
import Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
import Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType
import Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.StatusType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolMFAType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
import Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A container for information about the user pool.
--
--
--
-- /See:/ 'userPoolType' smart constructor.
data UserPoolType = UserPoolType'
  { _uptStatus ::
      !(Maybe StatusType),
    _uptUserPoolTags :: !(Maybe (Map Text (Text))),
    _uptEmailConfigurationFailure :: !(Maybe Text),
    _uptLastModifiedDate :: !(Maybe POSIX),
    _uptVerificationMessageTemplate ::
      !(Maybe VerificationMessageTemplateType),
    _uptEstimatedNumberOfUsers :: !(Maybe Int),
    _uptARN :: !(Maybe Text),
    _uptDomain :: !(Maybe Text),
    _uptCustomDomain :: !(Maybe Text),
    _uptEmailVerificationMessage :: !(Maybe Text),
    _uptSmsAuthenticationMessage :: !(Maybe Text),
    _uptUserPoolAddOns :: !(Maybe UserPoolAddOnsType),
    _uptSchemaAttributes :: !(Maybe (List1 SchemaAttributeType)),
    _uptEmailVerificationSubject :: !(Maybe Text),
    _uptUsernameAttributes :: !(Maybe [UsernameAttributeType]),
    _uptAliasAttributes :: !(Maybe [AliasAttributeType]),
    _uptAccountRecoverySetting :: !(Maybe AccountRecoverySettingType),
    _uptEmailConfiguration :: !(Maybe EmailConfigurationType),
    _uptSmsVerificationMessage :: !(Maybe Text),
    _uptName :: !(Maybe Text),
    _uptMFAConfiguration :: !(Maybe UserPoolMFAType),
    _uptId :: !(Maybe Text),
    _uptSmsConfigurationFailure :: !(Maybe Text),
    _uptCreationDate :: !(Maybe POSIX),
    _uptLambdaConfig :: !(Maybe LambdaConfigType),
    _uptSmsConfiguration :: !(Maybe SmsConfigurationType),
    _uptAdminCreateUserConfig :: !(Maybe AdminCreateUserConfigType),
    _uptDeviceConfiguration :: !(Maybe DeviceConfigurationType),
    _uptAutoVerifiedAttributes :: !(Maybe [VerifiedAttributeType]),
    _uptPolicies :: !(Maybe UserPoolPolicyType),
    _uptUsernameConfiguration :: !(Maybe UsernameConfigurationType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserPoolType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uptStatus' - The status of a user pool.
--
-- * 'uptUserPoolTags' - The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- * 'uptEmailConfigurationFailure' - The reason why the email configuration cannot send the messages to your users.
--
-- * 'uptLastModifiedDate' - The date the user pool was last modified.
--
-- * 'uptVerificationMessageTemplate' - The template for verification messages.
--
-- * 'uptEstimatedNumberOfUsers' - A number estimating the size of the user pool.
--
-- * 'uptARN' - The Amazon Resource Name (ARN) for the user pool.
--
-- * 'uptDomain' - Holds the domain prefix if the user pool has a domain associated with it.
--
-- * 'uptCustomDomain' - A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ . For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
--
-- * 'uptEmailVerificationMessage' - The contents of the email verification message.
--
-- * 'uptSmsAuthenticationMessage' - The contents of the SMS authentication message.
--
-- * 'uptUserPoolAddOns' - The user pool add-ons.
--
-- * 'uptSchemaAttributes' - A container with the schema attributes of a user pool.
--
-- * 'uptEmailVerificationSubject' - The subject of the email verification message.
--
-- * 'uptUsernameAttributes' - Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
--
-- * 'uptAliasAttributes' - Specifies the attributes that are aliased in a user pool.
--
-- * 'uptAccountRecoverySetting' - Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- * 'uptEmailConfiguration' - The email configuration.
--
-- * 'uptSmsVerificationMessage' - The contents of the SMS verification message.
--
-- * 'uptName' - The name of the user pool.
--
-- * 'uptMFAConfiguration' - Can be one of the following values:     * @OFF@ - MFA tokens are not required and cannot be specified during user registration.     * @ON@ - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.     * @OPTIONAL@ - Users have the option when registering to create an MFA token.
--
-- * 'uptId' - The ID of the user pool.
--
-- * 'uptSmsConfigurationFailure' - The reason why the SMS configuration cannot send the messages to your users.
--
-- * 'uptCreationDate' - The date the user pool was created.
--
-- * 'uptLambdaConfig' - The AWS Lambda triggers associated with the user pool.
--
-- * 'uptSmsConfiguration' - The SMS configuration.
--
-- * 'uptAdminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- * 'uptDeviceConfiguration' - The device configuration.
--
-- * 'uptAutoVerifiedAttributes' - Specifies the attributes that are auto-verified in a user pool.
--
-- * 'uptPolicies' - The policies associated with the user pool.
--
-- * 'uptUsernameConfiguration' - You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
userPoolType ::
  UserPoolType
userPoolType =
  UserPoolType'
    { _uptStatus = Nothing,
      _uptUserPoolTags = Nothing,
      _uptEmailConfigurationFailure = Nothing,
      _uptLastModifiedDate = Nothing,
      _uptVerificationMessageTemplate = Nothing,
      _uptEstimatedNumberOfUsers = Nothing,
      _uptARN = Nothing,
      _uptDomain = Nothing,
      _uptCustomDomain = Nothing,
      _uptEmailVerificationMessage = Nothing,
      _uptSmsAuthenticationMessage = Nothing,
      _uptUserPoolAddOns = Nothing,
      _uptSchemaAttributes = Nothing,
      _uptEmailVerificationSubject = Nothing,
      _uptUsernameAttributes = Nothing,
      _uptAliasAttributes = Nothing,
      _uptAccountRecoverySetting = Nothing,
      _uptEmailConfiguration = Nothing,
      _uptSmsVerificationMessage = Nothing,
      _uptName = Nothing,
      _uptMFAConfiguration = Nothing,
      _uptId = Nothing,
      _uptSmsConfigurationFailure = Nothing,
      _uptCreationDate = Nothing,
      _uptLambdaConfig = Nothing,
      _uptSmsConfiguration = Nothing,
      _uptAdminCreateUserConfig = Nothing,
      _uptDeviceConfiguration = Nothing,
      _uptAutoVerifiedAttributes = Nothing,
      _uptPolicies = Nothing,
      _uptUsernameConfiguration = Nothing
    }

-- | The status of a user pool.
uptStatus :: Lens' UserPoolType (Maybe StatusType)
uptStatus = lens _uptStatus (\s a -> s {_uptStatus = a})

-- | The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
uptUserPoolTags :: Lens' UserPoolType (HashMap Text (Text))
uptUserPoolTags = lens _uptUserPoolTags (\s a -> s {_uptUserPoolTags = a}) . _Default . _Map

-- | The reason why the email configuration cannot send the messages to your users.
uptEmailConfigurationFailure :: Lens' UserPoolType (Maybe Text)
uptEmailConfigurationFailure = lens _uptEmailConfigurationFailure (\s a -> s {_uptEmailConfigurationFailure = a})

-- | The date the user pool was last modified.
uptLastModifiedDate :: Lens' UserPoolType (Maybe UTCTime)
uptLastModifiedDate = lens _uptLastModifiedDate (\s a -> s {_uptLastModifiedDate = a}) . mapping _Time

-- | The template for verification messages.
uptVerificationMessageTemplate :: Lens' UserPoolType (Maybe VerificationMessageTemplateType)
uptVerificationMessageTemplate = lens _uptVerificationMessageTemplate (\s a -> s {_uptVerificationMessageTemplate = a})

-- | A number estimating the size of the user pool.
uptEstimatedNumberOfUsers :: Lens' UserPoolType (Maybe Int)
uptEstimatedNumberOfUsers = lens _uptEstimatedNumberOfUsers (\s a -> s {_uptEstimatedNumberOfUsers = a})

-- | The Amazon Resource Name (ARN) for the user pool.
uptARN :: Lens' UserPoolType (Maybe Text)
uptARN = lens _uptARN (\s a -> s {_uptARN = a})

-- | Holds the domain prefix if the user pool has a domain associated with it.
uptDomain :: Lens' UserPoolType (Maybe Text)
uptDomain = lens _uptDomain (\s a -> s {_uptDomain = a})

-- | A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ . For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
uptCustomDomain :: Lens' UserPoolType (Maybe Text)
uptCustomDomain = lens _uptCustomDomain (\s a -> s {_uptCustomDomain = a})

-- | The contents of the email verification message.
uptEmailVerificationMessage :: Lens' UserPoolType (Maybe Text)
uptEmailVerificationMessage = lens _uptEmailVerificationMessage (\s a -> s {_uptEmailVerificationMessage = a})

-- | The contents of the SMS authentication message.
uptSmsAuthenticationMessage :: Lens' UserPoolType (Maybe Text)
uptSmsAuthenticationMessage = lens _uptSmsAuthenticationMessage (\s a -> s {_uptSmsAuthenticationMessage = a})

-- | The user pool add-ons.
uptUserPoolAddOns :: Lens' UserPoolType (Maybe UserPoolAddOnsType)
uptUserPoolAddOns = lens _uptUserPoolAddOns (\s a -> s {_uptUserPoolAddOns = a})

-- | A container with the schema attributes of a user pool.
uptSchemaAttributes :: Lens' UserPoolType (Maybe (NonEmpty SchemaAttributeType))
uptSchemaAttributes = lens _uptSchemaAttributes (\s a -> s {_uptSchemaAttributes = a}) . mapping _List1

-- | The subject of the email verification message.
uptEmailVerificationSubject :: Lens' UserPoolType (Maybe Text)
uptEmailVerificationSubject = lens _uptEmailVerificationSubject (\s a -> s {_uptEmailVerificationSubject = a})

-- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
uptUsernameAttributes :: Lens' UserPoolType [UsernameAttributeType]
uptUsernameAttributes = lens _uptUsernameAttributes (\s a -> s {_uptUsernameAttributes = a}) . _Default . _Coerce

-- | Specifies the attributes that are aliased in a user pool.
uptAliasAttributes :: Lens' UserPoolType [AliasAttributeType]
uptAliasAttributes = lens _uptAliasAttributes (\s a -> s {_uptAliasAttributes = a}) . _Default . _Coerce

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
uptAccountRecoverySetting :: Lens' UserPoolType (Maybe AccountRecoverySettingType)
uptAccountRecoverySetting = lens _uptAccountRecoverySetting (\s a -> s {_uptAccountRecoverySetting = a})

-- | The email configuration.
uptEmailConfiguration :: Lens' UserPoolType (Maybe EmailConfigurationType)
uptEmailConfiguration = lens _uptEmailConfiguration (\s a -> s {_uptEmailConfiguration = a})

-- | The contents of the SMS verification message.
uptSmsVerificationMessage :: Lens' UserPoolType (Maybe Text)
uptSmsVerificationMessage = lens _uptSmsVerificationMessage (\s a -> s {_uptSmsVerificationMessage = a})

-- | The name of the user pool.
uptName :: Lens' UserPoolType (Maybe Text)
uptName = lens _uptName (\s a -> s {_uptName = a})

-- | Can be one of the following values:     * @OFF@ - MFA tokens are not required and cannot be specified during user registration.     * @ON@ - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.     * @OPTIONAL@ - Users have the option when registering to create an MFA token.
uptMFAConfiguration :: Lens' UserPoolType (Maybe UserPoolMFAType)
uptMFAConfiguration = lens _uptMFAConfiguration (\s a -> s {_uptMFAConfiguration = a})

-- | The ID of the user pool.
uptId :: Lens' UserPoolType (Maybe Text)
uptId = lens _uptId (\s a -> s {_uptId = a})

-- | The reason why the SMS configuration cannot send the messages to your users.
uptSmsConfigurationFailure :: Lens' UserPoolType (Maybe Text)
uptSmsConfigurationFailure = lens _uptSmsConfigurationFailure (\s a -> s {_uptSmsConfigurationFailure = a})

-- | The date the user pool was created.
uptCreationDate :: Lens' UserPoolType (Maybe UTCTime)
uptCreationDate = lens _uptCreationDate (\s a -> s {_uptCreationDate = a}) . mapping _Time

-- | The AWS Lambda triggers associated with the user pool.
uptLambdaConfig :: Lens' UserPoolType (Maybe LambdaConfigType)
uptLambdaConfig = lens _uptLambdaConfig (\s a -> s {_uptLambdaConfig = a})

-- | The SMS configuration.
uptSmsConfiguration :: Lens' UserPoolType (Maybe SmsConfigurationType)
uptSmsConfiguration = lens _uptSmsConfiguration (\s a -> s {_uptSmsConfiguration = a})

-- | The configuration for @AdminCreateUser@ requests.
uptAdminCreateUserConfig :: Lens' UserPoolType (Maybe AdminCreateUserConfigType)
uptAdminCreateUserConfig = lens _uptAdminCreateUserConfig (\s a -> s {_uptAdminCreateUserConfig = a})

-- | The device configuration.
uptDeviceConfiguration :: Lens' UserPoolType (Maybe DeviceConfigurationType)
uptDeviceConfiguration = lens _uptDeviceConfiguration (\s a -> s {_uptDeviceConfiguration = a})

-- | Specifies the attributes that are auto-verified in a user pool.
uptAutoVerifiedAttributes :: Lens' UserPoolType [VerifiedAttributeType]
uptAutoVerifiedAttributes = lens _uptAutoVerifiedAttributes (\s a -> s {_uptAutoVerifiedAttributes = a}) . _Default . _Coerce

-- | The policies associated with the user pool.
uptPolicies :: Lens' UserPoolType (Maybe UserPoolPolicyType)
uptPolicies = lens _uptPolicies (\s a -> s {_uptPolicies = a})

-- | You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
uptUsernameConfiguration :: Lens' UserPoolType (Maybe UsernameConfigurationType)
uptUsernameConfiguration = lens _uptUsernameConfiguration (\s a -> s {_uptUsernameConfiguration = a})

instance FromJSON UserPoolType where
  parseJSON =
    withObject
      "UserPoolType"
      ( \x ->
          UserPoolType'
            <$> (x .:? "Status")
            <*> (x .:? "UserPoolTags" .!= mempty)
            <*> (x .:? "EmailConfigurationFailure")
            <*> (x .:? "LastModifiedDate")
            <*> (x .:? "VerificationMessageTemplate")
            <*> (x .:? "EstimatedNumberOfUsers")
            <*> (x .:? "Arn")
            <*> (x .:? "Domain")
            <*> (x .:? "CustomDomain")
            <*> (x .:? "EmailVerificationMessage")
            <*> (x .:? "SmsAuthenticationMessage")
            <*> (x .:? "UserPoolAddOns")
            <*> (x .:? "SchemaAttributes")
            <*> (x .:? "EmailVerificationSubject")
            <*> (x .:? "UsernameAttributes" .!= mempty)
            <*> (x .:? "AliasAttributes" .!= mempty)
            <*> (x .:? "AccountRecoverySetting")
            <*> (x .:? "EmailConfiguration")
            <*> (x .:? "SmsVerificationMessage")
            <*> (x .:? "Name")
            <*> (x .:? "MfaConfiguration")
            <*> (x .:? "Id")
            <*> (x .:? "SmsConfigurationFailure")
            <*> (x .:? "CreationDate")
            <*> (x .:? "LambdaConfig")
            <*> (x .:? "SmsConfiguration")
            <*> (x .:? "AdminCreateUserConfig")
            <*> (x .:? "DeviceConfiguration")
            <*> (x .:? "AutoVerifiedAttributes" .!= mempty)
            <*> (x .:? "Policies")
            <*> (x .:? "UsernameConfiguration")
      )

instance Hashable UserPoolType

instance NFData UserPoolType
