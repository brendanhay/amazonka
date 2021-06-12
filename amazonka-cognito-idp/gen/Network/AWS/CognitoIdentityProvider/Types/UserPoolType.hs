{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolType
-- Copyright   : (c) 2013-2021 Brendan Hay
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
import Network.AWS.CognitoIdentityProvider.Types.UserPoolMfaType
import Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
import Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType
import Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
import Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A container for information about the user pool.
--
-- /See:/ 'newUserPoolType' smart constructor.
data UserPoolType = UserPoolType'
  { -- | The date the user pool was last modified.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The tags that are assigned to the user pool. A tag is a label that you
    -- can apply to user pools to categorize and manage them in different ways,
    -- such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The status of a user pool.
    status :: Core.Maybe StatusType,
    -- | Specifies whether email addresses or phone numbers can be specified as
    -- usernames when a user signs up.
    usernameAttributes :: Core.Maybe [UsernameAttributeType],
    -- | The subject of the email verification message.
    emailVerificationSubject :: Core.Maybe Core.Text,
    -- | Specifies the attributes that are auto-verified in a user pool.
    autoVerifiedAttributes :: Core.Maybe [VerifiedAttributeType],
    -- | The policies associated with the user pool.
    policies :: Core.Maybe UserPoolPolicyType,
    -- | A custom domain name that you provide to Amazon Cognito. This parameter
    -- applies only if you use a custom domain to host the sign-up and sign-in
    -- pages for your application. For example: @auth.example.com@.
    --
    -- For more information about adding a custom domain to your user pool, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
    customDomain :: Core.Maybe Core.Text,
    -- | Holds the domain prefix if the user pool has a domain associated with
    -- it.
    domain :: Core.Maybe Core.Text,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Core.Maybe AdminCreateUserConfigType,
    -- | The device configuration.
    deviceConfiguration :: Core.Maybe DeviceConfigurationType,
    -- | The Amazon Resource Name (ARN) for the user pool.
    arn :: Core.Maybe Core.Text,
    -- | The ID of the user pool.
    id :: Core.Maybe Core.Text,
    -- | The date the user pool was created.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The SMS configuration.
    smsConfiguration :: Core.Maybe SmsConfigurationType,
    -- | The AWS Lambda triggers associated with the user pool.
    lambdaConfig :: Core.Maybe LambdaConfigType,
    -- | A number estimating the size of the user pool.
    estimatedNumberOfUsers :: Core.Maybe Core.Int,
    -- | The contents of the SMS verification message.
    smsVerificationMessage :: Core.Maybe Core.Text,
    -- | The name of the user pool.
    name :: Core.Maybe Core.Text,
    -- | Use this setting to define which verified available method a user can
    -- use to recover their password when they call @ForgotPassword@. It allows
    -- you to define a preferred method when a user has more than one method
    -- available. With this setting, SMS does not qualify for a valid password
    -- recovery mechanism if the user also has SMS MFA enabled. In the absence
    -- of this setting, Cognito uses the legacy behavior to determine the
    -- recovery method where SMS is preferred over email.
    accountRecoverySetting :: Core.Maybe AccountRecoverySettingType,
    -- | The email configuration.
    emailConfiguration :: Core.Maybe EmailConfigurationType,
    -- | The reason why the email configuration cannot send the messages to your
    -- users.
    emailConfigurationFailure :: Core.Maybe Core.Text,
    -- | Specifies the attributes that are aliased in a user pool.
    aliasAttributes :: Core.Maybe [AliasAttributeType],
    -- | The contents of the email verification message.
    emailVerificationMessage :: Core.Maybe Core.Text,
    -- | The user pool add-ons.
    userPoolAddOns :: Core.Maybe UserPoolAddOnsType,
    -- | You can choose to enable case sensitivity on the username input for the
    -- selected sign-in option. For example, when this is set to @False@, users
    -- will be able to sign in using either \"username\" or \"Username\". This
    -- configuration is immutable once it has been set. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
    usernameConfiguration :: Core.Maybe UsernameConfigurationType,
    -- | The contents of the SMS authentication message.
    smsAuthenticationMessage :: Core.Maybe Core.Text,
    -- | A container with the schema attributes of a user pool.
    schemaAttributes :: Core.Maybe (Core.NonEmpty SchemaAttributeType),
    -- | The reason why the SMS configuration cannot send the messages to your
    -- users.
    smsConfigurationFailure :: Core.Maybe Core.Text,
    -- | Can be one of the following values:
    --
    -- -   @OFF@ - MFA tokens are not required and cannot be specified during
    --     user registration.
    --
    -- -   @ON@ - MFA tokens are required for all user registrations. You can
    --     only specify required when you are initially creating a user pool.
    --
    -- -   @OPTIONAL@ - Users have the option when registering to create an MFA
    --     token.
    mfaConfiguration :: Core.Maybe UserPoolMfaType,
    -- | The template for verification messages.
    verificationMessageTemplate :: Core.Maybe VerificationMessageTemplateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserPoolType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'userPoolType_lastModifiedDate' - The date the user pool was last modified.
--
-- 'userPoolTags', 'userPoolType_userPoolTags' - The tags that are assigned to the user pool. A tag is a label that you
-- can apply to user pools to categorize and manage them in different ways,
-- such as by purpose, owner, environment, or other criteria.
--
-- 'status', 'userPoolType_status' - The status of a user pool.
--
-- 'usernameAttributes', 'userPoolType_usernameAttributes' - Specifies whether email addresses or phone numbers can be specified as
-- usernames when a user signs up.
--
-- 'emailVerificationSubject', 'userPoolType_emailVerificationSubject' - The subject of the email verification message.
--
-- 'autoVerifiedAttributes', 'userPoolType_autoVerifiedAttributes' - Specifies the attributes that are auto-verified in a user pool.
--
-- 'policies', 'userPoolType_policies' - The policies associated with the user pool.
--
-- 'customDomain', 'userPoolType_customDomain' - A custom domain name that you provide to Amazon Cognito. This parameter
-- applies only if you use a custom domain to host the sign-up and sign-in
-- pages for your application. For example: @auth.example.com@.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
--
-- 'domain', 'userPoolType_domain' - Holds the domain prefix if the user pool has a domain associated with
-- it.
--
-- 'adminCreateUserConfig', 'userPoolType_adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- 'deviceConfiguration', 'userPoolType_deviceConfiguration' - The device configuration.
--
-- 'arn', 'userPoolType_arn' - The Amazon Resource Name (ARN) for the user pool.
--
-- 'id', 'userPoolType_id' - The ID of the user pool.
--
-- 'creationDate', 'userPoolType_creationDate' - The date the user pool was created.
--
-- 'smsConfiguration', 'userPoolType_smsConfiguration' - The SMS configuration.
--
-- 'lambdaConfig', 'userPoolType_lambdaConfig' - The AWS Lambda triggers associated with the user pool.
--
-- 'estimatedNumberOfUsers', 'userPoolType_estimatedNumberOfUsers' - A number estimating the size of the user pool.
--
-- 'smsVerificationMessage', 'userPoolType_smsVerificationMessage' - The contents of the SMS verification message.
--
-- 'name', 'userPoolType_name' - The name of the user pool.
--
-- 'accountRecoverySetting', 'userPoolType_accountRecoverySetting' - Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
--
-- 'emailConfiguration', 'userPoolType_emailConfiguration' - The email configuration.
--
-- 'emailConfigurationFailure', 'userPoolType_emailConfigurationFailure' - The reason why the email configuration cannot send the messages to your
-- users.
--
-- 'aliasAttributes', 'userPoolType_aliasAttributes' - Specifies the attributes that are aliased in a user pool.
--
-- 'emailVerificationMessage', 'userPoolType_emailVerificationMessage' - The contents of the email verification message.
--
-- 'userPoolAddOns', 'userPoolType_userPoolAddOns' - The user pool add-ons.
--
-- 'usernameConfiguration', 'userPoolType_usernameConfiguration' - You can choose to enable case sensitivity on the username input for the
-- selected sign-in option. For example, when this is set to @False@, users
-- will be able to sign in using either \"username\" or \"Username\". This
-- configuration is immutable once it has been set. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
--
-- 'smsAuthenticationMessage', 'userPoolType_smsAuthenticationMessage' - The contents of the SMS authentication message.
--
-- 'schemaAttributes', 'userPoolType_schemaAttributes' - A container with the schema attributes of a user pool.
--
-- 'smsConfigurationFailure', 'userPoolType_smsConfigurationFailure' - The reason why the SMS configuration cannot send the messages to your
-- users.
--
-- 'mfaConfiguration', 'userPoolType_mfaConfiguration' - Can be one of the following values:
--
-- -   @OFF@ - MFA tokens are not required and cannot be specified during
--     user registration.
--
-- -   @ON@ - MFA tokens are required for all user registrations. You can
--     only specify required when you are initially creating a user pool.
--
-- -   @OPTIONAL@ - Users have the option when registering to create an MFA
--     token.
--
-- 'verificationMessageTemplate', 'userPoolType_verificationMessageTemplate' - The template for verification messages.
newUserPoolType ::
  UserPoolType
newUserPoolType =
  UserPoolType'
    { lastModifiedDate = Core.Nothing,
      userPoolTags = Core.Nothing,
      status = Core.Nothing,
      usernameAttributes = Core.Nothing,
      emailVerificationSubject = Core.Nothing,
      autoVerifiedAttributes = Core.Nothing,
      policies = Core.Nothing,
      customDomain = Core.Nothing,
      domain = Core.Nothing,
      adminCreateUserConfig = Core.Nothing,
      deviceConfiguration = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      creationDate = Core.Nothing,
      smsConfiguration = Core.Nothing,
      lambdaConfig = Core.Nothing,
      estimatedNumberOfUsers = Core.Nothing,
      smsVerificationMessage = Core.Nothing,
      name = Core.Nothing,
      accountRecoverySetting = Core.Nothing,
      emailConfiguration = Core.Nothing,
      emailConfigurationFailure = Core.Nothing,
      aliasAttributes = Core.Nothing,
      emailVerificationMessage = Core.Nothing,
      userPoolAddOns = Core.Nothing,
      usernameConfiguration = Core.Nothing,
      smsAuthenticationMessage = Core.Nothing,
      schemaAttributes = Core.Nothing,
      smsConfigurationFailure = Core.Nothing,
      mfaConfiguration = Core.Nothing,
      verificationMessageTemplate = Core.Nothing
    }

-- | The date the user pool was last modified.
userPoolType_lastModifiedDate :: Lens.Lens' UserPoolType (Core.Maybe Core.UTCTime)
userPoolType_lastModifiedDate = Lens.lens (\UserPoolType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolType' {} a -> s {lastModifiedDate = a} :: UserPoolType) Core.. Lens.mapping Core._Time

-- | The tags that are assigned to the user pool. A tag is a label that you
-- can apply to user pools to categorize and manage them in different ways,
-- such as by purpose, owner, environment, or other criteria.
userPoolType_userPoolTags :: Lens.Lens' UserPoolType (Core.Maybe (Core.HashMap Core.Text Core.Text))
userPoolType_userPoolTags = Lens.lens (\UserPoolType' {userPoolTags} -> userPoolTags) (\s@UserPoolType' {} a -> s {userPoolTags = a} :: UserPoolType) Core.. Lens.mapping Lens._Coerce

-- | The status of a user pool.
userPoolType_status :: Lens.Lens' UserPoolType (Core.Maybe StatusType)
userPoolType_status = Lens.lens (\UserPoolType' {status} -> status) (\s@UserPoolType' {} a -> s {status = a} :: UserPoolType)

-- | Specifies whether email addresses or phone numbers can be specified as
-- usernames when a user signs up.
userPoolType_usernameAttributes :: Lens.Lens' UserPoolType (Core.Maybe [UsernameAttributeType])
userPoolType_usernameAttributes = Lens.lens (\UserPoolType' {usernameAttributes} -> usernameAttributes) (\s@UserPoolType' {} a -> s {usernameAttributes = a} :: UserPoolType) Core.. Lens.mapping Lens._Coerce

-- | The subject of the email verification message.
userPoolType_emailVerificationSubject :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_emailVerificationSubject = Lens.lens (\UserPoolType' {emailVerificationSubject} -> emailVerificationSubject) (\s@UserPoolType' {} a -> s {emailVerificationSubject = a} :: UserPoolType)

-- | Specifies the attributes that are auto-verified in a user pool.
userPoolType_autoVerifiedAttributes :: Lens.Lens' UserPoolType (Core.Maybe [VerifiedAttributeType])
userPoolType_autoVerifiedAttributes = Lens.lens (\UserPoolType' {autoVerifiedAttributes} -> autoVerifiedAttributes) (\s@UserPoolType' {} a -> s {autoVerifiedAttributes = a} :: UserPoolType) Core.. Lens.mapping Lens._Coerce

-- | The policies associated with the user pool.
userPoolType_policies :: Lens.Lens' UserPoolType (Core.Maybe UserPoolPolicyType)
userPoolType_policies = Lens.lens (\UserPoolType' {policies} -> policies) (\s@UserPoolType' {} a -> s {policies = a} :: UserPoolType)

-- | A custom domain name that you provide to Amazon Cognito. This parameter
-- applies only if you use a custom domain to host the sign-up and sign-in
-- pages for your application. For example: @auth.example.com@.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
userPoolType_customDomain :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_customDomain = Lens.lens (\UserPoolType' {customDomain} -> customDomain) (\s@UserPoolType' {} a -> s {customDomain = a} :: UserPoolType)

-- | Holds the domain prefix if the user pool has a domain associated with
-- it.
userPoolType_domain :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_domain = Lens.lens (\UserPoolType' {domain} -> domain) (\s@UserPoolType' {} a -> s {domain = a} :: UserPoolType)

-- | The configuration for @AdminCreateUser@ requests.
userPoolType_adminCreateUserConfig :: Lens.Lens' UserPoolType (Core.Maybe AdminCreateUserConfigType)
userPoolType_adminCreateUserConfig = Lens.lens (\UserPoolType' {adminCreateUserConfig} -> adminCreateUserConfig) (\s@UserPoolType' {} a -> s {adminCreateUserConfig = a} :: UserPoolType)

-- | The device configuration.
userPoolType_deviceConfiguration :: Lens.Lens' UserPoolType (Core.Maybe DeviceConfigurationType)
userPoolType_deviceConfiguration = Lens.lens (\UserPoolType' {deviceConfiguration} -> deviceConfiguration) (\s@UserPoolType' {} a -> s {deviceConfiguration = a} :: UserPoolType)

-- | The Amazon Resource Name (ARN) for the user pool.
userPoolType_arn :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_arn = Lens.lens (\UserPoolType' {arn} -> arn) (\s@UserPoolType' {} a -> s {arn = a} :: UserPoolType)

-- | The ID of the user pool.
userPoolType_id :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_id = Lens.lens (\UserPoolType' {id} -> id) (\s@UserPoolType' {} a -> s {id = a} :: UserPoolType)

-- | The date the user pool was created.
userPoolType_creationDate :: Lens.Lens' UserPoolType (Core.Maybe Core.UTCTime)
userPoolType_creationDate = Lens.lens (\UserPoolType' {creationDate} -> creationDate) (\s@UserPoolType' {} a -> s {creationDate = a} :: UserPoolType) Core.. Lens.mapping Core._Time

-- | The SMS configuration.
userPoolType_smsConfiguration :: Lens.Lens' UserPoolType (Core.Maybe SmsConfigurationType)
userPoolType_smsConfiguration = Lens.lens (\UserPoolType' {smsConfiguration} -> smsConfiguration) (\s@UserPoolType' {} a -> s {smsConfiguration = a} :: UserPoolType)

-- | The AWS Lambda triggers associated with the user pool.
userPoolType_lambdaConfig :: Lens.Lens' UserPoolType (Core.Maybe LambdaConfigType)
userPoolType_lambdaConfig = Lens.lens (\UserPoolType' {lambdaConfig} -> lambdaConfig) (\s@UserPoolType' {} a -> s {lambdaConfig = a} :: UserPoolType)

-- | A number estimating the size of the user pool.
userPoolType_estimatedNumberOfUsers :: Lens.Lens' UserPoolType (Core.Maybe Core.Int)
userPoolType_estimatedNumberOfUsers = Lens.lens (\UserPoolType' {estimatedNumberOfUsers} -> estimatedNumberOfUsers) (\s@UserPoolType' {} a -> s {estimatedNumberOfUsers = a} :: UserPoolType)

-- | The contents of the SMS verification message.
userPoolType_smsVerificationMessage :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_smsVerificationMessage = Lens.lens (\UserPoolType' {smsVerificationMessage} -> smsVerificationMessage) (\s@UserPoolType' {} a -> s {smsVerificationMessage = a} :: UserPoolType)

-- | The name of the user pool.
userPoolType_name :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_name = Lens.lens (\UserPoolType' {name} -> name) (\s@UserPoolType' {} a -> s {name = a} :: UserPoolType)

-- | Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
userPoolType_accountRecoverySetting :: Lens.Lens' UserPoolType (Core.Maybe AccountRecoverySettingType)
userPoolType_accountRecoverySetting = Lens.lens (\UserPoolType' {accountRecoverySetting} -> accountRecoverySetting) (\s@UserPoolType' {} a -> s {accountRecoverySetting = a} :: UserPoolType)

-- | The email configuration.
userPoolType_emailConfiguration :: Lens.Lens' UserPoolType (Core.Maybe EmailConfigurationType)
userPoolType_emailConfiguration = Lens.lens (\UserPoolType' {emailConfiguration} -> emailConfiguration) (\s@UserPoolType' {} a -> s {emailConfiguration = a} :: UserPoolType)

-- | The reason why the email configuration cannot send the messages to your
-- users.
userPoolType_emailConfigurationFailure :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_emailConfigurationFailure = Lens.lens (\UserPoolType' {emailConfigurationFailure} -> emailConfigurationFailure) (\s@UserPoolType' {} a -> s {emailConfigurationFailure = a} :: UserPoolType)

-- | Specifies the attributes that are aliased in a user pool.
userPoolType_aliasAttributes :: Lens.Lens' UserPoolType (Core.Maybe [AliasAttributeType])
userPoolType_aliasAttributes = Lens.lens (\UserPoolType' {aliasAttributes} -> aliasAttributes) (\s@UserPoolType' {} a -> s {aliasAttributes = a} :: UserPoolType) Core.. Lens.mapping Lens._Coerce

-- | The contents of the email verification message.
userPoolType_emailVerificationMessage :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_emailVerificationMessage = Lens.lens (\UserPoolType' {emailVerificationMessage} -> emailVerificationMessage) (\s@UserPoolType' {} a -> s {emailVerificationMessage = a} :: UserPoolType)

-- | The user pool add-ons.
userPoolType_userPoolAddOns :: Lens.Lens' UserPoolType (Core.Maybe UserPoolAddOnsType)
userPoolType_userPoolAddOns = Lens.lens (\UserPoolType' {userPoolAddOns} -> userPoolAddOns) (\s@UserPoolType' {} a -> s {userPoolAddOns = a} :: UserPoolType)

-- | You can choose to enable case sensitivity on the username input for the
-- selected sign-in option. For example, when this is set to @False@, users
-- will be able to sign in using either \"username\" or \"Username\". This
-- configuration is immutable once it has been set. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
userPoolType_usernameConfiguration :: Lens.Lens' UserPoolType (Core.Maybe UsernameConfigurationType)
userPoolType_usernameConfiguration = Lens.lens (\UserPoolType' {usernameConfiguration} -> usernameConfiguration) (\s@UserPoolType' {} a -> s {usernameConfiguration = a} :: UserPoolType)

-- | The contents of the SMS authentication message.
userPoolType_smsAuthenticationMessage :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_smsAuthenticationMessage = Lens.lens (\UserPoolType' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@UserPoolType' {} a -> s {smsAuthenticationMessage = a} :: UserPoolType)

-- | A container with the schema attributes of a user pool.
userPoolType_schemaAttributes :: Lens.Lens' UserPoolType (Core.Maybe (Core.NonEmpty SchemaAttributeType))
userPoolType_schemaAttributes = Lens.lens (\UserPoolType' {schemaAttributes} -> schemaAttributes) (\s@UserPoolType' {} a -> s {schemaAttributes = a} :: UserPoolType) Core.. Lens.mapping Lens._Coerce

-- | The reason why the SMS configuration cannot send the messages to your
-- users.
userPoolType_smsConfigurationFailure :: Lens.Lens' UserPoolType (Core.Maybe Core.Text)
userPoolType_smsConfigurationFailure = Lens.lens (\UserPoolType' {smsConfigurationFailure} -> smsConfigurationFailure) (\s@UserPoolType' {} a -> s {smsConfigurationFailure = a} :: UserPoolType)

-- | Can be one of the following values:
--
-- -   @OFF@ - MFA tokens are not required and cannot be specified during
--     user registration.
--
-- -   @ON@ - MFA tokens are required for all user registrations. You can
--     only specify required when you are initially creating a user pool.
--
-- -   @OPTIONAL@ - Users have the option when registering to create an MFA
--     token.
userPoolType_mfaConfiguration :: Lens.Lens' UserPoolType (Core.Maybe UserPoolMfaType)
userPoolType_mfaConfiguration = Lens.lens (\UserPoolType' {mfaConfiguration} -> mfaConfiguration) (\s@UserPoolType' {} a -> s {mfaConfiguration = a} :: UserPoolType)

-- | The template for verification messages.
userPoolType_verificationMessageTemplate :: Lens.Lens' UserPoolType (Core.Maybe VerificationMessageTemplateType)
userPoolType_verificationMessageTemplate = Lens.lens (\UserPoolType' {verificationMessageTemplate} -> verificationMessageTemplate) (\s@UserPoolType' {} a -> s {verificationMessageTemplate = a} :: UserPoolType)

instance Core.FromJSON UserPoolType where
  parseJSON =
    Core.withObject
      "UserPoolType"
      ( \x ->
          UserPoolType'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "UserPoolTags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Status")
            Core.<*> ( x Core..:? "UsernameAttributes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "EmailVerificationSubject")
            Core.<*> ( x Core..:? "AutoVerifiedAttributes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "Policies")
            Core.<*> (x Core..:? "CustomDomain")
            Core.<*> (x Core..:? "Domain")
            Core.<*> (x Core..:? "AdminCreateUserConfig")
            Core.<*> (x Core..:? "DeviceConfiguration")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "CreationDate")
            Core.<*> (x Core..:? "SmsConfiguration")
            Core.<*> (x Core..:? "LambdaConfig")
            Core.<*> (x Core..:? "EstimatedNumberOfUsers")
            Core.<*> (x Core..:? "SmsVerificationMessage")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "AccountRecoverySetting")
            Core.<*> (x Core..:? "EmailConfiguration")
            Core.<*> (x Core..:? "EmailConfigurationFailure")
            Core.<*> (x Core..:? "AliasAttributes" Core..!= Core.mempty)
            Core.<*> (x Core..:? "EmailVerificationMessage")
            Core.<*> (x Core..:? "UserPoolAddOns")
            Core.<*> (x Core..:? "UsernameConfiguration")
            Core.<*> (x Core..:? "SmsAuthenticationMessage")
            Core.<*> (x Core..:? "SchemaAttributes")
            Core.<*> (x Core..:? "SmsConfigurationFailure")
            Core.<*> (x Core..:? "MfaConfiguration")
            Core.<*> (x Core..:? "VerificationMessageTemplate")
      )

instance Core.Hashable UserPoolType

instance Core.NFData UserPoolType
