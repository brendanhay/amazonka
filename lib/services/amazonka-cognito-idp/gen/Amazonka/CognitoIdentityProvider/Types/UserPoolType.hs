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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolType where

import Amazonka.CognitoIdentityProvider.Types.AccountRecoverySettingType
import Amazonka.CognitoIdentityProvider.Types.AdminCreateUserConfigType
import Amazonka.CognitoIdentityProvider.Types.AliasAttributeType
import Amazonka.CognitoIdentityProvider.Types.DeletionProtectionType
import Amazonka.CognitoIdentityProvider.Types.DeviceConfigurationType
import Amazonka.CognitoIdentityProvider.Types.EmailConfigurationType
import Amazonka.CognitoIdentityProvider.Types.LambdaConfigType
import Amazonka.CognitoIdentityProvider.Types.SchemaAttributeType
import Amazonka.CognitoIdentityProvider.Types.SmsConfigurationType
import Amazonka.CognitoIdentityProvider.Types.StatusType
import Amazonka.CognitoIdentityProvider.Types.UserAttributeUpdateSettingsType
import Amazonka.CognitoIdentityProvider.Types.UserPoolAddOnsType
import Amazonka.CognitoIdentityProvider.Types.UserPoolMfaType
import Amazonka.CognitoIdentityProvider.Types.UserPoolPolicyType
import Amazonka.CognitoIdentityProvider.Types.UsernameAttributeType
import Amazonka.CognitoIdentityProvider.Types.UsernameConfigurationType
import Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType
import Amazonka.CognitoIdentityProvider.Types.VerifiedAttributeType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for information about the user pool.
--
-- /See:/ 'newUserPoolType' smart constructor.
data UserPoolType = UserPoolType'
  { -- | The available verified method a user can use to recover their password
    -- when they call @ForgotPassword@. You can use this setting to define a
    -- preferred method when a user has more than one method available. With
    -- this setting, SMS doesn\'t qualify for a valid password recovery
    -- mechanism if the user also has SMS multi-factor authentication (MFA)
    -- activated. In the absence of this setting, Amazon Cognito uses the
    -- legacy behavior to determine the recovery method where SMS is preferred
    -- through email.
    accountRecoverySetting :: Prelude.Maybe AccountRecoverySettingType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Prelude.Maybe AdminCreateUserConfigType,
    -- | The attributes that are aliased in a user pool.
    aliasAttributes :: Prelude.Maybe [AliasAttributeType],
    -- | The Amazon Resource Name (ARN) for the user pool.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The attributes that are auto-verified in a user pool.
    autoVerifiedAttributes :: Prelude.Maybe [VerifiedAttributeType],
    -- | The date the user pool was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | A custom domain name that you provide to Amazon Cognito. This parameter
    -- applies only if you use a custom domain to host the sign-up and sign-in
    -- pages for your application. An example of a custom domain name might be
    -- @auth.example.com@.
    --
    -- For more information about adding a custom domain to your user pool, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
    customDomain :: Prelude.Maybe Prelude.Text,
    -- | When active, @DeletionProtection@ prevents accidental deletion of your
    -- user pool. Before you can delete a user pool that you have protected
    -- against deletion, you must deactivate this feature.
    --
    -- When you try to delete a protected user pool in a @DeleteUserPool@ API
    -- request, Amazon Cognito returns an @InvalidParameterException@ error. To
    -- delete a protected user pool, send a new @DeleteUserPool@ request after
    -- you deactivate deletion protection in an @UpdateUserPool@ API request.
    deletionProtection :: Prelude.Maybe DeletionProtectionType,
    -- | The device-remembering configuration for a user pool. A null value
    -- indicates that you have deactivated device remembering in your user
    -- pool.
    --
    -- When you provide a value for any @DeviceConfiguration@ field, you
    -- activate the Amazon Cognito device-remembering feature.
    deviceConfiguration :: Prelude.Maybe DeviceConfigurationType,
    -- | The domain prefix, if the user pool has a domain associated with it.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The email configuration of your user pool. The email configuration type
    -- sets your preferred sending method, Amazon Web Services Region, and
    -- sender for messages tfrom your user pool.
    emailConfiguration :: Prelude.Maybe EmailConfigurationType,
    -- | Deprecated. Review error codes from API requests with
    -- @EventSource:cognito-idp.amazonaws.com@ in CloudTrail for information
    -- about problems with user pool email configuration.
    emailConfigurationFailure :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used. See
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
    emailVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used. See
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
    emailVerificationSubject :: Prelude.Maybe Prelude.Text,
    -- | A number estimating the size of the user pool.
    estimatedNumberOfUsers :: Prelude.Maybe Prelude.Int,
    -- | The ID of the user pool.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Lambda triggers associated with the user pool.
    lambdaConfig :: Prelude.Maybe LambdaConfigType,
    -- | The date the user pool was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Can be one of the following values:
    --
    -- -   @OFF@ - MFA tokens aren\'t required and can\'t be specified during
    --     user registration.
    --
    -- -   @ON@ - MFA tokens are required for all user registrations. You can
    --     only specify required when you\'re initially creating a user pool.
    --
    -- -   @OPTIONAL@ - Users have the option when registering to create an MFA
    --     token.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The name of the user pool.
    name :: Prelude.Maybe Prelude.Text,
    -- | The policies associated with the user pool.
    policies :: Prelude.Maybe UserPoolPolicyType,
    -- | A container with the schema attributes of a user pool.
    schemaAttributes :: Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType),
    -- | The contents of the SMS authentication message.
    smsAuthenticationMessage :: Prelude.Maybe Prelude.Text,
    -- | The SMS configuration with the settings that your Amazon Cognito user
    -- pool must use to send an SMS message from your Amazon Web Services
    -- account through Amazon Simple Notification Service. To send SMS messages
    -- with Amazon SNS in the Amazon Web Services Region that you want, the
    -- Amazon Cognito user pool uses an Identity and Access Management (IAM)
    -- role in your Amazon Web Services account.
    smsConfiguration :: Prelude.Maybe SmsConfigurationType,
    -- | The reason why the SMS configuration can\'t send the messages to your
    -- users.
    --
    -- This message might include comma-separated values to describe why your
    -- SMS configuration can\'t send messages to user pool end users.
    --
    -- [InvalidSmsRoleAccessPolicyException]
    --     The Identity and Access Management role that Amazon Cognito uses to
    --     send SMS messages isn\'t properly configured. For more information,
    --     see
    --     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SmsConfigurationType.html SmsConfigurationType>.
    --
    -- [SNSSandbox]
    --     The Amazon Web Services account is in the SNS SMS Sandbox and
    --     messages will only reach verified end users. This parameter won’t
    --     get populated with SNSSandbox if the IAM user creating the user pool
    --     doesn’t have SNS permissions. To learn how to move your Amazon Web
    --     Services account out of the sandbox, see
    --     <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox-moving-to-production.html Moving out of the SMS sandbox>.
    smsConfigurationFailure :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used. See
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
    smsVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | The status of a user pool.
    status :: Prelude.Maybe StatusType,
    -- | The settings for updates to user attributes. These settings include the
    -- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
    -- setting that tells Amazon Cognito how to handle changes to the value of
    -- your users\' email address and phone number attributes. For more
    -- information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
    userAttributeUpdateSettings :: Prelude.Maybe UserAttributeUpdateSettingsType,
    -- | The user pool add-ons.
    userPoolAddOns :: Prelude.Maybe UserPoolAddOnsType,
    -- | The tags that are assigned to the user pool. A tag is a label that you
    -- can apply to user pools to categorize and manage them in different ways,
    -- such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether a user can use an email address or phone number as a
    -- username when they sign up.
    usernameAttributes :: Prelude.Maybe [UsernameAttributeType],
    -- | Case sensitivity of the username input for the selected sign-in option.
    -- For example, when case sensitivity is set to @False@, users can sign in
    -- using either \"username\" or \"Username\". This configuration is
    -- immutable once it has been set. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
    usernameConfiguration :: Prelude.Maybe UsernameConfigurationType,
    -- | The template for verification messages.
    verificationMessageTemplate :: Prelude.Maybe VerificationMessageTemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountRecoverySetting', 'userPoolType_accountRecoverySetting' - The available verified method a user can use to recover their password
-- when they call @ForgotPassword@. You can use this setting to define a
-- preferred method when a user has more than one method available. With
-- this setting, SMS doesn\'t qualify for a valid password recovery
-- mechanism if the user also has SMS multi-factor authentication (MFA)
-- activated. In the absence of this setting, Amazon Cognito uses the
-- legacy behavior to determine the recovery method where SMS is preferred
-- through email.
--
-- 'adminCreateUserConfig', 'userPoolType_adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- 'aliasAttributes', 'userPoolType_aliasAttributes' - The attributes that are aliased in a user pool.
--
-- 'arn', 'userPoolType_arn' - The Amazon Resource Name (ARN) for the user pool.
--
-- 'autoVerifiedAttributes', 'userPoolType_autoVerifiedAttributes' - The attributes that are auto-verified in a user pool.
--
-- 'creationDate', 'userPoolType_creationDate' - The date the user pool was created.
--
-- 'customDomain', 'userPoolType_customDomain' - A custom domain name that you provide to Amazon Cognito. This parameter
-- applies only if you use a custom domain to host the sign-up and sign-in
-- pages for your application. An example of a custom domain name might be
-- @auth.example.com@.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
--
-- 'deletionProtection', 'userPoolType_deletionProtection' - When active, @DeletionProtection@ prevents accidental deletion of your
-- user pool. Before you can delete a user pool that you have protected
-- against deletion, you must deactivate this feature.
--
-- When you try to delete a protected user pool in a @DeleteUserPool@ API
-- request, Amazon Cognito returns an @InvalidParameterException@ error. To
-- delete a protected user pool, send a new @DeleteUserPool@ request after
-- you deactivate deletion protection in an @UpdateUserPool@ API request.
--
-- 'deviceConfiguration', 'userPoolType_deviceConfiguration' - The device-remembering configuration for a user pool. A null value
-- indicates that you have deactivated device remembering in your user
-- pool.
--
-- When you provide a value for any @DeviceConfiguration@ field, you
-- activate the Amazon Cognito device-remembering feature.
--
-- 'domain', 'userPoolType_domain' - The domain prefix, if the user pool has a domain associated with it.
--
-- 'emailConfiguration', 'userPoolType_emailConfiguration' - The email configuration of your user pool. The email configuration type
-- sets your preferred sending method, Amazon Web Services Region, and
-- sender for messages tfrom your user pool.
--
-- 'emailConfigurationFailure', 'userPoolType_emailConfigurationFailure' - Deprecated. Review error codes from API requests with
-- @EventSource:cognito-idp.amazonaws.com@ in CloudTrail for information
-- about problems with user pool email configuration.
--
-- 'emailVerificationMessage', 'userPoolType_emailVerificationMessage' - This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
--
-- 'emailVerificationSubject', 'userPoolType_emailVerificationSubject' - This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
--
-- 'estimatedNumberOfUsers', 'userPoolType_estimatedNumberOfUsers' - A number estimating the size of the user pool.
--
-- 'id', 'userPoolType_id' - The ID of the user pool.
--
-- 'lambdaConfig', 'userPoolType_lambdaConfig' - The Lambda triggers associated with the user pool.
--
-- 'lastModifiedDate', 'userPoolType_lastModifiedDate' - The date the user pool was last modified.
--
-- 'mfaConfiguration', 'userPoolType_mfaConfiguration' - Can be one of the following values:
--
-- -   @OFF@ - MFA tokens aren\'t required and can\'t be specified during
--     user registration.
--
-- -   @ON@ - MFA tokens are required for all user registrations. You can
--     only specify required when you\'re initially creating a user pool.
--
-- -   @OPTIONAL@ - Users have the option when registering to create an MFA
--     token.
--
-- 'name', 'userPoolType_name' - The name of the user pool.
--
-- 'policies', 'userPoolType_policies' - The policies associated with the user pool.
--
-- 'schemaAttributes', 'userPoolType_schemaAttributes' - A container with the schema attributes of a user pool.
--
-- 'smsAuthenticationMessage', 'userPoolType_smsAuthenticationMessage' - The contents of the SMS authentication message.
--
-- 'smsConfiguration', 'userPoolType_smsConfiguration' - The SMS configuration with the settings that your Amazon Cognito user
-- pool must use to send an SMS message from your Amazon Web Services
-- account through Amazon Simple Notification Service. To send SMS messages
-- with Amazon SNS in the Amazon Web Services Region that you want, the
-- Amazon Cognito user pool uses an Identity and Access Management (IAM)
-- role in your Amazon Web Services account.
--
-- 'smsConfigurationFailure', 'userPoolType_smsConfigurationFailure' - The reason why the SMS configuration can\'t send the messages to your
-- users.
--
-- This message might include comma-separated values to describe why your
-- SMS configuration can\'t send messages to user pool end users.
--
-- [InvalidSmsRoleAccessPolicyException]
--     The Identity and Access Management role that Amazon Cognito uses to
--     send SMS messages isn\'t properly configured. For more information,
--     see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SmsConfigurationType.html SmsConfigurationType>.
--
-- [SNSSandbox]
--     The Amazon Web Services account is in the SNS SMS Sandbox and
--     messages will only reach verified end users. This parameter won’t
--     get populated with SNSSandbox if the IAM user creating the user pool
--     doesn’t have SNS permissions. To learn how to move your Amazon Web
--     Services account out of the sandbox, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox-moving-to-production.html Moving out of the SMS sandbox>.
--
-- 'smsVerificationMessage', 'userPoolType_smsVerificationMessage' - This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
--
-- 'status', 'userPoolType_status' - The status of a user pool.
--
-- 'userAttributeUpdateSettings', 'userPoolType_userAttributeUpdateSettings' - The settings for updates to user attributes. These settings include the
-- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
-- setting that tells Amazon Cognito how to handle changes to the value of
-- your users\' email address and phone number attributes. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
--
-- 'userPoolAddOns', 'userPoolType_userPoolAddOns' - The user pool add-ons.
--
-- 'userPoolTags', 'userPoolType_userPoolTags' - The tags that are assigned to the user pool. A tag is a label that you
-- can apply to user pools to categorize and manage them in different ways,
-- such as by purpose, owner, environment, or other criteria.
--
-- 'usernameAttributes', 'userPoolType_usernameAttributes' - Specifies whether a user can use an email address or phone number as a
-- username when they sign up.
--
-- 'usernameConfiguration', 'userPoolType_usernameConfiguration' - Case sensitivity of the username input for the selected sign-in option.
-- For example, when case sensitivity is set to @False@, users can sign in
-- using either \"username\" or \"Username\". This configuration is
-- immutable once it has been set. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
--
-- 'verificationMessageTemplate', 'userPoolType_verificationMessageTemplate' - The template for verification messages.
newUserPoolType ::
  UserPoolType
newUserPoolType =
  UserPoolType'
    { accountRecoverySetting =
        Prelude.Nothing,
      adminCreateUserConfig = Prelude.Nothing,
      aliasAttributes = Prelude.Nothing,
      arn = Prelude.Nothing,
      autoVerifiedAttributes = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      customDomain = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      domain = Prelude.Nothing,
      emailConfiguration = Prelude.Nothing,
      emailConfigurationFailure = Prelude.Nothing,
      emailVerificationMessage = Prelude.Nothing,
      emailVerificationSubject = Prelude.Nothing,
      estimatedNumberOfUsers = Prelude.Nothing,
      id = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      mfaConfiguration = Prelude.Nothing,
      name = Prelude.Nothing,
      policies = Prelude.Nothing,
      schemaAttributes = Prelude.Nothing,
      smsAuthenticationMessage = Prelude.Nothing,
      smsConfiguration = Prelude.Nothing,
      smsConfigurationFailure = Prelude.Nothing,
      smsVerificationMessage = Prelude.Nothing,
      status = Prelude.Nothing,
      userAttributeUpdateSettings = Prelude.Nothing,
      userPoolAddOns = Prelude.Nothing,
      userPoolTags = Prelude.Nothing,
      usernameAttributes = Prelude.Nothing,
      usernameConfiguration = Prelude.Nothing,
      verificationMessageTemplate = Prelude.Nothing
    }

-- | The available verified method a user can use to recover their password
-- when they call @ForgotPassword@. You can use this setting to define a
-- preferred method when a user has more than one method available. With
-- this setting, SMS doesn\'t qualify for a valid password recovery
-- mechanism if the user also has SMS multi-factor authentication (MFA)
-- activated. In the absence of this setting, Amazon Cognito uses the
-- legacy behavior to determine the recovery method where SMS is preferred
-- through email.
userPoolType_accountRecoverySetting :: Lens.Lens' UserPoolType (Prelude.Maybe AccountRecoverySettingType)
userPoolType_accountRecoverySetting = Lens.lens (\UserPoolType' {accountRecoverySetting} -> accountRecoverySetting) (\s@UserPoolType' {} a -> s {accountRecoverySetting = a} :: UserPoolType)

-- | The configuration for @AdminCreateUser@ requests.
userPoolType_adminCreateUserConfig :: Lens.Lens' UserPoolType (Prelude.Maybe AdminCreateUserConfigType)
userPoolType_adminCreateUserConfig = Lens.lens (\UserPoolType' {adminCreateUserConfig} -> adminCreateUserConfig) (\s@UserPoolType' {} a -> s {adminCreateUserConfig = a} :: UserPoolType)

-- | The attributes that are aliased in a user pool.
userPoolType_aliasAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe [AliasAttributeType])
userPoolType_aliasAttributes = Lens.lens (\UserPoolType' {aliasAttributes} -> aliasAttributes) (\s@UserPoolType' {} a -> s {aliasAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the user pool.
userPoolType_arn :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_arn = Lens.lens (\UserPoolType' {arn} -> arn) (\s@UserPoolType' {} a -> s {arn = a} :: UserPoolType)

-- | The attributes that are auto-verified in a user pool.
userPoolType_autoVerifiedAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe [VerifiedAttributeType])
userPoolType_autoVerifiedAttributes = Lens.lens (\UserPoolType' {autoVerifiedAttributes} -> autoVerifiedAttributes) (\s@UserPoolType' {} a -> s {autoVerifiedAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | The date the user pool was created.
userPoolType_creationDate :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.UTCTime)
userPoolType_creationDate = Lens.lens (\UserPoolType' {creationDate} -> creationDate) (\s@UserPoolType' {} a -> s {creationDate = a} :: UserPoolType) Prelude.. Lens.mapping Data._Time

-- | A custom domain name that you provide to Amazon Cognito. This parameter
-- applies only if you use a custom domain to host the sign-up and sign-in
-- pages for your application. An example of a custom domain name might be
-- @auth.example.com@.
--
-- For more information about adding a custom domain to your user pool, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI>.
userPoolType_customDomain :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_customDomain = Lens.lens (\UserPoolType' {customDomain} -> customDomain) (\s@UserPoolType' {} a -> s {customDomain = a} :: UserPoolType)

-- | When active, @DeletionProtection@ prevents accidental deletion of your
-- user pool. Before you can delete a user pool that you have protected
-- against deletion, you must deactivate this feature.
--
-- When you try to delete a protected user pool in a @DeleteUserPool@ API
-- request, Amazon Cognito returns an @InvalidParameterException@ error. To
-- delete a protected user pool, send a new @DeleteUserPool@ request after
-- you deactivate deletion protection in an @UpdateUserPool@ API request.
userPoolType_deletionProtection :: Lens.Lens' UserPoolType (Prelude.Maybe DeletionProtectionType)
userPoolType_deletionProtection = Lens.lens (\UserPoolType' {deletionProtection} -> deletionProtection) (\s@UserPoolType' {} a -> s {deletionProtection = a} :: UserPoolType)

-- | The device-remembering configuration for a user pool. A null value
-- indicates that you have deactivated device remembering in your user
-- pool.
--
-- When you provide a value for any @DeviceConfiguration@ field, you
-- activate the Amazon Cognito device-remembering feature.
userPoolType_deviceConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe DeviceConfigurationType)
userPoolType_deviceConfiguration = Lens.lens (\UserPoolType' {deviceConfiguration} -> deviceConfiguration) (\s@UserPoolType' {} a -> s {deviceConfiguration = a} :: UserPoolType)

-- | The domain prefix, if the user pool has a domain associated with it.
userPoolType_domain :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_domain = Lens.lens (\UserPoolType' {domain} -> domain) (\s@UserPoolType' {} a -> s {domain = a} :: UserPoolType)

-- | The email configuration of your user pool. The email configuration type
-- sets your preferred sending method, Amazon Web Services Region, and
-- sender for messages tfrom your user pool.
userPoolType_emailConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe EmailConfigurationType)
userPoolType_emailConfiguration = Lens.lens (\UserPoolType' {emailConfiguration} -> emailConfiguration) (\s@UserPoolType' {} a -> s {emailConfiguration = a} :: UserPoolType)

-- | Deprecated. Review error codes from API requests with
-- @EventSource:cognito-idp.amazonaws.com@ in CloudTrail for information
-- about problems with user pool email configuration.
userPoolType_emailConfigurationFailure :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_emailConfigurationFailure = Lens.lens (\UserPoolType' {emailConfigurationFailure} -> emailConfigurationFailure) (\s@UserPoolType' {} a -> s {emailConfigurationFailure = a} :: UserPoolType)

-- | This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
userPoolType_emailVerificationMessage :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_emailVerificationMessage = Lens.lens (\UserPoolType' {emailVerificationMessage} -> emailVerificationMessage) (\s@UserPoolType' {} a -> s {emailVerificationMessage = a} :: UserPoolType)

-- | This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
userPoolType_emailVerificationSubject :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_emailVerificationSubject = Lens.lens (\UserPoolType' {emailVerificationSubject} -> emailVerificationSubject) (\s@UserPoolType' {} a -> s {emailVerificationSubject = a} :: UserPoolType)

-- | A number estimating the size of the user pool.
userPoolType_estimatedNumberOfUsers :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Int)
userPoolType_estimatedNumberOfUsers = Lens.lens (\UserPoolType' {estimatedNumberOfUsers} -> estimatedNumberOfUsers) (\s@UserPoolType' {} a -> s {estimatedNumberOfUsers = a} :: UserPoolType)

-- | The ID of the user pool.
userPoolType_id :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_id = Lens.lens (\UserPoolType' {id} -> id) (\s@UserPoolType' {} a -> s {id = a} :: UserPoolType)

-- | The Lambda triggers associated with the user pool.
userPoolType_lambdaConfig :: Lens.Lens' UserPoolType (Prelude.Maybe LambdaConfigType)
userPoolType_lambdaConfig = Lens.lens (\UserPoolType' {lambdaConfig} -> lambdaConfig) (\s@UserPoolType' {} a -> s {lambdaConfig = a} :: UserPoolType)

-- | The date the user pool was last modified.
userPoolType_lastModifiedDate :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.UTCTime)
userPoolType_lastModifiedDate = Lens.lens (\UserPoolType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolType' {} a -> s {lastModifiedDate = a} :: UserPoolType) Prelude.. Lens.mapping Data._Time

-- | Can be one of the following values:
--
-- -   @OFF@ - MFA tokens aren\'t required and can\'t be specified during
--     user registration.
--
-- -   @ON@ - MFA tokens are required for all user registrations. You can
--     only specify required when you\'re initially creating a user pool.
--
-- -   @OPTIONAL@ - Users have the option when registering to create an MFA
--     token.
userPoolType_mfaConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe UserPoolMfaType)
userPoolType_mfaConfiguration = Lens.lens (\UserPoolType' {mfaConfiguration} -> mfaConfiguration) (\s@UserPoolType' {} a -> s {mfaConfiguration = a} :: UserPoolType)

-- | The name of the user pool.
userPoolType_name :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_name = Lens.lens (\UserPoolType' {name} -> name) (\s@UserPoolType' {} a -> s {name = a} :: UserPoolType)

-- | The policies associated with the user pool.
userPoolType_policies :: Lens.Lens' UserPoolType (Prelude.Maybe UserPoolPolicyType)
userPoolType_policies = Lens.lens (\UserPoolType' {policies} -> policies) (\s@UserPoolType' {} a -> s {policies = a} :: UserPoolType)

-- | A container with the schema attributes of a user pool.
userPoolType_schemaAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType))
userPoolType_schemaAttributes = Lens.lens (\UserPoolType' {schemaAttributes} -> schemaAttributes) (\s@UserPoolType' {} a -> s {schemaAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | The contents of the SMS authentication message.
userPoolType_smsAuthenticationMessage :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_smsAuthenticationMessage = Lens.lens (\UserPoolType' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@UserPoolType' {} a -> s {smsAuthenticationMessage = a} :: UserPoolType)

-- | The SMS configuration with the settings that your Amazon Cognito user
-- pool must use to send an SMS message from your Amazon Web Services
-- account through Amazon Simple Notification Service. To send SMS messages
-- with Amazon SNS in the Amazon Web Services Region that you want, the
-- Amazon Cognito user pool uses an Identity and Access Management (IAM)
-- role in your Amazon Web Services account.
userPoolType_smsConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe SmsConfigurationType)
userPoolType_smsConfiguration = Lens.lens (\UserPoolType' {smsConfiguration} -> smsConfiguration) (\s@UserPoolType' {} a -> s {smsConfiguration = a} :: UserPoolType)

-- | The reason why the SMS configuration can\'t send the messages to your
-- users.
--
-- This message might include comma-separated values to describe why your
-- SMS configuration can\'t send messages to user pool end users.
--
-- [InvalidSmsRoleAccessPolicyException]
--     The Identity and Access Management role that Amazon Cognito uses to
--     send SMS messages isn\'t properly configured. For more information,
--     see
--     <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_SmsConfigurationType.html SmsConfigurationType>.
--
-- [SNSSandbox]
--     The Amazon Web Services account is in the SNS SMS Sandbox and
--     messages will only reach verified end users. This parameter won’t
--     get populated with SNSSandbox if the IAM user creating the user pool
--     doesn’t have SNS permissions. To learn how to move your Amazon Web
--     Services account out of the sandbox, see
--     <https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox-moving-to-production.html Moving out of the SMS sandbox>.
userPoolType_smsConfigurationFailure :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_smsConfigurationFailure = Lens.lens (\UserPoolType' {smsConfigurationFailure} -> smsConfigurationFailure) (\s@UserPoolType' {} a -> s {smsConfigurationFailure = a} :: UserPoolType)

-- | This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
userPoolType_smsVerificationMessage :: Lens.Lens' UserPoolType (Prelude.Maybe Prelude.Text)
userPoolType_smsVerificationMessage = Lens.lens (\UserPoolType' {smsVerificationMessage} -> smsVerificationMessage) (\s@UserPoolType' {} a -> s {smsVerificationMessage = a} :: UserPoolType)

-- | The status of a user pool.
userPoolType_status :: Lens.Lens' UserPoolType (Prelude.Maybe StatusType)
userPoolType_status = Lens.lens (\UserPoolType' {status} -> status) (\s@UserPoolType' {} a -> s {status = a} :: UserPoolType)

-- | The settings for updates to user attributes. These settings include the
-- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
-- setting that tells Amazon Cognito how to handle changes to the value of
-- your users\' email address and phone number attributes. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
userPoolType_userAttributeUpdateSettings :: Lens.Lens' UserPoolType (Prelude.Maybe UserAttributeUpdateSettingsType)
userPoolType_userAttributeUpdateSettings = Lens.lens (\UserPoolType' {userAttributeUpdateSettings} -> userAttributeUpdateSettings) (\s@UserPoolType' {} a -> s {userAttributeUpdateSettings = a} :: UserPoolType)

-- | The user pool add-ons.
userPoolType_userPoolAddOns :: Lens.Lens' UserPoolType (Prelude.Maybe UserPoolAddOnsType)
userPoolType_userPoolAddOns = Lens.lens (\UserPoolType' {userPoolAddOns} -> userPoolAddOns) (\s@UserPoolType' {} a -> s {userPoolAddOns = a} :: UserPoolType)

-- | The tags that are assigned to the user pool. A tag is a label that you
-- can apply to user pools to categorize and manage them in different ways,
-- such as by purpose, owner, environment, or other criteria.
userPoolType_userPoolTags :: Lens.Lens' UserPoolType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
userPoolType_userPoolTags = Lens.lens (\UserPoolType' {userPoolTags} -> userPoolTags) (\s@UserPoolType' {} a -> s {userPoolTags = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether a user can use an email address or phone number as a
-- username when they sign up.
userPoolType_usernameAttributes :: Lens.Lens' UserPoolType (Prelude.Maybe [UsernameAttributeType])
userPoolType_usernameAttributes = Lens.lens (\UserPoolType' {usernameAttributes} -> usernameAttributes) (\s@UserPoolType' {} a -> s {usernameAttributes = a} :: UserPoolType) Prelude.. Lens.mapping Lens.coerced

-- | Case sensitivity of the username input for the selected sign-in option.
-- For example, when case sensitivity is set to @False@, users can sign in
-- using either \"username\" or \"Username\". This configuration is
-- immutable once it has been set. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
userPoolType_usernameConfiguration :: Lens.Lens' UserPoolType (Prelude.Maybe UsernameConfigurationType)
userPoolType_usernameConfiguration = Lens.lens (\UserPoolType' {usernameConfiguration} -> usernameConfiguration) (\s@UserPoolType' {} a -> s {usernameConfiguration = a} :: UserPoolType)

-- | The template for verification messages.
userPoolType_verificationMessageTemplate :: Lens.Lens' UserPoolType (Prelude.Maybe VerificationMessageTemplateType)
userPoolType_verificationMessageTemplate = Lens.lens (\UserPoolType' {verificationMessageTemplate} -> verificationMessageTemplate) (\s@UserPoolType' {} a -> s {verificationMessageTemplate = a} :: UserPoolType)

instance Data.FromJSON UserPoolType where
  parseJSON =
    Data.withObject
      "UserPoolType"
      ( \x ->
          UserPoolType'
            Prelude.<$> (x Data..:? "AccountRecoverySetting")
            Prelude.<*> (x Data..:? "AdminCreateUserConfig")
            Prelude.<*> ( x Data..:? "AliasAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> ( x Data..:? "AutoVerifiedAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "CustomDomain")
            Prelude.<*> (x Data..:? "DeletionProtection")
            Prelude.<*> (x Data..:? "DeviceConfiguration")
            Prelude.<*> (x Data..:? "Domain")
            Prelude.<*> (x Data..:? "EmailConfiguration")
            Prelude.<*> (x Data..:? "EmailConfigurationFailure")
            Prelude.<*> (x Data..:? "EmailVerificationMessage")
            Prelude.<*> (x Data..:? "EmailVerificationSubject")
            Prelude.<*> (x Data..:? "EstimatedNumberOfUsers")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LambdaConfig")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "MfaConfiguration")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Policies")
            Prelude.<*> (x Data..:? "SchemaAttributes")
            Prelude.<*> (x Data..:? "SmsAuthenticationMessage")
            Prelude.<*> (x Data..:? "SmsConfiguration")
            Prelude.<*> (x Data..:? "SmsConfigurationFailure")
            Prelude.<*> (x Data..:? "SmsVerificationMessage")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserAttributeUpdateSettings")
            Prelude.<*> (x Data..:? "UserPoolAddOns")
            Prelude.<*> (x Data..:? "UserPoolTags" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "UsernameAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "UsernameConfiguration")
            Prelude.<*> (x Data..:? "VerificationMessageTemplate")
      )

instance Prelude.Hashable UserPoolType where
  hashWithSalt _salt UserPoolType' {..} =
    _salt `Prelude.hashWithSalt` accountRecoverySetting
      `Prelude.hashWithSalt` adminCreateUserConfig
      `Prelude.hashWithSalt` aliasAttributes
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` autoVerifiedAttributes
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` customDomain
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` deviceConfiguration
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` emailConfiguration
      `Prelude.hashWithSalt` emailConfigurationFailure
      `Prelude.hashWithSalt` emailVerificationMessage
      `Prelude.hashWithSalt` emailVerificationSubject
      `Prelude.hashWithSalt` estimatedNumberOfUsers
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` mfaConfiguration
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` schemaAttributes
      `Prelude.hashWithSalt` smsAuthenticationMessage
      `Prelude.hashWithSalt` smsConfiguration
      `Prelude.hashWithSalt` smsConfigurationFailure
      `Prelude.hashWithSalt` smsVerificationMessage
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userAttributeUpdateSettings
      `Prelude.hashWithSalt` userPoolAddOns
      `Prelude.hashWithSalt` userPoolTags
      `Prelude.hashWithSalt` usernameAttributes
      `Prelude.hashWithSalt` usernameConfiguration
      `Prelude.hashWithSalt` verificationMessageTemplate

instance Prelude.NFData UserPoolType where
  rnf UserPoolType' {..} =
    Prelude.rnf accountRecoverySetting
      `Prelude.seq` Prelude.rnf adminCreateUserConfig
      `Prelude.seq` Prelude.rnf aliasAttributes
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf autoVerifiedAttributes
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf customDomain
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf deviceConfiguration
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf emailConfiguration
      `Prelude.seq` Prelude.rnf emailConfigurationFailure
      `Prelude.seq` Prelude.rnf emailVerificationMessage
      `Prelude.seq` Prelude.rnf emailVerificationSubject
      `Prelude.seq` Prelude.rnf estimatedNumberOfUsers
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf
        schemaAttributes
      `Prelude.seq` Prelude.rnf
        smsAuthenticationMessage
      `Prelude.seq` Prelude.rnf
        smsConfiguration
      `Prelude.seq` Prelude.rnf
        smsConfigurationFailure
      `Prelude.seq` Prelude.rnf
        smsVerificationMessage
      `Prelude.seq` Prelude.rnf
        status
      `Prelude.seq` Prelude.rnf
        userAttributeUpdateSettings
      `Prelude.seq` Prelude.rnf
        userPoolAddOns
      `Prelude.seq` Prelude.rnf
        userPoolTags
      `Prelude.seq` Prelude.rnf
        usernameAttributes
      `Prelude.seq` Prelude.rnf
        usernameConfiguration
      `Prelude.seq` Prelude.rnf
        verificationMessageTemplate
