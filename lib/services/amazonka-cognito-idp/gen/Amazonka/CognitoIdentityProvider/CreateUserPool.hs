{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CognitoIdentityProvider.CreateUserPool
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Cognito user pool and sets the password policy for
-- the pool.
--
-- This action might generate an SMS text message. Starting June 1, 2021,
-- US telecom carriers require you to register an origination phone number
-- before you can send SMS messages to US phone numbers. If you use SMS
-- text messages in Amazon Cognito, you must register a phone number with
-- <https://console.aws.amazon.com/pinpoint/home/ Amazon Pinpoint>. Amazon
-- Cognito uses the registered number automatically. Otherwise, Amazon
-- Cognito users who must receive SMS messages might not be able to sign
-- up, activate their accounts, or sign in.
--
-- If you have never used SMS text messages with Amazon Cognito or any
-- other Amazon Web Service, Amazon Simple Notification Service might place
-- your account in the SMS sandbox. In
-- /<https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html sandbox mode>/
-- , you can send messages only to verified phone numbers. After you test
-- your app while in the sandbox environment, you can move out of the
-- sandbox and into production. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-sms-userpool-settings.html SMS message settings for Amazon Cognito user pools>
-- in the /Amazon Cognito Developer Guide/.
module Amazonka.CognitoIdentityProvider.CreateUserPool
  ( -- * Creating a Request
    CreateUserPool (..),
    newCreateUserPool,

    -- * Request Lenses
    createUserPool_accountRecoverySetting,
    createUserPool_adminCreateUserConfig,
    createUserPool_aliasAttributes,
    createUserPool_autoVerifiedAttributes,
    createUserPool_deletionProtection,
    createUserPool_deviceConfiguration,
    createUserPool_emailConfiguration,
    createUserPool_emailVerificationMessage,
    createUserPool_emailVerificationSubject,
    createUserPool_lambdaConfig,
    createUserPool_mfaConfiguration,
    createUserPool_policies,
    createUserPool_schema,
    createUserPool_smsAuthenticationMessage,
    createUserPool_smsConfiguration,
    createUserPool_smsVerificationMessage,
    createUserPool_userAttributeUpdateSettings,
    createUserPool_userPoolAddOns,
    createUserPool_userPoolTags,
    createUserPool_usernameAttributes,
    createUserPool_usernameConfiguration,
    createUserPool_verificationMessageTemplate,
    createUserPool_poolName,

    -- * Destructuring the Response
    CreateUserPoolResponse (..),
    newCreateUserPoolResponse,

    -- * Response Lenses
    createUserPoolResponse_userPool,
    createUserPoolResponse_httpStatus,
  )
where

import Amazonka.CognitoIdentityProvider.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to create a user pool.
--
-- /See:/ 'newCreateUserPool' smart constructor.
data CreateUserPool = CreateUserPool'
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
    -- | Attributes supported as an alias for this user pool. Possible values:
    -- __phone_number__, __email__, or __preferred_username__.
    aliasAttributes :: Prelude.Maybe [AliasAttributeType],
    -- | The attributes to be auto-verified. Possible values: __email__,
    -- __phone_number__.
    autoVerifiedAttributes :: Prelude.Maybe [VerifiedAttributeType],
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
    -- | The email configuration of your user pool. The email configuration type
    -- sets your preferred sending method, Amazon Web Services Region, and
    -- sender for messages from your user pool.
    emailConfiguration :: Prelude.Maybe EmailConfigurationType,
    -- | This parameter is no longer used. See
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
    emailVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | This parameter is no longer used. See
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
    emailVerificationSubject :: Prelude.Maybe Prelude.Text,
    -- | The Lambda trigger configuration information for the new user pool.
    --
    -- In a push model, event sources (such as Amazon S3 and custom
    -- applications) need permission to invoke a function. So you must make an
    -- extra call to add permission for these event sources to invoke your
    -- Lambda function.
    --
    -- For more information on using the Lambda API to add permission, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html AddPermission>
    -- .
    --
    -- For adding permission using the CLI, see
    -- <https://docs.aws.amazon.com/cli/latest/reference/lambda/add-permission.html add-permission>
    -- .
    lambdaConfig :: Prelude.Maybe LambdaConfigType,
    -- | Specifies MFA configuration details.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The policies associated with the new user pool.
    policies :: Prelude.Maybe UserPoolPolicyType,
    -- | An array of schema attributes for the new user pool. These attributes
    -- can be standard or custom attributes.
    schema :: Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType),
    -- | A string representing the SMS authentication message.
    smsAuthenticationMessage :: Prelude.Maybe Prelude.Text,
    -- | The SMS configuration with the settings that your Amazon Cognito user
    -- pool must use to send an SMS message from your Amazon Web Services
    -- account through Amazon Simple Notification Service. To send SMS messages
    -- with Amazon SNS in the Amazon Web Services Region that you want, the
    -- Amazon Cognito user pool uses an Identity and Access Management (IAM)
    -- role in your Amazon Web Services account.
    smsConfiguration :: Prelude.Maybe SmsConfigurationType,
    -- | This parameter is no longer used. See
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
    smsVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | The settings for updates to user attributes. These settings include the
    -- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
    -- setting that tells Amazon Cognito how to handle changes to the value of
    -- your users\' email address and phone number attributes. For more
    -- information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
    userAttributeUpdateSettings :: Prelude.Maybe UserAttributeUpdateSettingsType,
    -- | Enables advanced security risk detection. Set the key
    -- @AdvancedSecurityMode@ to the value \"AUDIT\".
    userPoolAddOns :: Prelude.Maybe UserPoolAddOnsType,
    -- | The tag keys and values to assign to the user pool. A tag is a label
    -- that you can use to categorize and manage user pools in different ways,
    -- such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies whether a user can use an email address or phone number as a
    -- username when they sign up.
    usernameAttributes :: Prelude.Maybe [UsernameAttributeType],
    -- | Case sensitivity on the username input for the selected sign-in option.
    -- For example, when case sensitivity is set to @False@, users can sign in
    -- using either \"username\" or \"Username\". This configuration is
    -- immutable once it has been set. For more information, see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
    usernameConfiguration :: Prelude.Maybe UsernameConfigurationType,
    -- | The template for the verification message that the user sees when the
    -- app requests permission to access the user\'s information.
    verificationMessageTemplate :: Prelude.Maybe VerificationMessageTemplateType,
    -- | A string used to name the user pool.
    poolName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountRecoverySetting', 'createUserPool_accountRecoverySetting' - The available verified method a user can use to recover their password
-- when they call @ForgotPassword@. You can use this setting to define a
-- preferred method when a user has more than one method available. With
-- this setting, SMS doesn\'t qualify for a valid password recovery
-- mechanism if the user also has SMS multi-factor authentication (MFA)
-- activated. In the absence of this setting, Amazon Cognito uses the
-- legacy behavior to determine the recovery method where SMS is preferred
-- through email.
--
-- 'adminCreateUserConfig', 'createUserPool_adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- 'aliasAttributes', 'createUserPool_aliasAttributes' - Attributes supported as an alias for this user pool. Possible values:
-- __phone_number__, __email__, or __preferred_username__.
--
-- 'autoVerifiedAttributes', 'createUserPool_autoVerifiedAttributes' - The attributes to be auto-verified. Possible values: __email__,
-- __phone_number__.
--
-- 'deletionProtection', 'createUserPool_deletionProtection' - When active, @DeletionProtection@ prevents accidental deletion of your
-- user pool. Before you can delete a user pool that you have protected
-- against deletion, you must deactivate this feature.
--
-- When you try to delete a protected user pool in a @DeleteUserPool@ API
-- request, Amazon Cognito returns an @InvalidParameterException@ error. To
-- delete a protected user pool, send a new @DeleteUserPool@ request after
-- you deactivate deletion protection in an @UpdateUserPool@ API request.
--
-- 'deviceConfiguration', 'createUserPool_deviceConfiguration' - The device-remembering configuration for a user pool. A null value
-- indicates that you have deactivated device remembering in your user
-- pool.
--
-- When you provide a value for any @DeviceConfiguration@ field, you
-- activate the Amazon Cognito device-remembering feature.
--
-- 'emailConfiguration', 'createUserPool_emailConfiguration' - The email configuration of your user pool. The email configuration type
-- sets your preferred sending method, Amazon Web Services Region, and
-- sender for messages from your user pool.
--
-- 'emailVerificationMessage', 'createUserPool_emailVerificationMessage' - This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
--
-- 'emailVerificationSubject', 'createUserPool_emailVerificationSubject' - This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
--
-- 'lambdaConfig', 'createUserPool_lambdaConfig' - The Lambda trigger configuration information for the new user pool.
--
-- In a push model, event sources (such as Amazon S3 and custom
-- applications) need permission to invoke a function. So you must make an
-- extra call to add permission for these event sources to invoke your
-- Lambda function.
--
-- For more information on using the Lambda API to add permission, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html AddPermission>
-- .
--
-- For adding permission using the CLI, see
-- <https://docs.aws.amazon.com/cli/latest/reference/lambda/add-permission.html add-permission>
-- .
--
-- 'mfaConfiguration', 'createUserPool_mfaConfiguration' - Specifies MFA configuration details.
--
-- 'policies', 'createUserPool_policies' - The policies associated with the new user pool.
--
-- 'schema', 'createUserPool_schema' - An array of schema attributes for the new user pool. These attributes
-- can be standard or custom attributes.
--
-- 'smsAuthenticationMessage', 'createUserPool_smsAuthenticationMessage' - A string representing the SMS authentication message.
--
-- 'smsConfiguration', 'createUserPool_smsConfiguration' - The SMS configuration with the settings that your Amazon Cognito user
-- pool must use to send an SMS message from your Amazon Web Services
-- account through Amazon Simple Notification Service. To send SMS messages
-- with Amazon SNS in the Amazon Web Services Region that you want, the
-- Amazon Cognito user pool uses an Identity and Access Management (IAM)
-- role in your Amazon Web Services account.
--
-- 'smsVerificationMessage', 'createUserPool_smsVerificationMessage' - This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
--
-- 'userAttributeUpdateSettings', 'createUserPool_userAttributeUpdateSettings' - The settings for updates to user attributes. These settings include the
-- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
-- setting that tells Amazon Cognito how to handle changes to the value of
-- your users\' email address and phone number attributes. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
--
-- 'userPoolAddOns', 'createUserPool_userPoolAddOns' - Enables advanced security risk detection. Set the key
-- @AdvancedSecurityMode@ to the value \"AUDIT\".
--
-- 'userPoolTags', 'createUserPool_userPoolTags' - The tag keys and values to assign to the user pool. A tag is a label
-- that you can use to categorize and manage user pools in different ways,
-- such as by purpose, owner, environment, or other criteria.
--
-- 'usernameAttributes', 'createUserPool_usernameAttributes' - Specifies whether a user can use an email address or phone number as a
-- username when they sign up.
--
-- 'usernameConfiguration', 'createUserPool_usernameConfiguration' - Case sensitivity on the username input for the selected sign-in option.
-- For example, when case sensitivity is set to @False@, users can sign in
-- using either \"username\" or \"Username\". This configuration is
-- immutable once it has been set. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
--
-- 'verificationMessageTemplate', 'createUserPool_verificationMessageTemplate' - The template for the verification message that the user sees when the
-- app requests permission to access the user\'s information.
--
-- 'poolName', 'createUserPool_poolName' - A string used to name the user pool.
newCreateUserPool ::
  -- | 'poolName'
  Prelude.Text ->
  CreateUserPool
newCreateUserPool pPoolName_ =
  CreateUserPool'
    { accountRecoverySetting =
        Prelude.Nothing,
      adminCreateUserConfig = Prelude.Nothing,
      aliasAttributes = Prelude.Nothing,
      autoVerifiedAttributes = Prelude.Nothing,
      deletionProtection = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      emailConfiguration = Prelude.Nothing,
      emailVerificationMessage = Prelude.Nothing,
      emailVerificationSubject = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      mfaConfiguration = Prelude.Nothing,
      policies = Prelude.Nothing,
      schema = Prelude.Nothing,
      smsAuthenticationMessage = Prelude.Nothing,
      smsConfiguration = Prelude.Nothing,
      smsVerificationMessage = Prelude.Nothing,
      userAttributeUpdateSettings = Prelude.Nothing,
      userPoolAddOns = Prelude.Nothing,
      userPoolTags = Prelude.Nothing,
      usernameAttributes = Prelude.Nothing,
      usernameConfiguration = Prelude.Nothing,
      verificationMessageTemplate = Prelude.Nothing,
      poolName = pPoolName_
    }

-- | The available verified method a user can use to recover their password
-- when they call @ForgotPassword@. You can use this setting to define a
-- preferred method when a user has more than one method available. With
-- this setting, SMS doesn\'t qualify for a valid password recovery
-- mechanism if the user also has SMS multi-factor authentication (MFA)
-- activated. In the absence of this setting, Amazon Cognito uses the
-- legacy behavior to determine the recovery method where SMS is preferred
-- through email.
createUserPool_accountRecoverySetting :: Lens.Lens' CreateUserPool (Prelude.Maybe AccountRecoverySettingType)
createUserPool_accountRecoverySetting = Lens.lens (\CreateUserPool' {accountRecoverySetting} -> accountRecoverySetting) (\s@CreateUserPool' {} a -> s {accountRecoverySetting = a} :: CreateUserPool)

-- | The configuration for @AdminCreateUser@ requests.
createUserPool_adminCreateUserConfig :: Lens.Lens' CreateUserPool (Prelude.Maybe AdminCreateUserConfigType)
createUserPool_adminCreateUserConfig = Lens.lens (\CreateUserPool' {adminCreateUserConfig} -> adminCreateUserConfig) (\s@CreateUserPool' {} a -> s {adminCreateUserConfig = a} :: CreateUserPool)

-- | Attributes supported as an alias for this user pool. Possible values:
-- __phone_number__, __email__, or __preferred_username__.
createUserPool_aliasAttributes :: Lens.Lens' CreateUserPool (Prelude.Maybe [AliasAttributeType])
createUserPool_aliasAttributes = Lens.lens (\CreateUserPool' {aliasAttributes} -> aliasAttributes) (\s@CreateUserPool' {} a -> s {aliasAttributes = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | The attributes to be auto-verified. Possible values: __email__,
-- __phone_number__.
createUserPool_autoVerifiedAttributes :: Lens.Lens' CreateUserPool (Prelude.Maybe [VerifiedAttributeType])
createUserPool_autoVerifiedAttributes = Lens.lens (\CreateUserPool' {autoVerifiedAttributes} -> autoVerifiedAttributes) (\s@CreateUserPool' {} a -> s {autoVerifiedAttributes = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | When active, @DeletionProtection@ prevents accidental deletion of your
-- user pool. Before you can delete a user pool that you have protected
-- against deletion, you must deactivate this feature.
--
-- When you try to delete a protected user pool in a @DeleteUserPool@ API
-- request, Amazon Cognito returns an @InvalidParameterException@ error. To
-- delete a protected user pool, send a new @DeleteUserPool@ request after
-- you deactivate deletion protection in an @UpdateUserPool@ API request.
createUserPool_deletionProtection :: Lens.Lens' CreateUserPool (Prelude.Maybe DeletionProtectionType)
createUserPool_deletionProtection = Lens.lens (\CreateUserPool' {deletionProtection} -> deletionProtection) (\s@CreateUserPool' {} a -> s {deletionProtection = a} :: CreateUserPool)

-- | The device-remembering configuration for a user pool. A null value
-- indicates that you have deactivated device remembering in your user
-- pool.
--
-- When you provide a value for any @DeviceConfiguration@ field, you
-- activate the Amazon Cognito device-remembering feature.
createUserPool_deviceConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe DeviceConfigurationType)
createUserPool_deviceConfiguration = Lens.lens (\CreateUserPool' {deviceConfiguration} -> deviceConfiguration) (\s@CreateUserPool' {} a -> s {deviceConfiguration = a} :: CreateUserPool)

-- | The email configuration of your user pool. The email configuration type
-- sets your preferred sending method, Amazon Web Services Region, and
-- sender for messages from your user pool.
createUserPool_emailConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe EmailConfigurationType)
createUserPool_emailConfiguration = Lens.lens (\CreateUserPool' {emailConfiguration} -> emailConfiguration) (\s@CreateUserPool' {} a -> s {emailConfiguration = a} :: CreateUserPool)

-- | This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
createUserPool_emailVerificationMessage :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_emailVerificationMessage = Lens.lens (\CreateUserPool' {emailVerificationMessage} -> emailVerificationMessage) (\s@CreateUserPool' {} a -> s {emailVerificationMessage = a} :: CreateUserPool)

-- | This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
createUserPool_emailVerificationSubject :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_emailVerificationSubject = Lens.lens (\CreateUserPool' {emailVerificationSubject} -> emailVerificationSubject) (\s@CreateUserPool' {} a -> s {emailVerificationSubject = a} :: CreateUserPool)

-- | The Lambda trigger configuration information for the new user pool.
--
-- In a push model, event sources (such as Amazon S3 and custom
-- applications) need permission to invoke a function. So you must make an
-- extra call to add permission for these event sources to invoke your
-- Lambda function.
--
-- For more information on using the Lambda API to add permission, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html AddPermission>
-- .
--
-- For adding permission using the CLI, see
-- <https://docs.aws.amazon.com/cli/latest/reference/lambda/add-permission.html add-permission>
-- .
createUserPool_lambdaConfig :: Lens.Lens' CreateUserPool (Prelude.Maybe LambdaConfigType)
createUserPool_lambdaConfig = Lens.lens (\CreateUserPool' {lambdaConfig} -> lambdaConfig) (\s@CreateUserPool' {} a -> s {lambdaConfig = a} :: CreateUserPool)

-- | Specifies MFA configuration details.
createUserPool_mfaConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe UserPoolMfaType)
createUserPool_mfaConfiguration = Lens.lens (\CreateUserPool' {mfaConfiguration} -> mfaConfiguration) (\s@CreateUserPool' {} a -> s {mfaConfiguration = a} :: CreateUserPool)

-- | The policies associated with the new user pool.
createUserPool_policies :: Lens.Lens' CreateUserPool (Prelude.Maybe UserPoolPolicyType)
createUserPool_policies = Lens.lens (\CreateUserPool' {policies} -> policies) (\s@CreateUserPool' {} a -> s {policies = a} :: CreateUserPool)

-- | An array of schema attributes for the new user pool. These attributes
-- can be standard or custom attributes.
createUserPool_schema :: Lens.Lens' CreateUserPool (Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType))
createUserPool_schema = Lens.lens (\CreateUserPool' {schema} -> schema) (\s@CreateUserPool' {} a -> s {schema = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | A string representing the SMS authentication message.
createUserPool_smsAuthenticationMessage :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_smsAuthenticationMessage = Lens.lens (\CreateUserPool' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@CreateUserPool' {} a -> s {smsAuthenticationMessage = a} :: CreateUserPool)

-- | The SMS configuration with the settings that your Amazon Cognito user
-- pool must use to send an SMS message from your Amazon Web Services
-- account through Amazon Simple Notification Service. To send SMS messages
-- with Amazon SNS in the Amazon Web Services Region that you want, the
-- Amazon Cognito user pool uses an Identity and Access Management (IAM)
-- role in your Amazon Web Services account.
createUserPool_smsConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe SmsConfigurationType)
createUserPool_smsConfiguration = Lens.lens (\CreateUserPool' {smsConfiguration} -> smsConfiguration) (\s@CreateUserPool' {} a -> s {smsConfiguration = a} :: CreateUserPool)

-- | This parameter is no longer used. See
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_VerificationMessageTemplateType.html VerificationMessageTemplateType>.
createUserPool_smsVerificationMessage :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_smsVerificationMessage = Lens.lens (\CreateUserPool' {smsVerificationMessage} -> smsVerificationMessage) (\s@CreateUserPool' {} a -> s {smsVerificationMessage = a} :: CreateUserPool)

-- | The settings for updates to user attributes. These settings include the
-- property @AttributesRequireVerificationBeforeUpdate@, a user-pool
-- setting that tells Amazon Cognito how to handle changes to the value of
-- your users\' email address and phone number attributes. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-settings-email-phone-verification.html#user-pool-settings-verifications-verify-attribute-updates Verifying updates to email addresses and phone numbers>.
createUserPool_userAttributeUpdateSettings :: Lens.Lens' CreateUserPool (Prelude.Maybe UserAttributeUpdateSettingsType)
createUserPool_userAttributeUpdateSettings = Lens.lens (\CreateUserPool' {userAttributeUpdateSettings} -> userAttributeUpdateSettings) (\s@CreateUserPool' {} a -> s {userAttributeUpdateSettings = a} :: CreateUserPool)

-- | Enables advanced security risk detection. Set the key
-- @AdvancedSecurityMode@ to the value \"AUDIT\".
createUserPool_userPoolAddOns :: Lens.Lens' CreateUserPool (Prelude.Maybe UserPoolAddOnsType)
createUserPool_userPoolAddOns = Lens.lens (\CreateUserPool' {userPoolAddOns} -> userPoolAddOns) (\s@CreateUserPool' {} a -> s {userPoolAddOns = a} :: CreateUserPool)

-- | The tag keys and values to assign to the user pool. A tag is a label
-- that you can use to categorize and manage user pools in different ways,
-- such as by purpose, owner, environment, or other criteria.
createUserPool_userPoolTags :: Lens.Lens' CreateUserPool (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUserPool_userPoolTags = Lens.lens (\CreateUserPool' {userPoolTags} -> userPoolTags) (\s@CreateUserPool' {} a -> s {userPoolTags = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether a user can use an email address or phone number as a
-- username when they sign up.
createUserPool_usernameAttributes :: Lens.Lens' CreateUserPool (Prelude.Maybe [UsernameAttributeType])
createUserPool_usernameAttributes = Lens.lens (\CreateUserPool' {usernameAttributes} -> usernameAttributes) (\s@CreateUserPool' {} a -> s {usernameAttributes = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | Case sensitivity on the username input for the selected sign-in option.
-- For example, when case sensitivity is set to @False@, users can sign in
-- using either \"username\" or \"Username\". This configuration is
-- immutable once it has been set. For more information, see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
createUserPool_usernameConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe UsernameConfigurationType)
createUserPool_usernameConfiguration = Lens.lens (\CreateUserPool' {usernameConfiguration} -> usernameConfiguration) (\s@CreateUserPool' {} a -> s {usernameConfiguration = a} :: CreateUserPool)

-- | The template for the verification message that the user sees when the
-- app requests permission to access the user\'s information.
createUserPool_verificationMessageTemplate :: Lens.Lens' CreateUserPool (Prelude.Maybe VerificationMessageTemplateType)
createUserPool_verificationMessageTemplate = Lens.lens (\CreateUserPool' {verificationMessageTemplate} -> verificationMessageTemplate) (\s@CreateUserPool' {} a -> s {verificationMessageTemplate = a} :: CreateUserPool)

-- | A string used to name the user pool.
createUserPool_poolName :: Lens.Lens' CreateUserPool Prelude.Text
createUserPool_poolName = Lens.lens (\CreateUserPool' {poolName} -> poolName) (\s@CreateUserPool' {} a -> s {poolName = a} :: CreateUserPool)

instance Core.AWSRequest CreateUserPool where
  type
    AWSResponse CreateUserPool =
      CreateUserPoolResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolResponse'
            Prelude.<$> (x Data..?> "UserPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPool where
  hashWithSalt _salt CreateUserPool' {..} =
    _salt
      `Prelude.hashWithSalt` accountRecoverySetting
      `Prelude.hashWithSalt` adminCreateUserConfig
      `Prelude.hashWithSalt` aliasAttributes
      `Prelude.hashWithSalt` autoVerifiedAttributes
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` deviceConfiguration
      `Prelude.hashWithSalt` emailConfiguration
      `Prelude.hashWithSalt` emailVerificationMessage
      `Prelude.hashWithSalt` emailVerificationSubject
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` mfaConfiguration
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` smsAuthenticationMessage
      `Prelude.hashWithSalt` smsConfiguration
      `Prelude.hashWithSalt` smsVerificationMessage
      `Prelude.hashWithSalt` userAttributeUpdateSettings
      `Prelude.hashWithSalt` userPoolAddOns
      `Prelude.hashWithSalt` userPoolTags
      `Prelude.hashWithSalt` usernameAttributes
      `Prelude.hashWithSalt` usernameConfiguration
      `Prelude.hashWithSalt` verificationMessageTemplate
      `Prelude.hashWithSalt` poolName

instance Prelude.NFData CreateUserPool where
  rnf CreateUserPool' {..} =
    Prelude.rnf accountRecoverySetting
      `Prelude.seq` Prelude.rnf adminCreateUserConfig
      `Prelude.seq` Prelude.rnf aliasAttributes
      `Prelude.seq` Prelude.rnf autoVerifiedAttributes
      `Prelude.seq` Prelude.rnf deletionProtection
      `Prelude.seq` Prelude.rnf deviceConfiguration
      `Prelude.seq` Prelude.rnf emailConfiguration
      `Prelude.seq` Prelude.rnf emailVerificationMessage
      `Prelude.seq` Prelude.rnf emailVerificationSubject
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf smsAuthenticationMessage
      `Prelude.seq` Prelude.rnf smsConfiguration
      `Prelude.seq` Prelude.rnf smsVerificationMessage
      `Prelude.seq` Prelude.rnf
        userAttributeUpdateSettings
      `Prelude.seq` Prelude.rnf userPoolAddOns
      `Prelude.seq` Prelude.rnf userPoolTags
      `Prelude.seq` Prelude.rnf usernameAttributes
      `Prelude.seq` Prelude.rnf
        usernameConfiguration
      `Prelude.seq` Prelude.rnf
        verificationMessageTemplate
      `Prelude.seq` Prelude.rnf poolName

instance Data.ToHeaders CreateUserPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityProviderService.CreateUserPool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateUserPool where
  toJSON CreateUserPool' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccountRecoverySetting" Data..=)
              Prelude.<$> accountRecoverySetting,
            ("AdminCreateUserConfig" Data..=)
              Prelude.<$> adminCreateUserConfig,
            ("AliasAttributes" Data..=)
              Prelude.<$> aliasAttributes,
            ("AutoVerifiedAttributes" Data..=)
              Prelude.<$> autoVerifiedAttributes,
            ("DeletionProtection" Data..=)
              Prelude.<$> deletionProtection,
            ("DeviceConfiguration" Data..=)
              Prelude.<$> deviceConfiguration,
            ("EmailConfiguration" Data..=)
              Prelude.<$> emailConfiguration,
            ("EmailVerificationMessage" Data..=)
              Prelude.<$> emailVerificationMessage,
            ("EmailVerificationSubject" Data..=)
              Prelude.<$> emailVerificationSubject,
            ("LambdaConfig" Data..=) Prelude.<$> lambdaConfig,
            ("MfaConfiguration" Data..=)
              Prelude.<$> mfaConfiguration,
            ("Policies" Data..=) Prelude.<$> policies,
            ("Schema" Data..=) Prelude.<$> schema,
            ("SmsAuthenticationMessage" Data..=)
              Prelude.<$> smsAuthenticationMessage,
            ("SmsConfiguration" Data..=)
              Prelude.<$> smsConfiguration,
            ("SmsVerificationMessage" Data..=)
              Prelude.<$> smsVerificationMessage,
            ("UserAttributeUpdateSettings" Data..=)
              Prelude.<$> userAttributeUpdateSettings,
            ("UserPoolAddOns" Data..=)
              Prelude.<$> userPoolAddOns,
            ("UserPoolTags" Data..=) Prelude.<$> userPoolTags,
            ("UsernameAttributes" Data..=)
              Prelude.<$> usernameAttributes,
            ("UsernameConfiguration" Data..=)
              Prelude.<$> usernameConfiguration,
            ("VerificationMessageTemplate" Data..=)
              Prelude.<$> verificationMessageTemplate,
            Prelude.Just ("PoolName" Data..= poolName)
          ]
      )

instance Data.ToPath CreateUserPool where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateUserPool where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server for the request to create a user
-- pool.
--
-- /See:/ 'newCreateUserPoolResponse' smart constructor.
data CreateUserPoolResponse = CreateUserPoolResponse'
  { -- | A container for the user pool details.
    userPool :: Prelude.Maybe UserPoolType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUserPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPool', 'createUserPoolResponse_userPool' - A container for the user pool details.
--
-- 'httpStatus', 'createUserPoolResponse_httpStatus' - The response's http status code.
newCreateUserPoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateUserPoolResponse
newCreateUserPoolResponse pHttpStatus_ =
  CreateUserPoolResponse'
    { userPool = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A container for the user pool details.
createUserPoolResponse_userPool :: Lens.Lens' CreateUserPoolResponse (Prelude.Maybe UserPoolType)
createUserPoolResponse_userPool = Lens.lens (\CreateUserPoolResponse' {userPool} -> userPool) (\s@CreateUserPoolResponse' {} a -> s {userPool = a} :: CreateUserPoolResponse)

-- | The response's http status code.
createUserPoolResponse_httpStatus :: Lens.Lens' CreateUserPoolResponse Prelude.Int
createUserPoolResponse_httpStatus = Lens.lens (\CreateUserPoolResponse' {httpStatus} -> httpStatus) (\s@CreateUserPoolResponse' {} a -> s {httpStatus = a} :: CreateUserPoolResponse)

instance Prelude.NFData CreateUserPoolResponse where
  rnf CreateUserPoolResponse' {..} =
    Prelude.rnf userPool
      `Prelude.seq` Prelude.rnf httpStatus
