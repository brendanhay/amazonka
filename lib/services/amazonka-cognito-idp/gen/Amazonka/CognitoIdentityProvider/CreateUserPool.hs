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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Cognito user pool and sets the password policy for
-- the pool.
--
-- This action might generate an SMS text message. Starting June 1, 2021,
-- U.S. telecom carriers require that you register an origination phone
-- number before you can send SMS messages to U.S. phone numbers. If you
-- use SMS text messages in Amazon Cognito, you must register a phone
-- number with
-- <https://console.aws.amazon.com/pinpoint/home/ Amazon Pinpoint>. Cognito
-- will use the the registered number automatically. Otherwise, Cognito
-- users that must receive SMS messages might be unable to sign up,
-- activate their accounts, or sign in.
--
-- If you have never used SMS text messages with Amazon Cognito or any
-- other Amazon Web Service, Amazon SNS might place your account in SMS
-- sandbox. In
-- /<https://docs.aws.amazon.com/sns/latest/dg/sns-sms-sandbox.html sandbox mode>/
-- , you’ll have limitations, such as sending messages to only verified
-- phone numbers. After testing in the sandbox environment, you can move
-- out of the SMS sandbox and into production. For more information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools-sms-userpool-settings.html SMS message settings for Cognito User Pools>
-- in the /Amazon Cognito Developer Guide/.
module Amazonka.CognitoIdentityProvider.CreateUserPool
  ( -- * Creating a Request
    CreateUserPool (..),
    newCreateUserPool,

    -- * Request Lenses
    createUserPool_userPoolTags,
    createUserPool_verificationMessageTemplate,
    createUserPool_emailVerificationMessage,
    createUserPool_smsAuthenticationMessage,
    createUserPool_userPoolAddOns,
    createUserPool_emailVerificationSubject,
    createUserPool_usernameAttributes,
    createUserPool_aliasAttributes,
    createUserPool_schema,
    createUserPool_accountRecoverySetting,
    createUserPool_emailConfiguration,
    createUserPool_smsVerificationMessage,
    createUserPool_mfaConfiguration,
    createUserPool_lambdaConfig,
    createUserPool_smsConfiguration,
    createUserPool_adminCreateUserConfig,
    createUserPool_deviceConfiguration,
    createUserPool_autoVerifiedAttributes,
    createUserPool_policies,
    createUserPool_usernameConfiguration,
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
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to create a user pool.
--
-- /See:/ 'newCreateUserPool' smart constructor.
data CreateUserPool = CreateUserPool'
  { -- | The tag keys and values to assign to the user pool. A tag is a label
    -- that you can use to categorize and manage user pools in different ways,
    -- such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The template for the verification message that the user sees when the
    -- app requests permission to access the user\'s information.
    verificationMessageTemplate :: Prelude.Maybe VerificationMessageTemplateType,
    -- | A string representing the email verification message.
    -- EmailVerificationMessage is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | A string representing the SMS authentication message.
    smsAuthenticationMessage :: Prelude.Maybe Prelude.Text,
    -- | Used to enable advanced security risk detection. Set the key
    -- @AdvancedSecurityMode@ to the value \"AUDIT\".
    userPoolAddOns :: Prelude.Maybe UserPoolAddOnsType,
    -- | A string representing the email verification subject.
    -- EmailVerificationSubject is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailVerificationSubject :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether email addresses or phone numbers can be specified as
    -- usernames when a user signs up.
    usernameAttributes :: Prelude.Maybe [UsernameAttributeType],
    -- | Attributes supported as an alias for this user pool. Possible values:
    -- __phone_number__, __email__, or __preferred_username__.
    aliasAttributes :: Prelude.Maybe [AliasAttributeType],
    -- | An array of schema attributes for the new user pool. These attributes
    -- can be standard or custom attributes.
    schema :: Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType),
    -- | Use this setting to define which verified available method a user can
    -- use to recover their password when they call @ForgotPassword@. It allows
    -- you to define a preferred method when a user has more than one method
    -- available. With this setting, SMS does not qualify for a valid password
    -- recovery mechanism if the user also has SMS MFA enabled. In the absence
    -- of this setting, Cognito uses the legacy behavior to determine the
    -- recovery method where SMS is preferred over email.
    accountRecoverySetting :: Prelude.Maybe AccountRecoverySettingType,
    -- | The email configuration.
    emailConfiguration :: Prelude.Maybe EmailConfigurationType,
    -- | A string representing the SMS verification message.
    smsVerificationMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies MFA configuration details.
    mfaConfiguration :: Prelude.Maybe UserPoolMfaType,
    -- | The Lambda trigger configuration information for the new user pool.
    --
    -- In a push model, event sources (such as Amazon S3 and custom
    -- applications) need permission to invoke a function. So you will need to
    -- make an extra call to add permission for these event sources to invoke
    -- your Lambda function.
    --
    -- For more information on using the Lambda API to add permission, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html AddPermission>
    -- .
    --
    -- For adding permission using the CLI, see
    -- <https://docs.aws.amazon.com/cli/latest/reference/lambda/add-permission.html add-permission>
    -- .
    lambdaConfig :: Prelude.Maybe LambdaConfigType,
    -- | The SMS configuration.
    smsConfiguration :: Prelude.Maybe SmsConfigurationType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Prelude.Maybe AdminCreateUserConfigType,
    -- | The device configuration.
    deviceConfiguration :: Prelude.Maybe DeviceConfigurationType,
    -- | The attributes to be auto-verified. Possible values: __email__,
    -- __phone_number__.
    autoVerifiedAttributes :: Prelude.Maybe [VerifiedAttributeType],
    -- | The policies associated with the new user pool.
    policies :: Prelude.Maybe UserPoolPolicyType,
    -- | You can choose to set case sensitivity on the username input for the
    -- selected sign-in option. For example, when this is set to @False@, users
    -- will be able to sign in using either \"username\" or \"Username\". This
    -- configuration is immutable once it has been set. For more information,
    -- see
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
    usernameConfiguration :: Prelude.Maybe UsernameConfigurationType,
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
-- 'userPoolTags', 'createUserPool_userPoolTags' - The tag keys and values to assign to the user pool. A tag is a label
-- that you can use to categorize and manage user pools in different ways,
-- such as by purpose, owner, environment, or other criteria.
--
-- 'verificationMessageTemplate', 'createUserPool_verificationMessageTemplate' - The template for the verification message that the user sees when the
-- app requests permission to access the user\'s information.
--
-- 'emailVerificationMessage', 'createUserPool_emailVerificationMessage' - A string representing the email verification message.
-- EmailVerificationMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'smsAuthenticationMessage', 'createUserPool_smsAuthenticationMessage' - A string representing the SMS authentication message.
--
-- 'userPoolAddOns', 'createUserPool_userPoolAddOns' - Used to enable advanced security risk detection. Set the key
-- @AdvancedSecurityMode@ to the value \"AUDIT\".
--
-- 'emailVerificationSubject', 'createUserPool_emailVerificationSubject' - A string representing the email verification subject.
-- EmailVerificationSubject is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'usernameAttributes', 'createUserPool_usernameAttributes' - Specifies whether email addresses or phone numbers can be specified as
-- usernames when a user signs up.
--
-- 'aliasAttributes', 'createUserPool_aliasAttributes' - Attributes supported as an alias for this user pool. Possible values:
-- __phone_number__, __email__, or __preferred_username__.
--
-- 'schema', 'createUserPool_schema' - An array of schema attributes for the new user pool. These attributes
-- can be standard or custom attributes.
--
-- 'accountRecoverySetting', 'createUserPool_accountRecoverySetting' - Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
--
-- 'emailConfiguration', 'createUserPool_emailConfiguration' - The email configuration.
--
-- 'smsVerificationMessage', 'createUserPool_smsVerificationMessage' - A string representing the SMS verification message.
--
-- 'mfaConfiguration', 'createUserPool_mfaConfiguration' - Specifies MFA configuration details.
--
-- 'lambdaConfig', 'createUserPool_lambdaConfig' - The Lambda trigger configuration information for the new user pool.
--
-- In a push model, event sources (such as Amazon S3 and custom
-- applications) need permission to invoke a function. So you will need to
-- make an extra call to add permission for these event sources to invoke
-- your Lambda function.
--
-- For more information on using the Lambda API to add permission, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/API_AddPermission.html AddPermission>
-- .
--
-- For adding permission using the CLI, see
-- <https://docs.aws.amazon.com/cli/latest/reference/lambda/add-permission.html add-permission>
-- .
--
-- 'smsConfiguration', 'createUserPool_smsConfiguration' - The SMS configuration.
--
-- 'adminCreateUserConfig', 'createUserPool_adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- 'deviceConfiguration', 'createUserPool_deviceConfiguration' - The device configuration.
--
-- 'autoVerifiedAttributes', 'createUserPool_autoVerifiedAttributes' - The attributes to be auto-verified. Possible values: __email__,
-- __phone_number__.
--
-- 'policies', 'createUserPool_policies' - The policies associated with the new user pool.
--
-- 'usernameConfiguration', 'createUserPool_usernameConfiguration' - You can choose to set case sensitivity on the username input for the
-- selected sign-in option. For example, when this is set to @False@, users
-- will be able to sign in using either \"username\" or \"Username\". This
-- configuration is immutable once it has been set. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
--
-- 'poolName', 'createUserPool_poolName' - A string used to name the user pool.
newCreateUserPool ::
  -- | 'poolName'
  Prelude.Text ->
  CreateUserPool
newCreateUserPool pPoolName_ =
  CreateUserPool'
    { userPoolTags = Prelude.Nothing,
      verificationMessageTemplate = Prelude.Nothing,
      emailVerificationMessage = Prelude.Nothing,
      smsAuthenticationMessage = Prelude.Nothing,
      userPoolAddOns = Prelude.Nothing,
      emailVerificationSubject = Prelude.Nothing,
      usernameAttributes = Prelude.Nothing,
      aliasAttributes = Prelude.Nothing,
      schema = Prelude.Nothing,
      accountRecoverySetting = Prelude.Nothing,
      emailConfiguration = Prelude.Nothing,
      smsVerificationMessage = Prelude.Nothing,
      mfaConfiguration = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      smsConfiguration = Prelude.Nothing,
      adminCreateUserConfig = Prelude.Nothing,
      deviceConfiguration = Prelude.Nothing,
      autoVerifiedAttributes = Prelude.Nothing,
      policies = Prelude.Nothing,
      usernameConfiguration = Prelude.Nothing,
      poolName = pPoolName_
    }

-- | The tag keys and values to assign to the user pool. A tag is a label
-- that you can use to categorize and manage user pools in different ways,
-- such as by purpose, owner, environment, or other criteria.
createUserPool_userPoolTags :: Lens.Lens' CreateUserPool (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUserPool_userPoolTags = Lens.lens (\CreateUserPool' {userPoolTags} -> userPoolTags) (\s@CreateUserPool' {} a -> s {userPoolTags = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | The template for the verification message that the user sees when the
-- app requests permission to access the user\'s information.
createUserPool_verificationMessageTemplate :: Lens.Lens' CreateUserPool (Prelude.Maybe VerificationMessageTemplateType)
createUserPool_verificationMessageTemplate = Lens.lens (\CreateUserPool' {verificationMessageTemplate} -> verificationMessageTemplate) (\s@CreateUserPool' {} a -> s {verificationMessageTemplate = a} :: CreateUserPool)

-- | A string representing the email verification message.
-- EmailVerificationMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
createUserPool_emailVerificationMessage :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_emailVerificationMessage = Lens.lens (\CreateUserPool' {emailVerificationMessage} -> emailVerificationMessage) (\s@CreateUserPool' {} a -> s {emailVerificationMessage = a} :: CreateUserPool)

-- | A string representing the SMS authentication message.
createUserPool_smsAuthenticationMessage :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_smsAuthenticationMessage = Lens.lens (\CreateUserPool' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@CreateUserPool' {} a -> s {smsAuthenticationMessage = a} :: CreateUserPool)

-- | Used to enable advanced security risk detection. Set the key
-- @AdvancedSecurityMode@ to the value \"AUDIT\".
createUserPool_userPoolAddOns :: Lens.Lens' CreateUserPool (Prelude.Maybe UserPoolAddOnsType)
createUserPool_userPoolAddOns = Lens.lens (\CreateUserPool' {userPoolAddOns} -> userPoolAddOns) (\s@CreateUserPool' {} a -> s {userPoolAddOns = a} :: CreateUserPool)

-- | A string representing the email verification subject.
-- EmailVerificationSubject is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
createUserPool_emailVerificationSubject :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_emailVerificationSubject = Lens.lens (\CreateUserPool' {emailVerificationSubject} -> emailVerificationSubject) (\s@CreateUserPool' {} a -> s {emailVerificationSubject = a} :: CreateUserPool)

-- | Specifies whether email addresses or phone numbers can be specified as
-- usernames when a user signs up.
createUserPool_usernameAttributes :: Lens.Lens' CreateUserPool (Prelude.Maybe [UsernameAttributeType])
createUserPool_usernameAttributes = Lens.lens (\CreateUserPool' {usernameAttributes} -> usernameAttributes) (\s@CreateUserPool' {} a -> s {usernameAttributes = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | Attributes supported as an alias for this user pool. Possible values:
-- __phone_number__, __email__, or __preferred_username__.
createUserPool_aliasAttributes :: Lens.Lens' CreateUserPool (Prelude.Maybe [AliasAttributeType])
createUserPool_aliasAttributes = Lens.lens (\CreateUserPool' {aliasAttributes} -> aliasAttributes) (\s@CreateUserPool' {} a -> s {aliasAttributes = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | An array of schema attributes for the new user pool. These attributes
-- can be standard or custom attributes.
createUserPool_schema :: Lens.Lens' CreateUserPool (Prelude.Maybe (Prelude.NonEmpty SchemaAttributeType))
createUserPool_schema = Lens.lens (\CreateUserPool' {schema} -> schema) (\s@CreateUserPool' {} a -> s {schema = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
createUserPool_accountRecoverySetting :: Lens.Lens' CreateUserPool (Prelude.Maybe AccountRecoverySettingType)
createUserPool_accountRecoverySetting = Lens.lens (\CreateUserPool' {accountRecoverySetting} -> accountRecoverySetting) (\s@CreateUserPool' {} a -> s {accountRecoverySetting = a} :: CreateUserPool)

-- | The email configuration.
createUserPool_emailConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe EmailConfigurationType)
createUserPool_emailConfiguration = Lens.lens (\CreateUserPool' {emailConfiguration} -> emailConfiguration) (\s@CreateUserPool' {} a -> s {emailConfiguration = a} :: CreateUserPool)

-- | A string representing the SMS verification message.
createUserPool_smsVerificationMessage :: Lens.Lens' CreateUserPool (Prelude.Maybe Prelude.Text)
createUserPool_smsVerificationMessage = Lens.lens (\CreateUserPool' {smsVerificationMessage} -> smsVerificationMessage) (\s@CreateUserPool' {} a -> s {smsVerificationMessage = a} :: CreateUserPool)

-- | Specifies MFA configuration details.
createUserPool_mfaConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe UserPoolMfaType)
createUserPool_mfaConfiguration = Lens.lens (\CreateUserPool' {mfaConfiguration} -> mfaConfiguration) (\s@CreateUserPool' {} a -> s {mfaConfiguration = a} :: CreateUserPool)

-- | The Lambda trigger configuration information for the new user pool.
--
-- In a push model, event sources (such as Amazon S3 and custom
-- applications) need permission to invoke a function. So you will need to
-- make an extra call to add permission for these event sources to invoke
-- your Lambda function.
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

-- | The SMS configuration.
createUserPool_smsConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe SmsConfigurationType)
createUserPool_smsConfiguration = Lens.lens (\CreateUserPool' {smsConfiguration} -> smsConfiguration) (\s@CreateUserPool' {} a -> s {smsConfiguration = a} :: CreateUserPool)

-- | The configuration for @AdminCreateUser@ requests.
createUserPool_adminCreateUserConfig :: Lens.Lens' CreateUserPool (Prelude.Maybe AdminCreateUserConfigType)
createUserPool_adminCreateUserConfig = Lens.lens (\CreateUserPool' {adminCreateUserConfig} -> adminCreateUserConfig) (\s@CreateUserPool' {} a -> s {adminCreateUserConfig = a} :: CreateUserPool)

-- | The device configuration.
createUserPool_deviceConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe DeviceConfigurationType)
createUserPool_deviceConfiguration = Lens.lens (\CreateUserPool' {deviceConfiguration} -> deviceConfiguration) (\s@CreateUserPool' {} a -> s {deviceConfiguration = a} :: CreateUserPool)

-- | The attributes to be auto-verified. Possible values: __email__,
-- __phone_number__.
createUserPool_autoVerifiedAttributes :: Lens.Lens' CreateUserPool (Prelude.Maybe [VerifiedAttributeType])
createUserPool_autoVerifiedAttributes = Lens.lens (\CreateUserPool' {autoVerifiedAttributes} -> autoVerifiedAttributes) (\s@CreateUserPool' {} a -> s {autoVerifiedAttributes = a} :: CreateUserPool) Prelude.. Lens.mapping Lens.coerced

-- | The policies associated with the new user pool.
createUserPool_policies :: Lens.Lens' CreateUserPool (Prelude.Maybe UserPoolPolicyType)
createUserPool_policies = Lens.lens (\CreateUserPool' {policies} -> policies) (\s@CreateUserPool' {} a -> s {policies = a} :: CreateUserPool)

-- | You can choose to set case sensitivity on the username input for the
-- selected sign-in option. For example, when this is set to @False@, users
-- will be able to sign in using either \"username\" or \"Username\". This
-- configuration is immutable once it has been set. For more information,
-- see
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType>.
createUserPool_usernameConfiguration :: Lens.Lens' CreateUserPool (Prelude.Maybe UsernameConfigurationType)
createUserPool_usernameConfiguration = Lens.lens (\CreateUserPool' {usernameConfiguration} -> usernameConfiguration) (\s@CreateUserPool' {} a -> s {usernameConfiguration = a} :: CreateUserPool)

-- | A string used to name the user pool.
createUserPool_poolName :: Lens.Lens' CreateUserPool Prelude.Text
createUserPool_poolName = Lens.lens (\CreateUserPool' {poolName} -> poolName) (\s@CreateUserPool' {} a -> s {poolName = a} :: CreateUserPool)

instance Core.AWSRequest CreateUserPool where
  type
    AWSResponse CreateUserPool =
      CreateUserPoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolResponse'
            Prelude.<$> (x Core..?> "UserPool")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateUserPool where
  hashWithSalt _salt CreateUserPool' {..} =
    _salt `Prelude.hashWithSalt` userPoolTags
      `Prelude.hashWithSalt` verificationMessageTemplate
      `Prelude.hashWithSalt` emailVerificationMessage
      `Prelude.hashWithSalt` smsAuthenticationMessage
      `Prelude.hashWithSalt` userPoolAddOns
      `Prelude.hashWithSalt` emailVerificationSubject
      `Prelude.hashWithSalt` usernameAttributes
      `Prelude.hashWithSalt` aliasAttributes
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` accountRecoverySetting
      `Prelude.hashWithSalt` emailConfiguration
      `Prelude.hashWithSalt` smsVerificationMessage
      `Prelude.hashWithSalt` mfaConfiguration
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` smsConfiguration
      `Prelude.hashWithSalt` adminCreateUserConfig
      `Prelude.hashWithSalt` deviceConfiguration
      `Prelude.hashWithSalt` autoVerifiedAttributes
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` usernameConfiguration
      `Prelude.hashWithSalt` poolName

instance Prelude.NFData CreateUserPool where
  rnf CreateUserPool' {..} =
    Prelude.rnf userPoolTags
      `Prelude.seq` Prelude.rnf verificationMessageTemplate
      `Prelude.seq` Prelude.rnf emailVerificationMessage
      `Prelude.seq` Prelude.rnf smsAuthenticationMessage
      `Prelude.seq` Prelude.rnf userPoolAddOns
      `Prelude.seq` Prelude.rnf emailVerificationSubject
      `Prelude.seq` Prelude.rnf usernameAttributes
      `Prelude.seq` Prelude.rnf aliasAttributes
      `Prelude.seq` Prelude.rnf schema
      `Prelude.seq` Prelude.rnf accountRecoverySetting
      `Prelude.seq` Prelude.rnf emailConfiguration
      `Prelude.seq` Prelude.rnf smsVerificationMessage
      `Prelude.seq` Prelude.rnf mfaConfiguration
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf smsConfiguration
      `Prelude.seq` Prelude.rnf adminCreateUserConfig
      `Prelude.seq` Prelude.rnf deviceConfiguration
      `Prelude.seq` Prelude.rnf autoVerifiedAttributes
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf
        usernameConfiguration
      `Prelude.seq` Prelude.rnf poolName

instance Core.ToHeaders CreateUserPool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.CreateUserPool" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateUserPool where
  toJSON CreateUserPool' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UserPoolTags" Core..=) Prelude.<$> userPoolTags,
            ("VerificationMessageTemplate" Core..=)
              Prelude.<$> verificationMessageTemplate,
            ("EmailVerificationMessage" Core..=)
              Prelude.<$> emailVerificationMessage,
            ("SmsAuthenticationMessage" Core..=)
              Prelude.<$> smsAuthenticationMessage,
            ("UserPoolAddOns" Core..=)
              Prelude.<$> userPoolAddOns,
            ("EmailVerificationSubject" Core..=)
              Prelude.<$> emailVerificationSubject,
            ("UsernameAttributes" Core..=)
              Prelude.<$> usernameAttributes,
            ("AliasAttributes" Core..=)
              Prelude.<$> aliasAttributes,
            ("Schema" Core..=) Prelude.<$> schema,
            ("AccountRecoverySetting" Core..=)
              Prelude.<$> accountRecoverySetting,
            ("EmailConfiguration" Core..=)
              Prelude.<$> emailConfiguration,
            ("SmsVerificationMessage" Core..=)
              Prelude.<$> smsVerificationMessage,
            ("MfaConfiguration" Core..=)
              Prelude.<$> mfaConfiguration,
            ("LambdaConfig" Core..=) Prelude.<$> lambdaConfig,
            ("SmsConfiguration" Core..=)
              Prelude.<$> smsConfiguration,
            ("AdminCreateUserConfig" Core..=)
              Prelude.<$> adminCreateUserConfig,
            ("DeviceConfiguration" Core..=)
              Prelude.<$> deviceConfiguration,
            ("AutoVerifiedAttributes" Core..=)
              Prelude.<$> autoVerifiedAttributes,
            ("Policies" Core..=) Prelude.<$> policies,
            ("UsernameConfiguration" Core..=)
              Prelude.<$> usernameConfiguration,
            Prelude.Just ("PoolName" Core..= poolName)
          ]
      )

instance Core.ToPath CreateUserPool where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateUserPool where
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
