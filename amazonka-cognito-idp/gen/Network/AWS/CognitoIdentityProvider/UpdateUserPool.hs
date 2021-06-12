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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user pool with the specified attributes. You can
-- get a list of the current user pool settings using
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_DescribeUserPool.html DescribeUserPool>.
--
-- If you don\'t provide a value for an attribute, it will be set to the
-- default value.
module Network.AWS.CognitoIdentityProvider.UpdateUserPool
  ( -- * Creating a Request
    UpdateUserPool (..),
    newUpdateUserPool,

    -- * Request Lenses
    updateUserPool_userPoolTags,
    updateUserPool_emailVerificationSubject,
    updateUserPool_autoVerifiedAttributes,
    updateUserPool_policies,
    updateUserPool_adminCreateUserConfig,
    updateUserPool_deviceConfiguration,
    updateUserPool_smsConfiguration,
    updateUserPool_lambdaConfig,
    updateUserPool_smsVerificationMessage,
    updateUserPool_accountRecoverySetting,
    updateUserPool_emailConfiguration,
    updateUserPool_emailVerificationMessage,
    updateUserPool_userPoolAddOns,
    updateUserPool_smsAuthenticationMessage,
    updateUserPool_mfaConfiguration,
    updateUserPool_verificationMessageTemplate,
    updateUserPool_userPoolId,

    -- * Destructuring the Response
    UpdateUserPoolResponse (..),
    newUpdateUserPoolResponse,

    -- * Response Lenses
    updateUserPoolResponse_httpStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to update the user pool.
--
-- /See:/ 'newUpdateUserPool' smart constructor.
data UpdateUserPool = UpdateUserPool'
  { -- | The tag keys and values to assign to the user pool. A tag is a label
    -- that you can use to categorize and manage user pools in different ways,
    -- such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The subject of the email verification message.
    emailVerificationSubject :: Core.Maybe Core.Text,
    -- | The attributes that are automatically verified when the Amazon Cognito
    -- service makes a request to update user pools.
    autoVerifiedAttributes :: Core.Maybe [VerifiedAttributeType],
    -- | A container with the policies you wish to update in a user pool.
    policies :: Core.Maybe UserPoolPolicyType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Core.Maybe AdminCreateUserConfigType,
    -- | Device configuration.
    deviceConfiguration :: Core.Maybe DeviceConfigurationType,
    -- | SMS configuration.
    smsConfiguration :: Core.Maybe SmsConfigurationType,
    -- | The AWS Lambda configuration information from the request to update the
    -- user pool.
    lambdaConfig :: Core.Maybe LambdaConfigType,
    -- | A container with information about the SMS verification message.
    smsVerificationMessage :: Core.Maybe Core.Text,
    -- | Use this setting to define which verified available method a user can
    -- use to recover their password when they call @ForgotPassword@. It allows
    -- you to define a preferred method when a user has more than one method
    -- available. With this setting, SMS does not qualify for a valid password
    -- recovery mechanism if the user also has SMS MFA enabled. In the absence
    -- of this setting, Cognito uses the legacy behavior to determine the
    -- recovery method where SMS is preferred over email.
    accountRecoverySetting :: Core.Maybe AccountRecoverySettingType,
    -- | Email configuration.
    emailConfiguration :: Core.Maybe EmailConfigurationType,
    -- | The contents of the email verification message.
    emailVerificationMessage :: Core.Maybe Core.Text,
    -- | Used to enable advanced security risk detection. Set the key
    -- @AdvancedSecurityMode@ to the value \"AUDIT\".
    userPoolAddOns :: Core.Maybe UserPoolAddOnsType,
    -- | The contents of the SMS authentication message.
    smsAuthenticationMessage :: Core.Maybe Core.Text,
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
    verificationMessageTemplate :: Core.Maybe VerificationMessageTemplateType,
    -- | The user pool ID for the user pool you want to update.
    userPoolId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserPool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPoolTags', 'updateUserPool_userPoolTags' - The tag keys and values to assign to the user pool. A tag is a label
-- that you can use to categorize and manage user pools in different ways,
-- such as by purpose, owner, environment, or other criteria.
--
-- 'emailVerificationSubject', 'updateUserPool_emailVerificationSubject' - The subject of the email verification message.
--
-- 'autoVerifiedAttributes', 'updateUserPool_autoVerifiedAttributes' - The attributes that are automatically verified when the Amazon Cognito
-- service makes a request to update user pools.
--
-- 'policies', 'updateUserPool_policies' - A container with the policies you wish to update in a user pool.
--
-- 'adminCreateUserConfig', 'updateUserPool_adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
--
-- 'deviceConfiguration', 'updateUserPool_deviceConfiguration' - Device configuration.
--
-- 'smsConfiguration', 'updateUserPool_smsConfiguration' - SMS configuration.
--
-- 'lambdaConfig', 'updateUserPool_lambdaConfig' - The AWS Lambda configuration information from the request to update the
-- user pool.
--
-- 'smsVerificationMessage', 'updateUserPool_smsVerificationMessage' - A container with information about the SMS verification message.
--
-- 'accountRecoverySetting', 'updateUserPool_accountRecoverySetting' - Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
--
-- 'emailConfiguration', 'updateUserPool_emailConfiguration' - Email configuration.
--
-- 'emailVerificationMessage', 'updateUserPool_emailVerificationMessage' - The contents of the email verification message.
--
-- 'userPoolAddOns', 'updateUserPool_userPoolAddOns' - Used to enable advanced security risk detection. Set the key
-- @AdvancedSecurityMode@ to the value \"AUDIT\".
--
-- 'smsAuthenticationMessage', 'updateUserPool_smsAuthenticationMessage' - The contents of the SMS authentication message.
--
-- 'mfaConfiguration', 'updateUserPool_mfaConfiguration' - Can be one of the following values:
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
-- 'verificationMessageTemplate', 'updateUserPool_verificationMessageTemplate' - The template for verification messages.
--
-- 'userPoolId', 'updateUserPool_userPoolId' - The user pool ID for the user pool you want to update.
newUpdateUserPool ::
  -- | 'userPoolId'
  Core.Text ->
  UpdateUserPool
newUpdateUserPool pUserPoolId_ =
  UpdateUserPool'
    { userPoolTags = Core.Nothing,
      emailVerificationSubject = Core.Nothing,
      autoVerifiedAttributes = Core.Nothing,
      policies = Core.Nothing,
      adminCreateUserConfig = Core.Nothing,
      deviceConfiguration = Core.Nothing,
      smsConfiguration = Core.Nothing,
      lambdaConfig = Core.Nothing,
      smsVerificationMessage = Core.Nothing,
      accountRecoverySetting = Core.Nothing,
      emailConfiguration = Core.Nothing,
      emailVerificationMessage = Core.Nothing,
      userPoolAddOns = Core.Nothing,
      smsAuthenticationMessage = Core.Nothing,
      mfaConfiguration = Core.Nothing,
      verificationMessageTemplate = Core.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The tag keys and values to assign to the user pool. A tag is a label
-- that you can use to categorize and manage user pools in different ways,
-- such as by purpose, owner, environment, or other criteria.
updateUserPool_userPoolTags :: Lens.Lens' UpdateUserPool (Core.Maybe (Core.HashMap Core.Text Core.Text))
updateUserPool_userPoolTags = Lens.lens (\UpdateUserPool' {userPoolTags} -> userPoolTags) (\s@UpdateUserPool' {} a -> s {userPoolTags = a} :: UpdateUserPool) Core.. Lens.mapping Lens._Coerce

-- | The subject of the email verification message.
updateUserPool_emailVerificationSubject :: Lens.Lens' UpdateUserPool (Core.Maybe Core.Text)
updateUserPool_emailVerificationSubject = Lens.lens (\UpdateUserPool' {emailVerificationSubject} -> emailVerificationSubject) (\s@UpdateUserPool' {} a -> s {emailVerificationSubject = a} :: UpdateUserPool)

-- | The attributes that are automatically verified when the Amazon Cognito
-- service makes a request to update user pools.
updateUserPool_autoVerifiedAttributes :: Lens.Lens' UpdateUserPool (Core.Maybe [VerifiedAttributeType])
updateUserPool_autoVerifiedAttributes = Lens.lens (\UpdateUserPool' {autoVerifiedAttributes} -> autoVerifiedAttributes) (\s@UpdateUserPool' {} a -> s {autoVerifiedAttributes = a} :: UpdateUserPool) Core.. Lens.mapping Lens._Coerce

-- | A container with the policies you wish to update in a user pool.
updateUserPool_policies :: Lens.Lens' UpdateUserPool (Core.Maybe UserPoolPolicyType)
updateUserPool_policies = Lens.lens (\UpdateUserPool' {policies} -> policies) (\s@UpdateUserPool' {} a -> s {policies = a} :: UpdateUserPool)

-- | The configuration for @AdminCreateUser@ requests.
updateUserPool_adminCreateUserConfig :: Lens.Lens' UpdateUserPool (Core.Maybe AdminCreateUserConfigType)
updateUserPool_adminCreateUserConfig = Lens.lens (\UpdateUserPool' {adminCreateUserConfig} -> adminCreateUserConfig) (\s@UpdateUserPool' {} a -> s {adminCreateUserConfig = a} :: UpdateUserPool)

-- | Device configuration.
updateUserPool_deviceConfiguration :: Lens.Lens' UpdateUserPool (Core.Maybe DeviceConfigurationType)
updateUserPool_deviceConfiguration = Lens.lens (\UpdateUserPool' {deviceConfiguration} -> deviceConfiguration) (\s@UpdateUserPool' {} a -> s {deviceConfiguration = a} :: UpdateUserPool)

-- | SMS configuration.
updateUserPool_smsConfiguration :: Lens.Lens' UpdateUserPool (Core.Maybe SmsConfigurationType)
updateUserPool_smsConfiguration = Lens.lens (\UpdateUserPool' {smsConfiguration} -> smsConfiguration) (\s@UpdateUserPool' {} a -> s {smsConfiguration = a} :: UpdateUserPool)

-- | The AWS Lambda configuration information from the request to update the
-- user pool.
updateUserPool_lambdaConfig :: Lens.Lens' UpdateUserPool (Core.Maybe LambdaConfigType)
updateUserPool_lambdaConfig = Lens.lens (\UpdateUserPool' {lambdaConfig} -> lambdaConfig) (\s@UpdateUserPool' {} a -> s {lambdaConfig = a} :: UpdateUserPool)

-- | A container with information about the SMS verification message.
updateUserPool_smsVerificationMessage :: Lens.Lens' UpdateUserPool (Core.Maybe Core.Text)
updateUserPool_smsVerificationMessage = Lens.lens (\UpdateUserPool' {smsVerificationMessage} -> smsVerificationMessage) (\s@UpdateUserPool' {} a -> s {smsVerificationMessage = a} :: UpdateUserPool)

-- | Use this setting to define which verified available method a user can
-- use to recover their password when they call @ForgotPassword@. It allows
-- you to define a preferred method when a user has more than one method
-- available. With this setting, SMS does not qualify for a valid password
-- recovery mechanism if the user also has SMS MFA enabled. In the absence
-- of this setting, Cognito uses the legacy behavior to determine the
-- recovery method where SMS is preferred over email.
updateUserPool_accountRecoverySetting :: Lens.Lens' UpdateUserPool (Core.Maybe AccountRecoverySettingType)
updateUserPool_accountRecoverySetting = Lens.lens (\UpdateUserPool' {accountRecoverySetting} -> accountRecoverySetting) (\s@UpdateUserPool' {} a -> s {accountRecoverySetting = a} :: UpdateUserPool)

-- | Email configuration.
updateUserPool_emailConfiguration :: Lens.Lens' UpdateUserPool (Core.Maybe EmailConfigurationType)
updateUserPool_emailConfiguration = Lens.lens (\UpdateUserPool' {emailConfiguration} -> emailConfiguration) (\s@UpdateUserPool' {} a -> s {emailConfiguration = a} :: UpdateUserPool)

-- | The contents of the email verification message.
updateUserPool_emailVerificationMessage :: Lens.Lens' UpdateUserPool (Core.Maybe Core.Text)
updateUserPool_emailVerificationMessage = Lens.lens (\UpdateUserPool' {emailVerificationMessage} -> emailVerificationMessage) (\s@UpdateUserPool' {} a -> s {emailVerificationMessage = a} :: UpdateUserPool)

-- | Used to enable advanced security risk detection. Set the key
-- @AdvancedSecurityMode@ to the value \"AUDIT\".
updateUserPool_userPoolAddOns :: Lens.Lens' UpdateUserPool (Core.Maybe UserPoolAddOnsType)
updateUserPool_userPoolAddOns = Lens.lens (\UpdateUserPool' {userPoolAddOns} -> userPoolAddOns) (\s@UpdateUserPool' {} a -> s {userPoolAddOns = a} :: UpdateUserPool)

-- | The contents of the SMS authentication message.
updateUserPool_smsAuthenticationMessage :: Lens.Lens' UpdateUserPool (Core.Maybe Core.Text)
updateUserPool_smsAuthenticationMessage = Lens.lens (\UpdateUserPool' {smsAuthenticationMessage} -> smsAuthenticationMessage) (\s@UpdateUserPool' {} a -> s {smsAuthenticationMessage = a} :: UpdateUserPool)

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
updateUserPool_mfaConfiguration :: Lens.Lens' UpdateUserPool (Core.Maybe UserPoolMfaType)
updateUserPool_mfaConfiguration = Lens.lens (\UpdateUserPool' {mfaConfiguration} -> mfaConfiguration) (\s@UpdateUserPool' {} a -> s {mfaConfiguration = a} :: UpdateUserPool)

-- | The template for verification messages.
updateUserPool_verificationMessageTemplate :: Lens.Lens' UpdateUserPool (Core.Maybe VerificationMessageTemplateType)
updateUserPool_verificationMessageTemplate = Lens.lens (\UpdateUserPool' {verificationMessageTemplate} -> verificationMessageTemplate) (\s@UpdateUserPool' {} a -> s {verificationMessageTemplate = a} :: UpdateUserPool)

-- | The user pool ID for the user pool you want to update.
updateUserPool_userPoolId :: Lens.Lens' UpdateUserPool Core.Text
updateUserPool_userPoolId = Lens.lens (\UpdateUserPool' {userPoolId} -> userPoolId) (\s@UpdateUserPool' {} a -> s {userPoolId = a} :: UpdateUserPool)

instance Core.AWSRequest UpdateUserPool where
  type
    AWSResponse UpdateUserPool =
      UpdateUserPoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateUserPoolResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateUserPool

instance Core.NFData UpdateUserPool

instance Core.ToHeaders UpdateUserPool where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityProviderService.UpdateUserPool" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateUserPool where
  toJSON UpdateUserPool' {..} =
    Core.object
      ( Core.catMaybes
          [ ("UserPoolTags" Core..=) Core.<$> userPoolTags,
            ("EmailVerificationSubject" Core..=)
              Core.<$> emailVerificationSubject,
            ("AutoVerifiedAttributes" Core..=)
              Core.<$> autoVerifiedAttributes,
            ("Policies" Core..=) Core.<$> policies,
            ("AdminCreateUserConfig" Core..=)
              Core.<$> adminCreateUserConfig,
            ("DeviceConfiguration" Core..=)
              Core.<$> deviceConfiguration,
            ("SmsConfiguration" Core..=)
              Core.<$> smsConfiguration,
            ("LambdaConfig" Core..=) Core.<$> lambdaConfig,
            ("SmsVerificationMessage" Core..=)
              Core.<$> smsVerificationMessage,
            ("AccountRecoverySetting" Core..=)
              Core.<$> accountRecoverySetting,
            ("EmailConfiguration" Core..=)
              Core.<$> emailConfiguration,
            ("EmailVerificationMessage" Core..=)
              Core.<$> emailVerificationMessage,
            ("UserPoolAddOns" Core..=) Core.<$> userPoolAddOns,
            ("SmsAuthenticationMessage" Core..=)
              Core.<$> smsAuthenticationMessage,
            ("MfaConfiguration" Core..=)
              Core.<$> mfaConfiguration,
            ("VerificationMessageTemplate" Core..=)
              Core.<$> verificationMessageTemplate,
            Core.Just ("UserPoolId" Core..= userPoolId)
          ]
      )

instance Core.ToPath UpdateUserPool where
  toPath = Core.const "/"

instance Core.ToQuery UpdateUserPool where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server when you make a request to
-- update the user pool.
--
-- /See:/ 'newUpdateUserPoolResponse' smart constructor.
data UpdateUserPoolResponse = UpdateUserPoolResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateUserPoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateUserPoolResponse_httpStatus' - The response's http status code.
newUpdateUserPoolResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateUserPoolResponse
newUpdateUserPoolResponse pHttpStatus_ =
  UpdateUserPoolResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateUserPoolResponse_httpStatus :: Lens.Lens' UpdateUserPoolResponse Core.Int
updateUserPoolResponse_httpStatus = Lens.lens (\UpdateUserPoolResponse' {httpStatus} -> httpStatus) (\s@UpdateUserPoolResponse' {} a -> s {httpStatus = a} :: UpdateUserPoolResponse)

instance Core.NFData UpdateUserPoolResponse
