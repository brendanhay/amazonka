{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateUserPool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified user pool with the specified attributes. You can get a list of the current user pool settings using <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_DescribeUserPool.html DescribeUserPool> .
--
-- /Important:/ If you don't provide a value for an attribute, it will be set to the default value.
module Network.AWS.CognitoIdentityProvider.UpdateUserPool
  ( -- * Creating a request
    UpdateUserPool (..),
    mkUpdateUserPool,

    -- ** Request lenses
    uupUserPoolTags,
    uupVerificationMessageTemplate,
    uupEmailVerificationMessage,
    uupSmsAuthenticationMessage,
    uupUserPoolAddOns,
    uupEmailVerificationSubject,
    uupAccountRecoverySetting,
    uupEmailConfiguration,
    uupSmsVerificationMessage,
    uupMFAConfiguration,
    uupLambdaConfig,
    uupSmsConfiguration,
    uupAdminCreateUserConfig,
    uupDeviceConfiguration,
    uupAutoVerifiedAttributes,
    uupPolicies,
    uupUserPoolId,

    -- * Destructuring the response
    UpdateUserPoolResponse (..),
    mkUpdateUserPoolResponse,

    -- ** Response lenses
    uuprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to update the user pool.
--
-- /See:/ 'mkUpdateUserPool' smart constructor.
data UpdateUserPool = UpdateUserPool'
  { userPoolTags ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    verificationMessageTemplate ::
      Lude.Maybe VerificationMessageTemplateType,
    emailVerificationMessage :: Lude.Maybe Lude.Text,
    smsAuthenticationMessage :: Lude.Maybe Lude.Text,
    userPoolAddOns :: Lude.Maybe UserPoolAddOnsType,
    emailVerificationSubject :: Lude.Maybe Lude.Text,
    accountRecoverySetting ::
      Lude.Maybe AccountRecoverySettingType,
    emailConfiguration :: Lude.Maybe EmailConfigurationType,
    smsVerificationMessage :: Lude.Maybe Lude.Text,
    mfaConfiguration :: Lude.Maybe UserPoolMFAType,
    lambdaConfig :: Lude.Maybe LambdaConfigType,
    smsConfiguration :: Lude.Maybe SmsConfigurationType,
    adminCreateUserConfig :: Lude.Maybe AdminCreateUserConfigType,
    deviceConfiguration :: Lude.Maybe DeviceConfigurationType,
    autoVerifiedAttributes :: Lude.Maybe [VerifiedAttributeType],
    policies :: Lude.Maybe UserPoolPolicyType,
    userPoolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPool' with the minimum fields required to make a request.
--
-- * 'accountRecoverySetting' - Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
-- * 'adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
-- * 'autoVerifiedAttributes' - The attributes that are automatically verified when the Amazon Cognito service makes a request to update user pools.
-- * 'deviceConfiguration' - Device configuration.
-- * 'emailConfiguration' - Email configuration.
-- * 'emailVerificationMessage' - The contents of the email verification message.
-- * 'emailVerificationSubject' - The subject of the email verification message.
-- * 'lambdaConfig' - The AWS Lambda configuration information from the request to update the user pool.
-- * 'mfaConfiguration' - Can be one of the following values:
--
--
--     * @OFF@ - MFA tokens are not required and cannot be specified during user registration.
--
--
--     * @ON@ - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.
--
--
--     * @OPTIONAL@ - Users have the option when registering to create an MFA token.
--
--
-- * 'policies' - A container with the policies you wish to update in a user pool.
-- * 'smsAuthenticationMessage' - The contents of the SMS authentication message.
-- * 'smsConfiguration' - SMS configuration.
-- * 'smsVerificationMessage' - A container with information about the SMS verification message.
-- * 'userPoolAddOns' - Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
-- * 'userPoolId' - The user pool ID for the user pool you want to update.
-- * 'userPoolTags' - The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
-- * 'verificationMessageTemplate' - The template for verification messages.
mkUpdateUserPool ::
  -- | 'userPoolId'
  Lude.Text ->
  UpdateUserPool
mkUpdateUserPool pUserPoolId_ =
  UpdateUserPool'
    { userPoolTags = Lude.Nothing,
      verificationMessageTemplate = Lude.Nothing,
      emailVerificationMessage = Lude.Nothing,
      smsAuthenticationMessage = Lude.Nothing,
      userPoolAddOns = Lude.Nothing,
      emailVerificationSubject = Lude.Nothing,
      accountRecoverySetting = Lude.Nothing,
      emailConfiguration = Lude.Nothing,
      smsVerificationMessage = Lude.Nothing,
      mfaConfiguration = Lude.Nothing,
      lambdaConfig = Lude.Nothing,
      smsConfiguration = Lude.Nothing,
      adminCreateUserConfig = Lude.Nothing,
      deviceConfiguration = Lude.Nothing,
      autoVerifiedAttributes = Lude.Nothing,
      policies = Lude.Nothing,
      userPoolId = pUserPoolId_
    }

-- | The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'userPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserPoolTags :: Lens.Lens' UpdateUserPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uupUserPoolTags = Lens.lens (userPoolTags :: UpdateUserPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userPoolTags = a} :: UpdateUserPool)
{-# DEPRECATED uupUserPoolTags "Use generic-lens or generic-optics with 'userPoolTags' instead." #-}

-- | The template for verification messages.
--
-- /Note:/ Consider using 'verificationMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupVerificationMessageTemplate :: Lens.Lens' UpdateUserPool (Lude.Maybe VerificationMessageTemplateType)
uupVerificationMessageTemplate = Lens.lens (verificationMessageTemplate :: UpdateUserPool -> Lude.Maybe VerificationMessageTemplateType) (\s a -> s {verificationMessageTemplate = a} :: UpdateUserPool)
{-# DEPRECATED uupVerificationMessageTemplate "Use generic-lens or generic-optics with 'verificationMessageTemplate' instead." #-}

-- | The contents of the email verification message.
--
-- /Note:/ Consider using 'emailVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupEmailVerificationMessage :: Lens.Lens' UpdateUserPool (Lude.Maybe Lude.Text)
uupEmailVerificationMessage = Lens.lens (emailVerificationMessage :: UpdateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {emailVerificationMessage = a} :: UpdateUserPool)
{-# DEPRECATED uupEmailVerificationMessage "Use generic-lens or generic-optics with 'emailVerificationMessage' instead." #-}

-- | The contents of the SMS authentication message.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSmsAuthenticationMessage :: Lens.Lens' UpdateUserPool (Lude.Maybe Lude.Text)
uupSmsAuthenticationMessage = Lens.lens (smsAuthenticationMessage :: UpdateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {smsAuthenticationMessage = a} :: UpdateUserPool)
{-# DEPRECATED uupSmsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead." #-}

-- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
--
-- /Note:/ Consider using 'userPoolAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserPoolAddOns :: Lens.Lens' UpdateUserPool (Lude.Maybe UserPoolAddOnsType)
uupUserPoolAddOns = Lens.lens (userPoolAddOns :: UpdateUserPool -> Lude.Maybe UserPoolAddOnsType) (\s a -> s {userPoolAddOns = a} :: UpdateUserPool)
{-# DEPRECATED uupUserPoolAddOns "Use generic-lens or generic-optics with 'userPoolAddOns' instead." #-}

-- | The subject of the email verification message.
--
-- /Note:/ Consider using 'emailVerificationSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupEmailVerificationSubject :: Lens.Lens' UpdateUserPool (Lude.Maybe Lude.Text)
uupEmailVerificationSubject = Lens.lens (emailVerificationSubject :: UpdateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {emailVerificationSubject = a} :: UpdateUserPool)
{-# DEPRECATED uupEmailVerificationSubject "Use generic-lens or generic-optics with 'emailVerificationSubject' instead." #-}

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- /Note:/ Consider using 'accountRecoverySetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupAccountRecoverySetting :: Lens.Lens' UpdateUserPool (Lude.Maybe AccountRecoverySettingType)
uupAccountRecoverySetting = Lens.lens (accountRecoverySetting :: UpdateUserPool -> Lude.Maybe AccountRecoverySettingType) (\s a -> s {accountRecoverySetting = a} :: UpdateUserPool)
{-# DEPRECATED uupAccountRecoverySetting "Use generic-lens or generic-optics with 'accountRecoverySetting' instead." #-}

-- | Email configuration.
--
-- /Note:/ Consider using 'emailConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupEmailConfiguration :: Lens.Lens' UpdateUserPool (Lude.Maybe EmailConfigurationType)
uupEmailConfiguration = Lens.lens (emailConfiguration :: UpdateUserPool -> Lude.Maybe EmailConfigurationType) (\s a -> s {emailConfiguration = a} :: UpdateUserPool)
{-# DEPRECATED uupEmailConfiguration "Use generic-lens or generic-optics with 'emailConfiguration' instead." #-}

-- | A container with information about the SMS verification message.
--
-- /Note:/ Consider using 'smsVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSmsVerificationMessage :: Lens.Lens' UpdateUserPool (Lude.Maybe Lude.Text)
uupSmsVerificationMessage = Lens.lens (smsVerificationMessage :: UpdateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {smsVerificationMessage = a} :: UpdateUserPool)
{-# DEPRECATED uupSmsVerificationMessage "Use generic-lens or generic-optics with 'smsVerificationMessage' instead." #-}

-- | Can be one of the following values:
--
--
--     * @OFF@ - MFA tokens are not required and cannot be specified during user registration.
--
--
--     * @ON@ - MFA tokens are required for all user registrations. You can only specify required when you are initially creating a user pool.
--
--
--     * @OPTIONAL@ - Users have the option when registering to create an MFA token.
--
--
--
-- /Note:/ Consider using 'mfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupMFAConfiguration :: Lens.Lens' UpdateUserPool (Lude.Maybe UserPoolMFAType)
uupMFAConfiguration = Lens.lens (mfaConfiguration :: UpdateUserPool -> Lude.Maybe UserPoolMFAType) (\s a -> s {mfaConfiguration = a} :: UpdateUserPool)
{-# DEPRECATED uupMFAConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The AWS Lambda configuration information from the request to update the user pool.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupLambdaConfig :: Lens.Lens' UpdateUserPool (Lude.Maybe LambdaConfigType)
uupLambdaConfig = Lens.lens (lambdaConfig :: UpdateUserPool -> Lude.Maybe LambdaConfigType) (\s a -> s {lambdaConfig = a} :: UpdateUserPool)
{-# DEPRECATED uupLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupSmsConfiguration :: Lens.Lens' UpdateUserPool (Lude.Maybe SmsConfigurationType)
uupSmsConfiguration = Lens.lens (smsConfiguration :: UpdateUserPool -> Lude.Maybe SmsConfigurationType) (\s a -> s {smsConfiguration = a} :: UpdateUserPool)
{-# DEPRECATED uupSmsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead." #-}

-- | The configuration for @AdminCreateUser@ requests.
--
-- /Note:/ Consider using 'adminCreateUserConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupAdminCreateUserConfig :: Lens.Lens' UpdateUserPool (Lude.Maybe AdminCreateUserConfigType)
uupAdminCreateUserConfig = Lens.lens (adminCreateUserConfig :: UpdateUserPool -> Lude.Maybe AdminCreateUserConfigType) (\s a -> s {adminCreateUserConfig = a} :: UpdateUserPool)
{-# DEPRECATED uupAdminCreateUserConfig "Use generic-lens or generic-optics with 'adminCreateUserConfig' instead." #-}

-- | Device configuration.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupDeviceConfiguration :: Lens.Lens' UpdateUserPool (Lude.Maybe DeviceConfigurationType)
uupDeviceConfiguration = Lens.lens (deviceConfiguration :: UpdateUserPool -> Lude.Maybe DeviceConfigurationType) (\s a -> s {deviceConfiguration = a} :: UpdateUserPool)
{-# DEPRECATED uupDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | The attributes that are automatically verified when the Amazon Cognito service makes a request to update user pools.
--
-- /Note:/ Consider using 'autoVerifiedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupAutoVerifiedAttributes :: Lens.Lens' UpdateUserPool (Lude.Maybe [VerifiedAttributeType])
uupAutoVerifiedAttributes = Lens.lens (autoVerifiedAttributes :: UpdateUserPool -> Lude.Maybe [VerifiedAttributeType]) (\s a -> s {autoVerifiedAttributes = a} :: UpdateUserPool)
{-# DEPRECATED uupAutoVerifiedAttributes "Use generic-lens or generic-optics with 'autoVerifiedAttributes' instead." #-}

-- | A container with the policies you wish to update in a user pool.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupPolicies :: Lens.Lens' UpdateUserPool (Lude.Maybe UserPoolPolicyType)
uupPolicies = Lens.lens (policies :: UpdateUserPool -> Lude.Maybe UserPoolPolicyType) (\s a -> s {policies = a} :: UpdateUserPool)
{-# DEPRECATED uupPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | The user pool ID for the user pool you want to update.
--
-- /Note:/ Consider using 'userPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uupUserPoolId :: Lens.Lens' UpdateUserPool Lude.Text
uupUserPoolId = Lens.lens (userPoolId :: UpdateUserPool -> Lude.Text) (\s a -> s {userPoolId = a} :: UpdateUserPool)
{-# DEPRECATED uupUserPoolId "Use generic-lens or generic-optics with 'userPoolId' instead." #-}

instance Lude.AWSRequest UpdateUserPool where
  type Rs UpdateUserPool = UpdateUserPoolResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateUserPoolResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateUserPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.UpdateUserPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateUserPool where
  toJSON UpdateUserPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserPoolTags" Lude..=) Lude.<$> userPoolTags,
            ("VerificationMessageTemplate" Lude..=)
              Lude.<$> verificationMessageTemplate,
            ("EmailVerificationMessage" Lude..=)
              Lude.<$> emailVerificationMessage,
            ("SmsAuthenticationMessage" Lude..=)
              Lude.<$> smsAuthenticationMessage,
            ("UserPoolAddOns" Lude..=) Lude.<$> userPoolAddOns,
            ("EmailVerificationSubject" Lude..=)
              Lude.<$> emailVerificationSubject,
            ("AccountRecoverySetting" Lude..=) Lude.<$> accountRecoverySetting,
            ("EmailConfiguration" Lude..=) Lude.<$> emailConfiguration,
            ("SmsVerificationMessage" Lude..=) Lude.<$> smsVerificationMessage,
            ("MfaConfiguration" Lude..=) Lude.<$> mfaConfiguration,
            ("LambdaConfig" Lude..=) Lude.<$> lambdaConfig,
            ("SmsConfiguration" Lude..=) Lude.<$> smsConfiguration,
            ("AdminCreateUserConfig" Lude..=) Lude.<$> adminCreateUserConfig,
            ("DeviceConfiguration" Lude..=) Lude.<$> deviceConfiguration,
            ("AutoVerifiedAttributes" Lude..=) Lude.<$> autoVerifiedAttributes,
            ("Policies" Lude..=) Lude.<$> policies,
            Lude.Just ("UserPoolId" Lude..= userPoolId)
          ]
      )

instance Lude.ToPath UpdateUserPool where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUserPool where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server when you make a request to update the user pool.
--
-- /See:/ 'mkUpdateUserPoolResponse' smart constructor.
newtype UpdateUserPoolResponse = UpdateUserPoolResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserPoolResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateUserPoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateUserPoolResponse
mkUpdateUserPoolResponse pResponseStatus_ =
  UpdateUserPoolResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uuprsResponseStatus :: Lens.Lens' UpdateUserPoolResponse Lude.Int
uuprsResponseStatus = Lens.lens (responseStatus :: UpdateUserPoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateUserPoolResponse)
{-# DEPRECATED uuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
