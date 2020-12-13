{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
  ( -- * Creating a request
    CreateUserPool (..),
    mkCreateUserPool,

    -- ** Request lenses
    cupUserPoolTags,
    cupVerificationMessageTemplate,
    cupPoolName,
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

    -- * Destructuring the response
    CreateUserPoolResponse (..),
    mkCreateUserPoolResponse,

    -- ** Response lenses
    cuprsUserPool,
    cuprsResponseStatus,
  )
where

import Network.AWS.CognitoIdentityProvider.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the request to create a user pool.
--
-- /See:/ 'mkCreateUserPool' smart constructor.
data CreateUserPool = CreateUserPool'
  { -- | The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The template for the verification message that the user sees when the app requests permission to access the user's information.
    verificationMessageTemplate :: Lude.Maybe VerificationMessageTemplateType,
    -- | A string used to name the user pool.
    poolName :: Lude.Text,
    -- | A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
    emailVerificationMessage :: Lude.Maybe Lude.Text,
    -- | A string representing the SMS authentication message.
    smsAuthenticationMessage :: Lude.Maybe Lude.Text,
    -- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
    userPoolAddOns :: Lude.Maybe UserPoolAddOnsType,
    -- | A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
    emailVerificationSubject :: Lude.Maybe Lude.Text,
    -- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
    usernameAttributes :: Lude.Maybe [UsernameAttributeType],
    -- | Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
    aliasAttributes :: Lude.Maybe [AliasAttributeType],
    -- | An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
    schema :: Lude.Maybe (Lude.NonEmpty SchemaAttributeType),
    -- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
    accountRecoverySetting :: Lude.Maybe AccountRecoverySettingType,
    -- | The email configuration.
    emailConfiguration :: Lude.Maybe EmailConfigurationType,
    -- | A string representing the SMS verification message.
    smsVerificationMessage :: Lude.Maybe Lude.Text,
    -- | Specifies MFA configuration details.
    mfaConfiguration :: Lude.Maybe UserPoolMFAType,
    -- | The Lambda trigger configuration information for the new user pool.
    lambdaConfig :: Lude.Maybe LambdaConfigType,
    -- | The SMS configuration.
    smsConfiguration :: Lude.Maybe SmsConfigurationType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Lude.Maybe AdminCreateUserConfigType,
    -- | The device configuration.
    deviceConfiguration :: Lude.Maybe DeviceConfigurationType,
    -- | The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
    autoVerifiedAttributes :: Lude.Maybe [VerifiedAttributeType],
    -- | The policies associated with the new user pool.
    policies :: Lude.Maybe UserPoolPolicyType,
    -- | You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
    usernameConfiguration :: Lude.Maybe UsernameConfigurationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserPool' with the minimum fields required to make a request.
--
-- * 'userPoolTags' - The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
-- * 'verificationMessageTemplate' - The template for the verification message that the user sees when the app requests permission to access the user's information.
-- * 'poolName' - A string used to name the user pool.
-- * 'emailVerificationMessage' - A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'smsAuthenticationMessage' - A string representing the SMS authentication message.
-- * 'userPoolAddOns' - Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
-- * 'emailVerificationSubject' - A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'usernameAttributes' - Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
-- * 'aliasAttributes' - Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
-- * 'schema' - An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
-- * 'accountRecoverySetting' - Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
-- * 'emailConfiguration' - The email configuration.
-- * 'smsVerificationMessage' - A string representing the SMS verification message.
-- * 'mfaConfiguration' - Specifies MFA configuration details.
-- * 'lambdaConfig' - The Lambda trigger configuration information for the new user pool.
-- * 'smsConfiguration' - The SMS configuration.
-- * 'adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
-- * 'deviceConfiguration' - The device configuration.
-- * 'autoVerifiedAttributes' - The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
-- * 'policies' - The policies associated with the new user pool.
-- * 'usernameConfiguration' - You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
mkCreateUserPool ::
  -- | 'poolName'
  Lude.Text ->
  CreateUserPool
mkCreateUserPool pPoolName_ =
  CreateUserPool'
    { userPoolTags = Lude.Nothing,
      verificationMessageTemplate = Lude.Nothing,
      poolName = pPoolName_,
      emailVerificationMessage = Lude.Nothing,
      smsAuthenticationMessage = Lude.Nothing,
      userPoolAddOns = Lude.Nothing,
      emailVerificationSubject = Lude.Nothing,
      usernameAttributes = Lude.Nothing,
      aliasAttributes = Lude.Nothing,
      schema = Lude.Nothing,
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
      usernameConfiguration = Lude.Nothing
    }

-- | The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'userPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserPoolTags :: Lens.Lens' CreateUserPool (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cupUserPoolTags = Lens.lens (userPoolTags :: CreateUserPool -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userPoolTags = a} :: CreateUserPool)
{-# DEPRECATED cupUserPoolTags "Use generic-lens or generic-optics with 'userPoolTags' instead." #-}

-- | The template for the verification message that the user sees when the app requests permission to access the user's information.
--
-- /Note:/ Consider using 'verificationMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupVerificationMessageTemplate :: Lens.Lens' CreateUserPool (Lude.Maybe VerificationMessageTemplateType)
cupVerificationMessageTemplate = Lens.lens (verificationMessageTemplate :: CreateUserPool -> Lude.Maybe VerificationMessageTemplateType) (\s a -> s {verificationMessageTemplate = a} :: CreateUserPool)
{-# DEPRECATED cupVerificationMessageTemplate "Use generic-lens or generic-optics with 'verificationMessageTemplate' instead." #-}

-- | A string used to name the user pool.
--
-- /Note:/ Consider using 'poolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupPoolName :: Lens.Lens' CreateUserPool Lude.Text
cupPoolName = Lens.lens (poolName :: CreateUserPool -> Lude.Text) (\s a -> s {poolName = a} :: CreateUserPool)
{-# DEPRECATED cupPoolName "Use generic-lens or generic-optics with 'poolName' instead." #-}

-- | A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailVerificationMessage :: Lens.Lens' CreateUserPool (Lude.Maybe Lude.Text)
cupEmailVerificationMessage = Lens.lens (emailVerificationMessage :: CreateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {emailVerificationMessage = a} :: CreateUserPool)
{-# DEPRECATED cupEmailVerificationMessage "Use generic-lens or generic-optics with 'emailVerificationMessage' instead." #-}

-- | A string representing the SMS authentication message.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSmsAuthenticationMessage :: Lens.Lens' CreateUserPool (Lude.Maybe Lude.Text)
cupSmsAuthenticationMessage = Lens.lens (smsAuthenticationMessage :: CreateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {smsAuthenticationMessage = a} :: CreateUserPool)
{-# DEPRECATED cupSmsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead." #-}

-- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
--
-- /Note:/ Consider using 'userPoolAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserPoolAddOns :: Lens.Lens' CreateUserPool (Lude.Maybe UserPoolAddOnsType)
cupUserPoolAddOns = Lens.lens (userPoolAddOns :: CreateUserPool -> Lude.Maybe UserPoolAddOnsType) (\s a -> s {userPoolAddOns = a} :: CreateUserPool)
{-# DEPRECATED cupUserPoolAddOns "Use generic-lens or generic-optics with 'userPoolAddOns' instead." #-}

-- | A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailVerificationSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailVerificationSubject :: Lens.Lens' CreateUserPool (Lude.Maybe Lude.Text)
cupEmailVerificationSubject = Lens.lens (emailVerificationSubject :: CreateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {emailVerificationSubject = a} :: CreateUserPool)
{-# DEPRECATED cupEmailVerificationSubject "Use generic-lens or generic-optics with 'emailVerificationSubject' instead." #-}

-- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
--
-- /Note:/ Consider using 'usernameAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUsernameAttributes :: Lens.Lens' CreateUserPool (Lude.Maybe [UsernameAttributeType])
cupUsernameAttributes = Lens.lens (usernameAttributes :: CreateUserPool -> Lude.Maybe [UsernameAttributeType]) (\s a -> s {usernameAttributes = a} :: CreateUserPool)
{-# DEPRECATED cupUsernameAttributes "Use generic-lens or generic-optics with 'usernameAttributes' instead." #-}

-- | Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
--
-- /Note:/ Consider using 'aliasAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAliasAttributes :: Lens.Lens' CreateUserPool (Lude.Maybe [AliasAttributeType])
cupAliasAttributes = Lens.lens (aliasAttributes :: CreateUserPool -> Lude.Maybe [AliasAttributeType]) (\s a -> s {aliasAttributes = a} :: CreateUserPool)
{-# DEPRECATED cupAliasAttributes "Use generic-lens or generic-optics with 'aliasAttributes' instead." #-}

-- | An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSchema :: Lens.Lens' CreateUserPool (Lude.Maybe (Lude.NonEmpty SchemaAttributeType))
cupSchema = Lens.lens (schema :: CreateUserPool -> Lude.Maybe (Lude.NonEmpty SchemaAttributeType)) (\s a -> s {schema = a} :: CreateUserPool)
{-# DEPRECATED cupSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- /Note:/ Consider using 'accountRecoverySetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAccountRecoverySetting :: Lens.Lens' CreateUserPool (Lude.Maybe AccountRecoverySettingType)
cupAccountRecoverySetting = Lens.lens (accountRecoverySetting :: CreateUserPool -> Lude.Maybe AccountRecoverySettingType) (\s a -> s {accountRecoverySetting = a} :: CreateUserPool)
{-# DEPRECATED cupAccountRecoverySetting "Use generic-lens or generic-optics with 'accountRecoverySetting' instead." #-}

-- | The email configuration.
--
-- /Note:/ Consider using 'emailConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailConfiguration :: Lens.Lens' CreateUserPool (Lude.Maybe EmailConfigurationType)
cupEmailConfiguration = Lens.lens (emailConfiguration :: CreateUserPool -> Lude.Maybe EmailConfigurationType) (\s a -> s {emailConfiguration = a} :: CreateUserPool)
{-# DEPRECATED cupEmailConfiguration "Use generic-lens or generic-optics with 'emailConfiguration' instead." #-}

-- | A string representing the SMS verification message.
--
-- /Note:/ Consider using 'smsVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSmsVerificationMessage :: Lens.Lens' CreateUserPool (Lude.Maybe Lude.Text)
cupSmsVerificationMessage = Lens.lens (smsVerificationMessage :: CreateUserPool -> Lude.Maybe Lude.Text) (\s a -> s {smsVerificationMessage = a} :: CreateUserPool)
{-# DEPRECATED cupSmsVerificationMessage "Use generic-lens or generic-optics with 'smsVerificationMessage' instead." #-}

-- | Specifies MFA configuration details.
--
-- /Note:/ Consider using 'mfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupMFAConfiguration :: Lens.Lens' CreateUserPool (Lude.Maybe UserPoolMFAType)
cupMFAConfiguration = Lens.lens (mfaConfiguration :: CreateUserPool -> Lude.Maybe UserPoolMFAType) (\s a -> s {mfaConfiguration = a} :: CreateUserPool)
{-# DEPRECATED cupMFAConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The Lambda trigger configuration information for the new user pool.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupLambdaConfig :: Lens.Lens' CreateUserPool (Lude.Maybe LambdaConfigType)
cupLambdaConfig = Lens.lens (lambdaConfig :: CreateUserPool -> Lude.Maybe LambdaConfigType) (\s a -> s {lambdaConfig = a} :: CreateUserPool)
{-# DEPRECATED cupLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | The SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSmsConfiguration :: Lens.Lens' CreateUserPool (Lude.Maybe SmsConfigurationType)
cupSmsConfiguration = Lens.lens (smsConfiguration :: CreateUserPool -> Lude.Maybe SmsConfigurationType) (\s a -> s {smsConfiguration = a} :: CreateUserPool)
{-# DEPRECATED cupSmsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead." #-}

-- | The configuration for @AdminCreateUser@ requests.
--
-- /Note:/ Consider using 'adminCreateUserConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAdminCreateUserConfig :: Lens.Lens' CreateUserPool (Lude.Maybe AdminCreateUserConfigType)
cupAdminCreateUserConfig = Lens.lens (adminCreateUserConfig :: CreateUserPool -> Lude.Maybe AdminCreateUserConfigType) (\s a -> s {adminCreateUserConfig = a} :: CreateUserPool)
{-# DEPRECATED cupAdminCreateUserConfig "Use generic-lens or generic-optics with 'adminCreateUserConfig' instead." #-}

-- | The device configuration.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDeviceConfiguration :: Lens.Lens' CreateUserPool (Lude.Maybe DeviceConfigurationType)
cupDeviceConfiguration = Lens.lens (deviceConfiguration :: CreateUserPool -> Lude.Maybe DeviceConfigurationType) (\s a -> s {deviceConfiguration = a} :: CreateUserPool)
{-# DEPRECATED cupDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
--
-- /Note:/ Consider using 'autoVerifiedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAutoVerifiedAttributes :: Lens.Lens' CreateUserPool (Lude.Maybe [VerifiedAttributeType])
cupAutoVerifiedAttributes = Lens.lens (autoVerifiedAttributes :: CreateUserPool -> Lude.Maybe [VerifiedAttributeType]) (\s a -> s {autoVerifiedAttributes = a} :: CreateUserPool)
{-# DEPRECATED cupAutoVerifiedAttributes "Use generic-lens or generic-optics with 'autoVerifiedAttributes' instead." #-}

-- | The policies associated with the new user pool.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupPolicies :: Lens.Lens' CreateUserPool (Lude.Maybe UserPoolPolicyType)
cupPolicies = Lens.lens (policies :: CreateUserPool -> Lude.Maybe UserPoolPolicyType) (\s a -> s {policies = a} :: CreateUserPool)
{-# DEPRECATED cupPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
--
-- /Note:/ Consider using 'usernameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUsernameConfiguration :: Lens.Lens' CreateUserPool (Lude.Maybe UsernameConfigurationType)
cupUsernameConfiguration = Lens.lens (usernameConfiguration :: CreateUserPool -> Lude.Maybe UsernameConfigurationType) (\s a -> s {usernameConfiguration = a} :: CreateUserPool)
{-# DEPRECATED cupUsernameConfiguration "Use generic-lens or generic-optics with 'usernameConfiguration' instead." #-}

instance Lude.AWSRequest CreateUserPool where
  type Rs CreateUserPool = CreateUserPoolResponse
  request = Req.postJSON cognitoIdentityProviderService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUserPoolResponse'
            Lude.<$> (x Lude..?> "UserPool") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUserPool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSCognitoIdentityProviderService.CreateUserPool" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUserPool where
  toJSON CreateUserPool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("UserPoolTags" Lude..=) Lude.<$> userPoolTags,
            ("VerificationMessageTemplate" Lude..=)
              Lude.<$> verificationMessageTemplate,
            Lude.Just ("PoolName" Lude..= poolName),
            ("EmailVerificationMessage" Lude..=)
              Lude.<$> emailVerificationMessage,
            ("SmsAuthenticationMessage" Lude..=)
              Lude.<$> smsAuthenticationMessage,
            ("UserPoolAddOns" Lude..=) Lude.<$> userPoolAddOns,
            ("EmailVerificationSubject" Lude..=)
              Lude.<$> emailVerificationSubject,
            ("UsernameAttributes" Lude..=) Lude.<$> usernameAttributes,
            ("AliasAttributes" Lude..=) Lude.<$> aliasAttributes,
            ("Schema" Lude..=) Lude.<$> schema,
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
            ("UsernameConfiguration" Lude..=) Lude.<$> usernameConfiguration
          ]
      )

instance Lude.ToPath CreateUserPool where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUserPool where
  toQuery = Lude.const Lude.mempty

-- | Represents the response from the server for the request to create a user pool.
--
-- /See:/ 'mkCreateUserPoolResponse' smart constructor.
data CreateUserPoolResponse = CreateUserPoolResponse'
  { -- | A container for the user pool details.
    userPool :: Lude.Maybe UserPoolType,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUserPoolResponse' with the minimum fields required to make a request.
--
-- * 'userPool' - A container for the user pool details.
-- * 'responseStatus' - The response status code.
mkCreateUserPoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUserPoolResponse
mkCreateUserPoolResponse pResponseStatus_ =
  CreateUserPoolResponse'
    { userPool = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A container for the user pool details.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsUserPool :: Lens.Lens' CreateUserPoolResponse (Lude.Maybe UserPoolType)
cuprsUserPool = Lens.lens (userPool :: CreateUserPoolResponse -> Lude.Maybe UserPoolType) (\s a -> s {userPool = a} :: CreateUserPoolResponse)
{-# DEPRECATED cuprsUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprsResponseStatus :: Lens.Lens' CreateUserPoolResponse Lude.Int
cuprsResponseStatus = Lens.lens (responseStatus :: CreateUserPoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUserPoolResponse)
{-# DEPRECATED cuprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
