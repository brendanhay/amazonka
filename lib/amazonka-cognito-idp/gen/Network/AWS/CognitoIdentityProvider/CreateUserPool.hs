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
    cupPoolName,
    cupAccountRecoverySetting,
    cupAdminCreateUserConfig,
    cupAliasAttributes,
    cupAutoVerifiedAttributes,
    cupDeviceConfiguration,
    cupEmailConfiguration,
    cupEmailVerificationMessage,
    cupEmailVerificationSubject,
    cupLambdaConfig,
    cupMfaConfiguration,
    cupPolicies,
    cupSchema,
    cupSmsAuthenticationMessage,
    cupSmsConfiguration,
    cupSmsVerificationMessage,
    cupUserPoolAddOns,
    cupUserPoolTags,
    cupUsernameAttributes,
    cupUsernameConfiguration,
    cupVerificationMessageTemplate,

    -- * Destructuring the response
    CreateUserPoolResponse (..),
    mkCreateUserPoolResponse,

    -- ** Response lenses
    cuprrsUserPool,
    cuprrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to create a user pool.
--
-- /See:/ 'mkCreateUserPool' smart constructor.
data CreateUserPool = CreateUserPool'
  { -- | A string used to name the user pool.
    poolName :: Types.PoolName,
    -- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
    accountRecoverySetting :: Core.Maybe Types.AccountRecoverySettingType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Core.Maybe Types.AdminCreateUserConfigType,
    -- | Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
    aliasAttributes :: Core.Maybe [Types.AliasAttributeType],
    -- | The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
    autoVerifiedAttributes :: Core.Maybe [Types.VerifiedAttributeType],
    -- | The device configuration.
    deviceConfiguration :: Core.Maybe Types.DeviceConfigurationType,
    -- | The email configuration.
    emailConfiguration :: Core.Maybe Types.EmailConfigurationType,
    -- | A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
    emailVerificationMessage :: Core.Maybe Types.EmailVerificationMessage,
    -- | A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
    emailVerificationSubject :: Core.Maybe Types.EmailVerificationSubject,
    -- | The Lambda trigger configuration information for the new user pool.
    lambdaConfig :: Core.Maybe Types.LambdaConfigType,
    -- | Specifies MFA configuration details.
    mfaConfiguration :: Core.Maybe Types.UserPoolMfaType,
    -- | The policies associated with the new user pool.
    policies :: Core.Maybe Types.UserPoolPolicyType,
    -- | An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
    schema :: Core.Maybe (Core.NonEmpty Types.SchemaAttributeType),
    -- | A string representing the SMS authentication message.
    smsAuthenticationMessage :: Core.Maybe Types.SmsVerificationMessageType,
    -- | The SMS configuration.
    smsConfiguration :: Core.Maybe Types.SmsConfigurationType,
    -- | A string representing the SMS verification message.
    smsVerificationMessage :: Core.Maybe Types.SmsVerificationMessageType,
    -- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
    userPoolAddOns :: Core.Maybe Types.UserPoolAddOnsType,
    -- | The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType),
    -- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
    usernameAttributes :: Core.Maybe [Types.UsernameAttributeType],
    -- | You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
    usernameConfiguration :: Core.Maybe Types.UsernameConfigurationType,
    -- | The template for the verification message that the user sees when the app requests permission to access the user's information.
    verificationMessageTemplate :: Core.Maybe Types.VerificationMessageTemplateType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateUserPool' value with any optional fields omitted.
mkCreateUserPool ::
  -- | 'poolName'
  Types.PoolName ->
  CreateUserPool
mkCreateUserPool poolName =
  CreateUserPool'
    { poolName,
      accountRecoverySetting = Core.Nothing,
      adminCreateUserConfig = Core.Nothing,
      aliasAttributes = Core.Nothing,
      autoVerifiedAttributes = Core.Nothing,
      deviceConfiguration = Core.Nothing,
      emailConfiguration = Core.Nothing,
      emailVerificationMessage = Core.Nothing,
      emailVerificationSubject = Core.Nothing,
      lambdaConfig = Core.Nothing,
      mfaConfiguration = Core.Nothing,
      policies = Core.Nothing,
      schema = Core.Nothing,
      smsAuthenticationMessage = Core.Nothing,
      smsConfiguration = Core.Nothing,
      smsVerificationMessage = Core.Nothing,
      userPoolAddOns = Core.Nothing,
      userPoolTags = Core.Nothing,
      usernameAttributes = Core.Nothing,
      usernameConfiguration = Core.Nothing,
      verificationMessageTemplate = Core.Nothing
    }

-- | A string used to name the user pool.
--
-- /Note:/ Consider using 'poolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupPoolName :: Lens.Lens' CreateUserPool Types.PoolName
cupPoolName = Lens.field @"poolName"
{-# DEPRECATED cupPoolName "Use generic-lens or generic-optics with 'poolName' instead." #-}

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- /Note:/ Consider using 'accountRecoverySetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAccountRecoverySetting :: Lens.Lens' CreateUserPool (Core.Maybe Types.AccountRecoverySettingType)
cupAccountRecoverySetting = Lens.field @"accountRecoverySetting"
{-# DEPRECATED cupAccountRecoverySetting "Use generic-lens or generic-optics with 'accountRecoverySetting' instead." #-}

-- | The configuration for @AdminCreateUser@ requests.
--
-- /Note:/ Consider using 'adminCreateUserConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAdminCreateUserConfig :: Lens.Lens' CreateUserPool (Core.Maybe Types.AdminCreateUserConfigType)
cupAdminCreateUserConfig = Lens.field @"adminCreateUserConfig"
{-# DEPRECATED cupAdminCreateUserConfig "Use generic-lens or generic-optics with 'adminCreateUserConfig' instead." #-}

-- | Attributes supported as an alias for this user pool. Possible values: __phone_number__ , __email__ , or __preferred_username__ .
--
-- /Note:/ Consider using 'aliasAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAliasAttributes :: Lens.Lens' CreateUserPool (Core.Maybe [Types.AliasAttributeType])
cupAliasAttributes = Lens.field @"aliasAttributes"
{-# DEPRECATED cupAliasAttributes "Use generic-lens or generic-optics with 'aliasAttributes' instead." #-}

-- | The attributes to be auto-verified. Possible values: __email__ , __phone_number__ .
--
-- /Note:/ Consider using 'autoVerifiedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupAutoVerifiedAttributes :: Lens.Lens' CreateUserPool (Core.Maybe [Types.VerifiedAttributeType])
cupAutoVerifiedAttributes = Lens.field @"autoVerifiedAttributes"
{-# DEPRECATED cupAutoVerifiedAttributes "Use generic-lens or generic-optics with 'autoVerifiedAttributes' instead." #-}

-- | The device configuration.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupDeviceConfiguration :: Lens.Lens' CreateUserPool (Core.Maybe Types.DeviceConfigurationType)
cupDeviceConfiguration = Lens.field @"deviceConfiguration"
{-# DEPRECATED cupDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | The email configuration.
--
-- /Note:/ Consider using 'emailConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailConfiguration :: Lens.Lens' CreateUserPool (Core.Maybe Types.EmailConfigurationType)
cupEmailConfiguration = Lens.field @"emailConfiguration"
{-# DEPRECATED cupEmailConfiguration "Use generic-lens or generic-optics with 'emailConfiguration' instead." #-}

-- | A string representing the email verification message. EmailVerificationMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailVerificationMessage :: Lens.Lens' CreateUserPool (Core.Maybe Types.EmailVerificationMessage)
cupEmailVerificationMessage = Lens.field @"emailVerificationMessage"
{-# DEPRECATED cupEmailVerificationMessage "Use generic-lens or generic-optics with 'emailVerificationMessage' instead." #-}

-- | A string representing the email verification subject. EmailVerificationSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailVerificationSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupEmailVerificationSubject :: Lens.Lens' CreateUserPool (Core.Maybe Types.EmailVerificationSubject)
cupEmailVerificationSubject = Lens.field @"emailVerificationSubject"
{-# DEPRECATED cupEmailVerificationSubject "Use generic-lens or generic-optics with 'emailVerificationSubject' instead." #-}

-- | The Lambda trigger configuration information for the new user pool.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupLambdaConfig :: Lens.Lens' CreateUserPool (Core.Maybe Types.LambdaConfigType)
cupLambdaConfig = Lens.field @"lambdaConfig"
{-# DEPRECATED cupLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | Specifies MFA configuration details.
--
-- /Note:/ Consider using 'mfaConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupMfaConfiguration :: Lens.Lens' CreateUserPool (Core.Maybe Types.UserPoolMfaType)
cupMfaConfiguration = Lens.field @"mfaConfiguration"
{-# DEPRECATED cupMfaConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The policies associated with the new user pool.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupPolicies :: Lens.Lens' CreateUserPool (Core.Maybe Types.UserPoolPolicyType)
cupPolicies = Lens.field @"policies"
{-# DEPRECATED cupPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | An array of schema attributes for the new user pool. These attributes can be standard or custom attributes.
--
-- /Note:/ Consider using 'schema' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSchema :: Lens.Lens' CreateUserPool (Core.Maybe (Core.NonEmpty Types.SchemaAttributeType))
cupSchema = Lens.field @"schema"
{-# DEPRECATED cupSchema "Use generic-lens or generic-optics with 'schema' instead." #-}

-- | A string representing the SMS authentication message.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSmsAuthenticationMessage :: Lens.Lens' CreateUserPool (Core.Maybe Types.SmsVerificationMessageType)
cupSmsAuthenticationMessage = Lens.field @"smsAuthenticationMessage"
{-# DEPRECATED cupSmsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead." #-}

-- | The SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSmsConfiguration :: Lens.Lens' CreateUserPool (Core.Maybe Types.SmsConfigurationType)
cupSmsConfiguration = Lens.field @"smsConfiguration"
{-# DEPRECATED cupSmsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead." #-}

-- | A string representing the SMS verification message.
--
-- /Note:/ Consider using 'smsVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupSmsVerificationMessage :: Lens.Lens' CreateUserPool (Core.Maybe Types.SmsVerificationMessageType)
cupSmsVerificationMessage = Lens.field @"smsVerificationMessage"
{-# DEPRECATED cupSmsVerificationMessage "Use generic-lens or generic-optics with 'smsVerificationMessage' instead." #-}

-- | Used to enable advanced security risk detection. Set the key @AdvancedSecurityMode@ to the value "AUDIT".
--
-- /Note:/ Consider using 'userPoolAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserPoolAddOns :: Lens.Lens' CreateUserPool (Core.Maybe Types.UserPoolAddOnsType)
cupUserPoolAddOns = Lens.field @"userPoolAddOns"
{-# DEPRECATED cupUserPoolAddOns "Use generic-lens or generic-optics with 'userPoolAddOns' instead." #-}

-- | The tag keys and values to assign to the user pool. A tag is a label that you can use to categorize and manage user pools in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'userPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUserPoolTags :: Lens.Lens' CreateUserPool (Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType))
cupUserPoolTags = Lens.field @"userPoolTags"
{-# DEPRECATED cupUserPoolTags "Use generic-lens or generic-optics with 'userPoolTags' instead." #-}

-- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
--
-- /Note:/ Consider using 'usernameAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUsernameAttributes :: Lens.Lens' CreateUserPool (Core.Maybe [Types.UsernameAttributeType])
cupUsernameAttributes = Lens.field @"usernameAttributes"
{-# DEPRECATED cupUsernameAttributes "Use generic-lens or generic-optics with 'usernameAttributes' instead." #-}

-- | You can choose to set case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
--
-- /Note:/ Consider using 'usernameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupUsernameConfiguration :: Lens.Lens' CreateUserPool (Core.Maybe Types.UsernameConfigurationType)
cupUsernameConfiguration = Lens.field @"usernameConfiguration"
{-# DEPRECATED cupUsernameConfiguration "Use generic-lens or generic-optics with 'usernameConfiguration' instead." #-}

-- | The template for the verification message that the user sees when the app requests permission to access the user's information.
--
-- /Note:/ Consider using 'verificationMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cupVerificationMessageTemplate :: Lens.Lens' CreateUserPool (Core.Maybe Types.VerificationMessageTemplateType)
cupVerificationMessageTemplate = Lens.field @"verificationMessageTemplate"
{-# DEPRECATED cupVerificationMessageTemplate "Use generic-lens or generic-optics with 'verificationMessageTemplate' instead." #-}

instance Core.FromJSON CreateUserPool where
  toJSON CreateUserPool {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("PoolName" Core..= poolName),
            ("AccountRecoverySetting" Core..=) Core.<$> accountRecoverySetting,
            ("AdminCreateUserConfig" Core..=) Core.<$> adminCreateUserConfig,
            ("AliasAttributes" Core..=) Core.<$> aliasAttributes,
            ("AutoVerifiedAttributes" Core..=) Core.<$> autoVerifiedAttributes,
            ("DeviceConfiguration" Core..=) Core.<$> deviceConfiguration,
            ("EmailConfiguration" Core..=) Core.<$> emailConfiguration,
            ("EmailVerificationMessage" Core..=)
              Core.<$> emailVerificationMessage,
            ("EmailVerificationSubject" Core..=)
              Core.<$> emailVerificationSubject,
            ("LambdaConfig" Core..=) Core.<$> lambdaConfig,
            ("MfaConfiguration" Core..=) Core.<$> mfaConfiguration,
            ("Policies" Core..=) Core.<$> policies,
            ("Schema" Core..=) Core.<$> schema,
            ("SmsAuthenticationMessage" Core..=)
              Core.<$> smsAuthenticationMessage,
            ("SmsConfiguration" Core..=) Core.<$> smsConfiguration,
            ("SmsVerificationMessage" Core..=) Core.<$> smsVerificationMessage,
            ("UserPoolAddOns" Core..=) Core.<$> userPoolAddOns,
            ("UserPoolTags" Core..=) Core.<$> userPoolTags,
            ("UsernameAttributes" Core..=) Core.<$> usernameAttributes,
            ("UsernameConfiguration" Core..=) Core.<$> usernameConfiguration,
            ("VerificationMessageTemplate" Core..=)
              Core.<$> verificationMessageTemplate
          ]
      )

instance Core.AWSRequest CreateUserPool where
  type Rs CreateUserPool = CreateUserPoolResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSCognitoIdentityProviderService.CreateUserPool"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateUserPoolResponse'
            Core.<$> (x Core..:? "UserPool") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Represents the response from the server for the request to create a user pool.
--
-- /See:/ 'mkCreateUserPoolResponse' smart constructor.
data CreateUserPoolResponse = CreateUserPoolResponse'
  { -- | A container for the user pool details.
    userPool :: Core.Maybe Types.UserPoolType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'CreateUserPoolResponse' value with any optional fields omitted.
mkCreateUserPoolResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateUserPoolResponse
mkCreateUserPoolResponse responseStatus =
  CreateUserPoolResponse' {userPool = Core.Nothing, responseStatus}

-- | A container for the user pool details.
--
-- /Note:/ Consider using 'userPool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsUserPool :: Lens.Lens' CreateUserPoolResponse (Core.Maybe Types.UserPoolType)
cuprrsUserPool = Lens.field @"userPool"
{-# DEPRECATED cuprrsUserPool "Use generic-lens or generic-optics with 'userPool' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cuprrsResponseStatus :: Lens.Lens' CreateUserPoolResponse Core.Int
cuprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cuprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
