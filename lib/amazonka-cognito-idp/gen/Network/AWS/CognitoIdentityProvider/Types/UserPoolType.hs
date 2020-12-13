{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolType
  ( UserPoolType (..),

    -- * Smart constructor
    mkUserPoolType,

    -- * Lenses
    uptStatus,
    uptUserPoolTags,
    uptEmailConfigurationFailure,
    uptLastModifiedDate,
    uptVerificationMessageTemplate,
    uptEstimatedNumberOfUsers,
    uptARN,
    uptDomain,
    uptCustomDomain,
    uptEmailVerificationMessage,
    uptSmsAuthenticationMessage,
    uptUserPoolAddOns,
    uptSchemaAttributes,
    uptEmailVerificationSubject,
    uptUsernameAttributes,
    uptAliasAttributes,
    uptAccountRecoverySetting,
    uptEmailConfiguration,
    uptSmsVerificationMessage,
    uptName,
    uptMFAConfiguration,
    uptId,
    uptSmsConfigurationFailure,
    uptCreationDate,
    uptLambdaConfig,
    uptSmsConfiguration,
    uptAdminCreateUserConfig,
    uptDeviceConfiguration,
    uptAutoVerifiedAttributes,
    uptPolicies,
    uptUsernameConfiguration,
  )
where

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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A container for information about the user pool.
--
-- /See:/ 'mkUserPoolType' smart constructor.
data UserPoolType = UserPoolType'
  { -- | The status of a user pool.
    status :: Lude.Maybe StatusType,
    -- | The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
    userPoolTags :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The reason why the email configuration cannot send the messages to your users.
    emailConfigurationFailure :: Lude.Maybe Lude.Text,
    -- | The date the user pool was last modified.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | The template for verification messages.
    verificationMessageTemplate :: Lude.Maybe VerificationMessageTemplateType,
    -- | A number estimating the size of the user pool.
    estimatedNumberOfUsers :: Lude.Maybe Lude.Int,
    -- | The Amazon Resource Name (ARN) for the user pool.
    arn :: Lude.Maybe Lude.Text,
    -- | Holds the domain prefix if the user pool has a domain associated with it.
    domain :: Lude.Maybe Lude.Text,
    -- | A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
    --
    -- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
    customDomain :: Lude.Maybe Lude.Text,
    -- | The contents of the email verification message.
    emailVerificationMessage :: Lude.Maybe Lude.Text,
    -- | The contents of the SMS authentication message.
    smsAuthenticationMessage :: Lude.Maybe Lude.Text,
    -- | The user pool add-ons.
    userPoolAddOns :: Lude.Maybe UserPoolAddOnsType,
    -- | A container with the schema attributes of a user pool.
    schemaAttributes :: Lude.Maybe (Lude.NonEmpty SchemaAttributeType),
    -- | The subject of the email verification message.
    emailVerificationSubject :: Lude.Maybe Lude.Text,
    -- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
    usernameAttributes :: Lude.Maybe [UsernameAttributeType],
    -- | Specifies the attributes that are aliased in a user pool.
    aliasAttributes :: Lude.Maybe [AliasAttributeType],
    -- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
    accountRecoverySetting :: Lude.Maybe AccountRecoverySettingType,
    -- | The email configuration.
    emailConfiguration :: Lude.Maybe EmailConfigurationType,
    -- | The contents of the SMS verification message.
    smsVerificationMessage :: Lude.Maybe Lude.Text,
    -- | The name of the user pool.
    name :: Lude.Maybe Lude.Text,
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
    mfaConfiguration :: Lude.Maybe UserPoolMFAType,
    -- | The ID of the user pool.
    id :: Lude.Maybe Lude.Text,
    -- | The reason why the SMS configuration cannot send the messages to your users.
    smsConfigurationFailure :: Lude.Maybe Lude.Text,
    -- | The date the user pool was created.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The AWS Lambda triggers associated with the user pool.
    lambdaConfig :: Lude.Maybe LambdaConfigType,
    -- | The SMS configuration.
    smsConfiguration :: Lude.Maybe SmsConfigurationType,
    -- | The configuration for @AdminCreateUser@ requests.
    adminCreateUserConfig :: Lude.Maybe AdminCreateUserConfigType,
    -- | The device configuration.
    deviceConfiguration :: Lude.Maybe DeviceConfigurationType,
    -- | Specifies the attributes that are auto-verified in a user pool.
    autoVerifiedAttributes :: Lude.Maybe [VerifiedAttributeType],
    -- | The policies associated with the user pool.
    policies :: Lude.Maybe UserPoolPolicyType,
    -- | You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
    usernameConfiguration :: Lude.Maybe UsernameConfigurationType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPoolType' with the minimum fields required to make a request.
--
-- * 'status' - The status of a user pool.
-- * 'userPoolTags' - The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
-- * 'emailConfigurationFailure' - The reason why the email configuration cannot send the messages to your users.
-- * 'lastModifiedDate' - The date the user pool was last modified.
-- * 'verificationMessageTemplate' - The template for verification messages.
-- * 'estimatedNumberOfUsers' - A number estimating the size of the user pool.
-- * 'arn' - The Amazon Resource Name (ARN) for the user pool.
-- * 'domain' - Holds the domain prefix if the user pool has a domain associated with it.
-- * 'customDomain' - A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
--
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
-- * 'emailVerificationMessage' - The contents of the email verification message.
-- * 'smsAuthenticationMessage' - The contents of the SMS authentication message.
-- * 'userPoolAddOns' - The user pool add-ons.
-- * 'schemaAttributes' - A container with the schema attributes of a user pool.
-- * 'emailVerificationSubject' - The subject of the email verification message.
-- * 'usernameAttributes' - Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
-- * 'aliasAttributes' - Specifies the attributes that are aliased in a user pool.
-- * 'accountRecoverySetting' - Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
-- * 'emailConfiguration' - The email configuration.
-- * 'smsVerificationMessage' - The contents of the SMS verification message.
-- * 'name' - The name of the user pool.
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
-- * 'id' - The ID of the user pool.
-- * 'smsConfigurationFailure' - The reason why the SMS configuration cannot send the messages to your users.
-- * 'creationDate' - The date the user pool was created.
-- * 'lambdaConfig' - The AWS Lambda triggers associated with the user pool.
-- * 'smsConfiguration' - The SMS configuration.
-- * 'adminCreateUserConfig' - The configuration for @AdminCreateUser@ requests.
-- * 'deviceConfiguration' - The device configuration.
-- * 'autoVerifiedAttributes' - Specifies the attributes that are auto-verified in a user pool.
-- * 'policies' - The policies associated with the user pool.
-- * 'usernameConfiguration' - You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
mkUserPoolType ::
  UserPoolType
mkUserPoolType =
  UserPoolType'
    { status = Lude.Nothing,
      userPoolTags = Lude.Nothing,
      emailConfigurationFailure = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      verificationMessageTemplate = Lude.Nothing,
      estimatedNumberOfUsers = Lude.Nothing,
      arn = Lude.Nothing,
      domain = Lude.Nothing,
      customDomain = Lude.Nothing,
      emailVerificationMessage = Lude.Nothing,
      smsAuthenticationMessage = Lude.Nothing,
      userPoolAddOns = Lude.Nothing,
      schemaAttributes = Lude.Nothing,
      emailVerificationSubject = Lude.Nothing,
      usernameAttributes = Lude.Nothing,
      aliasAttributes = Lude.Nothing,
      accountRecoverySetting = Lude.Nothing,
      emailConfiguration = Lude.Nothing,
      smsVerificationMessage = Lude.Nothing,
      name = Lude.Nothing,
      mfaConfiguration = Lude.Nothing,
      id = Lude.Nothing,
      smsConfigurationFailure = Lude.Nothing,
      creationDate = Lude.Nothing,
      lambdaConfig = Lude.Nothing,
      smsConfiguration = Lude.Nothing,
      adminCreateUserConfig = Lude.Nothing,
      deviceConfiguration = Lude.Nothing,
      autoVerifiedAttributes = Lude.Nothing,
      policies = Lude.Nothing,
      usernameConfiguration = Lude.Nothing
    }

-- | The status of a user pool.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptStatus :: Lens.Lens' UserPoolType (Lude.Maybe StatusType)
uptStatus = Lens.lens (status :: UserPoolType -> Lude.Maybe StatusType) (\s a -> s {status = a} :: UserPoolType)
{-# DEPRECATED uptStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'userPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUserPoolTags :: Lens.Lens' UserPoolType (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
uptUserPoolTags = Lens.lens (userPoolTags :: UserPoolType -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {userPoolTags = a} :: UserPoolType)
{-# DEPRECATED uptUserPoolTags "Use generic-lens or generic-optics with 'userPoolTags' instead." #-}

-- | The reason why the email configuration cannot send the messages to your users.
--
-- /Note:/ Consider using 'emailConfigurationFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailConfigurationFailure :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptEmailConfigurationFailure = Lens.lens (emailConfigurationFailure :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {emailConfigurationFailure = a} :: UserPoolType)
{-# DEPRECATED uptEmailConfigurationFailure "Use generic-lens or generic-optics with 'emailConfigurationFailure' instead." #-}

-- | The date the user pool was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptLastModifiedDate :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Timestamp)
uptLastModifiedDate = Lens.lens (lastModifiedDate :: UserPoolType -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: UserPoolType)
{-# DEPRECATED uptLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The template for verification messages.
--
-- /Note:/ Consider using 'verificationMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptVerificationMessageTemplate :: Lens.Lens' UserPoolType (Lude.Maybe VerificationMessageTemplateType)
uptVerificationMessageTemplate = Lens.lens (verificationMessageTemplate :: UserPoolType -> Lude.Maybe VerificationMessageTemplateType) (\s a -> s {verificationMessageTemplate = a} :: UserPoolType)
{-# DEPRECATED uptVerificationMessageTemplate "Use generic-lens or generic-optics with 'verificationMessageTemplate' instead." #-}

-- | A number estimating the size of the user pool.
--
-- /Note:/ Consider using 'estimatedNumberOfUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEstimatedNumberOfUsers :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Int)
uptEstimatedNumberOfUsers = Lens.lens (estimatedNumberOfUsers :: UserPoolType -> Lude.Maybe Lude.Int) (\s a -> s {estimatedNumberOfUsers = a} :: UserPoolType)
{-# DEPRECATED uptEstimatedNumberOfUsers "Use generic-lens or generic-optics with 'estimatedNumberOfUsers' instead." #-}

-- | The Amazon Resource Name (ARN) for the user pool.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptARN :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptARN = Lens.lens (arn :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UserPoolType)
{-# DEPRECATED uptARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Holds the domain prefix if the user pool has a domain associated with it.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDomain :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptDomain = Lens.lens (domain :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {domain = a} :: UserPoolType)
{-# DEPRECATED uptDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
--
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
--
-- /Note:/ Consider using 'customDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptCustomDomain :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptCustomDomain = Lens.lens (customDomain :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {customDomain = a} :: UserPoolType)
{-# DEPRECATED uptCustomDomain "Use generic-lens or generic-optics with 'customDomain' instead." #-}

-- | The contents of the email verification message.
--
-- /Note:/ Consider using 'emailVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailVerificationMessage :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptEmailVerificationMessage = Lens.lens (emailVerificationMessage :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {emailVerificationMessage = a} :: UserPoolType)
{-# DEPRECATED uptEmailVerificationMessage "Use generic-lens or generic-optics with 'emailVerificationMessage' instead." #-}

-- | The contents of the SMS authentication message.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsAuthenticationMessage :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptSmsAuthenticationMessage = Lens.lens (smsAuthenticationMessage :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {smsAuthenticationMessage = a} :: UserPoolType)
{-# DEPRECATED uptSmsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead." #-}

-- | The user pool add-ons.
--
-- /Note:/ Consider using 'userPoolAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUserPoolAddOns :: Lens.Lens' UserPoolType (Lude.Maybe UserPoolAddOnsType)
uptUserPoolAddOns = Lens.lens (userPoolAddOns :: UserPoolType -> Lude.Maybe UserPoolAddOnsType) (\s a -> s {userPoolAddOns = a} :: UserPoolType)
{-# DEPRECATED uptUserPoolAddOns "Use generic-lens or generic-optics with 'userPoolAddOns' instead." #-}

-- | A container with the schema attributes of a user pool.
--
-- /Note:/ Consider using 'schemaAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSchemaAttributes :: Lens.Lens' UserPoolType (Lude.Maybe (Lude.NonEmpty SchemaAttributeType))
uptSchemaAttributes = Lens.lens (schemaAttributes :: UserPoolType -> Lude.Maybe (Lude.NonEmpty SchemaAttributeType)) (\s a -> s {schemaAttributes = a} :: UserPoolType)
{-# DEPRECATED uptSchemaAttributes "Use generic-lens or generic-optics with 'schemaAttributes' instead." #-}

-- | The subject of the email verification message.
--
-- /Note:/ Consider using 'emailVerificationSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailVerificationSubject :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptEmailVerificationSubject = Lens.lens (emailVerificationSubject :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {emailVerificationSubject = a} :: UserPoolType)
{-# DEPRECATED uptEmailVerificationSubject "Use generic-lens or generic-optics with 'emailVerificationSubject' instead." #-}

-- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
--
-- /Note:/ Consider using 'usernameAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUsernameAttributes :: Lens.Lens' UserPoolType (Lude.Maybe [UsernameAttributeType])
uptUsernameAttributes = Lens.lens (usernameAttributes :: UserPoolType -> Lude.Maybe [UsernameAttributeType]) (\s a -> s {usernameAttributes = a} :: UserPoolType)
{-# DEPRECATED uptUsernameAttributes "Use generic-lens or generic-optics with 'usernameAttributes' instead." #-}

-- | Specifies the attributes that are aliased in a user pool.
--
-- /Note:/ Consider using 'aliasAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAliasAttributes :: Lens.Lens' UserPoolType (Lude.Maybe [AliasAttributeType])
uptAliasAttributes = Lens.lens (aliasAttributes :: UserPoolType -> Lude.Maybe [AliasAttributeType]) (\s a -> s {aliasAttributes = a} :: UserPoolType)
{-# DEPRECATED uptAliasAttributes "Use generic-lens or generic-optics with 'aliasAttributes' instead." #-}

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- /Note:/ Consider using 'accountRecoverySetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAccountRecoverySetting :: Lens.Lens' UserPoolType (Lude.Maybe AccountRecoverySettingType)
uptAccountRecoverySetting = Lens.lens (accountRecoverySetting :: UserPoolType -> Lude.Maybe AccountRecoverySettingType) (\s a -> s {accountRecoverySetting = a} :: UserPoolType)
{-# DEPRECATED uptAccountRecoverySetting "Use generic-lens or generic-optics with 'accountRecoverySetting' instead." #-}

-- | The email configuration.
--
-- /Note:/ Consider using 'emailConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailConfiguration :: Lens.Lens' UserPoolType (Lude.Maybe EmailConfigurationType)
uptEmailConfiguration = Lens.lens (emailConfiguration :: UserPoolType -> Lude.Maybe EmailConfigurationType) (\s a -> s {emailConfiguration = a} :: UserPoolType)
{-# DEPRECATED uptEmailConfiguration "Use generic-lens or generic-optics with 'emailConfiguration' instead." #-}

-- | The contents of the SMS verification message.
--
-- /Note:/ Consider using 'smsVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsVerificationMessage :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptSmsVerificationMessage = Lens.lens (smsVerificationMessage :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {smsVerificationMessage = a} :: UserPoolType)
{-# DEPRECATED uptSmsVerificationMessage "Use generic-lens or generic-optics with 'smsVerificationMessage' instead." #-}

-- | The name of the user pool.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptName :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptName = Lens.lens (name :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UserPoolType)
{-# DEPRECATED uptName "Use generic-lens or generic-optics with 'name' instead." #-}

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
uptMFAConfiguration :: Lens.Lens' UserPoolType (Lude.Maybe UserPoolMFAType)
uptMFAConfiguration = Lens.lens (mfaConfiguration :: UserPoolType -> Lude.Maybe UserPoolMFAType) (\s a -> s {mfaConfiguration = a} :: UserPoolType)
{-# DEPRECATED uptMFAConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead." #-}

-- | The ID of the user pool.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptId :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptId = Lens.lens (id :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: UserPoolType)
{-# DEPRECATED uptId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The reason why the SMS configuration cannot send the messages to your users.
--
-- /Note:/ Consider using 'smsConfigurationFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsConfigurationFailure :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Text)
uptSmsConfigurationFailure = Lens.lens (smsConfigurationFailure :: UserPoolType -> Lude.Maybe Lude.Text) (\s a -> s {smsConfigurationFailure = a} :: UserPoolType)
{-# DEPRECATED uptSmsConfigurationFailure "Use generic-lens or generic-optics with 'smsConfigurationFailure' instead." #-}

-- | The date the user pool was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptCreationDate :: Lens.Lens' UserPoolType (Lude.Maybe Lude.Timestamp)
uptCreationDate = Lens.lens (creationDate :: UserPoolType -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: UserPoolType)
{-# DEPRECATED uptCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The AWS Lambda triggers associated with the user pool.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptLambdaConfig :: Lens.Lens' UserPoolType (Lude.Maybe LambdaConfigType)
uptLambdaConfig = Lens.lens (lambdaConfig :: UserPoolType -> Lude.Maybe LambdaConfigType) (\s a -> s {lambdaConfig = a} :: UserPoolType)
{-# DEPRECATED uptLambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead." #-}

-- | The SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsConfiguration :: Lens.Lens' UserPoolType (Lude.Maybe SmsConfigurationType)
uptSmsConfiguration = Lens.lens (smsConfiguration :: UserPoolType -> Lude.Maybe SmsConfigurationType) (\s a -> s {smsConfiguration = a} :: UserPoolType)
{-# DEPRECATED uptSmsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead." #-}

-- | The configuration for @AdminCreateUser@ requests.
--
-- /Note:/ Consider using 'adminCreateUserConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAdminCreateUserConfig :: Lens.Lens' UserPoolType (Lude.Maybe AdminCreateUserConfigType)
uptAdminCreateUserConfig = Lens.lens (adminCreateUserConfig :: UserPoolType -> Lude.Maybe AdminCreateUserConfigType) (\s a -> s {adminCreateUserConfig = a} :: UserPoolType)
{-# DEPRECATED uptAdminCreateUserConfig "Use generic-lens or generic-optics with 'adminCreateUserConfig' instead." #-}

-- | The device configuration.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDeviceConfiguration :: Lens.Lens' UserPoolType (Lude.Maybe DeviceConfigurationType)
uptDeviceConfiguration = Lens.lens (deviceConfiguration :: UserPoolType -> Lude.Maybe DeviceConfigurationType) (\s a -> s {deviceConfiguration = a} :: UserPoolType)
{-# DEPRECATED uptDeviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead." #-}

-- | Specifies the attributes that are auto-verified in a user pool.
--
-- /Note:/ Consider using 'autoVerifiedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAutoVerifiedAttributes :: Lens.Lens' UserPoolType (Lude.Maybe [VerifiedAttributeType])
uptAutoVerifiedAttributes = Lens.lens (autoVerifiedAttributes :: UserPoolType -> Lude.Maybe [VerifiedAttributeType]) (\s a -> s {autoVerifiedAttributes = a} :: UserPoolType)
{-# DEPRECATED uptAutoVerifiedAttributes "Use generic-lens or generic-optics with 'autoVerifiedAttributes' instead." #-}

-- | The policies associated with the user pool.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptPolicies :: Lens.Lens' UserPoolType (Lude.Maybe UserPoolPolicyType)
uptPolicies = Lens.lens (policies :: UserPoolType -> Lude.Maybe UserPoolPolicyType) (\s a -> s {policies = a} :: UserPoolType)
{-# DEPRECATED uptPolicies "Use generic-lens or generic-optics with 'policies' instead." #-}

-- | You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
--
-- /Note:/ Consider using 'usernameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUsernameConfiguration :: Lens.Lens' UserPoolType (Lude.Maybe UsernameConfigurationType)
uptUsernameConfiguration = Lens.lens (usernameConfiguration :: UserPoolType -> Lude.Maybe UsernameConfigurationType) (\s a -> s {usernameConfiguration = a} :: UserPoolType)
{-# DEPRECATED uptUsernameConfiguration "Use generic-lens or generic-optics with 'usernameConfiguration' instead." #-}

instance Lude.FromJSON UserPoolType where
  parseJSON =
    Lude.withObject
      "UserPoolType"
      ( \x ->
          UserPoolType'
            Lude.<$> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "UserPoolTags" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EmailConfigurationFailure")
            Lude.<*> (x Lude..:? "LastModifiedDate")
            Lude.<*> (x Lude..:? "VerificationMessageTemplate")
            Lude.<*> (x Lude..:? "EstimatedNumberOfUsers")
            Lude.<*> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Domain")
            Lude.<*> (x Lude..:? "CustomDomain")
            Lude.<*> (x Lude..:? "EmailVerificationMessage")
            Lude.<*> (x Lude..:? "SmsAuthenticationMessage")
            Lude.<*> (x Lude..:? "UserPoolAddOns")
            Lude.<*> (x Lude..:? "SchemaAttributes")
            Lude.<*> (x Lude..:? "EmailVerificationSubject")
            Lude.<*> (x Lude..:? "UsernameAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AliasAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "AccountRecoverySetting")
            Lude.<*> (x Lude..:? "EmailConfiguration")
            Lude.<*> (x Lude..:? "SmsVerificationMessage")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "MfaConfiguration")
            Lude.<*> (x Lude..:? "Id")
            Lude.<*> (x Lude..:? "SmsConfigurationFailure")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "LambdaConfig")
            Lude.<*> (x Lude..:? "SmsConfiguration")
            Lude.<*> (x Lude..:? "AdminCreateUserConfig")
            Lude.<*> (x Lude..:? "DeviceConfiguration")
            Lude.<*> (x Lude..:? "AutoVerifiedAttributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Policies")
            Lude.<*> (x Lude..:? "UsernameConfiguration")
      )
