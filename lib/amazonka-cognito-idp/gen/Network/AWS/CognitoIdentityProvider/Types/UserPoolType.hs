{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UserPoolType
  ( UserPoolType (..)
  -- * Smart constructor
  , mkUserPoolType
  -- * Lenses
  , uptAccountRecoverySetting
  , uptAdminCreateUserConfig
  , uptAliasAttributes
  , uptArn
  , uptAutoVerifiedAttributes
  , uptCreationDate
  , uptCustomDomain
  , uptDeviceConfiguration
  , uptDomain
  , uptEmailConfiguration
  , uptEmailConfigurationFailure
  , uptEmailVerificationMessage
  , uptEmailVerificationSubject
  , uptEstimatedNumberOfUsers
  , uptId
  , uptLambdaConfig
  , uptLastModifiedDate
  , uptMfaConfiguration
  , uptName
  , uptPolicies
  , uptSchemaAttributes
  , uptSmsAuthenticationMessage
  , uptSmsConfiguration
  , uptSmsConfigurationFailure
  , uptSmsVerificationMessage
  , uptStatus
  , uptUserPoolAddOns
  , uptUserPoolTags
  , uptUsernameAttributes
  , uptUsernameConfiguration
  , uptVerificationMessageTemplate
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.AccountRecoverySettingType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.AdminCreateUserConfigType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.AliasAttributeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Arn as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.CustomDomain as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.DeviceConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Domain as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationFailure as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailVerificationMessage as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailVerificationSubject as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Id as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Name as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SchemaAttributeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationFailure as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SmsConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SmsVerificationMessageType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.StatusType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.TagKeysType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.TagValueType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolAddOnsType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolMfaType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UsernameAttributeType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.VerifiedAttributeType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A container for information about the user pool.
--
-- /See:/ 'mkUserPoolType' smart constructor.
data UserPoolType = UserPoolType'
  { accountRecoverySetting :: Core.Maybe Types.AccountRecoverySettingType
    -- ^ Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
  , adminCreateUserConfig :: Core.Maybe Types.AdminCreateUserConfigType
    -- ^ The configuration for @AdminCreateUser@ requests.
  , aliasAttributes :: Core.Maybe [Types.AliasAttributeType]
    -- ^ Specifies the attributes that are aliased in a user pool.
  , arn :: Core.Maybe Types.Arn
    -- ^ The Amazon Resource Name (ARN) for the user pool.
  , autoVerifiedAttributes :: Core.Maybe [Types.VerifiedAttributeType]
    -- ^ Specifies the attributes that are auto-verified in a user pool.
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the user pool was created.
  , customDomain :: Core.Maybe Types.CustomDomain
    -- ^ A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
--
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
  , deviceConfiguration :: Core.Maybe Types.DeviceConfigurationType
    -- ^ The device configuration.
  , domain :: Core.Maybe Types.Domain
    -- ^ Holds the domain prefix if the user pool has a domain associated with it.
  , emailConfiguration :: Core.Maybe Types.EmailConfigurationType
    -- ^ The email configuration.
  , emailConfigurationFailure :: Core.Maybe Types.EmailConfigurationFailure
    -- ^ The reason why the email configuration cannot send the messages to your users.
  , emailVerificationMessage :: Core.Maybe Types.EmailVerificationMessage
    -- ^ The contents of the email verification message.
  , emailVerificationSubject :: Core.Maybe Types.EmailVerificationSubject
    -- ^ The subject of the email verification message.
  , estimatedNumberOfUsers :: Core.Maybe Core.Int
    -- ^ A number estimating the size of the user pool.
  , id :: Core.Maybe Types.Id
    -- ^ The ID of the user pool.
  , lambdaConfig :: Core.Maybe Types.LambdaConfigType
    -- ^ The AWS Lambda triggers associated with the user pool.
  , lastModifiedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the user pool was last modified.
  , mfaConfiguration :: Core.Maybe Types.UserPoolMfaType
    -- ^ Can be one of the following values:
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
  , name :: Core.Maybe Types.Name
    -- ^ The name of the user pool.
  , policies :: Core.Maybe Types.UserPoolPolicyType
    -- ^ The policies associated with the user pool.
  , schemaAttributes :: Core.Maybe (Core.NonEmpty Types.SchemaAttributeType)
    -- ^ A container with the schema attributes of a user pool.
  , smsAuthenticationMessage :: Core.Maybe Types.SmsVerificationMessageType
    -- ^ The contents of the SMS authentication message.
  , smsConfiguration :: Core.Maybe Types.SmsConfigurationType
    -- ^ The SMS configuration.
  , smsConfigurationFailure :: Core.Maybe Types.SmsConfigurationFailure
    -- ^ The reason why the SMS configuration cannot send the messages to your users.
  , smsVerificationMessage :: Core.Maybe Types.SmsVerificationMessageType
    -- ^ The contents of the SMS verification message.
  , status :: Core.Maybe Types.StatusType
    -- ^ The status of a user pool.
  , userPoolAddOns :: Core.Maybe Types.UserPoolAddOnsType
    -- ^ The user pool add-ons.
  , userPoolTags :: Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType)
    -- ^ The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
  , usernameAttributes :: Core.Maybe [Types.UsernameAttributeType]
    -- ^ Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
  , usernameConfiguration :: Core.Maybe Types.UsernameConfigurationType
    -- ^ You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
  , verificationMessageTemplate :: Core.Maybe Types.VerificationMessageTemplateType
    -- ^ The template for verification messages.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'UserPoolType' value with any optional fields omitted.
mkUserPoolType
    :: UserPoolType
mkUserPoolType
  = UserPoolType'{accountRecoverySetting = Core.Nothing,
                  adminCreateUserConfig = Core.Nothing,
                  aliasAttributes = Core.Nothing, arn = Core.Nothing,
                  autoVerifiedAttributes = Core.Nothing, creationDate = Core.Nothing,
                  customDomain = Core.Nothing, deviceConfiguration = Core.Nothing,
                  domain = Core.Nothing, emailConfiguration = Core.Nothing,
                  emailConfigurationFailure = Core.Nothing,
                  emailVerificationMessage = Core.Nothing,
                  emailVerificationSubject = Core.Nothing,
                  estimatedNumberOfUsers = Core.Nothing, id = Core.Nothing,
                  lambdaConfig = Core.Nothing, lastModifiedDate = Core.Nothing,
                  mfaConfiguration = Core.Nothing, name = Core.Nothing,
                  policies = Core.Nothing, schemaAttributes = Core.Nothing,
                  smsAuthenticationMessage = Core.Nothing,
                  smsConfiguration = Core.Nothing,
                  smsConfigurationFailure = Core.Nothing,
                  smsVerificationMessage = Core.Nothing, status = Core.Nothing,
                  userPoolAddOns = Core.Nothing, userPoolTags = Core.Nothing,
                  usernameAttributes = Core.Nothing,
                  usernameConfiguration = Core.Nothing,
                  verificationMessageTemplate = Core.Nothing}

-- | Use this setting to define which verified available method a user can use to recover their password when they call @ForgotPassword@ . It allows you to define a preferred method when a user has more than one method available. With this setting, SMS does not qualify for a valid password recovery mechanism if the user also has SMS MFA enabled. In the absence of this setting, Cognito uses the legacy behavior to determine the recovery method where SMS is preferred over email.
--
-- /Note:/ Consider using 'accountRecoverySetting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAccountRecoverySetting :: Lens.Lens' UserPoolType (Core.Maybe Types.AccountRecoverySettingType)
uptAccountRecoverySetting = Lens.field @"accountRecoverySetting"
{-# INLINEABLE uptAccountRecoverySetting #-}
{-# DEPRECATED accountRecoverySetting "Use generic-lens or generic-optics with 'accountRecoverySetting' instead"  #-}

-- | The configuration for @AdminCreateUser@ requests.
--
-- /Note:/ Consider using 'adminCreateUserConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAdminCreateUserConfig :: Lens.Lens' UserPoolType (Core.Maybe Types.AdminCreateUserConfigType)
uptAdminCreateUserConfig = Lens.field @"adminCreateUserConfig"
{-# INLINEABLE uptAdminCreateUserConfig #-}
{-# DEPRECATED adminCreateUserConfig "Use generic-lens or generic-optics with 'adminCreateUserConfig' instead"  #-}

-- | Specifies the attributes that are aliased in a user pool.
--
-- /Note:/ Consider using 'aliasAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAliasAttributes :: Lens.Lens' UserPoolType (Core.Maybe [Types.AliasAttributeType])
uptAliasAttributes = Lens.field @"aliasAttributes"
{-# INLINEABLE uptAliasAttributes #-}
{-# DEPRECATED aliasAttributes "Use generic-lens or generic-optics with 'aliasAttributes' instead"  #-}

-- | The Amazon Resource Name (ARN) for the user pool.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptArn :: Lens.Lens' UserPoolType (Core.Maybe Types.Arn)
uptArn = Lens.field @"arn"
{-# INLINEABLE uptArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | Specifies the attributes that are auto-verified in a user pool.
--
-- /Note:/ Consider using 'autoVerifiedAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptAutoVerifiedAttributes :: Lens.Lens' UserPoolType (Core.Maybe [Types.VerifiedAttributeType])
uptAutoVerifiedAttributes = Lens.field @"autoVerifiedAttributes"
{-# INLINEABLE uptAutoVerifiedAttributes #-}
{-# DEPRECATED autoVerifiedAttributes "Use generic-lens or generic-optics with 'autoVerifiedAttributes' instead"  #-}

-- | The date the user pool was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptCreationDate :: Lens.Lens' UserPoolType (Core.Maybe Core.NominalDiffTime)
uptCreationDate = Lens.field @"creationDate"
{-# INLINEABLE uptCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | A custom domain name that you provide to Amazon Cognito. This parameter applies only if you use a custom domain to host the sign-up and sign-in pages for your application. For example: @auth.example.com@ .
--
-- For more information about adding a custom domain to your user pool, see <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-pools-add-custom-domain.html Using Your Own Domain for the Hosted UI> .
--
-- /Note:/ Consider using 'customDomain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptCustomDomain :: Lens.Lens' UserPoolType (Core.Maybe Types.CustomDomain)
uptCustomDomain = Lens.field @"customDomain"
{-# INLINEABLE uptCustomDomain #-}
{-# DEPRECATED customDomain "Use generic-lens or generic-optics with 'customDomain' instead"  #-}

-- | The device configuration.
--
-- /Note:/ Consider using 'deviceConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDeviceConfiguration :: Lens.Lens' UserPoolType (Core.Maybe Types.DeviceConfigurationType)
uptDeviceConfiguration = Lens.field @"deviceConfiguration"
{-# INLINEABLE uptDeviceConfiguration #-}
{-# DEPRECATED deviceConfiguration "Use generic-lens or generic-optics with 'deviceConfiguration' instead"  #-}

-- | Holds the domain prefix if the user pool has a domain associated with it.
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptDomain :: Lens.Lens' UserPoolType (Core.Maybe Types.Domain)
uptDomain = Lens.field @"domain"
{-# INLINEABLE uptDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The email configuration.
--
-- /Note:/ Consider using 'emailConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailConfiguration :: Lens.Lens' UserPoolType (Core.Maybe Types.EmailConfigurationType)
uptEmailConfiguration = Lens.field @"emailConfiguration"
{-# INLINEABLE uptEmailConfiguration #-}
{-# DEPRECATED emailConfiguration "Use generic-lens or generic-optics with 'emailConfiguration' instead"  #-}

-- | The reason why the email configuration cannot send the messages to your users.
--
-- /Note:/ Consider using 'emailConfigurationFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailConfigurationFailure :: Lens.Lens' UserPoolType (Core.Maybe Types.EmailConfigurationFailure)
uptEmailConfigurationFailure = Lens.field @"emailConfigurationFailure"
{-# INLINEABLE uptEmailConfigurationFailure #-}
{-# DEPRECATED emailConfigurationFailure "Use generic-lens or generic-optics with 'emailConfigurationFailure' instead"  #-}

-- | The contents of the email verification message.
--
-- /Note:/ Consider using 'emailVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailVerificationMessage :: Lens.Lens' UserPoolType (Core.Maybe Types.EmailVerificationMessage)
uptEmailVerificationMessage = Lens.field @"emailVerificationMessage"
{-# INLINEABLE uptEmailVerificationMessage #-}
{-# DEPRECATED emailVerificationMessage "Use generic-lens or generic-optics with 'emailVerificationMessage' instead"  #-}

-- | The subject of the email verification message.
--
-- /Note:/ Consider using 'emailVerificationSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEmailVerificationSubject :: Lens.Lens' UserPoolType (Core.Maybe Types.EmailVerificationSubject)
uptEmailVerificationSubject = Lens.field @"emailVerificationSubject"
{-# INLINEABLE uptEmailVerificationSubject #-}
{-# DEPRECATED emailVerificationSubject "Use generic-lens or generic-optics with 'emailVerificationSubject' instead"  #-}

-- | A number estimating the size of the user pool.
--
-- /Note:/ Consider using 'estimatedNumberOfUsers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptEstimatedNumberOfUsers :: Lens.Lens' UserPoolType (Core.Maybe Core.Int)
uptEstimatedNumberOfUsers = Lens.field @"estimatedNumberOfUsers"
{-# INLINEABLE uptEstimatedNumberOfUsers #-}
{-# DEPRECATED estimatedNumberOfUsers "Use generic-lens or generic-optics with 'estimatedNumberOfUsers' instead"  #-}

-- | The ID of the user pool.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptId :: Lens.Lens' UserPoolType (Core.Maybe Types.Id)
uptId = Lens.field @"id"
{-# INLINEABLE uptId #-}
{-# DEPRECATED id "Use generic-lens or generic-optics with 'id' instead"  #-}

-- | The AWS Lambda triggers associated with the user pool.
--
-- /Note:/ Consider using 'lambdaConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptLambdaConfig :: Lens.Lens' UserPoolType (Core.Maybe Types.LambdaConfigType)
uptLambdaConfig = Lens.field @"lambdaConfig"
{-# INLINEABLE uptLambdaConfig #-}
{-# DEPRECATED lambdaConfig "Use generic-lens or generic-optics with 'lambdaConfig' instead"  #-}

-- | The date the user pool was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptLastModifiedDate :: Lens.Lens' UserPoolType (Core.Maybe Core.NominalDiffTime)
uptLastModifiedDate = Lens.field @"lastModifiedDate"
{-# INLINEABLE uptLastModifiedDate #-}
{-# DEPRECATED lastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead"  #-}

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
uptMfaConfiguration :: Lens.Lens' UserPoolType (Core.Maybe Types.UserPoolMfaType)
uptMfaConfiguration = Lens.field @"mfaConfiguration"
{-# INLINEABLE uptMfaConfiguration #-}
{-# DEPRECATED mfaConfiguration "Use generic-lens or generic-optics with 'mfaConfiguration' instead"  #-}

-- | The name of the user pool.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptName :: Lens.Lens' UserPoolType (Core.Maybe Types.Name)
uptName = Lens.field @"name"
{-# INLINEABLE uptName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The policies associated with the user pool.
--
-- /Note:/ Consider using 'policies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptPolicies :: Lens.Lens' UserPoolType (Core.Maybe Types.UserPoolPolicyType)
uptPolicies = Lens.field @"policies"
{-# INLINEABLE uptPolicies #-}
{-# DEPRECATED policies "Use generic-lens or generic-optics with 'policies' instead"  #-}

-- | A container with the schema attributes of a user pool.
--
-- /Note:/ Consider using 'schemaAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSchemaAttributes :: Lens.Lens' UserPoolType (Core.Maybe (Core.NonEmpty Types.SchemaAttributeType))
uptSchemaAttributes = Lens.field @"schemaAttributes"
{-# INLINEABLE uptSchemaAttributes #-}
{-# DEPRECATED schemaAttributes "Use generic-lens or generic-optics with 'schemaAttributes' instead"  #-}

-- | The contents of the SMS authentication message.
--
-- /Note:/ Consider using 'smsAuthenticationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsAuthenticationMessage :: Lens.Lens' UserPoolType (Core.Maybe Types.SmsVerificationMessageType)
uptSmsAuthenticationMessage = Lens.field @"smsAuthenticationMessage"
{-# INLINEABLE uptSmsAuthenticationMessage #-}
{-# DEPRECATED smsAuthenticationMessage "Use generic-lens or generic-optics with 'smsAuthenticationMessage' instead"  #-}

-- | The SMS configuration.
--
-- /Note:/ Consider using 'smsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsConfiguration :: Lens.Lens' UserPoolType (Core.Maybe Types.SmsConfigurationType)
uptSmsConfiguration = Lens.field @"smsConfiguration"
{-# INLINEABLE uptSmsConfiguration #-}
{-# DEPRECATED smsConfiguration "Use generic-lens or generic-optics with 'smsConfiguration' instead"  #-}

-- | The reason why the SMS configuration cannot send the messages to your users.
--
-- /Note:/ Consider using 'smsConfigurationFailure' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsConfigurationFailure :: Lens.Lens' UserPoolType (Core.Maybe Types.SmsConfigurationFailure)
uptSmsConfigurationFailure = Lens.field @"smsConfigurationFailure"
{-# INLINEABLE uptSmsConfigurationFailure #-}
{-# DEPRECATED smsConfigurationFailure "Use generic-lens or generic-optics with 'smsConfigurationFailure' instead"  #-}

-- | The contents of the SMS verification message.
--
-- /Note:/ Consider using 'smsVerificationMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptSmsVerificationMessage :: Lens.Lens' UserPoolType (Core.Maybe Types.SmsVerificationMessageType)
uptSmsVerificationMessage = Lens.field @"smsVerificationMessage"
{-# INLINEABLE uptSmsVerificationMessage #-}
{-# DEPRECATED smsVerificationMessage "Use generic-lens or generic-optics with 'smsVerificationMessage' instead"  #-}

-- | The status of a user pool.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptStatus :: Lens.Lens' UserPoolType (Core.Maybe Types.StatusType)
uptStatus = Lens.field @"status"
{-# INLINEABLE uptStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The user pool add-ons.
--
-- /Note:/ Consider using 'userPoolAddOns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUserPoolAddOns :: Lens.Lens' UserPoolType (Core.Maybe Types.UserPoolAddOnsType)
uptUserPoolAddOns = Lens.field @"userPoolAddOns"
{-# INLINEABLE uptUserPoolAddOns #-}
{-# DEPRECATED userPoolAddOns "Use generic-lens or generic-optics with 'userPoolAddOns' instead"  #-}

-- | The tags that are assigned to the user pool. A tag is a label that you can apply to user pools to categorize and manage them in different ways, such as by purpose, owner, environment, or other criteria.
--
-- /Note:/ Consider using 'userPoolTags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUserPoolTags :: Lens.Lens' UserPoolType (Core.Maybe (Core.HashMap Types.TagKeysType Types.TagValueType))
uptUserPoolTags = Lens.field @"userPoolTags"
{-# INLINEABLE uptUserPoolTags #-}
{-# DEPRECATED userPoolTags "Use generic-lens or generic-optics with 'userPoolTags' instead"  #-}

-- | Specifies whether email addresses or phone numbers can be specified as usernames when a user signs up.
--
-- /Note:/ Consider using 'usernameAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUsernameAttributes :: Lens.Lens' UserPoolType (Core.Maybe [Types.UsernameAttributeType])
uptUsernameAttributes = Lens.field @"usernameAttributes"
{-# INLINEABLE uptUsernameAttributes #-}
{-# DEPRECATED usernameAttributes "Use generic-lens or generic-optics with 'usernameAttributes' instead"  #-}

-- | You can choose to enable case sensitivity on the username input for the selected sign-in option. For example, when this is set to @False@ , users will be able to sign in using either "username" or "Username". This configuration is immutable once it has been set. For more information, see <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_UsernameConfigurationType.html UsernameConfigurationType> .
--
-- /Note:/ Consider using 'usernameConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptUsernameConfiguration :: Lens.Lens' UserPoolType (Core.Maybe Types.UsernameConfigurationType)
uptUsernameConfiguration = Lens.field @"usernameConfiguration"
{-# INLINEABLE uptUsernameConfiguration #-}
{-# DEPRECATED usernameConfiguration "Use generic-lens or generic-optics with 'usernameConfiguration' instead"  #-}

-- | The template for verification messages.
--
-- /Note:/ Consider using 'verificationMessageTemplate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uptVerificationMessageTemplate :: Lens.Lens' UserPoolType (Core.Maybe Types.VerificationMessageTemplateType)
uptVerificationMessageTemplate = Lens.field @"verificationMessageTemplate"
{-# INLINEABLE uptVerificationMessageTemplate #-}
{-# DEPRECATED verificationMessageTemplate "Use generic-lens or generic-optics with 'verificationMessageTemplate' instead"  #-}

instance Core.FromJSON UserPoolType where
        parseJSON
          = Core.withObject "UserPoolType" Core.$
              \ x ->
                UserPoolType' Core.<$>
                  (x Core..:? "AccountRecoverySetting") Core.<*>
                    x Core..:? "AdminCreateUserConfig"
                    Core.<*> x Core..:? "AliasAttributes"
                    Core.<*> x Core..:? "Arn"
                    Core.<*> x Core..:? "AutoVerifiedAttributes"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "CustomDomain"
                    Core.<*> x Core..:? "DeviceConfiguration"
                    Core.<*> x Core..:? "Domain"
                    Core.<*> x Core..:? "EmailConfiguration"
                    Core.<*> x Core..:? "EmailConfigurationFailure"
                    Core.<*> x Core..:? "EmailVerificationMessage"
                    Core.<*> x Core..:? "EmailVerificationSubject"
                    Core.<*> x Core..:? "EstimatedNumberOfUsers"
                    Core.<*> x Core..:? "Id"
                    Core.<*> x Core..:? "LambdaConfig"
                    Core.<*> x Core..:? "LastModifiedDate"
                    Core.<*> x Core..:? "MfaConfiguration"
                    Core.<*> x Core..:? "Name"
                    Core.<*> x Core..:? "Policies"
                    Core.<*> x Core..:? "SchemaAttributes"
                    Core.<*> x Core..:? "SmsAuthenticationMessage"
                    Core.<*> x Core..:? "SmsConfiguration"
                    Core.<*> x Core..:? "SmsConfigurationFailure"
                    Core.<*> x Core..:? "SmsVerificationMessage"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "UserPoolAddOns"
                    Core.<*> x Core..:? "UserPoolTags"
                    Core.<*> x Core..:? "UsernameAttributes"
                    Core.<*> x Core..:? "UsernameConfiguration"
                    Core.<*> x Core..:? "VerificationMessageTemplate"
