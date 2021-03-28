{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
  ( EmailConfigurationType (..)
  -- * Smart constructor
  , mkEmailConfigurationType
  -- * Lenses
  , ectConfigurationSet
  , ectEmailSendingAccount
  , ectFrom
  , ectReplyToEmailAddress
  , ectSourceArn
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.ConfigurationSet as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.From as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.ReplyToEmailAddress as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SourceArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The email configuration type. 
--
-- /See:/ 'mkEmailConfigurationType' smart constructor.
data EmailConfigurationType = EmailConfigurationType'
  { configurationSet :: Core.Maybe Types.ConfigurationSet
    -- ^ The set of configuration rules that can be applied to emails sent using Amazon SES. A configuration set is applied to an email by including a reference to the configuration set in the headers of the email. Once applied, all of the rules in that configuration set are applied to the email. Configuration sets can be used to apply the following types of rules to emails: 
--
--
--     * Event publishing – Amazon SES can track the number of send, delivery, open, click, bounce, and complaint events for each email sent. Use event publishing to send information about these events to other AWS services such as SNS and CloudWatch.
--
--
--     * IP pool management – When leasing dedicated IP addresses with Amazon SES, you can create groups of IP addresses, called dedicated IP pools. You can then associate the dedicated IP pools with configuration sets.
--
--
  , emailSendingAccount :: Core.Maybe Types.EmailSendingAccountType
    -- ^ Specifies whether Amazon Cognito emails your users by using its built-in email functionality or your Amazon SES email configuration. Specify one of the following values:
--
--
--     * COGNITO_DEFAULT
--
--     * When Amazon Cognito emails your users, it uses its built-in email functionality. When you use the default option, Amazon Cognito allows only a limited number of emails each day for your user pool. For typical production environments, the default email limit is below the required delivery volume. To achieve a higher delivery volume, specify DEVELOPER to use your Amazon SES email configuration.
-- To look up the email delivery limit for the default option, see <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito> in the /Amazon Cognito Developer Guide/ .
-- The default FROM address is no-reply@verificationemail.com. To customize the FROM address, provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter.
-- If EmailSendingAccount is COGNITO_DEFAULT, the following parameters aren't allowed:
--
--     * EmailVerificationMessage
--
--
--     * EmailVerificationSubject
--
--
--     * InviteMessageTemplate.EmailMessage
--
--
--     * InviteMessageTemplate.EmailSubject
--
--
--     * VerificationMessageTemplate.EmailMessage
--
--
--     * VerificationMessageTemplate.EmailMessageByLink
--
--
--     * VerificationMessageTemplate.EmailSubject,
--
--
--     * VerificationMessageTemplate.EmailSubjectByLink
--
--
--
--
--     * DEVELOPER
--
--     * When Amazon Cognito emails your users, it uses your Amazon SES configuration. Amazon Cognito calls Amazon SES on your behalf to send email from your verified email address. When you use this option, the email delivery limits are the same limits that apply to your Amazon SES verified email address in your AWS account.
-- If you use this option, you must provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter.
-- Before Amazon Cognito can email your users, it requires additional permissions to call Amazon SES on your behalf. When you update your user pool with this option, Amazon Cognito creates a /service-linked role/ , which is a type of IAM role, in your AWS account. This role contains the permissions that allow Amazon Cognito to access Amazon SES and send email messages with your address. For more information about the service-linked role that Amazon Cognito creates, see <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito> in the /Amazon Cognito Developer Guide/ .
--
--
  , from :: Core.Maybe Types.From
    -- ^ Identifies either the sender’s email address or the sender’s name with their email address. For example, @testuser@example.com@ or @Test User <testuser@example.com>@ . This address will appear before the body of the email.
  , replyToEmailAddress :: Core.Maybe Types.ReplyToEmailAddress
    -- ^ The destination to which the receiver of the email should reply to.
  , sourceArn :: Core.Maybe Types.SourceArn
    -- ^ The Amazon Resource Name (ARN) of a verified email address in Amazon SES. This email address is used in one of the following ways, depending on the value that you specify for the @EmailSendingAccount@ parameter:
--
--
--     * If you specify @COGNITO_DEFAULT@ , Amazon Cognito uses this address as the custom FROM address when it emails your users by using its built-in email account.
--
--
--     * If you specify @DEVELOPER@ , Amazon Cognito emails your users with this address by calling Amazon SES on your behalf.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailConfigurationType' value with any optional fields omitted.
mkEmailConfigurationType
    :: EmailConfigurationType
mkEmailConfigurationType
  = EmailConfigurationType'{configurationSet = Core.Nothing,
                            emailSendingAccount = Core.Nothing, from = Core.Nothing,
                            replyToEmailAddress = Core.Nothing, sourceArn = Core.Nothing}

-- | The set of configuration rules that can be applied to emails sent using Amazon SES. A configuration set is applied to an email by including a reference to the configuration set in the headers of the email. Once applied, all of the rules in that configuration set are applied to the email. Configuration sets can be used to apply the following types of rules to emails: 
--
--
--     * Event publishing – Amazon SES can track the number of send, delivery, open, click, bounce, and complaint events for each email sent. Use event publishing to send information about these events to other AWS services such as SNS and CloudWatch.
--
--
--     * IP pool management – When leasing dedicated IP addresses with Amazon SES, you can create groups of IP addresses, called dedicated IP pools. You can then associate the dedicated IP pools with configuration sets.
--
--
--
-- /Note:/ Consider using 'configurationSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectConfigurationSet :: Lens.Lens' EmailConfigurationType (Core.Maybe Types.ConfigurationSet)
ectConfigurationSet = Lens.field @"configurationSet"
{-# INLINEABLE ectConfigurationSet #-}
{-# DEPRECATED configurationSet "Use generic-lens or generic-optics with 'configurationSet' instead"  #-}

-- | Specifies whether Amazon Cognito emails your users by using its built-in email functionality or your Amazon SES email configuration. Specify one of the following values:
--
--
--     * COGNITO_DEFAULT
--
--     * When Amazon Cognito emails your users, it uses its built-in email functionality. When you use the default option, Amazon Cognito allows only a limited number of emails each day for your user pool. For typical production environments, the default email limit is below the required delivery volume. To achieve a higher delivery volume, specify DEVELOPER to use your Amazon SES email configuration.
-- To look up the email delivery limit for the default option, see <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito> in the /Amazon Cognito Developer Guide/ .
-- The default FROM address is no-reply@verificationemail.com. To customize the FROM address, provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter.
-- If EmailSendingAccount is COGNITO_DEFAULT, the following parameters aren't allowed:
--
--     * EmailVerificationMessage
--
--
--     * EmailVerificationSubject
--
--
--     * InviteMessageTemplate.EmailMessage
--
--
--     * InviteMessageTemplate.EmailSubject
--
--
--     * VerificationMessageTemplate.EmailMessage
--
--
--     * VerificationMessageTemplate.EmailMessageByLink
--
--
--     * VerificationMessageTemplate.EmailSubject,
--
--
--     * VerificationMessageTemplate.EmailSubjectByLink
--
--
--
--
--     * DEVELOPER
--
--     * When Amazon Cognito emails your users, it uses your Amazon SES configuration. Amazon Cognito calls Amazon SES on your behalf to send email from your verified email address. When you use this option, the email delivery limits are the same limits that apply to your Amazon SES verified email address in your AWS account.
-- If you use this option, you must provide the ARN of an Amazon SES verified email address for the @SourceArn@ parameter.
-- Before Amazon Cognito can email your users, it requires additional permissions to call Amazon SES on your behalf. When you update your user pool with this option, Amazon Cognito creates a /service-linked role/ , which is a type of IAM role, in your AWS account. This role contains the permissions that allow Amazon Cognito to access Amazon SES and send email messages with your address. For more information about the service-linked role that Amazon Cognito creates, see <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito> in the /Amazon Cognito Developer Guide/ .
--
--
--
-- /Note:/ Consider using 'emailSendingAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectEmailSendingAccount :: Lens.Lens' EmailConfigurationType (Core.Maybe Types.EmailSendingAccountType)
ectEmailSendingAccount = Lens.field @"emailSendingAccount"
{-# INLINEABLE ectEmailSendingAccount #-}
{-# DEPRECATED emailSendingAccount "Use generic-lens or generic-optics with 'emailSendingAccount' instead"  #-}

-- | Identifies either the sender’s email address or the sender’s name with their email address. For example, @testuser@example.com@ or @Test User <testuser@example.com>@ . This address will appear before the body of the email.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectFrom :: Lens.Lens' EmailConfigurationType (Core.Maybe Types.From)
ectFrom = Lens.field @"from"
{-# INLINEABLE ectFrom #-}
{-# DEPRECATED from "Use generic-lens or generic-optics with 'from' instead"  #-}

-- | The destination to which the receiver of the email should reply to.
--
-- /Note:/ Consider using 'replyToEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectReplyToEmailAddress :: Lens.Lens' EmailConfigurationType (Core.Maybe Types.ReplyToEmailAddress)
ectReplyToEmailAddress = Lens.field @"replyToEmailAddress"
{-# INLINEABLE ectReplyToEmailAddress #-}
{-# DEPRECATED replyToEmailAddress "Use generic-lens or generic-optics with 'replyToEmailAddress' instead"  #-}

-- | The Amazon Resource Name (ARN) of a verified email address in Amazon SES. This email address is used in one of the following ways, depending on the value that you specify for the @EmailSendingAccount@ parameter:
--
--
--     * If you specify @COGNITO_DEFAULT@ , Amazon Cognito uses this address as the custom FROM address when it emails your users by using its built-in email account.
--
--
--     * If you specify @DEVELOPER@ , Amazon Cognito emails your users with this address by calling Amazon SES on your behalf.
--
--
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectSourceArn :: Lens.Lens' EmailConfigurationType (Core.Maybe Types.SourceArn)
ectSourceArn = Lens.field @"sourceArn"
{-# INLINEABLE ectSourceArn #-}
{-# DEPRECATED sourceArn "Use generic-lens or generic-optics with 'sourceArn' instead"  #-}

instance Core.FromJSON EmailConfigurationType where
        toJSON EmailConfigurationType{..}
          = Core.object
              (Core.catMaybes
                 [("ConfigurationSet" Core..=) Core.<$> configurationSet,
                  ("EmailSendingAccount" Core..=) Core.<$> emailSendingAccount,
                  ("From" Core..=) Core.<$> from,
                  ("ReplyToEmailAddress" Core..=) Core.<$> replyToEmailAddress,
                  ("SourceArn" Core..=) Core.<$> sourceArn])

instance Core.FromJSON EmailConfigurationType where
        parseJSON
          = Core.withObject "EmailConfigurationType" Core.$
              \ x ->
                EmailConfigurationType' Core.<$>
                  (x Core..:? "ConfigurationSet") Core.<*>
                    x Core..:? "EmailSendingAccount"
                    Core.<*> x Core..:? "From"
                    Core.<*> x Core..:? "ReplyToEmailAddress"
                    Core.<*> x Core..:? "SourceArn"
