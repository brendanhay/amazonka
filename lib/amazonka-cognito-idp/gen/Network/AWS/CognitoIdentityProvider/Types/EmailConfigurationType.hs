-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
  ( EmailConfigurationType (..),

    -- * Smart constructor
    mkEmailConfigurationType,

    -- * Lenses
    ectSourceARN,
    ectFrom,
    ectConfigurationSet,
    ectReplyToEmailAddress,
    ectEmailSendingAccount,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The email configuration type.
--
-- /See:/ 'mkEmailConfigurationType' smart constructor.
data EmailConfigurationType = EmailConfigurationType'
  { sourceARN ::
      Lude.Maybe Lude.Text,
    from :: Lude.Maybe Lude.Text,
    configurationSet :: Lude.Maybe Lude.Text,
    replyToEmailAddress :: Lude.Maybe Lude.Text,
    emailSendingAccount ::
      Lude.Maybe EmailSendingAccountType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailConfigurationType' with the minimum fields required to make a request.
--
-- * 'configurationSet' - The set of configuration rules that can be applied to emails sent using Amazon SES. A configuration set is applied to an email by including a reference to the configuration set in the headers of the email. Once applied, all of the rules in that configuration set are applied to the email. Configuration sets can be used to apply the following types of rules to emails:
--
--
--     * Event publishing – Amazon SES can track the number of send, delivery, open, click, bounce, and complaint events for each email sent. Use event publishing to send information about these events to other AWS services such as SNS and CloudWatch.
--
--
--     * IP pool management – When leasing dedicated IP addresses with Amazon SES, you can create groups of IP addresses, called dedicated IP pools. You can then associate the dedicated IP pools with configuration sets.
--
--
-- * 'emailSendingAccount' - Specifies whether Amazon Cognito emails your users by using its built-in email functionality or your Amazon SES email configuration. Specify one of the following values:
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
-- * 'from' - Identifies either the sender’s email address or the sender’s name with their email address. For example, @testuser@example.com@ or @Test User <testuser@example.com>@ . This address will appear before the body of the email.
-- * 'replyToEmailAddress' - The destination to which the receiver of the email should reply to.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of a verified email address in Amazon SES. This email address is used in one of the following ways, depending on the value that you specify for the @EmailSendingAccount@ parameter:
--
--
--     * If you specify @COGNITO_DEFAULT@ , Amazon Cognito uses this address as the custom FROM address when it emails your users by using its built-in email account.
--
--
--     * If you specify @DEVELOPER@ , Amazon Cognito emails your users with this address by calling Amazon SES on your behalf.
mkEmailConfigurationType ::
  EmailConfigurationType
mkEmailConfigurationType =
  EmailConfigurationType'
    { sourceARN = Lude.Nothing,
      from = Lude.Nothing,
      configurationSet = Lude.Nothing,
      replyToEmailAddress = Lude.Nothing,
      emailSendingAccount = Lude.Nothing
    }

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
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectSourceARN :: Lens.Lens' EmailConfigurationType (Lude.Maybe Lude.Text)
ectSourceARN = Lens.lens (sourceARN :: EmailConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {sourceARN = a} :: EmailConfigurationType)
{-# DEPRECATED ectSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

-- | Identifies either the sender’s email address or the sender’s name with their email address. For example, @testuser@example.com@ or @Test User <testuser@example.com>@ . This address will appear before the body of the email.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectFrom :: Lens.Lens' EmailConfigurationType (Lude.Maybe Lude.Text)
ectFrom = Lens.lens (from :: EmailConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {from = a} :: EmailConfigurationType)
{-# DEPRECATED ectFrom "Use generic-lens or generic-optics with 'from' instead." #-}

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
ectConfigurationSet :: Lens.Lens' EmailConfigurationType (Lude.Maybe Lude.Text)
ectConfigurationSet = Lens.lens (configurationSet :: EmailConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {configurationSet = a} :: EmailConfigurationType)
{-# DEPRECATED ectConfigurationSet "Use generic-lens or generic-optics with 'configurationSet' instead." #-}

-- | The destination to which the receiver of the email should reply to.
--
-- /Note:/ Consider using 'replyToEmailAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ectReplyToEmailAddress :: Lens.Lens' EmailConfigurationType (Lude.Maybe Lude.Text)
ectReplyToEmailAddress = Lens.lens (replyToEmailAddress :: EmailConfigurationType -> Lude.Maybe Lude.Text) (\s a -> s {replyToEmailAddress = a} :: EmailConfigurationType)
{-# DEPRECATED ectReplyToEmailAddress "Use generic-lens or generic-optics with 'replyToEmailAddress' instead." #-}

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
ectEmailSendingAccount :: Lens.Lens' EmailConfigurationType (Lude.Maybe EmailSendingAccountType)
ectEmailSendingAccount = Lens.lens (emailSendingAccount :: EmailConfigurationType -> Lude.Maybe EmailSendingAccountType) (\s a -> s {emailSendingAccount = a} :: EmailConfigurationType)
{-# DEPRECATED ectEmailSendingAccount "Use generic-lens or generic-optics with 'emailSendingAccount' instead." #-}

instance Lude.FromJSON EmailConfigurationType where
  parseJSON =
    Lude.withObject
      "EmailConfigurationType"
      ( \x ->
          EmailConfigurationType'
            Lude.<$> (x Lude..:? "SourceArn")
            Lude.<*> (x Lude..:? "From")
            Lude.<*> (x Lude..:? "ConfigurationSet")
            Lude.<*> (x Lude..:? "ReplyToEmailAddress")
            Lude.<*> (x Lude..:? "EmailSendingAccount")
      )

instance Lude.ToJSON EmailConfigurationType where
  toJSON EmailConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SourceArn" Lude..=) Lude.<$> sourceARN,
            ("From" Lude..=) Lude.<$> from,
            ("ConfigurationSet" Lude..=) Lude.<$> configurationSet,
            ("ReplyToEmailAddress" Lude..=) Lude.<$> replyToEmailAddress,
            ("EmailSendingAccount" Lude..=) Lude.<$> emailSendingAccount
          ]
      )
