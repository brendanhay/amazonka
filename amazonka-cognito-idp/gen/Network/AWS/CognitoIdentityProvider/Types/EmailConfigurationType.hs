{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.EmailConfigurationType where

import Network.AWS.CognitoIdentityProvider.Types.EmailSendingAccountType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The email configuration type.
--
-- Amazon Cognito has specific regions for use with Amazon SES. For more
-- information on the supported regions, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-email.html Email Settings for Amazon Cognito User Pools>.
--
-- /See:/ 'newEmailConfigurationType' smart constructor.
data EmailConfigurationType = EmailConfigurationType'
  { -- | Specifies whether Amazon Cognito emails your users by using its built-in
    -- email functionality or your Amazon SES email configuration. Specify one
    -- of the following values:
    --
    -- [COGNITO_DEFAULT]
    --     When Amazon Cognito emails your users, it uses its built-in email
    --     functionality. When you use the default option, Amazon Cognito
    --     allows only a limited number of emails each day for your user pool.
    --     For typical production environments, the default email limit is
    --     below the required delivery volume. To achieve a higher delivery
    --     volume, specify DEVELOPER to use your Amazon SES email
    --     configuration.
    --
    --     To look up the email delivery limit for the default option, see
    --     <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito>
    --     in the /Amazon Cognito Developer Guide/.
    --
    --     The default FROM address is no-reply\@verificationemail.com. To
    --     customize the FROM address, provide the ARN of an Amazon SES
    --     verified email address for the @SourceArn@ parameter.
    --
    --     If EmailSendingAccount is COGNITO_DEFAULT, the following parameters
    --     aren\'t allowed:
    --
    --     -   EmailVerificationMessage
    --
    --     -   EmailVerificationSubject
    --
    --     -   InviteMessageTemplate.EmailMessage
    --
    --     -   InviteMessageTemplate.EmailSubject
    --
    --     -   VerificationMessageTemplate.EmailMessage
    --
    --     -   VerificationMessageTemplate.EmailMessageByLink
    --
    --     -   VerificationMessageTemplate.EmailSubject,
    --
    --     -   VerificationMessageTemplate.EmailSubjectByLink
    --
    --     DEVELOPER EmailSendingAccount is required.
    --
    -- [DEVELOPER]
    --     When Amazon Cognito emails your users, it uses your Amazon SES
    --     configuration. Amazon Cognito calls Amazon SES on your behalf to
    --     send email from your verified email address. When you use this
    --     option, the email delivery limits are the same limits that apply to
    --     your Amazon SES verified email address in your AWS account.
    --
    --     If you use this option, you must provide the ARN of an Amazon SES
    --     verified email address for the @SourceArn@ parameter.
    --
    --     Before Amazon Cognito can email your users, it requires additional
    --     permissions to call Amazon SES on your behalf. When you update your
    --     user pool with this option, Amazon Cognito creates a /service-linked
    --     role/, which is a type of IAM role, in your AWS account. This role
    --     contains the permissions that allow Amazon Cognito to access Amazon
    --     SES and send email messages with your address. For more information
    --     about the service-linked role that Amazon Cognito creates, see
    --     <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito>
    --     in the /Amazon Cognito Developer Guide/.
    emailSendingAccount :: Prelude.Maybe EmailSendingAccountType,
    -- | The destination to which the receiver of the email should reply to.
    replyToEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | Identifies either the sender’s email address or the sender’s name with
    -- their email address. For example, @testuser\@example.com@ or
    -- @Test User \<testuser\@example.com>@. This address will appear before
    -- the body of the email.
    from :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a verified email address in Amazon
    -- SES. This email address is used in one of the following ways, depending
    -- on the value that you specify for the @EmailSendingAccount@ parameter:
    --
    -- -   If you specify @COGNITO_DEFAULT@, Amazon Cognito uses this address
    --     as the custom FROM address when it emails your users by using its
    --     built-in email account.
    --
    -- -   If you specify @DEVELOPER@, Amazon Cognito emails your users with
    --     this address by calling Amazon SES on your behalf.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The set of configuration rules that can be applied to emails sent using
    -- Amazon SES. A configuration set is applied to an email by including a
    -- reference to the configuration set in the headers of the email. Once
    -- applied, all of the rules in that configuration set are applied to the
    -- email. Configuration sets can be used to apply the following types of
    -- rules to emails:
    --
    -- -   Event publishing – Amazon SES can track the number of send,
    --     delivery, open, click, bounce, and complaint events for each email
    --     sent. Use event publishing to send information about these events to
    --     other AWS services such as SNS and CloudWatch.
    --
    -- -   IP pool management – When leasing dedicated IP addresses with Amazon
    --     SES, you can create groups of IP addresses, called dedicated IP
    --     pools. You can then associate the dedicated IP pools with
    --     configuration sets.
    configurationSet :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EmailConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailSendingAccount', 'emailConfigurationType_emailSendingAccount' - Specifies whether Amazon Cognito emails your users by using its built-in
-- email functionality or your Amazon SES email configuration. Specify one
-- of the following values:
--
-- [COGNITO_DEFAULT]
--     When Amazon Cognito emails your users, it uses its built-in email
--     functionality. When you use the default option, Amazon Cognito
--     allows only a limited number of emails each day for your user pool.
--     For typical production environments, the default email limit is
--     below the required delivery volume. To achieve a higher delivery
--     volume, specify DEVELOPER to use your Amazon SES email
--     configuration.
--
--     To look up the email delivery limit for the default option, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito>
--     in the /Amazon Cognito Developer Guide/.
--
--     The default FROM address is no-reply\@verificationemail.com. To
--     customize the FROM address, provide the ARN of an Amazon SES
--     verified email address for the @SourceArn@ parameter.
--
--     If EmailSendingAccount is COGNITO_DEFAULT, the following parameters
--     aren\'t allowed:
--
--     -   EmailVerificationMessage
--
--     -   EmailVerificationSubject
--
--     -   InviteMessageTemplate.EmailMessage
--
--     -   InviteMessageTemplate.EmailSubject
--
--     -   VerificationMessageTemplate.EmailMessage
--
--     -   VerificationMessageTemplate.EmailMessageByLink
--
--     -   VerificationMessageTemplate.EmailSubject,
--
--     -   VerificationMessageTemplate.EmailSubjectByLink
--
--     DEVELOPER EmailSendingAccount is required.
--
-- [DEVELOPER]
--     When Amazon Cognito emails your users, it uses your Amazon SES
--     configuration. Amazon Cognito calls Amazon SES on your behalf to
--     send email from your verified email address. When you use this
--     option, the email delivery limits are the same limits that apply to
--     your Amazon SES verified email address in your AWS account.
--
--     If you use this option, you must provide the ARN of an Amazon SES
--     verified email address for the @SourceArn@ parameter.
--
--     Before Amazon Cognito can email your users, it requires additional
--     permissions to call Amazon SES on your behalf. When you update your
--     user pool with this option, Amazon Cognito creates a /service-linked
--     role/, which is a type of IAM role, in your AWS account. This role
--     contains the permissions that allow Amazon Cognito to access Amazon
--     SES and send email messages with your address. For more information
--     about the service-linked role that Amazon Cognito creates, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito>
--     in the /Amazon Cognito Developer Guide/.
--
-- 'replyToEmailAddress', 'emailConfigurationType_replyToEmailAddress' - The destination to which the receiver of the email should reply to.
--
-- 'from', 'emailConfigurationType_from' - Identifies either the sender’s email address or the sender’s name with
-- their email address. For example, @testuser\@example.com@ or
-- @Test User \<testuser\@example.com>@. This address will appear before
-- the body of the email.
--
-- 'sourceArn', 'emailConfigurationType_sourceArn' - The Amazon Resource Name (ARN) of a verified email address in Amazon
-- SES. This email address is used in one of the following ways, depending
-- on the value that you specify for the @EmailSendingAccount@ parameter:
--
-- -   If you specify @COGNITO_DEFAULT@, Amazon Cognito uses this address
--     as the custom FROM address when it emails your users by using its
--     built-in email account.
--
-- -   If you specify @DEVELOPER@, Amazon Cognito emails your users with
--     this address by calling Amazon SES on your behalf.
--
-- 'configurationSet', 'emailConfigurationType_configurationSet' - The set of configuration rules that can be applied to emails sent using
-- Amazon SES. A configuration set is applied to an email by including a
-- reference to the configuration set in the headers of the email. Once
-- applied, all of the rules in that configuration set are applied to the
-- email. Configuration sets can be used to apply the following types of
-- rules to emails:
--
-- -   Event publishing – Amazon SES can track the number of send,
--     delivery, open, click, bounce, and complaint events for each email
--     sent. Use event publishing to send information about these events to
--     other AWS services such as SNS and CloudWatch.
--
-- -   IP pool management – When leasing dedicated IP addresses with Amazon
--     SES, you can create groups of IP addresses, called dedicated IP
--     pools. You can then associate the dedicated IP pools with
--     configuration sets.
newEmailConfigurationType ::
  EmailConfigurationType
newEmailConfigurationType =
  EmailConfigurationType'
    { emailSendingAccount =
        Prelude.Nothing,
      replyToEmailAddress = Prelude.Nothing,
      from = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      configurationSet = Prelude.Nothing
    }

-- | Specifies whether Amazon Cognito emails your users by using its built-in
-- email functionality or your Amazon SES email configuration. Specify one
-- of the following values:
--
-- [COGNITO_DEFAULT]
--     When Amazon Cognito emails your users, it uses its built-in email
--     functionality. When you use the default option, Amazon Cognito
--     allows only a limited number of emails each day for your user pool.
--     For typical production environments, the default email limit is
--     below the required delivery volume. To achieve a higher delivery
--     volume, specify DEVELOPER to use your Amazon SES email
--     configuration.
--
--     To look up the email delivery limit for the default option, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits in Amazon Cognito>
--     in the /Amazon Cognito Developer Guide/.
--
--     The default FROM address is no-reply\@verificationemail.com. To
--     customize the FROM address, provide the ARN of an Amazon SES
--     verified email address for the @SourceArn@ parameter.
--
--     If EmailSendingAccount is COGNITO_DEFAULT, the following parameters
--     aren\'t allowed:
--
--     -   EmailVerificationMessage
--
--     -   EmailVerificationSubject
--
--     -   InviteMessageTemplate.EmailMessage
--
--     -   InviteMessageTemplate.EmailSubject
--
--     -   VerificationMessageTemplate.EmailMessage
--
--     -   VerificationMessageTemplate.EmailMessageByLink
--
--     -   VerificationMessageTemplate.EmailSubject,
--
--     -   VerificationMessageTemplate.EmailSubjectByLink
--
--     DEVELOPER EmailSendingAccount is required.
--
-- [DEVELOPER]
--     When Amazon Cognito emails your users, it uses your Amazon SES
--     configuration. Amazon Cognito calls Amazon SES on your behalf to
--     send email from your verified email address. When you use this
--     option, the email delivery limits are the same limits that apply to
--     your Amazon SES verified email address in your AWS account.
--
--     If you use this option, you must provide the ARN of an Amazon SES
--     verified email address for the @SourceArn@ parameter.
--
--     Before Amazon Cognito can email your users, it requires additional
--     permissions to call Amazon SES on your behalf. When you update your
--     user pool with this option, Amazon Cognito creates a /service-linked
--     role/, which is a type of IAM role, in your AWS account. This role
--     contains the permissions that allow Amazon Cognito to access Amazon
--     SES and send email messages with your address. For more information
--     about the service-linked role that Amazon Cognito creates, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito>
--     in the /Amazon Cognito Developer Guide/.
emailConfigurationType_emailSendingAccount :: Lens.Lens' EmailConfigurationType (Prelude.Maybe EmailSendingAccountType)
emailConfigurationType_emailSendingAccount = Lens.lens (\EmailConfigurationType' {emailSendingAccount} -> emailSendingAccount) (\s@EmailConfigurationType' {} a -> s {emailSendingAccount = a} :: EmailConfigurationType)

-- | The destination to which the receiver of the email should reply to.
emailConfigurationType_replyToEmailAddress :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_replyToEmailAddress = Lens.lens (\EmailConfigurationType' {replyToEmailAddress} -> replyToEmailAddress) (\s@EmailConfigurationType' {} a -> s {replyToEmailAddress = a} :: EmailConfigurationType)

-- | Identifies either the sender’s email address or the sender’s name with
-- their email address. For example, @testuser\@example.com@ or
-- @Test User \<testuser\@example.com>@. This address will appear before
-- the body of the email.
emailConfigurationType_from :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_from = Lens.lens (\EmailConfigurationType' {from} -> from) (\s@EmailConfigurationType' {} a -> s {from = a} :: EmailConfigurationType)

-- | The Amazon Resource Name (ARN) of a verified email address in Amazon
-- SES. This email address is used in one of the following ways, depending
-- on the value that you specify for the @EmailSendingAccount@ parameter:
--
-- -   If you specify @COGNITO_DEFAULT@, Amazon Cognito uses this address
--     as the custom FROM address when it emails your users by using its
--     built-in email account.
--
-- -   If you specify @DEVELOPER@, Amazon Cognito emails your users with
--     this address by calling Amazon SES on your behalf.
emailConfigurationType_sourceArn :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_sourceArn = Lens.lens (\EmailConfigurationType' {sourceArn} -> sourceArn) (\s@EmailConfigurationType' {} a -> s {sourceArn = a} :: EmailConfigurationType)

-- | The set of configuration rules that can be applied to emails sent using
-- Amazon SES. A configuration set is applied to an email by including a
-- reference to the configuration set in the headers of the email. Once
-- applied, all of the rules in that configuration set are applied to the
-- email. Configuration sets can be used to apply the following types of
-- rules to emails:
--
-- -   Event publishing – Amazon SES can track the number of send,
--     delivery, open, click, bounce, and complaint events for each email
--     sent. Use event publishing to send information about these events to
--     other AWS services such as SNS and CloudWatch.
--
-- -   IP pool management – When leasing dedicated IP addresses with Amazon
--     SES, you can create groups of IP addresses, called dedicated IP
--     pools. You can then associate the dedicated IP pools with
--     configuration sets.
emailConfigurationType_configurationSet :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_configurationSet = Lens.lens (\EmailConfigurationType' {configurationSet} -> configurationSet) (\s@EmailConfigurationType' {} a -> s {configurationSet = a} :: EmailConfigurationType)

instance Prelude.FromJSON EmailConfigurationType where
  parseJSON =
    Prelude.withObject
      "EmailConfigurationType"
      ( \x ->
          EmailConfigurationType'
            Prelude.<$> (x Prelude..:? "EmailSendingAccount")
            Prelude.<*> (x Prelude..:? "ReplyToEmailAddress")
            Prelude.<*> (x Prelude..:? "From")
            Prelude.<*> (x Prelude..:? "SourceArn")
            Prelude.<*> (x Prelude..:? "ConfigurationSet")
      )

instance Prelude.Hashable EmailConfigurationType

instance Prelude.NFData EmailConfigurationType

instance Prelude.ToJSON EmailConfigurationType where
  toJSON EmailConfigurationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EmailSendingAccount" Prelude..=)
              Prelude.<$> emailSendingAccount,
            ("ReplyToEmailAddress" Prelude..=)
              Prelude.<$> replyToEmailAddress,
            ("From" Prelude..=) Prelude.<$> from,
            ("SourceArn" Prelude..=) Prelude.<$> sourceArn,
            ("ConfigurationSet" Prelude..=)
              Prelude.<$> configurationSet
          ]
      )
