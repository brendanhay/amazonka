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
-- Module      : Amazonka.CognitoIdentityProvider.Types.EmailConfigurationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.EmailConfigurationType where

import Amazonka.CognitoIdentityProvider.Types.EmailSendingAccountType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The email configuration of your user pool. The email configuration type
-- sets your preferred sending method, Amazon Web Services Region, and
-- sender for messages from your user pool.
--
-- Amazon Cognito can send email messages with Amazon Simple Email Service
-- resources in the Amazon Web Services Region where you created your user
-- pool, and in alternate Regions in some cases. For more information on
-- the supported Regions, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-email.html Email settings for Amazon Cognito user pools>.
--
-- /See:/ 'newEmailConfigurationType' smart constructor.
data EmailConfigurationType = EmailConfigurationType'
  { -- | The set of configuration rules that can be applied to emails sent using
    -- Amazon Simple Email Service. A configuration set is applied to an email
    -- by including a reference to the configuration set in the headers of the
    -- email. Once applied, all of the rules in that configuration set are
    -- applied to the email. Configuration sets can be used to apply the
    -- following types of rules to emails:
    --
    -- [Event publishing]
    --     Amazon Simple Email Service can track the number of send, delivery,
    --     open, click, bounce, and complaint events for each email sent. Use
    --     event publishing to send information about these events to other
    --     Amazon Web Services services such as and Amazon CloudWatch
    --
    -- [IP pool management]
    --     When leasing dedicated IP addresses with Amazon Simple Email
    --     Service, you can create groups of IP addresses, called dedicated IP
    --     pools. You can then associate the dedicated IP pools with
    --     configuration sets.
    configurationSet :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether Amazon Cognito uses its built-in functionality to send
    -- your users email messages, or uses your Amazon Simple Email Service
    -- email configuration. Specify one of the following values:
    --
    -- [COGNITO_DEFAULT]
    --     When Amazon Cognito emails your users, it uses its built-in email
    --     functionality. When you use the default option, Amazon Cognito
    --     allows only a limited number of emails each day for your user pool.
    --     For typical production environments, the default email limit is less
    --     than the required delivery volume. To achieve a higher delivery
    --     volume, specify DEVELOPER to use your Amazon SES email
    --     configuration.
    --
    --     To look up the email delivery limit for the default option, see
    --     <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits>
    --     in the /Amazon Cognito Developer Guide/.
    --
    --     The default FROM address is @no-reply\@verificationemail.com@. To
    --     customize the FROM address, provide the Amazon Resource Name (ARN)
    --     of an Amazon SES verified email address for the @SourceArn@
    --     parameter.
    --
    -- [DEVELOPER]
    --     When Amazon Cognito emails your users, it uses your Amazon SES
    --     configuration. Amazon Cognito calls Amazon SES on your behalf to
    --     send email from your verified email address. When you use this
    --     option, the email delivery limits are the same limits that apply to
    --     your Amazon SES verified email address in your Amazon Web Services
    --     account.
    --
    --     If you use this option, provide the ARN of an Amazon SES verified
    --     email address for the @SourceArn@ parameter.
    --
    --     Before Amazon Cognito can email your users, it requires additional
    --     permissions to call Amazon SES on your behalf. When you update your
    --     user pool with this option, Amazon Cognito creates a /service-linked
    --     role/, which is a type of role in your Amazon Web Services account.
    --     This role contains the permissions that allow you to access Amazon
    --     SES and send email messages from your email address. For more
    --     information about the service-linked role that Amazon Cognito
    --     creates, see
    --     <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito>
    --     in the /Amazon Cognito Developer Guide/.
    emailSendingAccount :: Prelude.Maybe EmailSendingAccountType,
    -- | Either the sender’s email address or the sender’s name with their email
    -- address. For example, @testuser\@example.com@ or
    -- @Test User \<testuser\@example.com>@. This address appears before the
    -- body of the email.
    from :: Prelude.Maybe Prelude.Text,
    -- | The destination to which the receiver of the email should reply.
    replyToEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a verified email address in Amazon SES. Amazon Cognito uses
    -- this email address in one of the following ways, depending on the value
    -- that you specify for the @EmailSendingAccount@ parameter:
    --
    -- -   If you specify @COGNITO_DEFAULT@, Amazon Cognito uses this address
    --     as the custom FROM address when it emails your users using its
    --     built-in email account.
    --
    -- -   If you specify @DEVELOPER@, Amazon Cognito emails your users with
    --     this address by calling Amazon SES on your behalf.
    --
    -- The Region value of the @SourceArn@ parameter must indicate a supported
    -- Amazon Web Services Region of your user pool. Typically, the Region in
    -- the @SourceArn@ and the user pool Region are the same. For more
    -- information, see
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-email.html#user-pool-email-developer-region-mapping Amazon SES email configuration regions>
    -- in the
    -- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito Developer Guide>.
    sourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailConfigurationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationSet', 'emailConfigurationType_configurationSet' - The set of configuration rules that can be applied to emails sent using
-- Amazon Simple Email Service. A configuration set is applied to an email
-- by including a reference to the configuration set in the headers of the
-- email. Once applied, all of the rules in that configuration set are
-- applied to the email. Configuration sets can be used to apply the
-- following types of rules to emails:
--
-- [Event publishing]
--     Amazon Simple Email Service can track the number of send, delivery,
--     open, click, bounce, and complaint events for each email sent. Use
--     event publishing to send information about these events to other
--     Amazon Web Services services such as and Amazon CloudWatch
--
-- [IP pool management]
--     When leasing dedicated IP addresses with Amazon Simple Email
--     Service, you can create groups of IP addresses, called dedicated IP
--     pools. You can then associate the dedicated IP pools with
--     configuration sets.
--
-- 'emailSendingAccount', 'emailConfigurationType_emailSendingAccount' - Specifies whether Amazon Cognito uses its built-in functionality to send
-- your users email messages, or uses your Amazon Simple Email Service
-- email configuration. Specify one of the following values:
--
-- [COGNITO_DEFAULT]
--     When Amazon Cognito emails your users, it uses its built-in email
--     functionality. When you use the default option, Amazon Cognito
--     allows only a limited number of emails each day for your user pool.
--     For typical production environments, the default email limit is less
--     than the required delivery volume. To achieve a higher delivery
--     volume, specify DEVELOPER to use your Amazon SES email
--     configuration.
--
--     To look up the email delivery limit for the default option, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits>
--     in the /Amazon Cognito Developer Guide/.
--
--     The default FROM address is @no-reply\@verificationemail.com@. To
--     customize the FROM address, provide the Amazon Resource Name (ARN)
--     of an Amazon SES verified email address for the @SourceArn@
--     parameter.
--
-- [DEVELOPER]
--     When Amazon Cognito emails your users, it uses your Amazon SES
--     configuration. Amazon Cognito calls Amazon SES on your behalf to
--     send email from your verified email address. When you use this
--     option, the email delivery limits are the same limits that apply to
--     your Amazon SES verified email address in your Amazon Web Services
--     account.
--
--     If you use this option, provide the ARN of an Amazon SES verified
--     email address for the @SourceArn@ parameter.
--
--     Before Amazon Cognito can email your users, it requires additional
--     permissions to call Amazon SES on your behalf. When you update your
--     user pool with this option, Amazon Cognito creates a /service-linked
--     role/, which is a type of role in your Amazon Web Services account.
--     This role contains the permissions that allow you to access Amazon
--     SES and send email messages from your email address. For more
--     information about the service-linked role that Amazon Cognito
--     creates, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito>
--     in the /Amazon Cognito Developer Guide/.
--
-- 'from', 'emailConfigurationType_from' - Either the sender’s email address or the sender’s name with their email
-- address. For example, @testuser\@example.com@ or
-- @Test User \<testuser\@example.com>@. This address appears before the
-- body of the email.
--
-- 'replyToEmailAddress', 'emailConfigurationType_replyToEmailAddress' - The destination to which the receiver of the email should reply.
--
-- 'sourceArn', 'emailConfigurationType_sourceArn' - The ARN of a verified email address in Amazon SES. Amazon Cognito uses
-- this email address in one of the following ways, depending on the value
-- that you specify for the @EmailSendingAccount@ parameter:
--
-- -   If you specify @COGNITO_DEFAULT@, Amazon Cognito uses this address
--     as the custom FROM address when it emails your users using its
--     built-in email account.
--
-- -   If you specify @DEVELOPER@, Amazon Cognito emails your users with
--     this address by calling Amazon SES on your behalf.
--
-- The Region value of the @SourceArn@ parameter must indicate a supported
-- Amazon Web Services Region of your user pool. Typically, the Region in
-- the @SourceArn@ and the user pool Region are the same. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-email.html#user-pool-email-developer-region-mapping Amazon SES email configuration regions>
-- in the
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito Developer Guide>.
newEmailConfigurationType ::
  EmailConfigurationType
newEmailConfigurationType =
  EmailConfigurationType'
    { configurationSet =
        Prelude.Nothing,
      emailSendingAccount = Prelude.Nothing,
      from = Prelude.Nothing,
      replyToEmailAddress = Prelude.Nothing,
      sourceArn = Prelude.Nothing
    }

-- | The set of configuration rules that can be applied to emails sent using
-- Amazon Simple Email Service. A configuration set is applied to an email
-- by including a reference to the configuration set in the headers of the
-- email. Once applied, all of the rules in that configuration set are
-- applied to the email. Configuration sets can be used to apply the
-- following types of rules to emails:
--
-- [Event publishing]
--     Amazon Simple Email Service can track the number of send, delivery,
--     open, click, bounce, and complaint events for each email sent. Use
--     event publishing to send information about these events to other
--     Amazon Web Services services such as and Amazon CloudWatch
--
-- [IP pool management]
--     When leasing dedicated IP addresses with Amazon Simple Email
--     Service, you can create groups of IP addresses, called dedicated IP
--     pools. You can then associate the dedicated IP pools with
--     configuration sets.
emailConfigurationType_configurationSet :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_configurationSet = Lens.lens (\EmailConfigurationType' {configurationSet} -> configurationSet) (\s@EmailConfigurationType' {} a -> s {configurationSet = a} :: EmailConfigurationType)

-- | Specifies whether Amazon Cognito uses its built-in functionality to send
-- your users email messages, or uses your Amazon Simple Email Service
-- email configuration. Specify one of the following values:
--
-- [COGNITO_DEFAULT]
--     When Amazon Cognito emails your users, it uses its built-in email
--     functionality. When you use the default option, Amazon Cognito
--     allows only a limited number of emails each day for your user pool.
--     For typical production environments, the default email limit is less
--     than the required delivery volume. To achieve a higher delivery
--     volume, specify DEVELOPER to use your Amazon SES email
--     configuration.
--
--     To look up the email delivery limit for the default option, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/limits.html Limits>
--     in the /Amazon Cognito Developer Guide/.
--
--     The default FROM address is @no-reply\@verificationemail.com@. To
--     customize the FROM address, provide the Amazon Resource Name (ARN)
--     of an Amazon SES verified email address for the @SourceArn@
--     parameter.
--
-- [DEVELOPER]
--     When Amazon Cognito emails your users, it uses your Amazon SES
--     configuration. Amazon Cognito calls Amazon SES on your behalf to
--     send email from your verified email address. When you use this
--     option, the email delivery limits are the same limits that apply to
--     your Amazon SES verified email address in your Amazon Web Services
--     account.
--
--     If you use this option, provide the ARN of an Amazon SES verified
--     email address for the @SourceArn@ parameter.
--
--     Before Amazon Cognito can email your users, it requires additional
--     permissions to call Amazon SES on your behalf. When you update your
--     user pool with this option, Amazon Cognito creates a /service-linked
--     role/, which is a type of role in your Amazon Web Services account.
--     This role contains the permissions that allow you to access Amazon
--     SES and send email messages from your email address. For more
--     information about the service-linked role that Amazon Cognito
--     creates, see
--     <https://docs.aws.amazon.com/cognito/latest/developerguide/using-service-linked-roles.html Using Service-Linked Roles for Amazon Cognito>
--     in the /Amazon Cognito Developer Guide/.
emailConfigurationType_emailSendingAccount :: Lens.Lens' EmailConfigurationType (Prelude.Maybe EmailSendingAccountType)
emailConfigurationType_emailSendingAccount = Lens.lens (\EmailConfigurationType' {emailSendingAccount} -> emailSendingAccount) (\s@EmailConfigurationType' {} a -> s {emailSendingAccount = a} :: EmailConfigurationType)

-- | Either the sender’s email address or the sender’s name with their email
-- address. For example, @testuser\@example.com@ or
-- @Test User \<testuser\@example.com>@. This address appears before the
-- body of the email.
emailConfigurationType_from :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_from = Lens.lens (\EmailConfigurationType' {from} -> from) (\s@EmailConfigurationType' {} a -> s {from = a} :: EmailConfigurationType)

-- | The destination to which the receiver of the email should reply.
emailConfigurationType_replyToEmailAddress :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_replyToEmailAddress = Lens.lens (\EmailConfigurationType' {replyToEmailAddress} -> replyToEmailAddress) (\s@EmailConfigurationType' {} a -> s {replyToEmailAddress = a} :: EmailConfigurationType)

-- | The ARN of a verified email address in Amazon SES. Amazon Cognito uses
-- this email address in one of the following ways, depending on the value
-- that you specify for the @EmailSendingAccount@ parameter:
--
-- -   If you specify @COGNITO_DEFAULT@, Amazon Cognito uses this address
--     as the custom FROM address when it emails your users using its
--     built-in email account.
--
-- -   If you specify @DEVELOPER@, Amazon Cognito emails your users with
--     this address by calling Amazon SES on your behalf.
--
-- The Region value of the @SourceArn@ parameter must indicate a supported
-- Amazon Web Services Region of your user pool. Typically, the Region in
-- the @SourceArn@ and the user pool Region are the same. For more
-- information, see
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/user-pool-email.html#user-pool-email-developer-region-mapping Amazon SES email configuration regions>
-- in the
-- <https://docs.aws.amazon.com/cognito/latest/developerguide/cognito-user-identity-pools.html Amazon Cognito Developer Guide>.
emailConfigurationType_sourceArn :: Lens.Lens' EmailConfigurationType (Prelude.Maybe Prelude.Text)
emailConfigurationType_sourceArn = Lens.lens (\EmailConfigurationType' {sourceArn} -> sourceArn) (\s@EmailConfigurationType' {} a -> s {sourceArn = a} :: EmailConfigurationType)

instance Data.FromJSON EmailConfigurationType where
  parseJSON =
    Data.withObject
      "EmailConfigurationType"
      ( \x ->
          EmailConfigurationType'
            Prelude.<$> (x Data..:? "ConfigurationSet")
            Prelude.<*> (x Data..:? "EmailSendingAccount")
            Prelude.<*> (x Data..:? "From")
            Prelude.<*> (x Data..:? "ReplyToEmailAddress")
            Prelude.<*> (x Data..:? "SourceArn")
      )

instance Prelude.Hashable EmailConfigurationType where
  hashWithSalt _salt EmailConfigurationType' {..} =
    _salt
      `Prelude.hashWithSalt` configurationSet
      `Prelude.hashWithSalt` emailSendingAccount
      `Prelude.hashWithSalt` from
      `Prelude.hashWithSalt` replyToEmailAddress
      `Prelude.hashWithSalt` sourceArn

instance Prelude.NFData EmailConfigurationType where
  rnf EmailConfigurationType' {..} =
    Prelude.rnf configurationSet
      `Prelude.seq` Prelude.rnf emailSendingAccount
      `Prelude.seq` Prelude.rnf from
      `Prelude.seq` Prelude.rnf replyToEmailAddress
      `Prelude.seq` Prelude.rnf sourceArn

instance Data.ToJSON EmailConfigurationType where
  toJSON EmailConfigurationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConfigurationSet" Data..=)
              Prelude.<$> configurationSet,
            ("EmailSendingAccount" Data..=)
              Prelude.<$> emailSendingAccount,
            ("From" Data..=) Prelude.<$> from,
            ("ReplyToEmailAddress" Data..=)
              Prelude.<$> replyToEmailAddress,
            ("SourceArn" Data..=) Prelude.<$> sourceArn
          ]
      )
