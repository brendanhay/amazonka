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
-- Module      : Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType where

import Amazonka.CognitoIdentityProvider.Types.DefaultEmailOptionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The template for verification messages.
--
-- /See:/ 'newVerificationMessageTemplateType' smart constructor.
data VerificationMessageTemplateType = VerificationMessageTemplateType'
  { -- | The default email option.
    defaultEmailOption :: Prelude.Maybe DefaultEmailOptionType,
    -- | The template for email messages that Amazon Cognito sends to your users.
    -- You can set an @EmailMessage@ template only if the value of
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@. When your
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@, your user pool sends email messages with your own Amazon
    -- SES configuration.
    emailMessage :: Prelude.Maybe Prelude.Text,
    -- | The email message template for sending a confirmation link to the user.
    -- You can set an @EmailMessageByLink@ template only if the value of
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@. When your
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@, your user pool sends email messages with your own Amazon
    -- SES configuration.
    emailMessageByLink :: Prelude.Maybe Prelude.Text,
    -- | The subject line for the email message template. You can set an
    -- @EmailSubject@ template only if the value of
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@. When your
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@, your user pool sends email messages with your own Amazon
    -- SES configuration.
    emailSubject :: Prelude.Maybe Prelude.Text,
    -- | The subject line for the email message template for sending a
    -- confirmation link to the user. You can set an @EmailSubjectByLink@
    -- template only if the value of
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@. When your
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is @DEVELOPER@, your user pool sends email messages with your own Amazon
    -- SES configuration.
    emailSubjectByLink :: Prelude.Maybe Prelude.Text,
    -- | The template for SMS messages that Amazon Cognito sends to your users.
    smsMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VerificationMessageTemplateType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultEmailOption', 'verificationMessageTemplateType_defaultEmailOption' - The default email option.
--
-- 'emailMessage', 'verificationMessageTemplateType_emailMessage' - The template for email messages that Amazon Cognito sends to your users.
-- You can set an @EmailMessage@ template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
--
-- 'emailMessageByLink', 'verificationMessageTemplateType_emailMessageByLink' - The email message template for sending a confirmation link to the user.
-- You can set an @EmailMessageByLink@ template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
--
-- 'emailSubject', 'verificationMessageTemplateType_emailSubject' - The subject line for the email message template. You can set an
-- @EmailSubject@ template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
--
-- 'emailSubjectByLink', 'verificationMessageTemplateType_emailSubjectByLink' - The subject line for the email message template for sending a
-- confirmation link to the user. You can set an @EmailSubjectByLink@
-- template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
--
-- 'smsMessage', 'verificationMessageTemplateType_smsMessage' - The template for SMS messages that Amazon Cognito sends to your users.
newVerificationMessageTemplateType ::
  VerificationMessageTemplateType
newVerificationMessageTemplateType =
  VerificationMessageTemplateType'
    { defaultEmailOption =
        Prelude.Nothing,
      emailMessage = Prelude.Nothing,
      emailMessageByLink = Prelude.Nothing,
      emailSubject = Prelude.Nothing,
      emailSubjectByLink = Prelude.Nothing,
      smsMessage = Prelude.Nothing
    }

-- | The default email option.
verificationMessageTemplateType_defaultEmailOption :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe DefaultEmailOptionType)
verificationMessageTemplateType_defaultEmailOption = Lens.lens (\VerificationMessageTemplateType' {defaultEmailOption} -> defaultEmailOption) (\s@VerificationMessageTemplateType' {} a -> s {defaultEmailOption = a} :: VerificationMessageTemplateType)

-- | The template for email messages that Amazon Cognito sends to your users.
-- You can set an @EmailMessage@ template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
verificationMessageTemplateType_emailMessage :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailMessage = Lens.lens (\VerificationMessageTemplateType' {emailMessage} -> emailMessage) (\s@VerificationMessageTemplateType' {} a -> s {emailMessage = a} :: VerificationMessageTemplateType)

-- | The email message template for sending a confirmation link to the user.
-- You can set an @EmailMessageByLink@ template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
verificationMessageTemplateType_emailMessageByLink :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailMessageByLink = Lens.lens (\VerificationMessageTemplateType' {emailMessageByLink} -> emailMessageByLink) (\s@VerificationMessageTemplateType' {} a -> s {emailMessageByLink = a} :: VerificationMessageTemplateType)

-- | The subject line for the email message template. You can set an
-- @EmailSubject@ template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
verificationMessageTemplateType_emailSubject :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailSubject = Lens.lens (\VerificationMessageTemplateType' {emailSubject} -> emailSubject) (\s@VerificationMessageTemplateType' {} a -> s {emailSubject = a} :: VerificationMessageTemplateType)

-- | The subject line for the email message template for sending a
-- confirmation link to the user. You can set an @EmailSubjectByLink@
-- template only if the value of
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@. When your
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is @DEVELOPER@, your user pool sends email messages with your own Amazon
-- SES configuration.
verificationMessageTemplateType_emailSubjectByLink :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailSubjectByLink = Lens.lens (\VerificationMessageTemplateType' {emailSubjectByLink} -> emailSubjectByLink) (\s@VerificationMessageTemplateType' {} a -> s {emailSubjectByLink = a} :: VerificationMessageTemplateType)

-- | The template for SMS messages that Amazon Cognito sends to your users.
verificationMessageTemplateType_smsMessage :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_smsMessage = Lens.lens (\VerificationMessageTemplateType' {smsMessage} -> smsMessage) (\s@VerificationMessageTemplateType' {} a -> s {smsMessage = a} :: VerificationMessageTemplateType)

instance
  Data.FromJSON
    VerificationMessageTemplateType
  where
  parseJSON =
    Data.withObject
      "VerificationMessageTemplateType"
      ( \x ->
          VerificationMessageTemplateType'
            Prelude.<$> (x Data..:? "DefaultEmailOption")
            Prelude.<*> (x Data..:? "EmailMessage")
            Prelude.<*> (x Data..:? "EmailMessageByLink")
            Prelude.<*> (x Data..:? "EmailSubject")
            Prelude.<*> (x Data..:? "EmailSubjectByLink")
            Prelude.<*> (x Data..:? "SmsMessage")
      )

instance
  Prelude.Hashable
    VerificationMessageTemplateType
  where
  hashWithSalt
    _salt
    VerificationMessageTemplateType' {..} =
      _salt
        `Prelude.hashWithSalt` defaultEmailOption
        `Prelude.hashWithSalt` emailMessage
        `Prelude.hashWithSalt` emailMessageByLink
        `Prelude.hashWithSalt` emailSubject
        `Prelude.hashWithSalt` emailSubjectByLink
        `Prelude.hashWithSalt` smsMessage

instance
  Prelude.NFData
    VerificationMessageTemplateType
  where
  rnf VerificationMessageTemplateType' {..} =
    Prelude.rnf defaultEmailOption
      `Prelude.seq` Prelude.rnf emailMessage
      `Prelude.seq` Prelude.rnf emailMessageByLink
      `Prelude.seq` Prelude.rnf emailSubject
      `Prelude.seq` Prelude.rnf emailSubjectByLink
      `Prelude.seq` Prelude.rnf smsMessage

instance Data.ToJSON VerificationMessageTemplateType where
  toJSON VerificationMessageTemplateType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultEmailOption" Data..=)
              Prelude.<$> defaultEmailOption,
            ("EmailMessage" Data..=) Prelude.<$> emailMessage,
            ("EmailMessageByLink" Data..=)
              Prelude.<$> emailMessageByLink,
            ("EmailSubject" Data..=) Prelude.<$> emailSubject,
            ("EmailSubjectByLink" Data..=)
              Prelude.<$> emailSubjectByLink,
            ("SmsMessage" Data..=) Prelude.<$> smsMessage
          ]
      )
