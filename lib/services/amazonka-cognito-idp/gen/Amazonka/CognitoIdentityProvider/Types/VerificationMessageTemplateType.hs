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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.VerificationMessageTemplateType where

import Amazonka.CognitoIdentityProvider.Types.DefaultEmailOptionType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The template for verification messages.
--
-- /See:/ 'newVerificationMessageTemplateType' smart constructor.
data VerificationMessageTemplateType = VerificationMessageTemplateType'
  { -- | The subject line for the email message template. EmailSubject is allowed
    -- only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailSubject :: Prelude.Maybe Prelude.Text,
    -- | The SMS message template.
    smsMessage :: Prelude.Maybe Prelude.Text,
    -- | The subject line for the email message template for sending a
    -- confirmation link to the user. EmailSubjectByLink is allowed only
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailSubjectByLink :: Prelude.Maybe Prelude.Text,
    -- | The email message template for sending a confirmation link to the user.
    -- EmailMessageByLink is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailMessageByLink :: Prelude.Maybe Prelude.Text,
    -- | The default email option.
    defaultEmailOption :: Prelude.Maybe DefaultEmailOptionType,
    -- | The email message template. EmailMessage is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailMessage :: Prelude.Maybe Prelude.Text
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
-- 'emailSubject', 'verificationMessageTemplateType_emailSubject' - The subject line for the email message template. EmailSubject is allowed
-- only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'smsMessage', 'verificationMessageTemplateType_smsMessage' - The SMS message template.
--
-- 'emailSubjectByLink', 'verificationMessageTemplateType_emailSubjectByLink' - The subject line for the email message template for sending a
-- confirmation link to the user. EmailSubjectByLink is allowed only
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'emailMessageByLink', 'verificationMessageTemplateType_emailMessageByLink' - The email message template for sending a confirmation link to the user.
-- EmailMessageByLink is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'defaultEmailOption', 'verificationMessageTemplateType_defaultEmailOption' - The default email option.
--
-- 'emailMessage', 'verificationMessageTemplateType_emailMessage' - The email message template. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
newVerificationMessageTemplateType ::
  VerificationMessageTemplateType
newVerificationMessageTemplateType =
  VerificationMessageTemplateType'
    { emailSubject =
        Prelude.Nothing,
      smsMessage = Prelude.Nothing,
      emailSubjectByLink = Prelude.Nothing,
      emailMessageByLink = Prelude.Nothing,
      defaultEmailOption = Prelude.Nothing,
      emailMessage = Prelude.Nothing
    }

-- | The subject line for the email message template. EmailSubject is allowed
-- only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailSubject :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailSubject = Lens.lens (\VerificationMessageTemplateType' {emailSubject} -> emailSubject) (\s@VerificationMessageTemplateType' {} a -> s {emailSubject = a} :: VerificationMessageTemplateType)

-- | The SMS message template.
verificationMessageTemplateType_smsMessage :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_smsMessage = Lens.lens (\VerificationMessageTemplateType' {smsMessage} -> smsMessage) (\s@VerificationMessageTemplateType' {} a -> s {smsMessage = a} :: VerificationMessageTemplateType)

-- | The subject line for the email message template for sending a
-- confirmation link to the user. EmailSubjectByLink is allowed only
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailSubjectByLink :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailSubjectByLink = Lens.lens (\VerificationMessageTemplateType' {emailSubjectByLink} -> emailSubjectByLink) (\s@VerificationMessageTemplateType' {} a -> s {emailSubjectByLink = a} :: VerificationMessageTemplateType)

-- | The email message template for sending a confirmation link to the user.
-- EmailMessageByLink is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailMessageByLink :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailMessageByLink = Lens.lens (\VerificationMessageTemplateType' {emailMessageByLink} -> emailMessageByLink) (\s@VerificationMessageTemplateType' {} a -> s {emailMessageByLink = a} :: VerificationMessageTemplateType)

-- | The default email option.
verificationMessageTemplateType_defaultEmailOption :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe DefaultEmailOptionType)
verificationMessageTemplateType_defaultEmailOption = Lens.lens (\VerificationMessageTemplateType' {defaultEmailOption} -> defaultEmailOption) (\s@VerificationMessageTemplateType' {} a -> s {defaultEmailOption = a} :: VerificationMessageTemplateType)

-- | The email message template. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailMessage :: Lens.Lens' VerificationMessageTemplateType (Prelude.Maybe Prelude.Text)
verificationMessageTemplateType_emailMessage = Lens.lens (\VerificationMessageTemplateType' {emailMessage} -> emailMessage) (\s@VerificationMessageTemplateType' {} a -> s {emailMessage = a} :: VerificationMessageTemplateType)

instance
  Core.FromJSON
    VerificationMessageTemplateType
  where
  parseJSON =
    Core.withObject
      "VerificationMessageTemplateType"
      ( \x ->
          VerificationMessageTemplateType'
            Prelude.<$> (x Core..:? "EmailSubject")
            Prelude.<*> (x Core..:? "SmsMessage")
            Prelude.<*> (x Core..:? "EmailSubjectByLink")
            Prelude.<*> (x Core..:? "EmailMessageByLink")
            Prelude.<*> (x Core..:? "DefaultEmailOption")
            Prelude.<*> (x Core..:? "EmailMessage")
      )

instance
  Prelude.Hashable
    VerificationMessageTemplateType
  where
  hashWithSalt
    _salt
    VerificationMessageTemplateType' {..} =
      _salt `Prelude.hashWithSalt` emailSubject
        `Prelude.hashWithSalt` smsMessage
        `Prelude.hashWithSalt` emailSubjectByLink
        `Prelude.hashWithSalt` emailMessageByLink
        `Prelude.hashWithSalt` defaultEmailOption
        `Prelude.hashWithSalt` emailMessage

instance
  Prelude.NFData
    VerificationMessageTemplateType
  where
  rnf VerificationMessageTemplateType' {..} =
    Prelude.rnf emailSubject
      `Prelude.seq` Prelude.rnf smsMessage
      `Prelude.seq` Prelude.rnf emailSubjectByLink
      `Prelude.seq` Prelude.rnf emailMessageByLink
      `Prelude.seq` Prelude.rnf defaultEmailOption
      `Prelude.seq` Prelude.rnf emailMessage

instance Core.ToJSON VerificationMessageTemplateType where
  toJSON VerificationMessageTemplateType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EmailSubject" Core..=) Prelude.<$> emailSubject,
            ("SmsMessage" Core..=) Prelude.<$> smsMessage,
            ("EmailSubjectByLink" Core..=)
              Prelude.<$> emailSubjectByLink,
            ("EmailMessageByLink" Core..=)
              Prelude.<$> emailMessageByLink,
            ("DefaultEmailOption" Core..=)
              Prelude.<$> defaultEmailOption,
            ("EmailMessage" Core..=) Prelude.<$> emailMessage
          ]
      )
