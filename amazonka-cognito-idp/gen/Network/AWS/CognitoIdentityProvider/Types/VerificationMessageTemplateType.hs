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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType where

import Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The template for verification messages.
--
-- /See:/ 'newVerificationMessageTemplateType' smart constructor.
data VerificationMessageTemplateType = VerificationMessageTemplateType'
  { -- | The default email option.
    defaultEmailOption :: Core.Maybe DefaultEmailOptionType,
    -- | The subject line for the email message template for sending a
    -- confirmation link to the user. EmailSubjectByLink is allowed only
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailSubjectByLink :: Core.Maybe Core.Text,
    -- | The subject line for the email message template. EmailSubject is allowed
    -- only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailSubject :: Core.Maybe Core.Text,
    -- | The email message template. EmailMessage is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailMessage :: Core.Maybe Core.Text,
    -- | The email message template for sending a confirmation link to the user.
    -- EmailMessageByLink is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailMessageByLink :: Core.Maybe Core.Text,
    -- | The SMS message template.
    smsMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'emailSubjectByLink', 'verificationMessageTemplateType_emailSubjectByLink' - The subject line for the email message template for sending a
-- confirmation link to the user. EmailSubjectByLink is allowed only
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'emailSubject', 'verificationMessageTemplateType_emailSubject' - The subject line for the email message template. EmailSubject is allowed
-- only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'emailMessage', 'verificationMessageTemplateType_emailMessage' - The email message template. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'emailMessageByLink', 'verificationMessageTemplateType_emailMessageByLink' - The email message template for sending a confirmation link to the user.
-- EmailMessageByLink is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'smsMessage', 'verificationMessageTemplateType_smsMessage' - The SMS message template.
newVerificationMessageTemplateType ::
  VerificationMessageTemplateType
newVerificationMessageTemplateType =
  VerificationMessageTemplateType'
    { defaultEmailOption =
        Core.Nothing,
      emailSubjectByLink = Core.Nothing,
      emailSubject = Core.Nothing,
      emailMessage = Core.Nothing,
      emailMessageByLink = Core.Nothing,
      smsMessage = Core.Nothing
    }

-- | The default email option.
verificationMessageTemplateType_defaultEmailOption :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe DefaultEmailOptionType)
verificationMessageTemplateType_defaultEmailOption = Lens.lens (\VerificationMessageTemplateType' {defaultEmailOption} -> defaultEmailOption) (\s@VerificationMessageTemplateType' {} a -> s {defaultEmailOption = a} :: VerificationMessageTemplateType)

-- | The subject line for the email message template for sending a
-- confirmation link to the user. EmailSubjectByLink is allowed only
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailSubjectByLink :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Core.Text)
verificationMessageTemplateType_emailSubjectByLink = Lens.lens (\VerificationMessageTemplateType' {emailSubjectByLink} -> emailSubjectByLink) (\s@VerificationMessageTemplateType' {} a -> s {emailSubjectByLink = a} :: VerificationMessageTemplateType)

-- | The subject line for the email message template. EmailSubject is allowed
-- only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailSubject :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Core.Text)
verificationMessageTemplateType_emailSubject = Lens.lens (\VerificationMessageTemplateType' {emailSubject} -> emailSubject) (\s@VerificationMessageTemplateType' {} a -> s {emailSubject = a} :: VerificationMessageTemplateType)

-- | The email message template. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailMessage :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Core.Text)
verificationMessageTemplateType_emailMessage = Lens.lens (\VerificationMessageTemplateType' {emailMessage} -> emailMessage) (\s@VerificationMessageTemplateType' {} a -> s {emailMessage = a} :: VerificationMessageTemplateType)

-- | The email message template for sending a confirmation link to the user.
-- EmailMessageByLink is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
verificationMessageTemplateType_emailMessageByLink :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Core.Text)
verificationMessageTemplateType_emailMessageByLink = Lens.lens (\VerificationMessageTemplateType' {emailMessageByLink} -> emailMessageByLink) (\s@VerificationMessageTemplateType' {} a -> s {emailMessageByLink = a} :: VerificationMessageTemplateType)

-- | The SMS message template.
verificationMessageTemplateType_smsMessage :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Core.Text)
verificationMessageTemplateType_smsMessage = Lens.lens (\VerificationMessageTemplateType' {smsMessage} -> smsMessage) (\s@VerificationMessageTemplateType' {} a -> s {smsMessage = a} :: VerificationMessageTemplateType)

instance
  Core.FromJSON
    VerificationMessageTemplateType
  where
  parseJSON =
    Core.withObject
      "VerificationMessageTemplateType"
      ( \x ->
          VerificationMessageTemplateType'
            Core.<$> (x Core..:? "DefaultEmailOption")
            Core.<*> (x Core..:? "EmailSubjectByLink")
            Core.<*> (x Core..:? "EmailSubject")
            Core.<*> (x Core..:? "EmailMessage")
            Core.<*> (x Core..:? "EmailMessageByLink")
            Core.<*> (x Core..:? "SmsMessage")
      )

instance
  Core.Hashable
    VerificationMessageTemplateType

instance Core.NFData VerificationMessageTemplateType

instance Core.ToJSON VerificationMessageTemplateType where
  toJSON VerificationMessageTemplateType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DefaultEmailOption" Core..=)
              Core.<$> defaultEmailOption,
            ("EmailSubjectByLink" Core..=)
              Core.<$> emailSubjectByLink,
            ("EmailSubject" Core..=) Core.<$> emailSubject,
            ("EmailMessage" Core..=) Core.<$> emailMessage,
            ("EmailMessageByLink" Core..=)
              Core.<$> emailMessageByLink,
            ("SmsMessage" Core..=) Core.<$> smsMessage
          ]
      )
