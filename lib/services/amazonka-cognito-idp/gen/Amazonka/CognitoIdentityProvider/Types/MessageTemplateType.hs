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
-- Module      : Amazonka.CognitoIdentityProvider.Types.MessageTemplateType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.MessageTemplateType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The message template structure.
--
-- /See:/ 'newMessageTemplateType' smart constructor.
data MessageTemplateType = MessageTemplateType'
  { -- | The subject line for email messages. EmailSubject is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailSubject :: Prelude.Maybe Prelude.Text,
    -- | The message template for SMS messages.
    sMSMessage :: Prelude.Maybe Prelude.Text,
    -- | The message template for email messages. EmailMessage is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MessageTemplateType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailSubject', 'messageTemplateType_emailSubject' - The subject line for email messages. EmailSubject is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'sMSMessage', 'messageTemplateType_sMSMessage' - The message template for SMS messages.
--
-- 'emailMessage', 'messageTemplateType_emailMessage' - The message template for email messages. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
newMessageTemplateType ::
  MessageTemplateType
newMessageTemplateType =
  MessageTemplateType'
    { emailSubject =
        Prelude.Nothing,
      sMSMessage = Prelude.Nothing,
      emailMessage = Prelude.Nothing
    }

-- | The subject line for email messages. EmailSubject is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
messageTemplateType_emailSubject :: Lens.Lens' MessageTemplateType (Prelude.Maybe Prelude.Text)
messageTemplateType_emailSubject = Lens.lens (\MessageTemplateType' {emailSubject} -> emailSubject) (\s@MessageTemplateType' {} a -> s {emailSubject = a} :: MessageTemplateType)

-- | The message template for SMS messages.
messageTemplateType_sMSMessage :: Lens.Lens' MessageTemplateType (Prelude.Maybe Prelude.Text)
messageTemplateType_sMSMessage = Lens.lens (\MessageTemplateType' {sMSMessage} -> sMSMessage) (\s@MessageTemplateType' {} a -> s {sMSMessage = a} :: MessageTemplateType)

-- | The message template for email messages. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
messageTemplateType_emailMessage :: Lens.Lens' MessageTemplateType (Prelude.Maybe Prelude.Text)
messageTemplateType_emailMessage = Lens.lens (\MessageTemplateType' {emailMessage} -> emailMessage) (\s@MessageTemplateType' {} a -> s {emailMessage = a} :: MessageTemplateType)

instance Data.FromJSON MessageTemplateType where
  parseJSON =
    Data.withObject
      "MessageTemplateType"
      ( \x ->
          MessageTemplateType'
            Prelude.<$> (x Data..:? "EmailSubject")
            Prelude.<*> (x Data..:? "SMSMessage")
            Prelude.<*> (x Data..:? "EmailMessage")
      )

instance Prelude.Hashable MessageTemplateType where
  hashWithSalt _salt MessageTemplateType' {..} =
    _salt `Prelude.hashWithSalt` emailSubject
      `Prelude.hashWithSalt` sMSMessage
      `Prelude.hashWithSalt` emailMessage

instance Prelude.NFData MessageTemplateType where
  rnf MessageTemplateType' {..} =
    Prelude.rnf emailSubject
      `Prelude.seq` Prelude.rnf sMSMessage
      `Prelude.seq` Prelude.rnf emailMessage

instance Data.ToJSON MessageTemplateType where
  toJSON MessageTemplateType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EmailSubject" Data..=) Prelude.<$> emailSubject,
            ("SMSMessage" Data..=) Prelude.<$> sMSMessage,
            ("EmailMessage" Data..=) Prelude.<$> emailMessage
          ]
      )
