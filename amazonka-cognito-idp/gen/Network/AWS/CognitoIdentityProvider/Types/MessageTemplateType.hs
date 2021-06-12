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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The message template structure.
--
-- /See:/ 'newMessageTemplateType' smart constructor.
data MessageTemplateType = MessageTemplateType'
  { -- | The subject line for email messages. EmailSubject is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailSubject :: Core.Maybe Core.Text,
    -- | The message template for email messages. EmailMessage is allowed only if
    -- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
    -- is DEVELOPER.
    emailMessage :: Core.Maybe Core.Text,
    -- | The message template for SMS messages.
    sMSMessage :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'emailMessage', 'messageTemplateType_emailMessage' - The message template for email messages. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
--
-- 'sMSMessage', 'messageTemplateType_sMSMessage' - The message template for SMS messages.
newMessageTemplateType ::
  MessageTemplateType
newMessageTemplateType =
  MessageTemplateType'
    { emailSubject = Core.Nothing,
      emailMessage = Core.Nothing,
      sMSMessage = Core.Nothing
    }

-- | The subject line for email messages. EmailSubject is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
messageTemplateType_emailSubject :: Lens.Lens' MessageTemplateType (Core.Maybe Core.Text)
messageTemplateType_emailSubject = Lens.lens (\MessageTemplateType' {emailSubject} -> emailSubject) (\s@MessageTemplateType' {} a -> s {emailSubject = a} :: MessageTemplateType)

-- | The message template for email messages. EmailMessage is allowed only if
-- <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount>
-- is DEVELOPER.
messageTemplateType_emailMessage :: Lens.Lens' MessageTemplateType (Core.Maybe Core.Text)
messageTemplateType_emailMessage = Lens.lens (\MessageTemplateType' {emailMessage} -> emailMessage) (\s@MessageTemplateType' {} a -> s {emailMessage = a} :: MessageTemplateType)

-- | The message template for SMS messages.
messageTemplateType_sMSMessage :: Lens.Lens' MessageTemplateType (Core.Maybe Core.Text)
messageTemplateType_sMSMessage = Lens.lens (\MessageTemplateType' {sMSMessage} -> sMSMessage) (\s@MessageTemplateType' {} a -> s {sMSMessage = a} :: MessageTemplateType)

instance Core.FromJSON MessageTemplateType where
  parseJSON =
    Core.withObject
      "MessageTemplateType"
      ( \x ->
          MessageTemplateType'
            Core.<$> (x Core..:? "EmailSubject")
            Core.<*> (x Core..:? "EmailMessage")
            Core.<*> (x Core..:? "SMSMessage")
      )

instance Core.Hashable MessageTemplateType

instance Core.NFData MessageTemplateType

instance Core.ToJSON MessageTemplateType where
  toJSON MessageTemplateType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EmailSubject" Core..=) Core.<$> emailSubject,
            ("EmailMessage" Core..=) Core.<$> emailMessage,
            ("SMSMessage" Core..=) Core.<$> sMSMessage
          ]
      )
