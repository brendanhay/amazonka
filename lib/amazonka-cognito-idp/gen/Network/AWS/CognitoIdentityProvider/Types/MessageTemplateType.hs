{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.MessageTemplateType
  ( MessageTemplateType (..),

    -- * Smart constructor
    mkMessageTemplateType,

    -- * Lenses
    mttEmailMessage,
    mttEmailSubject,
    mttSMSMessage,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.EmailMessage as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailSubject as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SmsVerificationMessageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The message template structure.
--
-- /See:/ 'mkMessageTemplateType' smart constructor.
data MessageTemplateType = MessageTemplateType'
  { -- | The message template for email messages. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
    emailMessage :: Core.Maybe Types.EmailMessage,
    -- | The subject line for email messages. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
    emailSubject :: Core.Maybe Types.EmailSubject,
    -- | The message template for SMS messages.
    sMSMessage :: Core.Maybe Types.SmsVerificationMessageType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'MessageTemplateType' value with any optional fields omitted.
mkMessageTemplateType ::
  MessageTemplateType
mkMessageTemplateType =
  MessageTemplateType'
    { emailMessage = Core.Nothing,
      emailSubject = Core.Nothing,
      sMSMessage = Core.Nothing
    }

-- | The message template for email messages. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mttEmailMessage :: Lens.Lens' MessageTemplateType (Core.Maybe Types.EmailMessage)
mttEmailMessage = Lens.field @"emailMessage"
{-# DEPRECATED mttEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

-- | The subject line for email messages. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mttEmailSubject :: Lens.Lens' MessageTemplateType (Core.Maybe Types.EmailSubject)
mttEmailSubject = Lens.field @"emailSubject"
{-# DEPRECATED mttEmailSubject "Use generic-lens or generic-optics with 'emailSubject' instead." #-}

-- | The message template for SMS messages.
--
-- /Note:/ Consider using 'sMSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mttSMSMessage :: Lens.Lens' MessageTemplateType (Core.Maybe Types.SmsVerificationMessageType)
mttSMSMessage = Lens.field @"sMSMessage"
{-# DEPRECATED mttSMSMessage "Use generic-lens or generic-optics with 'sMSMessage' instead." #-}

instance Core.FromJSON MessageTemplateType where
  toJSON MessageTemplateType {..} =
    Core.object
      ( Core.catMaybes
          [ ("EmailMessage" Core..=) Core.<$> emailMessage,
            ("EmailSubject" Core..=) Core.<$> emailSubject,
            ("SMSMessage" Core..=) Core.<$> sMSMessage
          ]
      )

instance Core.FromJSON MessageTemplateType where
  parseJSON =
    Core.withObject "MessageTemplateType" Core.$
      \x ->
        MessageTemplateType'
          Core.<$> (x Core..:? "EmailMessage")
          Core.<*> (x Core..:? "EmailSubject")
          Core.<*> (x Core..:? "SMSMessage")
