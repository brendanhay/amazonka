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
    mttEmailSubject,
    mttSMSMessage,
    mttEmailMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The message template structure.
--
-- /See:/ 'mkMessageTemplateType' smart constructor.
data MessageTemplateType = MessageTemplateType'
  { emailSubject ::
      Lude.Maybe Lude.Text,
    sMSMessage :: Lude.Maybe Lude.Text,
    emailMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MessageTemplateType' with the minimum fields required to make a request.
--
-- * 'emailMessage' - The message template for email messages. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'emailSubject' - The subject line for email messages. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'sMSMessage' - The message template for SMS messages.
mkMessageTemplateType ::
  MessageTemplateType
mkMessageTemplateType =
  MessageTemplateType'
    { emailSubject = Lude.Nothing,
      sMSMessage = Lude.Nothing,
      emailMessage = Lude.Nothing
    }

-- | The subject line for email messages. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mttEmailSubject :: Lens.Lens' MessageTemplateType (Lude.Maybe Lude.Text)
mttEmailSubject = Lens.lens (emailSubject :: MessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {emailSubject = a} :: MessageTemplateType)
{-# DEPRECATED mttEmailSubject "Use generic-lens or generic-optics with 'emailSubject' instead." #-}

-- | The message template for SMS messages.
--
-- /Note:/ Consider using 'sMSMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mttSMSMessage :: Lens.Lens' MessageTemplateType (Lude.Maybe Lude.Text)
mttSMSMessage = Lens.lens (sMSMessage :: MessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {sMSMessage = a} :: MessageTemplateType)
{-# DEPRECATED mttSMSMessage "Use generic-lens or generic-optics with 'sMSMessage' instead." #-}

-- | The message template for email messages. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mttEmailMessage :: Lens.Lens' MessageTemplateType (Lude.Maybe Lude.Text)
mttEmailMessage = Lens.lens (emailMessage :: MessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {emailMessage = a} :: MessageTemplateType)
{-# DEPRECATED mttEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

instance Lude.FromJSON MessageTemplateType where
  parseJSON =
    Lude.withObject
      "MessageTemplateType"
      ( \x ->
          MessageTemplateType'
            Lude.<$> (x Lude..:? "EmailSubject")
            Lude.<*> (x Lude..:? "SMSMessage")
            Lude.<*> (x Lude..:? "EmailMessage")
      )

instance Lude.ToJSON MessageTemplateType where
  toJSON MessageTemplateType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EmailSubject" Lude..=) Lude.<$> emailSubject,
            ("SMSMessage" Lude..=) Lude.<$> sMSMessage,
            ("EmailMessage" Lude..=) Lude.<$> emailMessage
          ]
      )
