{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
  ( VerificationMessageTemplateType (..),

    -- * Smart constructor
    mkVerificationMessageTemplateType,

    -- * Lenses
    vmttDefaultEmailOption,
    vmttEmailSubject,
    vmttEmailSubjectByLink,
    vmttSmsMessage,
    vmttEmailMessageByLink,
    vmttEmailMessage,
  )
where

import Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The template for verification messages.
--
-- /See:/ 'mkVerificationMessageTemplateType' smart constructor.
data VerificationMessageTemplateType = VerificationMessageTemplateType'
  { defaultEmailOption ::
      Lude.Maybe
        DefaultEmailOptionType,
    emailSubject ::
      Lude.Maybe Lude.Text,
    emailSubjectByLink ::
      Lude.Maybe Lude.Text,
    smsMessage ::
      Lude.Maybe Lude.Text,
    emailMessageByLink ::
      Lude.Maybe Lude.Text,
    emailMessage ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VerificationMessageTemplateType' with the minimum fields required to make a request.
--
-- * 'defaultEmailOption' - The default email option.
-- * 'emailMessage' - The email message template. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'emailMessageByLink' - The email message template for sending a confirmation link to the user. EmailMessageByLink is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'emailSubject' - The subject line for the email message template. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'emailSubjectByLink' - The subject line for the email message template for sending a confirmation link to the user. EmailSubjectByLink is allowed only <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
-- * 'smsMessage' - The SMS message template.
mkVerificationMessageTemplateType ::
  VerificationMessageTemplateType
mkVerificationMessageTemplateType =
  VerificationMessageTemplateType'
    { defaultEmailOption =
        Lude.Nothing,
      emailSubject = Lude.Nothing,
      emailSubjectByLink = Lude.Nothing,
      smsMessage = Lude.Nothing,
      emailMessageByLink = Lude.Nothing,
      emailMessage = Lude.Nothing
    }

-- | The default email option.
--
-- /Note:/ Consider using 'defaultEmailOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttDefaultEmailOption :: Lens.Lens' VerificationMessageTemplateType (Lude.Maybe DefaultEmailOptionType)
vmttDefaultEmailOption = Lens.lens (defaultEmailOption :: VerificationMessageTemplateType -> Lude.Maybe DefaultEmailOptionType) (\s a -> s {defaultEmailOption = a} :: VerificationMessageTemplateType)
{-# DEPRECATED vmttDefaultEmailOption "Use generic-lens or generic-optics with 'defaultEmailOption' instead." #-}

-- | The subject line for the email message template. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailSubject :: Lens.Lens' VerificationMessageTemplateType (Lude.Maybe Lude.Text)
vmttEmailSubject = Lens.lens (emailSubject :: VerificationMessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {emailSubject = a} :: VerificationMessageTemplateType)
{-# DEPRECATED vmttEmailSubject "Use generic-lens or generic-optics with 'emailSubject' instead." #-}

-- | The subject line for the email message template for sending a confirmation link to the user. EmailSubjectByLink is allowed only <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailSubjectByLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailSubjectByLink :: Lens.Lens' VerificationMessageTemplateType (Lude.Maybe Lude.Text)
vmttEmailSubjectByLink = Lens.lens (emailSubjectByLink :: VerificationMessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {emailSubjectByLink = a} :: VerificationMessageTemplateType)
{-# DEPRECATED vmttEmailSubjectByLink "Use generic-lens or generic-optics with 'emailSubjectByLink' instead." #-}

-- | The SMS message template.
--
-- /Note:/ Consider using 'smsMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttSmsMessage :: Lens.Lens' VerificationMessageTemplateType (Lude.Maybe Lude.Text)
vmttSmsMessage = Lens.lens (smsMessage :: VerificationMessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {smsMessage = a} :: VerificationMessageTemplateType)
{-# DEPRECATED vmttSmsMessage "Use generic-lens or generic-optics with 'smsMessage' instead." #-}

-- | The email message template for sending a confirmation link to the user. EmailMessageByLink is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailMessageByLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailMessageByLink :: Lens.Lens' VerificationMessageTemplateType (Lude.Maybe Lude.Text)
vmttEmailMessageByLink = Lens.lens (emailMessageByLink :: VerificationMessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {emailMessageByLink = a} :: VerificationMessageTemplateType)
{-# DEPRECATED vmttEmailMessageByLink "Use generic-lens or generic-optics with 'emailMessageByLink' instead." #-}

-- | The email message template. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailMessage :: Lens.Lens' VerificationMessageTemplateType (Lude.Maybe Lude.Text)
vmttEmailMessage = Lens.lens (emailMessage :: VerificationMessageTemplateType -> Lude.Maybe Lude.Text) (\s a -> s {emailMessage = a} :: VerificationMessageTemplateType)
{-# DEPRECATED vmttEmailMessage "Use generic-lens or generic-optics with 'emailMessage' instead." #-}

instance Lude.FromJSON VerificationMessageTemplateType where
  parseJSON =
    Lude.withObject
      "VerificationMessageTemplateType"
      ( \x ->
          VerificationMessageTemplateType'
            Lude.<$> (x Lude..:? "DefaultEmailOption")
            Lude.<*> (x Lude..:? "EmailSubject")
            Lude.<*> (x Lude..:? "EmailSubjectByLink")
            Lude.<*> (x Lude..:? "SmsMessage")
            Lude.<*> (x Lude..:? "EmailMessageByLink")
            Lude.<*> (x Lude..:? "EmailMessage")
      )

instance Lude.ToJSON VerificationMessageTemplateType where
  toJSON VerificationMessageTemplateType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DefaultEmailOption" Lude..=) Lude.<$> defaultEmailOption,
            ("EmailSubject" Lude..=) Lude.<$> emailSubject,
            ("EmailSubjectByLink" Lude..=) Lude.<$> emailSubjectByLink,
            ("SmsMessage" Lude..=) Lude.<$> smsMessage,
            ("EmailMessageByLink" Lude..=) Lude.<$> emailMessageByLink,
            ("EmailMessage" Lude..=) Lude.<$> emailMessage
          ]
      )
