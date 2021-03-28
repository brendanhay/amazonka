{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.VerificationMessageTemplateType
  ( VerificationMessageTemplateType (..)
  -- * Smart constructor
  , mkVerificationMessageTemplateType
  -- * Lenses
  , vmttDefaultEmailOption
  , vmttEmailMessage
  , vmttEmailMessageByLink
  , vmttEmailSubject
  , vmttEmailSubjectByLink
  , vmttSmsMessage
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.DefaultEmailOptionType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailMessage as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailSubject as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailVerificationMessageByLinkType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.EmailVerificationSubjectByLinkType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.SmsVerificationMessageType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The template for verification messages.
--
-- /See:/ 'mkVerificationMessageTemplateType' smart constructor.
data VerificationMessageTemplateType = VerificationMessageTemplateType'
  { defaultEmailOption :: Core.Maybe Types.DefaultEmailOptionType
    -- ^ The default email option.
  , emailMessage :: Core.Maybe Types.EmailMessage
    -- ^ The email message template. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER. 
  , emailMessageByLink :: Core.Maybe Types.EmailVerificationMessageByLinkType
    -- ^ The email message template for sending a confirmation link to the user. EmailMessageByLink is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
  , emailSubject :: Core.Maybe Types.EmailSubject
    -- ^ The subject line for the email message template. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER. 
  , emailSubjectByLink :: Core.Maybe Types.EmailVerificationSubjectByLinkType
    -- ^ The subject line for the email message template for sending a confirmation link to the user. EmailSubjectByLink is allowed only <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
  , smsMessage :: Core.Maybe Types.SmsVerificationMessageType
    -- ^ The SMS message template.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VerificationMessageTemplateType' value with any optional fields omitted.
mkVerificationMessageTemplateType
    :: VerificationMessageTemplateType
mkVerificationMessageTemplateType
  = VerificationMessageTemplateType'{defaultEmailOption =
                                       Core.Nothing,
                                     emailMessage = Core.Nothing, emailMessageByLink = Core.Nothing,
                                     emailSubject = Core.Nothing, emailSubjectByLink = Core.Nothing,
                                     smsMessage = Core.Nothing}

-- | The default email option.
--
-- /Note:/ Consider using 'defaultEmailOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttDefaultEmailOption :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Types.DefaultEmailOptionType)
vmttDefaultEmailOption = Lens.field @"defaultEmailOption"
{-# INLINEABLE vmttDefaultEmailOption #-}
{-# DEPRECATED defaultEmailOption "Use generic-lens or generic-optics with 'defaultEmailOption' instead"  #-}

-- | The email message template. EmailMessage is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER. 
--
-- /Note:/ Consider using 'emailMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailMessage :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Types.EmailMessage)
vmttEmailMessage = Lens.field @"emailMessage"
{-# INLINEABLE vmttEmailMessage #-}
{-# DEPRECATED emailMessage "Use generic-lens or generic-optics with 'emailMessage' instead"  #-}

-- | The email message template for sending a confirmation link to the user. EmailMessageByLink is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailMessageByLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailMessageByLink :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Types.EmailVerificationMessageByLinkType)
vmttEmailMessageByLink = Lens.field @"emailMessageByLink"
{-# INLINEABLE vmttEmailMessageByLink #-}
{-# DEPRECATED emailMessageByLink "Use generic-lens or generic-optics with 'emailMessageByLink' instead"  #-}

-- | The subject line for the email message template. EmailSubject is allowed only if <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER. 
--
-- /Note:/ Consider using 'emailSubject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailSubject :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Types.EmailSubject)
vmttEmailSubject = Lens.field @"emailSubject"
{-# INLINEABLE vmttEmailSubject #-}
{-# DEPRECATED emailSubject "Use generic-lens or generic-optics with 'emailSubject' instead"  #-}

-- | The subject line for the email message template for sending a confirmation link to the user. EmailSubjectByLink is allowed only <https://docs.aws.amazon.com/cognito-user-identity-pools/latest/APIReference/API_EmailConfigurationType.html#CognitoUserPools-Type-EmailConfigurationType-EmailSendingAccount EmailSendingAccount> is DEVELOPER.
--
-- /Note:/ Consider using 'emailSubjectByLink' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttEmailSubjectByLink :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Types.EmailVerificationSubjectByLinkType)
vmttEmailSubjectByLink = Lens.field @"emailSubjectByLink"
{-# INLINEABLE vmttEmailSubjectByLink #-}
{-# DEPRECATED emailSubjectByLink "Use generic-lens or generic-optics with 'emailSubjectByLink' instead"  #-}

-- | The SMS message template.
--
-- /Note:/ Consider using 'smsMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmttSmsMessage :: Lens.Lens' VerificationMessageTemplateType (Core.Maybe Types.SmsVerificationMessageType)
vmttSmsMessage = Lens.field @"smsMessage"
{-# INLINEABLE vmttSmsMessage #-}
{-# DEPRECATED smsMessage "Use generic-lens or generic-optics with 'smsMessage' instead"  #-}

instance Core.FromJSON VerificationMessageTemplateType where
        toJSON VerificationMessageTemplateType{..}
          = Core.object
              (Core.catMaybes
                 [("DefaultEmailOption" Core..=) Core.<$> defaultEmailOption,
                  ("EmailMessage" Core..=) Core.<$> emailMessage,
                  ("EmailMessageByLink" Core..=) Core.<$> emailMessageByLink,
                  ("EmailSubject" Core..=) Core.<$> emailSubject,
                  ("EmailSubjectByLink" Core..=) Core.<$> emailSubjectByLink,
                  ("SmsMessage" Core..=) Core.<$> smsMessage])

instance Core.FromJSON VerificationMessageTemplateType where
        parseJSON
          = Core.withObject "VerificationMessageTemplateType" Core.$
              \ x ->
                VerificationMessageTemplateType' Core.<$>
                  (x Core..:? "DefaultEmailOption") Core.<*>
                    x Core..:? "EmailMessage"
                    Core.<*> x Core..:? "EmailMessageByLink"
                    Core.<*> x Core..:? "EmailSubject"
                    Core.<*> x Core..:? "EmailSubjectByLink"
                    Core.<*> x Core..:? "SmsMessage"
