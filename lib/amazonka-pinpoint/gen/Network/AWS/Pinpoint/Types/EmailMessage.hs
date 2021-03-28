{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EmailMessage
  ( EmailMessage (..)
  -- * Smart constructor
  , mkEmailMessage
  -- * Lenses
  , emBody
  , emFeedbackForwardingAddress
  , emFromAddress
  , emRawEmail
  , emReplyToAddresses
  , emSimpleEmail
  , emSubstitutions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.RawEmail as Types
import qualified Network.AWS.Pinpoint.Types.SimpleEmail as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies the default settings and content for a one-time email message that's sent directly to an endpoint.
--
-- /See:/ 'mkEmailMessage' smart constructor.
data EmailMessage = EmailMessage'
  { body :: Core.Maybe Core.Text
    -- ^ The body of the email message.
  , feedbackForwardingAddress :: Core.Maybe Core.Text
    -- ^ The email address to forward bounces and complaints to, if feedback forwarding is enabled.
  , fromAddress :: Core.Maybe Core.Text
    -- ^ The verified email address to send the email message from. The default value is the FromAddress specified for the email channel.
  , rawEmail :: Core.Maybe Types.RawEmail
    -- ^ The email message, represented as a raw MIME message.
  , replyToAddresses :: Core.Maybe [Core.Text]
    -- ^ The reply-to email address(es) for the email message. If a recipient replies to the email, each reply-to address receives the reply.
  , simpleEmail :: Core.Maybe Types.SimpleEmail
    -- ^ The email message, composed of a subject, a text part, and an HTML part.
  , substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text])
    -- ^ The default message variables to use in the email message. You can override the default variables with individual address variables.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EmailMessage' value with any optional fields omitted.
mkEmailMessage
    :: EmailMessage
mkEmailMessage
  = EmailMessage'{body = Core.Nothing,
                  feedbackForwardingAddress = Core.Nothing,
                  fromAddress = Core.Nothing, rawEmail = Core.Nothing,
                  replyToAddresses = Core.Nothing, simpleEmail = Core.Nothing,
                  substitutions = Core.Nothing}

-- | The body of the email message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emBody :: Lens.Lens' EmailMessage (Core.Maybe Core.Text)
emBody = Lens.field @"body"
{-# INLINEABLE emBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The email address to forward bounces and complaints to, if feedback forwarding is enabled.
--
-- /Note:/ Consider using 'feedbackForwardingAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emFeedbackForwardingAddress :: Lens.Lens' EmailMessage (Core.Maybe Core.Text)
emFeedbackForwardingAddress = Lens.field @"feedbackForwardingAddress"
{-# INLINEABLE emFeedbackForwardingAddress #-}
{-# DEPRECATED feedbackForwardingAddress "Use generic-lens or generic-optics with 'feedbackForwardingAddress' instead"  #-}

-- | The verified email address to send the email message from. The default value is the FromAddress specified for the email channel.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emFromAddress :: Lens.Lens' EmailMessage (Core.Maybe Core.Text)
emFromAddress = Lens.field @"fromAddress"
{-# INLINEABLE emFromAddress #-}
{-# DEPRECATED fromAddress "Use generic-lens or generic-optics with 'fromAddress' instead"  #-}

-- | The email message, represented as a raw MIME message.
--
-- /Note:/ Consider using 'rawEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emRawEmail :: Lens.Lens' EmailMessage (Core.Maybe Types.RawEmail)
emRawEmail = Lens.field @"rawEmail"
{-# INLINEABLE emRawEmail #-}
{-# DEPRECATED rawEmail "Use generic-lens or generic-optics with 'rawEmail' instead"  #-}

-- | The reply-to email address(es) for the email message. If a recipient replies to the email, each reply-to address receives the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emReplyToAddresses :: Lens.Lens' EmailMessage (Core.Maybe [Core.Text])
emReplyToAddresses = Lens.field @"replyToAddresses"
{-# INLINEABLE emReplyToAddresses #-}
{-# DEPRECATED replyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead"  #-}

-- | The email message, composed of a subject, a text part, and an HTML part.
--
-- /Note:/ Consider using 'simpleEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emSimpleEmail :: Lens.Lens' EmailMessage (Core.Maybe Types.SimpleEmail)
emSimpleEmail = Lens.field @"simpleEmail"
{-# INLINEABLE emSimpleEmail #-}
{-# DEPRECATED simpleEmail "Use generic-lens or generic-optics with 'simpleEmail' instead"  #-}

-- | The default message variables to use in the email message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emSubstitutions :: Lens.Lens' EmailMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
emSubstitutions = Lens.field @"substitutions"
{-# INLINEABLE emSubstitutions #-}
{-# DEPRECATED substitutions "Use generic-lens or generic-optics with 'substitutions' instead"  #-}

instance Core.FromJSON EmailMessage where
        toJSON EmailMessage{..}
          = Core.object
              (Core.catMaybes
                 [("Body" Core..=) Core.<$> body,
                  ("FeedbackForwardingAddress" Core..=) Core.<$>
                    feedbackForwardingAddress,
                  ("FromAddress" Core..=) Core.<$> fromAddress,
                  ("RawEmail" Core..=) Core.<$> rawEmail,
                  ("ReplyToAddresses" Core..=) Core.<$> replyToAddresses,
                  ("SimpleEmail" Core..=) Core.<$> simpleEmail,
                  ("Substitutions" Core..=) Core.<$> substitutions])
