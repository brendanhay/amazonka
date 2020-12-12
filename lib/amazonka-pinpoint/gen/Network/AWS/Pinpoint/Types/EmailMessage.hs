{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EmailMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailMessage
  ( EmailMessage (..),

    -- * Smart constructor
    mkEmailMessage,

    -- * Lenses
    emSubstitutions,
    emBody,
    emFromAddress,
    emRawEmail,
    emFeedbackForwardingAddress,
    emSimpleEmail,
    emReplyToAddresses,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RawEmail
import Network.AWS.Pinpoint.Types.SimpleEmail
import qualified Network.AWS.Prelude as Lude

-- | Specifies the default settings and content for a one-time email message that's sent directly to an endpoint.
--
-- /See:/ 'mkEmailMessage' smart constructor.
data EmailMessage = EmailMessage'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    body :: Lude.Maybe Lude.Text,
    fromAddress :: Lude.Maybe Lude.Text,
    rawEmail :: Lude.Maybe RawEmail,
    feedbackForwardingAddress :: Lude.Maybe Lude.Text,
    simpleEmail :: Lude.Maybe SimpleEmail,
    replyToAddresses :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EmailMessage' with the minimum fields required to make a request.
--
-- * 'body' - The body of the email message.
-- * 'feedbackForwardingAddress' - The email address to forward bounces and complaints to, if feedback forwarding is enabled.
-- * 'fromAddress' - The verified email address to send the email message from. The default value is the FromAddress specified for the email channel.
-- * 'rawEmail' - The email message, represented as a raw MIME message.
-- * 'replyToAddresses' - The reply-to email address(es) for the email message. If a recipient replies to the email, each reply-to address receives the reply.
-- * 'simpleEmail' - The email message, composed of a subject, a text part, and an HTML part.
-- * 'substitutions' - The default message variables to use in the email message. You can override the default variables with individual address variables.
mkEmailMessage ::
  EmailMessage
mkEmailMessage =
  EmailMessage'
    { substitutions = Lude.Nothing,
      body = Lude.Nothing,
      fromAddress = Lude.Nothing,
      rawEmail = Lude.Nothing,
      feedbackForwardingAddress = Lude.Nothing,
      simpleEmail = Lude.Nothing,
      replyToAddresses = Lude.Nothing
    }

-- | The default message variables to use in the email message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emSubstitutions :: Lens.Lens' EmailMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
emSubstitutions = Lens.lens (substitutions :: EmailMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: EmailMessage)
{-# DEPRECATED emSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The body of the email message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emBody :: Lens.Lens' EmailMessage (Lude.Maybe Lude.Text)
emBody = Lens.lens (body :: EmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: EmailMessage)
{-# DEPRECATED emBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The verified email address to send the email message from. The default value is the FromAddress specified for the email channel.
--
-- /Note:/ Consider using 'fromAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emFromAddress :: Lens.Lens' EmailMessage (Lude.Maybe Lude.Text)
emFromAddress = Lens.lens (fromAddress :: EmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {fromAddress = a} :: EmailMessage)
{-# DEPRECATED emFromAddress "Use generic-lens or generic-optics with 'fromAddress' instead." #-}

-- | The email message, represented as a raw MIME message.
--
-- /Note:/ Consider using 'rawEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emRawEmail :: Lens.Lens' EmailMessage (Lude.Maybe RawEmail)
emRawEmail = Lens.lens (rawEmail :: EmailMessage -> Lude.Maybe RawEmail) (\s a -> s {rawEmail = a} :: EmailMessage)
{-# DEPRECATED emRawEmail "Use generic-lens or generic-optics with 'rawEmail' instead." #-}

-- | The email address to forward bounces and complaints to, if feedback forwarding is enabled.
--
-- /Note:/ Consider using 'feedbackForwardingAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emFeedbackForwardingAddress :: Lens.Lens' EmailMessage (Lude.Maybe Lude.Text)
emFeedbackForwardingAddress = Lens.lens (feedbackForwardingAddress :: EmailMessage -> Lude.Maybe Lude.Text) (\s a -> s {feedbackForwardingAddress = a} :: EmailMessage)
{-# DEPRECATED emFeedbackForwardingAddress "Use generic-lens or generic-optics with 'feedbackForwardingAddress' instead." #-}

-- | The email message, composed of a subject, a text part, and an HTML part.
--
-- /Note:/ Consider using 'simpleEmail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emSimpleEmail :: Lens.Lens' EmailMessage (Lude.Maybe SimpleEmail)
emSimpleEmail = Lens.lens (simpleEmail :: EmailMessage -> Lude.Maybe SimpleEmail) (\s a -> s {simpleEmail = a} :: EmailMessage)
{-# DEPRECATED emSimpleEmail "Use generic-lens or generic-optics with 'simpleEmail' instead." #-}

-- | The reply-to email address(es) for the email message. If a recipient replies to the email, each reply-to address receives the reply.
--
-- /Note:/ Consider using 'replyToAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emReplyToAddresses :: Lens.Lens' EmailMessage (Lude.Maybe [Lude.Text])
emReplyToAddresses = Lens.lens (replyToAddresses :: EmailMessage -> Lude.Maybe [Lude.Text]) (\s a -> s {replyToAddresses = a} :: EmailMessage)
{-# DEPRECATED emReplyToAddresses "Use generic-lens or generic-optics with 'replyToAddresses' instead." #-}

instance Lude.ToJSON EmailMessage where
  toJSON EmailMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("Body" Lude..=) Lude.<$> body,
            ("FromAddress" Lude..=) Lude.<$> fromAddress,
            ("RawEmail" Lude..=) Lude.<$> rawEmail,
            ("FeedbackForwardingAddress" Lude..=)
              Lude.<$> feedbackForwardingAddress,
            ("SimpleEmail" Lude..=) Lude.<$> simpleEmail,
            ("ReplyToAddresses" Lude..=) Lude.<$> replyToAddresses
          ]
      )
