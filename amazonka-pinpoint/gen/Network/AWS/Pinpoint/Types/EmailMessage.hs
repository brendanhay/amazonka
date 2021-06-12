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
-- Module      : Network.AWS.Pinpoint.Types.EmailMessage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailMessage where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.RawEmail
import Network.AWS.Pinpoint.Types.SimpleEmail

-- | Specifies the default settings and content for a one-time email message
-- that\'s sent directly to an endpoint.
--
-- /See:/ 'newEmailMessage' smart constructor.
data EmailMessage = EmailMessage'
  { -- | The email address to forward bounces and complaints to, if feedback
    -- forwarding is enabled.
    feedbackForwardingAddress :: Core.Maybe Core.Text,
    -- | The email message, represented as a raw MIME message.
    rawEmail :: Core.Maybe RawEmail,
    -- | The body of the email message.
    body :: Core.Maybe Core.Text,
    -- | The email message, composed of a subject, a text part, and an HTML part.
    simpleEmail :: Core.Maybe SimpleEmail,
    -- | The default message variables to use in the email message. You can
    -- override the default variables with individual address variables.
    substitutions :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The reply-to email address(es) for the email message. If a recipient
    -- replies to the email, each reply-to address receives the reply.
    replyToAddresses :: Core.Maybe [Core.Text],
    -- | The verified email address to send the email message from. The default
    -- value is the FromAddress specified for the email channel.
    fromAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmailMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'feedbackForwardingAddress', 'emailMessage_feedbackForwardingAddress' - The email address to forward bounces and complaints to, if feedback
-- forwarding is enabled.
--
-- 'rawEmail', 'emailMessage_rawEmail' - The email message, represented as a raw MIME message.
--
-- 'body', 'emailMessage_body' - The body of the email message.
--
-- 'simpleEmail', 'emailMessage_simpleEmail' - The email message, composed of a subject, a text part, and an HTML part.
--
-- 'substitutions', 'emailMessage_substitutions' - The default message variables to use in the email message. You can
-- override the default variables with individual address variables.
--
-- 'replyToAddresses', 'emailMessage_replyToAddresses' - The reply-to email address(es) for the email message. If a recipient
-- replies to the email, each reply-to address receives the reply.
--
-- 'fromAddress', 'emailMessage_fromAddress' - The verified email address to send the email message from. The default
-- value is the FromAddress specified for the email channel.
newEmailMessage ::
  EmailMessage
newEmailMessage =
  EmailMessage'
    { feedbackForwardingAddress =
        Core.Nothing,
      rawEmail = Core.Nothing,
      body = Core.Nothing,
      simpleEmail = Core.Nothing,
      substitutions = Core.Nothing,
      replyToAddresses = Core.Nothing,
      fromAddress = Core.Nothing
    }

-- | The email address to forward bounces and complaints to, if feedback
-- forwarding is enabled.
emailMessage_feedbackForwardingAddress :: Lens.Lens' EmailMessage (Core.Maybe Core.Text)
emailMessage_feedbackForwardingAddress = Lens.lens (\EmailMessage' {feedbackForwardingAddress} -> feedbackForwardingAddress) (\s@EmailMessage' {} a -> s {feedbackForwardingAddress = a} :: EmailMessage)

-- | The email message, represented as a raw MIME message.
emailMessage_rawEmail :: Lens.Lens' EmailMessage (Core.Maybe RawEmail)
emailMessage_rawEmail = Lens.lens (\EmailMessage' {rawEmail} -> rawEmail) (\s@EmailMessage' {} a -> s {rawEmail = a} :: EmailMessage)

-- | The body of the email message.
emailMessage_body :: Lens.Lens' EmailMessage (Core.Maybe Core.Text)
emailMessage_body = Lens.lens (\EmailMessage' {body} -> body) (\s@EmailMessage' {} a -> s {body = a} :: EmailMessage)

-- | The email message, composed of a subject, a text part, and an HTML part.
emailMessage_simpleEmail :: Lens.Lens' EmailMessage (Core.Maybe SimpleEmail)
emailMessage_simpleEmail = Lens.lens (\EmailMessage' {simpleEmail} -> simpleEmail) (\s@EmailMessage' {} a -> s {simpleEmail = a} :: EmailMessage)

-- | The default message variables to use in the email message. You can
-- override the default variables with individual address variables.
emailMessage_substitutions :: Lens.Lens' EmailMessage (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
emailMessage_substitutions = Lens.lens (\EmailMessage' {substitutions} -> substitutions) (\s@EmailMessage' {} a -> s {substitutions = a} :: EmailMessage) Core.. Lens.mapping Lens._Coerce

-- | The reply-to email address(es) for the email message. If a recipient
-- replies to the email, each reply-to address receives the reply.
emailMessage_replyToAddresses :: Lens.Lens' EmailMessage (Core.Maybe [Core.Text])
emailMessage_replyToAddresses = Lens.lens (\EmailMessage' {replyToAddresses} -> replyToAddresses) (\s@EmailMessage' {} a -> s {replyToAddresses = a} :: EmailMessage) Core.. Lens.mapping Lens._Coerce

-- | The verified email address to send the email message from. The default
-- value is the FromAddress specified for the email channel.
emailMessage_fromAddress :: Lens.Lens' EmailMessage (Core.Maybe Core.Text)
emailMessage_fromAddress = Lens.lens (\EmailMessage' {fromAddress} -> fromAddress) (\s@EmailMessage' {} a -> s {fromAddress = a} :: EmailMessage)

instance Core.Hashable EmailMessage

instance Core.NFData EmailMessage

instance Core.ToJSON EmailMessage where
  toJSON EmailMessage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("FeedbackForwardingAddress" Core..=)
              Core.<$> feedbackForwardingAddress,
            ("RawEmail" Core..=) Core.<$> rawEmail,
            ("Body" Core..=) Core.<$> body,
            ("SimpleEmail" Core..=) Core.<$> simpleEmail,
            ("Substitutions" Core..=) Core.<$> substitutions,
            ("ReplyToAddresses" Core..=)
              Core.<$> replyToAddresses,
            ("FromAddress" Core..=) Core.<$> fromAddress
          ]
      )
