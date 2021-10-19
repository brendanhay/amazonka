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
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the default settings and content for a one-time email message
-- that\'s sent directly to an endpoint.
--
-- /See:/ 'newEmailMessage' smart constructor.
data EmailMessage = EmailMessage'
  { -- | The default message variables to use in the email message. You can
    -- override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The body of the email message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The verified email address to send the email message from. The default
    -- value is the FromAddress specified for the email channel.
    fromAddress :: Prelude.Maybe Prelude.Text,
    -- | The email message, represented as a raw MIME message.
    rawEmail :: Prelude.Maybe RawEmail,
    -- | The email address to forward bounces and complaints to, if feedback
    -- forwarding is enabled.
    feedbackForwardingAddress :: Prelude.Maybe Prelude.Text,
    -- | The email message, composed of a subject, a text part, and an HTML part.
    simpleEmail :: Prelude.Maybe SimpleEmail,
    -- | The reply-to email address(es) for the email message. If a recipient
    -- replies to the email, each reply-to address receives the reply.
    replyToAddresses :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailMessage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'substitutions', 'emailMessage_substitutions' - The default message variables to use in the email message. You can
-- override the default variables with individual address variables.
--
-- 'body', 'emailMessage_body' - The body of the email message.
--
-- 'fromAddress', 'emailMessage_fromAddress' - The verified email address to send the email message from. The default
-- value is the FromAddress specified for the email channel.
--
-- 'rawEmail', 'emailMessage_rawEmail' - The email message, represented as a raw MIME message.
--
-- 'feedbackForwardingAddress', 'emailMessage_feedbackForwardingAddress' - The email address to forward bounces and complaints to, if feedback
-- forwarding is enabled.
--
-- 'simpleEmail', 'emailMessage_simpleEmail' - The email message, composed of a subject, a text part, and an HTML part.
--
-- 'replyToAddresses', 'emailMessage_replyToAddresses' - The reply-to email address(es) for the email message. If a recipient
-- replies to the email, each reply-to address receives the reply.
newEmailMessage ::
  EmailMessage
newEmailMessage =
  EmailMessage'
    { substitutions = Prelude.Nothing,
      body = Prelude.Nothing,
      fromAddress = Prelude.Nothing,
      rawEmail = Prelude.Nothing,
      feedbackForwardingAddress = Prelude.Nothing,
      simpleEmail = Prelude.Nothing,
      replyToAddresses = Prelude.Nothing
    }

-- | The default message variables to use in the email message. You can
-- override the default variables with individual address variables.
emailMessage_substitutions :: Lens.Lens' EmailMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
emailMessage_substitutions = Lens.lens (\EmailMessage' {substitutions} -> substitutions) (\s@EmailMessage' {} a -> s {substitutions = a} :: EmailMessage) Prelude.. Lens.mapping Lens.coerced

-- | The body of the email message.
emailMessage_body :: Lens.Lens' EmailMessage (Prelude.Maybe Prelude.Text)
emailMessage_body = Lens.lens (\EmailMessage' {body} -> body) (\s@EmailMessage' {} a -> s {body = a} :: EmailMessage)

-- | The verified email address to send the email message from. The default
-- value is the FromAddress specified for the email channel.
emailMessage_fromAddress :: Lens.Lens' EmailMessage (Prelude.Maybe Prelude.Text)
emailMessage_fromAddress = Lens.lens (\EmailMessage' {fromAddress} -> fromAddress) (\s@EmailMessage' {} a -> s {fromAddress = a} :: EmailMessage)

-- | The email message, represented as a raw MIME message.
emailMessage_rawEmail :: Lens.Lens' EmailMessage (Prelude.Maybe RawEmail)
emailMessage_rawEmail = Lens.lens (\EmailMessage' {rawEmail} -> rawEmail) (\s@EmailMessage' {} a -> s {rawEmail = a} :: EmailMessage)

-- | The email address to forward bounces and complaints to, if feedback
-- forwarding is enabled.
emailMessage_feedbackForwardingAddress :: Lens.Lens' EmailMessage (Prelude.Maybe Prelude.Text)
emailMessage_feedbackForwardingAddress = Lens.lens (\EmailMessage' {feedbackForwardingAddress} -> feedbackForwardingAddress) (\s@EmailMessage' {} a -> s {feedbackForwardingAddress = a} :: EmailMessage)

-- | The email message, composed of a subject, a text part, and an HTML part.
emailMessage_simpleEmail :: Lens.Lens' EmailMessage (Prelude.Maybe SimpleEmail)
emailMessage_simpleEmail = Lens.lens (\EmailMessage' {simpleEmail} -> simpleEmail) (\s@EmailMessage' {} a -> s {simpleEmail = a} :: EmailMessage)

-- | The reply-to email address(es) for the email message. If a recipient
-- replies to the email, each reply-to address receives the reply.
emailMessage_replyToAddresses :: Lens.Lens' EmailMessage (Prelude.Maybe [Prelude.Text])
emailMessage_replyToAddresses = Lens.lens (\EmailMessage' {replyToAddresses} -> replyToAddresses) (\s@EmailMessage' {} a -> s {replyToAddresses = a} :: EmailMessage) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EmailMessage

instance Prelude.NFData EmailMessage

instance Core.ToJSON EmailMessage where
  toJSON EmailMessage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Substitutions" Core..=) Prelude.<$> substitutions,
            ("Body" Core..=) Prelude.<$> body,
            ("FromAddress" Core..=) Prelude.<$> fromAddress,
            ("RawEmail" Core..=) Prelude.<$> rawEmail,
            ("FeedbackForwardingAddress" Core..=)
              Prelude.<$> feedbackForwardingAddress,
            ("SimpleEmail" Core..=) Prelude.<$> simpleEmail,
            ("ReplyToAddresses" Core..=)
              Prelude.<$> replyToAddresses
          ]
      )
