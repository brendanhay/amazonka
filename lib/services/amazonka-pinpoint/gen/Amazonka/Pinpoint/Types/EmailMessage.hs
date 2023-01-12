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
-- Module      : Amazonka.Pinpoint.Types.EmailMessage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EmailMessage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.RawEmail
import Amazonka.Pinpoint.Types.SimpleEmail
import qualified Amazonka.Prelude as Prelude

-- | Specifies the default settings and content for a one-time email message
-- that\'s sent directly to an endpoint.
--
-- /See:/ 'newEmailMessage' smart constructor.
data EmailMessage = EmailMessage'
  { -- | The body of the email message.
    body :: Prelude.Maybe Prelude.Text,
    -- | The email address to forward bounces and complaints to, if feedback
    -- forwarding is enabled.
    feedbackForwardingAddress :: Prelude.Maybe Prelude.Text,
    -- | The verified email address to send the email message from. The default
    -- value is the FromAddress specified for the email channel.
    fromAddress :: Prelude.Maybe Prelude.Text,
    -- | The email message, represented as a raw MIME message.
    rawEmail :: Prelude.Maybe RawEmail,
    -- | The reply-to email address(es) for the email message. If a recipient
    -- replies to the email, each reply-to address receives the reply.
    replyToAddresses :: Prelude.Maybe [Prelude.Text],
    -- | The email message, composed of a subject, a text part, and an HTML part.
    simpleEmail :: Prelude.Maybe SimpleEmail,
    -- | The default message variables to use in the email message. You can
    -- override the default variables with individual address variables.
    substitutions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text])
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
-- 'body', 'emailMessage_body' - The body of the email message.
--
-- 'feedbackForwardingAddress', 'emailMessage_feedbackForwardingAddress' - The email address to forward bounces and complaints to, if feedback
-- forwarding is enabled.
--
-- 'fromAddress', 'emailMessage_fromAddress' - The verified email address to send the email message from. The default
-- value is the FromAddress specified for the email channel.
--
-- 'rawEmail', 'emailMessage_rawEmail' - The email message, represented as a raw MIME message.
--
-- 'replyToAddresses', 'emailMessage_replyToAddresses' - The reply-to email address(es) for the email message. If a recipient
-- replies to the email, each reply-to address receives the reply.
--
-- 'simpleEmail', 'emailMessage_simpleEmail' - The email message, composed of a subject, a text part, and an HTML part.
--
-- 'substitutions', 'emailMessage_substitutions' - The default message variables to use in the email message. You can
-- override the default variables with individual address variables.
newEmailMessage ::
  EmailMessage
newEmailMessage =
  EmailMessage'
    { body = Prelude.Nothing,
      feedbackForwardingAddress = Prelude.Nothing,
      fromAddress = Prelude.Nothing,
      rawEmail = Prelude.Nothing,
      replyToAddresses = Prelude.Nothing,
      simpleEmail = Prelude.Nothing,
      substitutions = Prelude.Nothing
    }

-- | The body of the email message.
emailMessage_body :: Lens.Lens' EmailMessage (Prelude.Maybe Prelude.Text)
emailMessage_body = Lens.lens (\EmailMessage' {body} -> body) (\s@EmailMessage' {} a -> s {body = a} :: EmailMessage)

-- | The email address to forward bounces and complaints to, if feedback
-- forwarding is enabled.
emailMessage_feedbackForwardingAddress :: Lens.Lens' EmailMessage (Prelude.Maybe Prelude.Text)
emailMessage_feedbackForwardingAddress = Lens.lens (\EmailMessage' {feedbackForwardingAddress} -> feedbackForwardingAddress) (\s@EmailMessage' {} a -> s {feedbackForwardingAddress = a} :: EmailMessage)

-- | The verified email address to send the email message from. The default
-- value is the FromAddress specified for the email channel.
emailMessage_fromAddress :: Lens.Lens' EmailMessage (Prelude.Maybe Prelude.Text)
emailMessage_fromAddress = Lens.lens (\EmailMessage' {fromAddress} -> fromAddress) (\s@EmailMessage' {} a -> s {fromAddress = a} :: EmailMessage)

-- | The email message, represented as a raw MIME message.
emailMessage_rawEmail :: Lens.Lens' EmailMessage (Prelude.Maybe RawEmail)
emailMessage_rawEmail = Lens.lens (\EmailMessage' {rawEmail} -> rawEmail) (\s@EmailMessage' {} a -> s {rawEmail = a} :: EmailMessage)

-- | The reply-to email address(es) for the email message. If a recipient
-- replies to the email, each reply-to address receives the reply.
emailMessage_replyToAddresses :: Lens.Lens' EmailMessage (Prelude.Maybe [Prelude.Text])
emailMessage_replyToAddresses = Lens.lens (\EmailMessage' {replyToAddresses} -> replyToAddresses) (\s@EmailMessage' {} a -> s {replyToAddresses = a} :: EmailMessage) Prelude.. Lens.mapping Lens.coerced

-- | The email message, composed of a subject, a text part, and an HTML part.
emailMessage_simpleEmail :: Lens.Lens' EmailMessage (Prelude.Maybe SimpleEmail)
emailMessage_simpleEmail = Lens.lens (\EmailMessage' {simpleEmail} -> simpleEmail) (\s@EmailMessage' {} a -> s {simpleEmail = a} :: EmailMessage)

-- | The default message variables to use in the email message. You can
-- override the default variables with individual address variables.
emailMessage_substitutions :: Lens.Lens' EmailMessage (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
emailMessage_substitutions = Lens.lens (\EmailMessage' {substitutions} -> substitutions) (\s@EmailMessage' {} a -> s {substitutions = a} :: EmailMessage) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable EmailMessage where
  hashWithSalt _salt EmailMessage' {..} =
    _salt `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` feedbackForwardingAddress
      `Prelude.hashWithSalt` fromAddress
      `Prelude.hashWithSalt` rawEmail
      `Prelude.hashWithSalt` replyToAddresses
      `Prelude.hashWithSalt` simpleEmail
      `Prelude.hashWithSalt` substitutions

instance Prelude.NFData EmailMessage where
  rnf EmailMessage' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf feedbackForwardingAddress
      `Prelude.seq` Prelude.rnf fromAddress
      `Prelude.seq` Prelude.rnf rawEmail
      `Prelude.seq` Prelude.rnf replyToAddresses
      `Prelude.seq` Prelude.rnf simpleEmail
      `Prelude.seq` Prelude.rnf substitutions

instance Data.ToJSON EmailMessage where
  toJSON EmailMessage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("FeedbackForwardingAddress" Data..=)
              Prelude.<$> feedbackForwardingAddress,
            ("FromAddress" Data..=) Prelude.<$> fromAddress,
            ("RawEmail" Data..=) Prelude.<$> rawEmail,
            ("ReplyToAddresses" Data..=)
              Prelude.<$> replyToAddresses,
            ("SimpleEmail" Data..=) Prelude.<$> simpleEmail,
            ("Substitutions" Data..=) Prelude.<$> substitutions
          ]
      )
