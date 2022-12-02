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
-- Module      : Amazonka.PinpointEmail.Types.EmailContent
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.EmailContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types.Message
import Amazonka.PinpointEmail.Types.RawMessage
import Amazonka.PinpointEmail.Types.Template
import qualified Amazonka.Prelude as Prelude

-- | An object that defines the entire content of the email, including the
-- message headers and the body content. You can create a simple email
-- message, in which you specify the subject and the text and HTML versions
-- of the message body. You can also create raw messages, in which you
-- specify a complete MIME-formatted message. Raw messages can include
-- attachments and custom headers.
--
-- /See:/ 'newEmailContent' smart constructor.
data EmailContent = EmailContent'
  { -- | The simple email message. The message consists of a subject and a
    -- message body.
    simple :: Prelude.Maybe Message,
    -- | The raw email message. The message has to meet the following criteria:
    --
    -- -   The message has to contain a header and a body, separated by one
    --     blank line.
    --
    -- -   All of the required header fields must be present in the message.
    --
    -- -   Each part of a multipart MIME message must be formatted properly.
    --
    -- -   If you include attachments, they must be in a file format that
    --     Amazon Pinpoint supports.
    --
    -- -   The entire message must be Base64 encoded.
    --
    -- -   If any of the MIME parts in your message contain content that is
    --     outside of the 7-bit ASCII character range, you should encode that
    --     content to ensure that recipients\' email clients render the message
    --     properly.
    --
    -- -   The length of any single line of text in the message can\'t exceed
    --     1,000 characters. This restriction is defined in
    --     <https://tools.ietf.org/html/rfc5321 RFC 5321>.
    raw :: Prelude.Maybe RawMessage,
    -- | The template to use for the email message.
    template :: Prelude.Maybe Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'simple', 'emailContent_simple' - The simple email message. The message consists of a subject and a
-- message body.
--
-- 'raw', 'emailContent_raw' - The raw email message. The message has to meet the following criteria:
--
-- -   The message has to contain a header and a body, separated by one
--     blank line.
--
-- -   All of the required header fields must be present in the message.
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   If you include attachments, they must be in a file format that
--     Amazon Pinpoint supports.
--
-- -   The entire message must be Base64 encoded.
--
-- -   If any of the MIME parts in your message contain content that is
--     outside of the 7-bit ASCII character range, you should encode that
--     content to ensure that recipients\' email clients render the message
--     properly.
--
-- -   The length of any single line of text in the message can\'t exceed
--     1,000 characters. This restriction is defined in
--     <https://tools.ietf.org/html/rfc5321 RFC 5321>.
--
-- 'template', 'emailContent_template' - The template to use for the email message.
newEmailContent ::
  EmailContent
newEmailContent =
  EmailContent'
    { simple = Prelude.Nothing,
      raw = Prelude.Nothing,
      template = Prelude.Nothing
    }

-- | The simple email message. The message consists of a subject and a
-- message body.
emailContent_simple :: Lens.Lens' EmailContent (Prelude.Maybe Message)
emailContent_simple = Lens.lens (\EmailContent' {simple} -> simple) (\s@EmailContent' {} a -> s {simple = a} :: EmailContent)

-- | The raw email message. The message has to meet the following criteria:
--
-- -   The message has to contain a header and a body, separated by one
--     blank line.
--
-- -   All of the required header fields must be present in the message.
--
-- -   Each part of a multipart MIME message must be formatted properly.
--
-- -   If you include attachments, they must be in a file format that
--     Amazon Pinpoint supports.
--
-- -   The entire message must be Base64 encoded.
--
-- -   If any of the MIME parts in your message contain content that is
--     outside of the 7-bit ASCII character range, you should encode that
--     content to ensure that recipients\' email clients render the message
--     properly.
--
-- -   The length of any single line of text in the message can\'t exceed
--     1,000 characters. This restriction is defined in
--     <https://tools.ietf.org/html/rfc5321 RFC 5321>.
emailContent_raw :: Lens.Lens' EmailContent (Prelude.Maybe RawMessage)
emailContent_raw = Lens.lens (\EmailContent' {raw} -> raw) (\s@EmailContent' {} a -> s {raw = a} :: EmailContent)

-- | The template to use for the email message.
emailContent_template :: Lens.Lens' EmailContent (Prelude.Maybe Template)
emailContent_template = Lens.lens (\EmailContent' {template} -> template) (\s@EmailContent' {} a -> s {template = a} :: EmailContent)

instance Prelude.Hashable EmailContent where
  hashWithSalt _salt EmailContent' {..} =
    _salt `Prelude.hashWithSalt` simple
      `Prelude.hashWithSalt` raw
      `Prelude.hashWithSalt` template

instance Prelude.NFData EmailContent where
  rnf EmailContent' {..} =
    Prelude.rnf simple
      `Prelude.seq` Prelude.rnf raw
      `Prelude.seq` Prelude.rnf template

instance Data.ToJSON EmailContent where
  toJSON EmailContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Simple" Data..=) Prelude.<$> simple,
            ("Raw" Data..=) Prelude.<$> raw,
            ("Template" Data..=) Prelude.<$> template
          ]
      )
