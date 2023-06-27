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
-- Module      : Amazonka.Pinpoint.Types.SimpleEmail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SimpleEmail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.SimpleEmailPart
import qualified Amazonka.Prelude as Prelude

-- | Specifies the contents of an email message, composed of a subject, a
-- text part, and an HTML part.
--
-- /See:/ 'newSimpleEmail' smart constructor.
data SimpleEmail = SimpleEmail'
  { -- | The body of the email message, in HTML format. We recommend using HTML
    -- format for email clients that render HTML content. You can include
    -- links, formatted text, and more in an HTML message.
    htmlPart :: Prelude.Maybe SimpleEmailPart,
    -- | The subject line, or title, of the email.
    subject :: Prelude.Maybe SimpleEmailPart,
    -- | The body of the email message, in plain text format. We recommend using
    -- plain text format for email clients that don\'t render HTML content and
    -- clients that are connected to high-latency networks, such as mobile
    -- devices.
    textPart :: Prelude.Maybe SimpleEmailPart
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SimpleEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'htmlPart', 'simpleEmail_htmlPart' - The body of the email message, in HTML format. We recommend using HTML
-- format for email clients that render HTML content. You can include
-- links, formatted text, and more in an HTML message.
--
-- 'subject', 'simpleEmail_subject' - The subject line, or title, of the email.
--
-- 'textPart', 'simpleEmail_textPart' - The body of the email message, in plain text format. We recommend using
-- plain text format for email clients that don\'t render HTML content and
-- clients that are connected to high-latency networks, such as mobile
-- devices.
newSimpleEmail ::
  SimpleEmail
newSimpleEmail =
  SimpleEmail'
    { htmlPart = Prelude.Nothing,
      subject = Prelude.Nothing,
      textPart = Prelude.Nothing
    }

-- | The body of the email message, in HTML format. We recommend using HTML
-- format for email clients that render HTML content. You can include
-- links, formatted text, and more in an HTML message.
simpleEmail_htmlPart :: Lens.Lens' SimpleEmail (Prelude.Maybe SimpleEmailPart)
simpleEmail_htmlPart = Lens.lens (\SimpleEmail' {htmlPart} -> htmlPart) (\s@SimpleEmail' {} a -> s {htmlPart = a} :: SimpleEmail)

-- | The subject line, or title, of the email.
simpleEmail_subject :: Lens.Lens' SimpleEmail (Prelude.Maybe SimpleEmailPart)
simpleEmail_subject = Lens.lens (\SimpleEmail' {subject} -> subject) (\s@SimpleEmail' {} a -> s {subject = a} :: SimpleEmail)

-- | The body of the email message, in plain text format. We recommend using
-- plain text format for email clients that don\'t render HTML content and
-- clients that are connected to high-latency networks, such as mobile
-- devices.
simpleEmail_textPart :: Lens.Lens' SimpleEmail (Prelude.Maybe SimpleEmailPart)
simpleEmail_textPart = Lens.lens (\SimpleEmail' {textPart} -> textPart) (\s@SimpleEmail' {} a -> s {textPart = a} :: SimpleEmail)

instance Prelude.Hashable SimpleEmail where
  hashWithSalt _salt SimpleEmail' {..} =
    _salt
      `Prelude.hashWithSalt` htmlPart
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` textPart

instance Prelude.NFData SimpleEmail where
  rnf SimpleEmail' {..} =
    Prelude.rnf htmlPart
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf textPart

instance Data.ToJSON SimpleEmail where
  toJSON SimpleEmail' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HtmlPart" Data..=) Prelude.<$> htmlPart,
            ("Subject" Data..=) Prelude.<$> subject,
            ("TextPart" Data..=) Prelude.<$> textPart
          ]
      )
