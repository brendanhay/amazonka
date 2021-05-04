{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.SimpleEmail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SimpleEmail where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.SimpleEmailPart
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the contents of an email message, composed of a subject, a
-- text part, and an HTML part.
--
-- /See:/ 'newSimpleEmail' smart constructor.
data SimpleEmail = SimpleEmail'
  { -- | The body of the email message, in plain text format. We recommend using
    -- plain text format for email clients that don\'t render HTML content and
    -- clients that are connected to high-latency networks, such as mobile
    -- devices.
    textPart :: Prelude.Maybe SimpleEmailPart,
    -- | The subject line, or title, of the email.
    subject :: Prelude.Maybe SimpleEmailPart,
    -- | The body of the email message, in HTML format. We recommend using HTML
    -- format for email clients that render HTML content. You can include
    -- links, formatted text, and more in an HTML message.
    htmlPart :: Prelude.Maybe SimpleEmailPart
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SimpleEmail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textPart', 'simpleEmail_textPart' - The body of the email message, in plain text format. We recommend using
-- plain text format for email clients that don\'t render HTML content and
-- clients that are connected to high-latency networks, such as mobile
-- devices.
--
-- 'subject', 'simpleEmail_subject' - The subject line, or title, of the email.
--
-- 'htmlPart', 'simpleEmail_htmlPart' - The body of the email message, in HTML format. We recommend using HTML
-- format for email clients that render HTML content. You can include
-- links, formatted text, and more in an HTML message.
newSimpleEmail ::
  SimpleEmail
newSimpleEmail =
  SimpleEmail'
    { textPart = Prelude.Nothing,
      subject = Prelude.Nothing,
      htmlPart = Prelude.Nothing
    }

-- | The body of the email message, in plain text format. We recommend using
-- plain text format for email clients that don\'t render HTML content and
-- clients that are connected to high-latency networks, such as mobile
-- devices.
simpleEmail_textPart :: Lens.Lens' SimpleEmail (Prelude.Maybe SimpleEmailPart)
simpleEmail_textPart = Lens.lens (\SimpleEmail' {textPart} -> textPart) (\s@SimpleEmail' {} a -> s {textPart = a} :: SimpleEmail)

-- | The subject line, or title, of the email.
simpleEmail_subject :: Lens.Lens' SimpleEmail (Prelude.Maybe SimpleEmailPart)
simpleEmail_subject = Lens.lens (\SimpleEmail' {subject} -> subject) (\s@SimpleEmail' {} a -> s {subject = a} :: SimpleEmail)

-- | The body of the email message, in HTML format. We recommend using HTML
-- format for email clients that render HTML content. You can include
-- links, formatted text, and more in an HTML message.
simpleEmail_htmlPart :: Lens.Lens' SimpleEmail (Prelude.Maybe SimpleEmailPart)
simpleEmail_htmlPart = Lens.lens (\SimpleEmail' {htmlPart} -> htmlPart) (\s@SimpleEmail' {} a -> s {htmlPart = a} :: SimpleEmail)

instance Prelude.Hashable SimpleEmail

instance Prelude.NFData SimpleEmail

instance Prelude.ToJSON SimpleEmail where
  toJSON SimpleEmail' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TextPart" Prelude..=) Prelude.<$> textPart,
            ("Subject" Prelude..=) Prelude.<$> subject,
            ("HtmlPart" Prelude..=) Prelude.<$> htmlPart
          ]
      )
