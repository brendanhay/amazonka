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
-- Module      : Amazonka.SESV2.Types.EmailTemplateContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.EmailTemplateContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The content of the email, composed of a subject line, an HTML part, and
-- a text-only part.
--
-- /See:/ 'newEmailTemplateContent' smart constructor.
data EmailTemplateContent = EmailTemplateContent'
  { -- | The HTML body of the email.
    html :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the email.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The email body that will be visible to recipients whose email clients do
    -- not display HTML.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailTemplateContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'html', 'emailTemplateContent_html' - The HTML body of the email.
--
-- 'subject', 'emailTemplateContent_subject' - The subject line of the email.
--
-- 'text', 'emailTemplateContent_text' - The email body that will be visible to recipients whose email clients do
-- not display HTML.
newEmailTemplateContent ::
  EmailTemplateContent
newEmailTemplateContent =
  EmailTemplateContent'
    { html = Prelude.Nothing,
      subject = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The HTML body of the email.
emailTemplateContent_html :: Lens.Lens' EmailTemplateContent (Prelude.Maybe Prelude.Text)
emailTemplateContent_html = Lens.lens (\EmailTemplateContent' {html} -> html) (\s@EmailTemplateContent' {} a -> s {html = a} :: EmailTemplateContent)

-- | The subject line of the email.
emailTemplateContent_subject :: Lens.Lens' EmailTemplateContent (Prelude.Maybe Prelude.Text)
emailTemplateContent_subject = Lens.lens (\EmailTemplateContent' {subject} -> subject) (\s@EmailTemplateContent' {} a -> s {subject = a} :: EmailTemplateContent)

-- | The email body that will be visible to recipients whose email clients do
-- not display HTML.
emailTemplateContent_text :: Lens.Lens' EmailTemplateContent (Prelude.Maybe Prelude.Text)
emailTemplateContent_text = Lens.lens (\EmailTemplateContent' {text} -> text) (\s@EmailTemplateContent' {} a -> s {text = a} :: EmailTemplateContent)

instance Data.FromJSON EmailTemplateContent where
  parseJSON =
    Data.withObject
      "EmailTemplateContent"
      ( \x ->
          EmailTemplateContent'
            Prelude.<$> (x Data..:? "Html")
            Prelude.<*> (x Data..:? "Subject")
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable EmailTemplateContent where
  hashWithSalt _salt EmailTemplateContent' {..} =
    _salt
      `Prelude.hashWithSalt` html
      `Prelude.hashWithSalt` subject
      `Prelude.hashWithSalt` text

instance Prelude.NFData EmailTemplateContent where
  rnf EmailTemplateContent' {..} =
    Prelude.rnf html
      `Prelude.seq` Prelude.rnf subject
      `Prelude.seq` Prelude.rnf text

instance Data.ToJSON EmailTemplateContent where
  toJSON EmailTemplateContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Html" Data..=) Prelude.<$> html,
            ("Subject" Data..=) Prelude.<$> subject,
            ("Text" Data..=) Prelude.<$> text
          ]
      )
