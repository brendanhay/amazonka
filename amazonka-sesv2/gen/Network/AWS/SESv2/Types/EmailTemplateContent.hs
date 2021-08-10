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
-- Module      : Network.AWS.SESv2.Types.EmailTemplateContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.EmailTemplateContent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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

instance Core.FromJSON EmailTemplateContent where
  parseJSON =
    Core.withObject
      "EmailTemplateContent"
      ( \x ->
          EmailTemplateContent'
            Prelude.<$> (x Core..:? "Html")
            Prelude.<*> (x Core..:? "Subject")
            Prelude.<*> (x Core..:? "Text")
      )

instance Prelude.Hashable EmailTemplateContent

instance Prelude.NFData EmailTemplateContent

instance Core.ToJSON EmailTemplateContent where
  toJSON EmailTemplateContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Html" Core..=) Prelude.<$> html,
            ("Subject" Core..=) Prelude.<$> subject,
            ("Text" Core..=) Prelude.<$> text
          ]
      )
