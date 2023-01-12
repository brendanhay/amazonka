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
-- Module      : Amazonka.SES.Types.Template
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.Template where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The content of the email, composed of a subject line, an HTML part, and
-- a text-only part.
--
-- /See:/ 'newTemplate' smart constructor.
data Template = Template'
  { -- | The HTML body of the email.
    htmlPart :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the email.
    subjectPart :: Prelude.Maybe Prelude.Text,
    -- | The email body that will be visible to recipients whose email clients do
    -- not display HTML.
    textPart :: Prelude.Maybe Prelude.Text,
    -- | The name of the template. You will refer to this name when you send
    -- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
    -- operations.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Template' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'htmlPart', 'template_htmlPart' - The HTML body of the email.
--
-- 'subjectPart', 'template_subjectPart' - The subject line of the email.
--
-- 'textPart', 'template_textPart' - The email body that will be visible to recipients whose email clients do
-- not display HTML.
--
-- 'templateName', 'template_templateName' - The name of the template. You will refer to this name when you send
-- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
-- operations.
newTemplate ::
  -- | 'templateName'
  Prelude.Text ->
  Template
newTemplate pTemplateName_ =
  Template'
    { htmlPart = Prelude.Nothing,
      subjectPart = Prelude.Nothing,
      textPart = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | The HTML body of the email.
template_htmlPart :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_htmlPart = Lens.lens (\Template' {htmlPart} -> htmlPart) (\s@Template' {} a -> s {htmlPart = a} :: Template)

-- | The subject line of the email.
template_subjectPart :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_subjectPart = Lens.lens (\Template' {subjectPart} -> subjectPart) (\s@Template' {} a -> s {subjectPart = a} :: Template)

-- | The email body that will be visible to recipients whose email clients do
-- not display HTML.
template_textPart :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_textPart = Lens.lens (\Template' {textPart} -> textPart) (\s@Template' {} a -> s {textPart = a} :: Template)

-- | The name of the template. You will refer to this name when you send
-- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
-- operations.
template_templateName :: Lens.Lens' Template Prelude.Text
template_templateName = Lens.lens (\Template' {templateName} -> templateName) (\s@Template' {} a -> s {templateName = a} :: Template)

instance Data.FromXML Template where
  parseXML x =
    Template'
      Prelude.<$> (x Data..@? "HtmlPart")
      Prelude.<*> (x Data..@? "SubjectPart")
      Prelude.<*> (x Data..@? "TextPart")
      Prelude.<*> (x Data..@ "TemplateName")

instance Prelude.Hashable Template where
  hashWithSalt _salt Template' {..} =
    _salt `Prelude.hashWithSalt` htmlPart
      `Prelude.hashWithSalt` subjectPart
      `Prelude.hashWithSalt` textPart
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData Template where
  rnf Template' {..} =
    Prelude.rnf htmlPart
      `Prelude.seq` Prelude.rnf subjectPart
      `Prelude.seq` Prelude.rnf textPart
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToQuery Template where
  toQuery Template' {..} =
    Prelude.mconcat
      [ "HtmlPart" Data.=: htmlPart,
        "SubjectPart" Data.=: subjectPart,
        "TextPart" Data.=: textPart,
        "TemplateName" Data.=: templateName
      ]
