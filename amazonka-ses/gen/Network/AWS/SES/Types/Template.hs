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
-- Module      : Network.AWS.SES.Types.Template
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.Template where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The content of the email, composed of a subject line, an HTML part, and
-- a text-only part.
--
-- /See:/ 'newTemplate' smart constructor.
data Template = Template'
  { -- | The email body that will be visible to recipients whose email clients do
    -- not display HTML.
    textPart :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the email.
    subjectPart :: Prelude.Maybe Prelude.Text,
    -- | The HTML body of the email.
    htmlPart :: Prelude.Maybe Prelude.Text,
    -- | The name of the template. You will refer to this name when you send
    -- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
    -- operations.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Template' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textPart', 'template_textPart' - The email body that will be visible to recipients whose email clients do
-- not display HTML.
--
-- 'subjectPart', 'template_subjectPart' - The subject line of the email.
--
-- 'htmlPart', 'template_htmlPart' - The HTML body of the email.
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
    { textPart = Prelude.Nothing,
      subjectPart = Prelude.Nothing,
      htmlPart = Prelude.Nothing,
      templateName = pTemplateName_
    }

-- | The email body that will be visible to recipients whose email clients do
-- not display HTML.
template_textPart :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_textPart = Lens.lens (\Template' {textPart} -> textPart) (\s@Template' {} a -> s {textPart = a} :: Template)

-- | The subject line of the email.
template_subjectPart :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_subjectPart = Lens.lens (\Template' {subjectPart} -> subjectPart) (\s@Template' {} a -> s {subjectPart = a} :: Template)

-- | The HTML body of the email.
template_htmlPart :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_htmlPart = Lens.lens (\Template' {htmlPart} -> htmlPart) (\s@Template' {} a -> s {htmlPart = a} :: Template)

-- | The name of the template. You will refer to this name when you send
-- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
-- operations.
template_templateName :: Lens.Lens' Template Prelude.Text
template_templateName = Lens.lens (\Template' {templateName} -> templateName) (\s@Template' {} a -> s {templateName = a} :: Template)

instance Prelude.FromXML Template where
  parseXML x =
    Template'
      Prelude.<$> (x Prelude..@? "TextPart")
      Prelude.<*> (x Prelude..@? "SubjectPart")
      Prelude.<*> (x Prelude..@? "HtmlPart")
      Prelude.<*> (x Prelude..@ "TemplateName")

instance Prelude.Hashable Template

instance Prelude.NFData Template

instance Prelude.ToQuery Template where
  toQuery Template' {..} =
    Prelude.mconcat
      [ "TextPart" Prelude.=: textPart,
        "SubjectPart" Prelude.=: subjectPart,
        "HtmlPart" Prelude.=: htmlPart,
        "TemplateName" Prelude.=: templateName
      ]
