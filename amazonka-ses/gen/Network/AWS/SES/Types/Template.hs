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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The content of the email, composed of a subject line, an HTML part, and
-- a text-only part.
--
-- /See:/ 'newTemplate' smart constructor.
data Template = Template'
  { -- | The email body that will be visible to recipients whose email clients do
    -- not display HTML.
    textPart :: Core.Maybe Core.Text,
    -- | The subject line of the email.
    subjectPart :: Core.Maybe Core.Text,
    -- | The HTML body of the email.
    htmlPart :: Core.Maybe Core.Text,
    -- | The name of the template. You will refer to this name when you send
    -- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
    -- operations.
    templateName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  Template
newTemplate pTemplateName_ =
  Template'
    { textPart = Core.Nothing,
      subjectPart = Core.Nothing,
      htmlPart = Core.Nothing,
      templateName = pTemplateName_
    }

-- | The email body that will be visible to recipients whose email clients do
-- not display HTML.
template_textPart :: Lens.Lens' Template (Core.Maybe Core.Text)
template_textPart = Lens.lens (\Template' {textPart} -> textPart) (\s@Template' {} a -> s {textPart = a} :: Template)

-- | The subject line of the email.
template_subjectPart :: Lens.Lens' Template (Core.Maybe Core.Text)
template_subjectPart = Lens.lens (\Template' {subjectPart} -> subjectPart) (\s@Template' {} a -> s {subjectPart = a} :: Template)

-- | The HTML body of the email.
template_htmlPart :: Lens.Lens' Template (Core.Maybe Core.Text)
template_htmlPart = Lens.lens (\Template' {htmlPart} -> htmlPart) (\s@Template' {} a -> s {htmlPart = a} :: Template)

-- | The name of the template. You will refer to this name when you send
-- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
-- operations.
template_templateName :: Lens.Lens' Template Core.Text
template_templateName = Lens.lens (\Template' {templateName} -> templateName) (\s@Template' {} a -> s {templateName = a} :: Template)

instance Core.FromXML Template where
  parseXML x =
    Template'
      Core.<$> (x Core..@? "TextPart")
      Core.<*> (x Core..@? "SubjectPart")
      Core.<*> (x Core..@? "HtmlPart")
      Core.<*> (x Core..@ "TemplateName")

instance Core.Hashable Template

instance Core.NFData Template

instance Core.ToQuery Template where
  toQuery Template' {..} =
    Core.mconcat
      [ "TextPart" Core.=: textPart,
        "SubjectPart" Core.=: subjectPart,
        "HtmlPart" Core.=: htmlPart,
        "TemplateName" Core.=: templateName
      ]
