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
-- Module      : Amazonka.SESV2.Types.Template
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.Template where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that defines the email template to use for an email message,
-- and the values to use for any message variables in that template. An
-- /email template/ is a type of message template that contains content
-- that you want to define, save, and reuse in email messages that you
-- send.
--
-- /See:/ 'newTemplate' smart constructor.
data Template = Template'
  { -- | The Amazon Resource Name (ARN) of the template.
    templateArn :: Prelude.Maybe Prelude.Text,
    -- | An object that defines the values to use for message variables in the
    -- template. This object is a set of key-value pairs. Each key defines a
    -- message variable in the template. The corresponding value defines the
    -- value to use for that variable.
    templateData :: Prelude.Maybe Prelude.Text,
    -- | The name of the template. You will refer to this name when you send
    -- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
    -- operations.
    templateName :: Prelude.Maybe Prelude.Text
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
-- 'templateArn', 'template_templateArn' - The Amazon Resource Name (ARN) of the template.
--
-- 'templateData', 'template_templateData' - An object that defines the values to use for message variables in the
-- template. This object is a set of key-value pairs. Each key defines a
-- message variable in the template. The corresponding value defines the
-- value to use for that variable.
--
-- 'templateName', 'template_templateName' - The name of the template. You will refer to this name when you send
-- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
-- operations.
newTemplate ::
  Template
newTemplate =
  Template'
    { templateArn = Prelude.Nothing,
      templateData = Prelude.Nothing,
      templateName = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the template.
template_templateArn :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_templateArn = Lens.lens (\Template' {templateArn} -> templateArn) (\s@Template' {} a -> s {templateArn = a} :: Template)

-- | An object that defines the values to use for message variables in the
-- template. This object is a set of key-value pairs. Each key defines a
-- message variable in the template. The corresponding value defines the
-- value to use for that variable.
template_templateData :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_templateData = Lens.lens (\Template' {templateData} -> templateData) (\s@Template' {} a -> s {templateData = a} :: Template)

-- | The name of the template. You will refer to this name when you send
-- email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@
-- operations.
template_templateName :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_templateName = Lens.lens (\Template' {templateName} -> templateName) (\s@Template' {} a -> s {templateName = a} :: Template)

instance Prelude.Hashable Template where
  hashWithSalt _salt Template' {..} =
    _salt
      `Prelude.hashWithSalt` templateArn
      `Prelude.hashWithSalt` templateData
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData Template where
  rnf Template' {..} =
    Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateData
      `Prelude.seq` Prelude.rnf templateName

instance Data.ToJSON Template where
  toJSON Template' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TemplateArn" Data..=) Prelude.<$> templateArn,
            ("TemplateData" Data..=) Prelude.<$> templateData,
            ("TemplateName" Data..=) Prelude.<$> templateName
          ]
      )
