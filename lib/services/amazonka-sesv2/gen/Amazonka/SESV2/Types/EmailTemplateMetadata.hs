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
-- Module      : Amazonka.SESV2.Types.EmailTemplateMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.EmailTemplateMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an email template.
--
-- /See:/ 'newEmailTemplateMetadata' smart constructor.
data EmailTemplateMetadata = EmailTemplateMetadata'
  { -- | The time and date the template was created.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The name of the template.
    templateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailTemplateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTimestamp', 'emailTemplateMetadata_createdTimestamp' - The time and date the template was created.
--
-- 'templateName', 'emailTemplateMetadata_templateName' - The name of the template.
newEmailTemplateMetadata ::
  EmailTemplateMetadata
newEmailTemplateMetadata =
  EmailTemplateMetadata'
    { createdTimestamp =
        Prelude.Nothing,
      templateName = Prelude.Nothing
    }

-- | The time and date the template was created.
emailTemplateMetadata_createdTimestamp :: Lens.Lens' EmailTemplateMetadata (Prelude.Maybe Prelude.UTCTime)
emailTemplateMetadata_createdTimestamp = Lens.lens (\EmailTemplateMetadata' {createdTimestamp} -> createdTimestamp) (\s@EmailTemplateMetadata' {} a -> s {createdTimestamp = a} :: EmailTemplateMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the template.
emailTemplateMetadata_templateName :: Lens.Lens' EmailTemplateMetadata (Prelude.Maybe Prelude.Text)
emailTemplateMetadata_templateName = Lens.lens (\EmailTemplateMetadata' {templateName} -> templateName) (\s@EmailTemplateMetadata' {} a -> s {templateName = a} :: EmailTemplateMetadata)

instance Data.FromJSON EmailTemplateMetadata where
  parseJSON =
    Data.withObject
      "EmailTemplateMetadata"
      ( \x ->
          EmailTemplateMetadata'
            Prelude.<$> (x Data..:? "CreatedTimestamp")
            Prelude.<*> (x Data..:? "TemplateName")
      )

instance Prelude.Hashable EmailTemplateMetadata where
  hashWithSalt _salt EmailTemplateMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` templateName

instance Prelude.NFData EmailTemplateMetadata where
  rnf EmailTemplateMetadata' {..} =
    Prelude.rnf createdTimestamp `Prelude.seq`
      Prelude.rnf templateName
