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
-- Module      : Amazonka.SES.Types.TemplateMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.TemplateMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an email template.
--
-- /See:/ 'newTemplateMetadata' smart constructor.
data TemplateMetadata = TemplateMetadata'
  { -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The time and date the template was created.
    createdTimestamp :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'templateMetadata_name' - The name of the template.
--
-- 'createdTimestamp', 'templateMetadata_createdTimestamp' - The time and date the template was created.
newTemplateMetadata ::
  TemplateMetadata
newTemplateMetadata =
  TemplateMetadata'
    { name = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The name of the template.
templateMetadata_name :: Lens.Lens' TemplateMetadata (Prelude.Maybe Prelude.Text)
templateMetadata_name = Lens.lens (\TemplateMetadata' {name} -> name) (\s@TemplateMetadata' {} a -> s {name = a} :: TemplateMetadata)

-- | The time and date the template was created.
templateMetadata_createdTimestamp :: Lens.Lens' TemplateMetadata (Prelude.Maybe Prelude.UTCTime)
templateMetadata_createdTimestamp = Lens.lens (\TemplateMetadata' {createdTimestamp} -> createdTimestamp) (\s@TemplateMetadata' {} a -> s {createdTimestamp = a} :: TemplateMetadata) Prelude.. Lens.mapping Data._Time

instance Data.FromXML TemplateMetadata where
  parseXML x =
    TemplateMetadata'
      Prelude.<$> (x Data..@? "Name")
      Prelude.<*> (x Data..@? "CreatedTimestamp")

instance Prelude.Hashable TemplateMetadata where
  hashWithSalt _salt TemplateMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTimestamp

instance Prelude.NFData TemplateMetadata where
  rnf TemplateMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTimestamp
