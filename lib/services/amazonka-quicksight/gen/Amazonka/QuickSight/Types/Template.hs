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
-- Module      : Amazonka.QuickSight.Types.Template
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Template where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TemplateVersion

-- | A template object. A /template/ is an entity in Amazon QuickSight that
-- encapsulates the metadata required to create an analysis and that you
-- can use to create a dashboard. A template adds a layer of abstraction by
-- using placeholders to replace the dataset associated with an analysis.
-- You can use templates to create dashboards by replacing dataset
-- placeholders with datasets that follow the same schema that was used to
-- create the source analysis and template.
--
-- You can share templates across Amazon Web Services accounts by allowing
-- users in other Amazon Web Services accounts to create a template or a
-- dashboard from an existing template.
--
-- /See:/ 'newTemplate' smart constructor.
data Template = Template'
  { -- | The Amazon Resource Name (ARN) of the template.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Time when this was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | Time when this was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The display name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID for the template. This is unique per Amazon Web Services Region
    -- for each Amazon Web Services account.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | A structure describing the versions of the template.
    version :: Prelude.Maybe TemplateVersion
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
-- 'arn', 'template_arn' - The Amazon Resource Name (ARN) of the template.
--
-- 'createdTime', 'template_createdTime' - Time when this was created.
--
-- 'lastUpdatedTime', 'template_lastUpdatedTime' - Time when this was last updated.
--
-- 'name', 'template_name' - The display name of the template.
--
-- 'templateId', 'template_templateId' - The ID for the template. This is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
--
-- 'version', 'template_version' - A structure describing the versions of the template.
newTemplate ::
  Template
newTemplate =
  Template'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      templateId = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the template.
template_arn :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_arn = Lens.lens (\Template' {arn} -> arn) (\s@Template' {} a -> s {arn = a} :: Template)

-- | Time when this was created.
template_createdTime :: Lens.Lens' Template (Prelude.Maybe Prelude.UTCTime)
template_createdTime = Lens.lens (\Template' {createdTime} -> createdTime) (\s@Template' {} a -> s {createdTime = a} :: Template) Prelude.. Lens.mapping Data._Time

-- | Time when this was last updated.
template_lastUpdatedTime :: Lens.Lens' Template (Prelude.Maybe Prelude.UTCTime)
template_lastUpdatedTime = Lens.lens (\Template' {lastUpdatedTime} -> lastUpdatedTime) (\s@Template' {} a -> s {lastUpdatedTime = a} :: Template) Prelude.. Lens.mapping Data._Time

-- | The display name of the template.
template_name :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_name = Lens.lens (\Template' {name} -> name) (\s@Template' {} a -> s {name = a} :: Template)

-- | The ID for the template. This is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
template_templateId :: Lens.Lens' Template (Prelude.Maybe Prelude.Text)
template_templateId = Lens.lens (\Template' {templateId} -> templateId) (\s@Template' {} a -> s {templateId = a} :: Template)

-- | A structure describing the versions of the template.
template_version :: Lens.Lens' Template (Prelude.Maybe TemplateVersion)
template_version = Lens.lens (\Template' {version} -> version) (\s@Template' {} a -> s {version = a} :: Template)

instance Data.FromJSON Template where
  parseJSON =
    Data.withObject
      "Template"
      ( \x ->
          Template'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TemplateId")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable Template where
  hashWithSalt _salt Template' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` templateId
      `Prelude.hashWithSalt` version

instance Prelude.NFData Template where
  rnf Template' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf templateId
      `Prelude.seq` Prelude.rnf version
