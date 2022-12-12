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
-- Module      : Amazonka.QuickSight.Types.TemplateAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TemplateAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The template alias.
--
-- /See:/ 'newTemplateAlias' smart constructor.
data TemplateAlias = TemplateAlias'
  { -- | The display name of the template alias.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the template alias.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The version number of the template alias.
    templateVersionNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aliasName', 'templateAlias_aliasName' - The display name of the template alias.
--
-- 'arn', 'templateAlias_arn' - The Amazon Resource Name (ARN) of the template alias.
--
-- 'templateVersionNumber', 'templateAlias_templateVersionNumber' - The version number of the template alias.
newTemplateAlias ::
  TemplateAlias
newTemplateAlias =
  TemplateAlias'
    { aliasName = Prelude.Nothing,
      arn = Prelude.Nothing,
      templateVersionNumber = Prelude.Nothing
    }

-- | The display name of the template alias.
templateAlias_aliasName :: Lens.Lens' TemplateAlias (Prelude.Maybe Prelude.Text)
templateAlias_aliasName = Lens.lens (\TemplateAlias' {aliasName} -> aliasName) (\s@TemplateAlias' {} a -> s {aliasName = a} :: TemplateAlias)

-- | The Amazon Resource Name (ARN) of the template alias.
templateAlias_arn :: Lens.Lens' TemplateAlias (Prelude.Maybe Prelude.Text)
templateAlias_arn = Lens.lens (\TemplateAlias' {arn} -> arn) (\s@TemplateAlias' {} a -> s {arn = a} :: TemplateAlias)

-- | The version number of the template alias.
templateAlias_templateVersionNumber :: Lens.Lens' TemplateAlias (Prelude.Maybe Prelude.Natural)
templateAlias_templateVersionNumber = Lens.lens (\TemplateAlias' {templateVersionNumber} -> templateVersionNumber) (\s@TemplateAlias' {} a -> s {templateVersionNumber = a} :: TemplateAlias)

instance Data.FromJSON TemplateAlias where
  parseJSON =
    Data.withObject
      "TemplateAlias"
      ( \x ->
          TemplateAlias'
            Prelude.<$> (x Data..:? "AliasName")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "TemplateVersionNumber")
      )

instance Prelude.Hashable TemplateAlias where
  hashWithSalt _salt TemplateAlias' {..} =
    _salt `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` templateVersionNumber

instance Prelude.NFData TemplateAlias where
  rnf TemplateAlias' {..} =
    Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf templateVersionNumber
