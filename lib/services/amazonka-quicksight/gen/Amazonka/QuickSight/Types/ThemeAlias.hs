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
-- Module      : Amazonka.QuickSight.Types.ThemeAlias
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeAlias where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An alias for a theme.
--
-- /See:/ 'newThemeAlias' smart constructor.
data ThemeAlias = ThemeAlias'
  { -- | The Amazon Resource Name (ARN) of the theme alias.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The display name of the theme alias.
    aliasName :: Prelude.Maybe Prelude.Text,
    -- | The version number of the theme alias.
    themeVersionNumber :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'themeAlias_arn' - The Amazon Resource Name (ARN) of the theme alias.
--
-- 'aliasName', 'themeAlias_aliasName' - The display name of the theme alias.
--
-- 'themeVersionNumber', 'themeAlias_themeVersionNumber' - The version number of the theme alias.
newThemeAlias ::
  ThemeAlias
newThemeAlias =
  ThemeAlias'
    { arn = Prelude.Nothing,
      aliasName = Prelude.Nothing,
      themeVersionNumber = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the theme alias.
themeAlias_arn :: Lens.Lens' ThemeAlias (Prelude.Maybe Prelude.Text)
themeAlias_arn = Lens.lens (\ThemeAlias' {arn} -> arn) (\s@ThemeAlias' {} a -> s {arn = a} :: ThemeAlias)

-- | The display name of the theme alias.
themeAlias_aliasName :: Lens.Lens' ThemeAlias (Prelude.Maybe Prelude.Text)
themeAlias_aliasName = Lens.lens (\ThemeAlias' {aliasName} -> aliasName) (\s@ThemeAlias' {} a -> s {aliasName = a} :: ThemeAlias)

-- | The version number of the theme alias.
themeAlias_themeVersionNumber :: Lens.Lens' ThemeAlias (Prelude.Maybe Prelude.Natural)
themeAlias_themeVersionNumber = Lens.lens (\ThemeAlias' {themeVersionNumber} -> themeVersionNumber) (\s@ThemeAlias' {} a -> s {themeVersionNumber = a} :: ThemeAlias)

instance Core.FromJSON ThemeAlias where
  parseJSON =
    Core.withObject
      "ThemeAlias"
      ( \x ->
          ThemeAlias'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "AliasName")
            Prelude.<*> (x Core..:? "ThemeVersionNumber")
      )

instance Prelude.Hashable ThemeAlias where
  hashWithSalt _salt ThemeAlias' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` aliasName
      `Prelude.hashWithSalt` themeVersionNumber

instance Prelude.NFData ThemeAlias where
  rnf ThemeAlias' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf aliasName
      `Prelude.seq` Prelude.rnf themeVersionNumber
