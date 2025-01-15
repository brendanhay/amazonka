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
-- Module      : Amazonka.QuickSight.Types.Theme
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Theme where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ThemeType
import Amazonka.QuickSight.Types.ThemeVersion

-- | Summary information about a theme.
--
-- /See:/ 'newTheme' smart constructor.
data Theme = Theme'
  { -- | The Amazon Resource Name (ARN) of the theme.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the theme was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time that the theme was last updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | The name that the user gives to the theme.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier that the user gives to the theme.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The type of theme, based on how it was created. Valid values include:
    -- @QUICKSIGHT@ and @CUSTOM@.
    type' :: Prelude.Maybe ThemeType,
    version :: Prelude.Maybe ThemeVersion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Theme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'theme_arn' - The Amazon Resource Name (ARN) of the theme.
--
-- 'createdTime', 'theme_createdTime' - The date and time that the theme was created.
--
-- 'lastUpdatedTime', 'theme_lastUpdatedTime' - The date and time that the theme was last updated.
--
-- 'name', 'theme_name' - The name that the user gives to the theme.
--
-- 'themeId', 'theme_themeId' - The identifier that the user gives to the theme.
--
-- 'type'', 'theme_type' - The type of theme, based on how it was created. Valid values include:
-- @QUICKSIGHT@ and @CUSTOM@.
--
-- 'version', 'theme_version' - Undocumented member.
newTheme ::
  Theme
newTheme =
  Theme'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      themeId = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the theme.
theme_arn :: Lens.Lens' Theme (Prelude.Maybe Prelude.Text)
theme_arn = Lens.lens (\Theme' {arn} -> arn) (\s@Theme' {} a -> s {arn = a} :: Theme)

-- | The date and time that the theme was created.
theme_createdTime :: Lens.Lens' Theme (Prelude.Maybe Prelude.UTCTime)
theme_createdTime = Lens.lens (\Theme' {createdTime} -> createdTime) (\s@Theme' {} a -> s {createdTime = a} :: Theme) Prelude.. Lens.mapping Data._Time

-- | The date and time that the theme was last updated.
theme_lastUpdatedTime :: Lens.Lens' Theme (Prelude.Maybe Prelude.UTCTime)
theme_lastUpdatedTime = Lens.lens (\Theme' {lastUpdatedTime} -> lastUpdatedTime) (\s@Theme' {} a -> s {lastUpdatedTime = a} :: Theme) Prelude.. Lens.mapping Data._Time

-- | The name that the user gives to the theme.
theme_name :: Lens.Lens' Theme (Prelude.Maybe Prelude.Text)
theme_name = Lens.lens (\Theme' {name} -> name) (\s@Theme' {} a -> s {name = a} :: Theme)

-- | The identifier that the user gives to the theme.
theme_themeId :: Lens.Lens' Theme (Prelude.Maybe Prelude.Text)
theme_themeId = Lens.lens (\Theme' {themeId} -> themeId) (\s@Theme' {} a -> s {themeId = a} :: Theme)

-- | The type of theme, based on how it was created. Valid values include:
-- @QUICKSIGHT@ and @CUSTOM@.
theme_type :: Lens.Lens' Theme (Prelude.Maybe ThemeType)
theme_type = Lens.lens (\Theme' {type'} -> type') (\s@Theme' {} a -> s {type' = a} :: Theme)

-- | Undocumented member.
theme_version :: Lens.Lens' Theme (Prelude.Maybe ThemeVersion)
theme_version = Lens.lens (\Theme' {version} -> version) (\s@Theme' {} a -> s {version = a} :: Theme)

instance Data.FromJSON Theme where
  parseJSON =
    Data.withObject
      "Theme"
      ( \x ->
          Theme'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "ThemeId")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable Theme where
  hashWithSalt _salt Theme' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` themeId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData Theme where
  rnf Theme' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf lastUpdatedTime `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf themeId `Prelude.seq`
              Prelude.rnf type' `Prelude.seq`
                Prelude.rnf version
