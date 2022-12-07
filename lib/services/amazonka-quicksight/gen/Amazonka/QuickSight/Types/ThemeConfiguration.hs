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
-- Module      : Amazonka.QuickSight.Types.ThemeConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataColorPalette
import Amazonka.QuickSight.Types.SheetStyle
import Amazonka.QuickSight.Types.UIColorPalette

-- | The theme configuration. This configuration contains all of the display
-- properties for a theme.
--
-- /See:/ 'newThemeConfiguration' smart constructor.
data ThemeConfiguration = ThemeConfiguration'
  { -- | Color properties that apply to chart data colors.
    dataColorPalette :: Prelude.Maybe DataColorPalette,
    -- | Display options related to sheets.
    sheet :: Prelude.Maybe SheetStyle,
    -- | Color properties that apply to the UI and to charts, excluding the
    -- colors that apply to data.
    uIColorPalette :: Prelude.Maybe UIColorPalette
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataColorPalette', 'themeConfiguration_dataColorPalette' - Color properties that apply to chart data colors.
--
-- 'sheet', 'themeConfiguration_sheet' - Display options related to sheets.
--
-- 'uIColorPalette', 'themeConfiguration_uIColorPalette' - Color properties that apply to the UI and to charts, excluding the
-- colors that apply to data.
newThemeConfiguration ::
  ThemeConfiguration
newThemeConfiguration =
  ThemeConfiguration'
    { dataColorPalette =
        Prelude.Nothing,
      sheet = Prelude.Nothing,
      uIColorPalette = Prelude.Nothing
    }

-- | Color properties that apply to chart data colors.
themeConfiguration_dataColorPalette :: Lens.Lens' ThemeConfiguration (Prelude.Maybe DataColorPalette)
themeConfiguration_dataColorPalette = Lens.lens (\ThemeConfiguration' {dataColorPalette} -> dataColorPalette) (\s@ThemeConfiguration' {} a -> s {dataColorPalette = a} :: ThemeConfiguration)

-- | Display options related to sheets.
themeConfiguration_sheet :: Lens.Lens' ThemeConfiguration (Prelude.Maybe SheetStyle)
themeConfiguration_sheet = Lens.lens (\ThemeConfiguration' {sheet} -> sheet) (\s@ThemeConfiguration' {} a -> s {sheet = a} :: ThemeConfiguration)

-- | Color properties that apply to the UI and to charts, excluding the
-- colors that apply to data.
themeConfiguration_uIColorPalette :: Lens.Lens' ThemeConfiguration (Prelude.Maybe UIColorPalette)
themeConfiguration_uIColorPalette = Lens.lens (\ThemeConfiguration' {uIColorPalette} -> uIColorPalette) (\s@ThemeConfiguration' {} a -> s {uIColorPalette = a} :: ThemeConfiguration)

instance Data.FromJSON ThemeConfiguration where
  parseJSON =
    Data.withObject
      "ThemeConfiguration"
      ( \x ->
          ThemeConfiguration'
            Prelude.<$> (x Data..:? "DataColorPalette")
            Prelude.<*> (x Data..:? "Sheet")
            Prelude.<*> (x Data..:? "UIColorPalette")
      )

instance Prelude.Hashable ThemeConfiguration where
  hashWithSalt _salt ThemeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` dataColorPalette
      `Prelude.hashWithSalt` sheet
      `Prelude.hashWithSalt` uIColorPalette

instance Prelude.NFData ThemeConfiguration where
  rnf ThemeConfiguration' {..} =
    Prelude.rnf dataColorPalette
      `Prelude.seq` Prelude.rnf sheet
      `Prelude.seq` Prelude.rnf uIColorPalette

instance Data.ToJSON ThemeConfiguration where
  toJSON ThemeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataColorPalette" Data..=)
              Prelude.<$> dataColorPalette,
            ("Sheet" Data..=) Prelude.<$> sheet,
            ("UIColorPalette" Data..=)
              Prelude.<$> uIColorPalette
          ]
      )
