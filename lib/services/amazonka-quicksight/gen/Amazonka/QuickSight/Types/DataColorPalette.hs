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
-- Module      : Amazonka.QuickSight.Types.DataColorPalette
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataColorPalette where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The theme colors that are used for data colors in charts. The colors
-- description is a hexadecimal color code that consists of six
-- alphanumerical characters, prefixed with @#@, for example #37BFF5.
--
-- /See:/ 'newDataColorPalette' smart constructor.
data DataColorPalette = DataColorPalette'
  { -- | The hexadecimal codes for the colors.
    colors :: Prelude.Maybe [Prelude.Text],
    -- | The hexadecimal code of a color that applies to charts where a lack of
    -- data is highlighted.
    emptyFillColor :: Prelude.Maybe Prelude.Text,
    -- | The minimum and maximum hexadecimal codes that describe a color
    -- gradient.
    minMaxGradient :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataColorPalette' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'colors', 'dataColorPalette_colors' - The hexadecimal codes for the colors.
--
-- 'emptyFillColor', 'dataColorPalette_emptyFillColor' - The hexadecimal code of a color that applies to charts where a lack of
-- data is highlighted.
--
-- 'minMaxGradient', 'dataColorPalette_minMaxGradient' - The minimum and maximum hexadecimal codes that describe a color
-- gradient.
newDataColorPalette ::
  DataColorPalette
newDataColorPalette =
  DataColorPalette'
    { colors = Prelude.Nothing,
      emptyFillColor = Prelude.Nothing,
      minMaxGradient = Prelude.Nothing
    }

-- | The hexadecimal codes for the colors.
dataColorPalette_colors :: Lens.Lens' DataColorPalette (Prelude.Maybe [Prelude.Text])
dataColorPalette_colors = Lens.lens (\DataColorPalette' {colors} -> colors) (\s@DataColorPalette' {} a -> s {colors = a} :: DataColorPalette) Prelude.. Lens.mapping Lens.coerced

-- | The hexadecimal code of a color that applies to charts where a lack of
-- data is highlighted.
dataColorPalette_emptyFillColor :: Lens.Lens' DataColorPalette (Prelude.Maybe Prelude.Text)
dataColorPalette_emptyFillColor = Lens.lens (\DataColorPalette' {emptyFillColor} -> emptyFillColor) (\s@DataColorPalette' {} a -> s {emptyFillColor = a} :: DataColorPalette)

-- | The minimum and maximum hexadecimal codes that describe a color
-- gradient.
dataColorPalette_minMaxGradient :: Lens.Lens' DataColorPalette (Prelude.Maybe [Prelude.Text])
dataColorPalette_minMaxGradient = Lens.lens (\DataColorPalette' {minMaxGradient} -> minMaxGradient) (\s@DataColorPalette' {} a -> s {minMaxGradient = a} :: DataColorPalette) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataColorPalette where
  parseJSON =
    Data.withObject
      "DataColorPalette"
      ( \x ->
          DataColorPalette'
            Prelude.<$> (x Data..:? "Colors" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EmptyFillColor")
            Prelude.<*> ( x
                            Data..:? "MinMaxGradient"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataColorPalette where
  hashWithSalt _salt DataColorPalette' {..} =
    _salt
      `Prelude.hashWithSalt` colors
      `Prelude.hashWithSalt` emptyFillColor
      `Prelude.hashWithSalt` minMaxGradient

instance Prelude.NFData DataColorPalette where
  rnf DataColorPalette' {..} =
    Prelude.rnf colors
      `Prelude.seq` Prelude.rnf emptyFillColor
      `Prelude.seq` Prelude.rnf minMaxGradient

instance Data.ToJSON DataColorPalette where
  toJSON DataColorPalette' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Colors" Data..=) Prelude.<$> colors,
            ("EmptyFillColor" Data..=)
              Prelude.<$> emptyFillColor,
            ("MinMaxGradient" Data..=)
              Prelude.<$> minMaxGradient
          ]
      )
