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
-- Module      : Amazonka.QuickSight.Types.ColorScale
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColorScale where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColorFillType
import Amazonka.QuickSight.Types.DataColor

-- | Determines the color scale that is applied to the visual.
--
-- /See:/ 'newColorScale' smart constructor.
data ColorScale = ColorScale'
  { -- | Determines the color that is applied to null values.
    nullValueColor :: Prelude.Maybe DataColor,
    -- | Determines the list of colors that are applied to the visual.
    colors :: Prelude.NonEmpty DataColor,
    -- | Determines the color fill type.
    colorFillType :: ColorFillType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColorScale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nullValueColor', 'colorScale_nullValueColor' - Determines the color that is applied to null values.
--
-- 'colors', 'colorScale_colors' - Determines the list of colors that are applied to the visual.
--
-- 'colorFillType', 'colorScale_colorFillType' - Determines the color fill type.
newColorScale ::
  -- | 'colors'
  Prelude.NonEmpty DataColor ->
  -- | 'colorFillType'
  ColorFillType ->
  ColorScale
newColorScale pColors_ pColorFillType_ =
  ColorScale'
    { nullValueColor = Prelude.Nothing,
      colors = Lens.coerced Lens.# pColors_,
      colorFillType = pColorFillType_
    }

-- | Determines the color that is applied to null values.
colorScale_nullValueColor :: Lens.Lens' ColorScale (Prelude.Maybe DataColor)
colorScale_nullValueColor = Lens.lens (\ColorScale' {nullValueColor} -> nullValueColor) (\s@ColorScale' {} a -> s {nullValueColor = a} :: ColorScale)

-- | Determines the list of colors that are applied to the visual.
colorScale_colors :: Lens.Lens' ColorScale (Prelude.NonEmpty DataColor)
colorScale_colors = Lens.lens (\ColorScale' {colors} -> colors) (\s@ColorScale' {} a -> s {colors = a} :: ColorScale) Prelude.. Lens.coerced

-- | Determines the color fill type.
colorScale_colorFillType :: Lens.Lens' ColorScale ColorFillType
colorScale_colorFillType = Lens.lens (\ColorScale' {colorFillType} -> colorFillType) (\s@ColorScale' {} a -> s {colorFillType = a} :: ColorScale)

instance Data.FromJSON ColorScale where
  parseJSON =
    Data.withObject
      "ColorScale"
      ( \x ->
          ColorScale'
            Prelude.<$> (x Data..:? "NullValueColor")
            Prelude.<*> (x Data..: "Colors")
            Prelude.<*> (x Data..: "ColorFillType")
      )

instance Prelude.Hashable ColorScale where
  hashWithSalt _salt ColorScale' {..} =
    _salt
      `Prelude.hashWithSalt` nullValueColor
      `Prelude.hashWithSalt` colors
      `Prelude.hashWithSalt` colorFillType

instance Prelude.NFData ColorScale where
  rnf ColorScale' {..} =
    Prelude.rnf nullValueColor
      `Prelude.seq` Prelude.rnf colors
      `Prelude.seq` Prelude.rnf colorFillType

instance Data.ToJSON ColorScale where
  toJSON ColorScale' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NullValueColor" Data..=)
              Prelude.<$> nullValueColor,
            Prelude.Just ("Colors" Data..= colors),
            Prelude.Just
              ("ColorFillType" Data..= colorFillType)
          ]
      )
