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
-- Module      : Amazonka.QuickSight.Types.VisualPalette
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.VisualPalette where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataPathColor

-- | The visual display options for the visual palette.
--
-- /See:/ 'newVisualPalette' smart constructor.
data VisualPalette = VisualPalette'
  { -- | The chart color options for the visual palette.
    chartColor :: Prelude.Maybe Prelude.Text,
    -- | The color map options for the visual palette.
    colorMap :: Prelude.Maybe [DataPathColor]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VisualPalette' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'chartColor', 'visualPalette_chartColor' - The chart color options for the visual palette.
--
-- 'colorMap', 'visualPalette_colorMap' - The color map options for the visual palette.
newVisualPalette ::
  VisualPalette
newVisualPalette =
  VisualPalette'
    { chartColor = Prelude.Nothing,
      colorMap = Prelude.Nothing
    }

-- | The chart color options for the visual palette.
visualPalette_chartColor :: Lens.Lens' VisualPalette (Prelude.Maybe Prelude.Text)
visualPalette_chartColor = Lens.lens (\VisualPalette' {chartColor} -> chartColor) (\s@VisualPalette' {} a -> s {chartColor = a} :: VisualPalette)

-- | The color map options for the visual palette.
visualPalette_colorMap :: Lens.Lens' VisualPalette (Prelude.Maybe [DataPathColor])
visualPalette_colorMap = Lens.lens (\VisualPalette' {colorMap} -> colorMap) (\s@VisualPalette' {} a -> s {colorMap = a} :: VisualPalette) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON VisualPalette where
  parseJSON =
    Data.withObject
      "VisualPalette"
      ( \x ->
          VisualPalette'
            Prelude.<$> (x Data..:? "ChartColor")
            Prelude.<*> (x Data..:? "ColorMap" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable VisualPalette where
  hashWithSalt _salt VisualPalette' {..} =
    _salt
      `Prelude.hashWithSalt` chartColor
      `Prelude.hashWithSalt` colorMap

instance Prelude.NFData VisualPalette where
  rnf VisualPalette' {..} =
    Prelude.rnf chartColor `Prelude.seq`
      Prelude.rnf colorMap

instance Data.ToJSON VisualPalette where
  toJSON VisualPalette' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ChartColor" Data..=) Prelude.<$> chartColor,
            ("ColorMap" Data..=) Prelude.<$> colorMap
          ]
      )
