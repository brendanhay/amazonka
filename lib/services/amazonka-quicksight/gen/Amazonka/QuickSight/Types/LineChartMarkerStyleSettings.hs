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
-- Module      : Amazonka.QuickSight.Types.LineChartMarkerStyleSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartMarkerStyleSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LineChartMarkerShape
import Amazonka.QuickSight.Types.Visibility

-- | Marker styles options for a line series in @LineChartVisual@.
--
-- /See:/ 'newLineChartMarkerStyleSettings' smart constructor.
data LineChartMarkerStyleSettings = LineChartMarkerStyleSettings'
  { -- | Color of marker in the series.
    markerColor :: Prelude.Maybe Prelude.Text,
    -- | Shape option for markers in the series.
    --
    -- -   @CIRCLE@: Show marker as a circle.
    --
    -- -   @TRIANGLE@: Show marker as a triangle.
    --
    -- -   @SQUARE@: Show marker as a square.
    --
    -- -   @DIAMOND@: Show marker as a diamond.
    --
    -- -   @ROUNDED_SQUARE@: Show marker as a rounded square.
    markerShape :: Prelude.Maybe LineChartMarkerShape,
    -- | Size of marker in the series.
    markerSize :: Prelude.Maybe Prelude.Text,
    -- | Configuration option that determines whether to show the markers in the
    -- series.
    markerVisibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartMarkerStyleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'markerColor', 'lineChartMarkerStyleSettings_markerColor' - Color of marker in the series.
--
-- 'markerShape', 'lineChartMarkerStyleSettings_markerShape' - Shape option for markers in the series.
--
-- -   @CIRCLE@: Show marker as a circle.
--
-- -   @TRIANGLE@: Show marker as a triangle.
--
-- -   @SQUARE@: Show marker as a square.
--
-- -   @DIAMOND@: Show marker as a diamond.
--
-- -   @ROUNDED_SQUARE@: Show marker as a rounded square.
--
-- 'markerSize', 'lineChartMarkerStyleSettings_markerSize' - Size of marker in the series.
--
-- 'markerVisibility', 'lineChartMarkerStyleSettings_markerVisibility' - Configuration option that determines whether to show the markers in the
-- series.
newLineChartMarkerStyleSettings ::
  LineChartMarkerStyleSettings
newLineChartMarkerStyleSettings =
  LineChartMarkerStyleSettings'
    { markerColor =
        Prelude.Nothing,
      markerShape = Prelude.Nothing,
      markerSize = Prelude.Nothing,
      markerVisibility = Prelude.Nothing
    }

-- | Color of marker in the series.
lineChartMarkerStyleSettings_markerColor :: Lens.Lens' LineChartMarkerStyleSettings (Prelude.Maybe Prelude.Text)
lineChartMarkerStyleSettings_markerColor = Lens.lens (\LineChartMarkerStyleSettings' {markerColor} -> markerColor) (\s@LineChartMarkerStyleSettings' {} a -> s {markerColor = a} :: LineChartMarkerStyleSettings)

-- | Shape option for markers in the series.
--
-- -   @CIRCLE@: Show marker as a circle.
--
-- -   @TRIANGLE@: Show marker as a triangle.
--
-- -   @SQUARE@: Show marker as a square.
--
-- -   @DIAMOND@: Show marker as a diamond.
--
-- -   @ROUNDED_SQUARE@: Show marker as a rounded square.
lineChartMarkerStyleSettings_markerShape :: Lens.Lens' LineChartMarkerStyleSettings (Prelude.Maybe LineChartMarkerShape)
lineChartMarkerStyleSettings_markerShape = Lens.lens (\LineChartMarkerStyleSettings' {markerShape} -> markerShape) (\s@LineChartMarkerStyleSettings' {} a -> s {markerShape = a} :: LineChartMarkerStyleSettings)

-- | Size of marker in the series.
lineChartMarkerStyleSettings_markerSize :: Lens.Lens' LineChartMarkerStyleSettings (Prelude.Maybe Prelude.Text)
lineChartMarkerStyleSettings_markerSize = Lens.lens (\LineChartMarkerStyleSettings' {markerSize} -> markerSize) (\s@LineChartMarkerStyleSettings' {} a -> s {markerSize = a} :: LineChartMarkerStyleSettings)

-- | Configuration option that determines whether to show the markers in the
-- series.
lineChartMarkerStyleSettings_markerVisibility :: Lens.Lens' LineChartMarkerStyleSettings (Prelude.Maybe Visibility)
lineChartMarkerStyleSettings_markerVisibility = Lens.lens (\LineChartMarkerStyleSettings' {markerVisibility} -> markerVisibility) (\s@LineChartMarkerStyleSettings' {} a -> s {markerVisibility = a} :: LineChartMarkerStyleSettings)

instance Data.FromJSON LineChartMarkerStyleSettings where
  parseJSON =
    Data.withObject
      "LineChartMarkerStyleSettings"
      ( \x ->
          LineChartMarkerStyleSettings'
            Prelude.<$> (x Data..:? "MarkerColor")
            Prelude.<*> (x Data..:? "MarkerShape")
            Prelude.<*> (x Data..:? "MarkerSize")
            Prelude.<*> (x Data..:? "MarkerVisibility")
      )

instance
  Prelude.Hashable
    LineChartMarkerStyleSettings
  where
  hashWithSalt _salt LineChartMarkerStyleSettings' {..} =
    _salt `Prelude.hashWithSalt` markerColor
      `Prelude.hashWithSalt` markerShape
      `Prelude.hashWithSalt` markerSize
      `Prelude.hashWithSalt` markerVisibility

instance Prelude.NFData LineChartMarkerStyleSettings where
  rnf LineChartMarkerStyleSettings' {..} =
    Prelude.rnf markerColor
      `Prelude.seq` Prelude.rnf markerShape
      `Prelude.seq` Prelude.rnf markerSize
      `Prelude.seq` Prelude.rnf markerVisibility

instance Data.ToJSON LineChartMarkerStyleSettings where
  toJSON LineChartMarkerStyleSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MarkerColor" Data..=) Prelude.<$> markerColor,
            ("MarkerShape" Data..=) Prelude.<$> markerShape,
            ("MarkerSize" Data..=) Prelude.<$> markerSize,
            ("MarkerVisibility" Data..=)
              Prelude.<$> markerVisibility
          ]
      )
