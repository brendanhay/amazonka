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
-- Module      : Amazonka.QuickSight.Types.LineChartDefaultSeriesSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartDefaultSeriesSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisBinding
import Amazonka.QuickSight.Types.LineChartLineStyleSettings
import Amazonka.QuickSight.Types.LineChartMarkerStyleSettings

-- | The options that determine the default presentation of all line series
-- in @LineChartVisual@.
--
-- /See:/ 'newLineChartDefaultSeriesSettings' smart constructor.
data LineChartDefaultSeriesSettings = LineChartDefaultSeriesSettings'
  { -- | The axis to which you are binding all line series to.
    axisBinding :: Prelude.Maybe AxisBinding,
    -- | Line styles options for all line series in the visual.
    lineStyleSettings :: Prelude.Maybe LineChartLineStyleSettings,
    -- | Marker styles options for all line series in the visual.
    markerStyleSettings :: Prelude.Maybe LineChartMarkerStyleSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartDefaultSeriesSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'axisBinding', 'lineChartDefaultSeriesSettings_axisBinding' - The axis to which you are binding all line series to.
--
-- 'lineStyleSettings', 'lineChartDefaultSeriesSettings_lineStyleSettings' - Line styles options for all line series in the visual.
--
-- 'markerStyleSettings', 'lineChartDefaultSeriesSettings_markerStyleSettings' - Marker styles options for all line series in the visual.
newLineChartDefaultSeriesSettings ::
  LineChartDefaultSeriesSettings
newLineChartDefaultSeriesSettings =
  LineChartDefaultSeriesSettings'
    { axisBinding =
        Prelude.Nothing,
      lineStyleSettings = Prelude.Nothing,
      markerStyleSettings = Prelude.Nothing
    }

-- | The axis to which you are binding all line series to.
lineChartDefaultSeriesSettings_axisBinding :: Lens.Lens' LineChartDefaultSeriesSettings (Prelude.Maybe AxisBinding)
lineChartDefaultSeriesSettings_axisBinding = Lens.lens (\LineChartDefaultSeriesSettings' {axisBinding} -> axisBinding) (\s@LineChartDefaultSeriesSettings' {} a -> s {axisBinding = a} :: LineChartDefaultSeriesSettings)

-- | Line styles options for all line series in the visual.
lineChartDefaultSeriesSettings_lineStyleSettings :: Lens.Lens' LineChartDefaultSeriesSettings (Prelude.Maybe LineChartLineStyleSettings)
lineChartDefaultSeriesSettings_lineStyleSettings = Lens.lens (\LineChartDefaultSeriesSettings' {lineStyleSettings} -> lineStyleSettings) (\s@LineChartDefaultSeriesSettings' {} a -> s {lineStyleSettings = a} :: LineChartDefaultSeriesSettings)

-- | Marker styles options for all line series in the visual.
lineChartDefaultSeriesSettings_markerStyleSettings :: Lens.Lens' LineChartDefaultSeriesSettings (Prelude.Maybe LineChartMarkerStyleSettings)
lineChartDefaultSeriesSettings_markerStyleSettings = Lens.lens (\LineChartDefaultSeriesSettings' {markerStyleSettings} -> markerStyleSettings) (\s@LineChartDefaultSeriesSettings' {} a -> s {markerStyleSettings = a} :: LineChartDefaultSeriesSettings)

instance Data.FromJSON LineChartDefaultSeriesSettings where
  parseJSON =
    Data.withObject
      "LineChartDefaultSeriesSettings"
      ( \x ->
          LineChartDefaultSeriesSettings'
            Prelude.<$> (x Data..:? "AxisBinding")
            Prelude.<*> (x Data..:? "LineStyleSettings")
            Prelude.<*> (x Data..:? "MarkerStyleSettings")
      )

instance
  Prelude.Hashable
    LineChartDefaultSeriesSettings
  where
  hashWithSalt
    _salt
    LineChartDefaultSeriesSettings' {..} =
      _salt `Prelude.hashWithSalt` axisBinding
        `Prelude.hashWithSalt` lineStyleSettings
        `Prelude.hashWithSalt` markerStyleSettings

instance
  Prelude.NFData
    LineChartDefaultSeriesSettings
  where
  rnf LineChartDefaultSeriesSettings' {..} =
    Prelude.rnf axisBinding
      `Prelude.seq` Prelude.rnf lineStyleSettings
      `Prelude.seq` Prelude.rnf markerStyleSettings

instance Data.ToJSON LineChartDefaultSeriesSettings where
  toJSON LineChartDefaultSeriesSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AxisBinding" Data..=) Prelude.<$> axisBinding,
            ("LineStyleSettings" Data..=)
              Prelude.<$> lineStyleSettings,
            ("MarkerStyleSettings" Data..=)
              Prelude.<$> markerStyleSettings
          ]
      )
