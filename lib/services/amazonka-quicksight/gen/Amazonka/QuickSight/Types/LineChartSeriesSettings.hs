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
-- Module      : Amazonka.QuickSight.Types.LineChartSeriesSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.LineChartSeriesSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.LineChartLineStyleSettings
import Amazonka.QuickSight.Types.LineChartMarkerStyleSettings

-- | The options that determine the presentation of a line series in the
-- visual
--
-- /See:/ 'newLineChartSeriesSettings' smart constructor.
data LineChartSeriesSettings = LineChartSeriesSettings'
  { -- | Line styles options for a line series in @LineChartVisual@.
    lineStyleSettings :: Prelude.Maybe LineChartLineStyleSettings,
    -- | Marker styles options for a line series in @LineChartVisual@.
    markerStyleSettings :: Prelude.Maybe LineChartMarkerStyleSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineChartSeriesSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineStyleSettings', 'lineChartSeriesSettings_lineStyleSettings' - Line styles options for a line series in @LineChartVisual@.
--
-- 'markerStyleSettings', 'lineChartSeriesSettings_markerStyleSettings' - Marker styles options for a line series in @LineChartVisual@.
newLineChartSeriesSettings ::
  LineChartSeriesSettings
newLineChartSeriesSettings =
  LineChartSeriesSettings'
    { lineStyleSettings =
        Prelude.Nothing,
      markerStyleSettings = Prelude.Nothing
    }

-- | Line styles options for a line series in @LineChartVisual@.
lineChartSeriesSettings_lineStyleSettings :: Lens.Lens' LineChartSeriesSettings (Prelude.Maybe LineChartLineStyleSettings)
lineChartSeriesSettings_lineStyleSettings = Lens.lens (\LineChartSeriesSettings' {lineStyleSettings} -> lineStyleSettings) (\s@LineChartSeriesSettings' {} a -> s {lineStyleSettings = a} :: LineChartSeriesSettings)

-- | Marker styles options for a line series in @LineChartVisual@.
lineChartSeriesSettings_markerStyleSettings :: Lens.Lens' LineChartSeriesSettings (Prelude.Maybe LineChartMarkerStyleSettings)
lineChartSeriesSettings_markerStyleSettings = Lens.lens (\LineChartSeriesSettings' {markerStyleSettings} -> markerStyleSettings) (\s@LineChartSeriesSettings' {} a -> s {markerStyleSettings = a} :: LineChartSeriesSettings)

instance Data.FromJSON LineChartSeriesSettings where
  parseJSON =
    Data.withObject
      "LineChartSeriesSettings"
      ( \x ->
          LineChartSeriesSettings'
            Prelude.<$> (x Data..:? "LineStyleSettings")
            Prelude.<*> (x Data..:? "MarkerStyleSettings")
      )

instance Prelude.Hashable LineChartSeriesSettings where
  hashWithSalt _salt LineChartSeriesSettings' {..} =
    _salt
      `Prelude.hashWithSalt` lineStyleSettings
      `Prelude.hashWithSalt` markerStyleSettings

instance Prelude.NFData LineChartSeriesSettings where
  rnf LineChartSeriesSettings' {..} =
    Prelude.rnf lineStyleSettings
      `Prelude.seq` Prelude.rnf markerStyleSettings

instance Data.ToJSON LineChartSeriesSettings where
  toJSON LineChartSeriesSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LineStyleSettings" Data..=)
              Prelude.<$> lineStyleSettings,
            ("MarkerStyleSettings" Data..=)
              Prelude.<$> markerStyleSettings
          ]
      )
