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
-- Module      : Amazonka.QuickSight.Types.RadarChartSeriesSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartSeriesSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.RadarChartAreaStyleSettings

-- | The series settings of a radar chart.
--
-- /See:/ 'newRadarChartSeriesSettings' smart constructor.
data RadarChartSeriesSettings = RadarChartSeriesSettings'
  { -- | The area style settings of a radar chart.
    areaStyleSettings :: Prelude.Maybe RadarChartAreaStyleSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadarChartSeriesSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'areaStyleSettings', 'radarChartSeriesSettings_areaStyleSettings' - The area style settings of a radar chart.
newRadarChartSeriesSettings ::
  RadarChartSeriesSettings
newRadarChartSeriesSettings =
  RadarChartSeriesSettings'
    { areaStyleSettings =
        Prelude.Nothing
    }

-- | The area style settings of a radar chart.
radarChartSeriesSettings_areaStyleSettings :: Lens.Lens' RadarChartSeriesSettings (Prelude.Maybe RadarChartAreaStyleSettings)
radarChartSeriesSettings_areaStyleSettings = Lens.lens (\RadarChartSeriesSettings' {areaStyleSettings} -> areaStyleSettings) (\s@RadarChartSeriesSettings' {} a -> s {areaStyleSettings = a} :: RadarChartSeriesSettings)

instance Data.FromJSON RadarChartSeriesSettings where
  parseJSON =
    Data.withObject
      "RadarChartSeriesSettings"
      ( \x ->
          RadarChartSeriesSettings'
            Prelude.<$> (x Data..:? "AreaStyleSettings")
      )

instance Prelude.Hashable RadarChartSeriesSettings where
  hashWithSalt _salt RadarChartSeriesSettings' {..} =
    _salt `Prelude.hashWithSalt` areaStyleSettings

instance Prelude.NFData RadarChartSeriesSettings where
  rnf RadarChartSeriesSettings' {..} =
    Prelude.rnf areaStyleSettings

instance Data.ToJSON RadarChartSeriesSettings where
  toJSON RadarChartSeriesSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AreaStyleSettings" Data..=)
              Prelude.<$> areaStyleSettings
          ]
      )
