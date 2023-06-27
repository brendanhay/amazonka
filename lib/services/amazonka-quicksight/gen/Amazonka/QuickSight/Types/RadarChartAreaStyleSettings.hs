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
-- Module      : Amazonka.QuickSight.Types.RadarChartAreaStyleSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RadarChartAreaStyleSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.Visibility

-- | The configured style settings of a radar chart.
--
-- /See:/ 'newRadarChartAreaStyleSettings' smart constructor.
data RadarChartAreaStyleSettings = RadarChartAreaStyleSettings'
  { -- | The visibility settings of a radar chart.
    visibility :: Prelude.Maybe Visibility
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RadarChartAreaStyleSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibility', 'radarChartAreaStyleSettings_visibility' - The visibility settings of a radar chart.
newRadarChartAreaStyleSettings ::
  RadarChartAreaStyleSettings
newRadarChartAreaStyleSettings =
  RadarChartAreaStyleSettings'
    { visibility =
        Prelude.Nothing
    }

-- | The visibility settings of a radar chart.
radarChartAreaStyleSettings_visibility :: Lens.Lens' RadarChartAreaStyleSettings (Prelude.Maybe Visibility)
radarChartAreaStyleSettings_visibility = Lens.lens (\RadarChartAreaStyleSettings' {visibility} -> visibility) (\s@RadarChartAreaStyleSettings' {} a -> s {visibility = a} :: RadarChartAreaStyleSettings)

instance Data.FromJSON RadarChartAreaStyleSettings where
  parseJSON =
    Data.withObject
      "RadarChartAreaStyleSettings"
      ( \x ->
          RadarChartAreaStyleSettings'
            Prelude.<$> (x Data..:? "Visibility")
      )

instance Prelude.Hashable RadarChartAreaStyleSettings where
  hashWithSalt _salt RadarChartAreaStyleSettings' {..} =
    _salt `Prelude.hashWithSalt` visibility

instance Prelude.NFData RadarChartAreaStyleSettings where
  rnf RadarChartAreaStyleSettings' {..} =
    Prelude.rnf visibility

instance Data.ToJSON RadarChartAreaStyleSettings where
  toJSON RadarChartAreaStyleSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Visibility" Data..=) Prelude.<$> visibility]
      )
