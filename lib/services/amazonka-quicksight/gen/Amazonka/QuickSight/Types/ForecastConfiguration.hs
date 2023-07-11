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
-- Module      : Amazonka.QuickSight.Types.ForecastConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ForecastConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ForecastScenario
import Amazonka.QuickSight.Types.TimeBasedForecastProperties

-- | The forecast configuration that is used in a line chart\'s display
-- properties.
--
-- /See:/ 'newForecastConfiguration' smart constructor.
data ForecastConfiguration = ForecastConfiguration'
  { -- | The forecast properties setup of a forecast in the line chart.
    forecastProperties :: Prelude.Maybe TimeBasedForecastProperties,
    -- | The forecast scenario of a forecast in the line chart.
    scenario :: Prelude.Maybe ForecastScenario
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecastProperties', 'forecastConfiguration_forecastProperties' - The forecast properties setup of a forecast in the line chart.
--
-- 'scenario', 'forecastConfiguration_scenario' - The forecast scenario of a forecast in the line chart.
newForecastConfiguration ::
  ForecastConfiguration
newForecastConfiguration =
  ForecastConfiguration'
    { forecastProperties =
        Prelude.Nothing,
      scenario = Prelude.Nothing
    }

-- | The forecast properties setup of a forecast in the line chart.
forecastConfiguration_forecastProperties :: Lens.Lens' ForecastConfiguration (Prelude.Maybe TimeBasedForecastProperties)
forecastConfiguration_forecastProperties = Lens.lens (\ForecastConfiguration' {forecastProperties} -> forecastProperties) (\s@ForecastConfiguration' {} a -> s {forecastProperties = a} :: ForecastConfiguration)

-- | The forecast scenario of a forecast in the line chart.
forecastConfiguration_scenario :: Lens.Lens' ForecastConfiguration (Prelude.Maybe ForecastScenario)
forecastConfiguration_scenario = Lens.lens (\ForecastConfiguration' {scenario} -> scenario) (\s@ForecastConfiguration' {} a -> s {scenario = a} :: ForecastConfiguration)

instance Data.FromJSON ForecastConfiguration where
  parseJSON =
    Data.withObject
      "ForecastConfiguration"
      ( \x ->
          ForecastConfiguration'
            Prelude.<$> (x Data..:? "ForecastProperties")
            Prelude.<*> (x Data..:? "Scenario")
      )

instance Prelude.Hashable ForecastConfiguration where
  hashWithSalt _salt ForecastConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` forecastProperties
      `Prelude.hashWithSalt` scenario

instance Prelude.NFData ForecastConfiguration where
  rnf ForecastConfiguration' {..} =
    Prelude.rnf forecastProperties
      `Prelude.seq` Prelude.rnf scenario

instance Data.ToJSON ForecastConfiguration where
  toJSON ForecastConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForecastProperties" Data..=)
              Prelude.<$> forecastProperties,
            ("Scenario" Data..=) Prelude.<$> scenario
          ]
      )
