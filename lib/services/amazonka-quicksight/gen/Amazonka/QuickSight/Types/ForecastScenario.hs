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
-- Module      : Amazonka.QuickSight.Types.ForecastScenario
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ForecastScenario where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.WhatIfPointScenario
import Amazonka.QuickSight.Types.WhatIfRangeScenario

-- | The forecast scenario of a forecast in the line chart.
--
-- /See:/ 'newForecastScenario' smart constructor.
data ForecastScenario = ForecastScenario'
  { -- | The what-if analysis forecast setup with the target date.
    whatIfPointScenario :: Prelude.Maybe WhatIfPointScenario,
    -- | The what-if analysis forecast setup with the date range.
    whatIfRangeScenario :: Prelude.Maybe WhatIfRangeScenario
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastScenario' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfPointScenario', 'forecastScenario_whatIfPointScenario' - The what-if analysis forecast setup with the target date.
--
-- 'whatIfRangeScenario', 'forecastScenario_whatIfRangeScenario' - The what-if analysis forecast setup with the date range.
newForecastScenario ::
  ForecastScenario
newForecastScenario =
  ForecastScenario'
    { whatIfPointScenario =
        Prelude.Nothing,
      whatIfRangeScenario = Prelude.Nothing
    }

-- | The what-if analysis forecast setup with the target date.
forecastScenario_whatIfPointScenario :: Lens.Lens' ForecastScenario (Prelude.Maybe WhatIfPointScenario)
forecastScenario_whatIfPointScenario = Lens.lens (\ForecastScenario' {whatIfPointScenario} -> whatIfPointScenario) (\s@ForecastScenario' {} a -> s {whatIfPointScenario = a} :: ForecastScenario)

-- | The what-if analysis forecast setup with the date range.
forecastScenario_whatIfRangeScenario :: Lens.Lens' ForecastScenario (Prelude.Maybe WhatIfRangeScenario)
forecastScenario_whatIfRangeScenario = Lens.lens (\ForecastScenario' {whatIfRangeScenario} -> whatIfRangeScenario) (\s@ForecastScenario' {} a -> s {whatIfRangeScenario = a} :: ForecastScenario)

instance Data.FromJSON ForecastScenario where
  parseJSON =
    Data.withObject
      "ForecastScenario"
      ( \x ->
          ForecastScenario'
            Prelude.<$> (x Data..:? "WhatIfPointScenario")
            Prelude.<*> (x Data..:? "WhatIfRangeScenario")
      )

instance Prelude.Hashable ForecastScenario where
  hashWithSalt _salt ForecastScenario' {..} =
    _salt
      `Prelude.hashWithSalt` whatIfPointScenario
      `Prelude.hashWithSalt` whatIfRangeScenario

instance Prelude.NFData ForecastScenario where
  rnf ForecastScenario' {..} =
    Prelude.rnf whatIfPointScenario
      `Prelude.seq` Prelude.rnf whatIfRangeScenario

instance Data.ToJSON ForecastScenario where
  toJSON ForecastScenario' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WhatIfPointScenario" Data..=)
              Prelude.<$> whatIfPointScenario,
            ("WhatIfRangeScenario" Data..=)
              Prelude.<$> whatIfRangeScenario
          ]
      )
