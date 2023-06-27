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
-- Module      : Amazonka.QuickSight.Types.TimeBasedForecastProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TimeBasedForecastProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The forecast properties setup of a forecast in the line chart.
--
-- /See:/ 'newTimeBasedForecastProperties' smart constructor.
data TimeBasedForecastProperties = TimeBasedForecastProperties'
  { -- | The lower boundary setup of a forecast computation.
    lowerBoundary :: Prelude.Maybe Prelude.Double,
    -- | The periods backward setup of a forecast computation.
    periodsBackward :: Prelude.Maybe Prelude.Natural,
    -- | The periods forward setup of a forecast computation.
    periodsForward :: Prelude.Maybe Prelude.Natural,
    -- | The prediction interval setup of a forecast computation.
    predictionInterval :: Prelude.Maybe Prelude.Natural,
    -- | The seasonality setup of a forecast computation. Choose one of the
    -- following options:
    --
    -- -   @NULL@: The input is set to @NULL@.
    --
    -- -   @NON_NULL@: The input is set to a custom value.
    seasonality :: Prelude.Maybe Prelude.Natural,
    -- | The upper boundary setup of a forecast computation.
    upperBoundary :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeBasedForecastProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lowerBoundary', 'timeBasedForecastProperties_lowerBoundary' - The lower boundary setup of a forecast computation.
--
-- 'periodsBackward', 'timeBasedForecastProperties_periodsBackward' - The periods backward setup of a forecast computation.
--
-- 'periodsForward', 'timeBasedForecastProperties_periodsForward' - The periods forward setup of a forecast computation.
--
-- 'predictionInterval', 'timeBasedForecastProperties_predictionInterval' - The prediction interval setup of a forecast computation.
--
-- 'seasonality', 'timeBasedForecastProperties_seasonality' - The seasonality setup of a forecast computation. Choose one of the
-- following options:
--
-- -   @NULL@: The input is set to @NULL@.
--
-- -   @NON_NULL@: The input is set to a custom value.
--
-- 'upperBoundary', 'timeBasedForecastProperties_upperBoundary' - The upper boundary setup of a forecast computation.
newTimeBasedForecastProperties ::
  TimeBasedForecastProperties
newTimeBasedForecastProperties =
  TimeBasedForecastProperties'
    { lowerBoundary =
        Prelude.Nothing,
      periodsBackward = Prelude.Nothing,
      periodsForward = Prelude.Nothing,
      predictionInterval = Prelude.Nothing,
      seasonality = Prelude.Nothing,
      upperBoundary = Prelude.Nothing
    }

-- | The lower boundary setup of a forecast computation.
timeBasedForecastProperties_lowerBoundary :: Lens.Lens' TimeBasedForecastProperties (Prelude.Maybe Prelude.Double)
timeBasedForecastProperties_lowerBoundary = Lens.lens (\TimeBasedForecastProperties' {lowerBoundary} -> lowerBoundary) (\s@TimeBasedForecastProperties' {} a -> s {lowerBoundary = a} :: TimeBasedForecastProperties)

-- | The periods backward setup of a forecast computation.
timeBasedForecastProperties_periodsBackward :: Lens.Lens' TimeBasedForecastProperties (Prelude.Maybe Prelude.Natural)
timeBasedForecastProperties_periodsBackward = Lens.lens (\TimeBasedForecastProperties' {periodsBackward} -> periodsBackward) (\s@TimeBasedForecastProperties' {} a -> s {periodsBackward = a} :: TimeBasedForecastProperties)

-- | The periods forward setup of a forecast computation.
timeBasedForecastProperties_periodsForward :: Lens.Lens' TimeBasedForecastProperties (Prelude.Maybe Prelude.Natural)
timeBasedForecastProperties_periodsForward = Lens.lens (\TimeBasedForecastProperties' {periodsForward} -> periodsForward) (\s@TimeBasedForecastProperties' {} a -> s {periodsForward = a} :: TimeBasedForecastProperties)

-- | The prediction interval setup of a forecast computation.
timeBasedForecastProperties_predictionInterval :: Lens.Lens' TimeBasedForecastProperties (Prelude.Maybe Prelude.Natural)
timeBasedForecastProperties_predictionInterval = Lens.lens (\TimeBasedForecastProperties' {predictionInterval} -> predictionInterval) (\s@TimeBasedForecastProperties' {} a -> s {predictionInterval = a} :: TimeBasedForecastProperties)

-- | The seasonality setup of a forecast computation. Choose one of the
-- following options:
--
-- -   @NULL@: The input is set to @NULL@.
--
-- -   @NON_NULL@: The input is set to a custom value.
timeBasedForecastProperties_seasonality :: Lens.Lens' TimeBasedForecastProperties (Prelude.Maybe Prelude.Natural)
timeBasedForecastProperties_seasonality = Lens.lens (\TimeBasedForecastProperties' {seasonality} -> seasonality) (\s@TimeBasedForecastProperties' {} a -> s {seasonality = a} :: TimeBasedForecastProperties)

-- | The upper boundary setup of a forecast computation.
timeBasedForecastProperties_upperBoundary :: Lens.Lens' TimeBasedForecastProperties (Prelude.Maybe Prelude.Double)
timeBasedForecastProperties_upperBoundary = Lens.lens (\TimeBasedForecastProperties' {upperBoundary} -> upperBoundary) (\s@TimeBasedForecastProperties' {} a -> s {upperBoundary = a} :: TimeBasedForecastProperties)

instance Data.FromJSON TimeBasedForecastProperties where
  parseJSON =
    Data.withObject
      "TimeBasedForecastProperties"
      ( \x ->
          TimeBasedForecastProperties'
            Prelude.<$> (x Data..:? "LowerBoundary")
            Prelude.<*> (x Data..:? "PeriodsBackward")
            Prelude.<*> (x Data..:? "PeriodsForward")
            Prelude.<*> (x Data..:? "PredictionInterval")
            Prelude.<*> (x Data..:? "Seasonality")
            Prelude.<*> (x Data..:? "UpperBoundary")
      )

instance Prelude.Hashable TimeBasedForecastProperties where
  hashWithSalt _salt TimeBasedForecastProperties' {..} =
    _salt
      `Prelude.hashWithSalt` lowerBoundary
      `Prelude.hashWithSalt` periodsBackward
      `Prelude.hashWithSalt` periodsForward
      `Prelude.hashWithSalt` predictionInterval
      `Prelude.hashWithSalt` seasonality
      `Prelude.hashWithSalt` upperBoundary

instance Prelude.NFData TimeBasedForecastProperties where
  rnf TimeBasedForecastProperties' {..} =
    Prelude.rnf lowerBoundary
      `Prelude.seq` Prelude.rnf periodsBackward
      `Prelude.seq` Prelude.rnf periodsForward
      `Prelude.seq` Prelude.rnf predictionInterval
      `Prelude.seq` Prelude.rnf seasonality
      `Prelude.seq` Prelude.rnf upperBoundary

instance Data.ToJSON TimeBasedForecastProperties where
  toJSON TimeBasedForecastProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LowerBoundary" Data..=) Prelude.<$> lowerBoundary,
            ("PeriodsBackward" Data..=)
              Prelude.<$> periodsBackward,
            ("PeriodsForward" Data..=)
              Prelude.<$> periodsForward,
            ("PredictionInterval" Data..=)
              Prelude.<$> predictionInterval,
            ("Seasonality" Data..=) Prelude.<$> seasonality,
            ("UpperBoundary" Data..=) Prelude.<$> upperBoundary
          ]
      )
