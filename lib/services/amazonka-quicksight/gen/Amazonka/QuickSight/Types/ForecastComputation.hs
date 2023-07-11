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
-- Module      : Amazonka.QuickSight.Types.ForecastComputation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ForecastComputation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DimensionField
import Amazonka.QuickSight.Types.ForecastComputationSeasonality
import Amazonka.QuickSight.Types.MeasureField

-- | The forecast computation configuration.
--
-- /See:/ 'newForecastComputation' smart constructor.
data ForecastComputation = ForecastComputation'
  { -- | The custom seasonality value setup of a forecast computation.
    customSeasonalityValue :: Prelude.Maybe Prelude.Natural,
    -- | The lower boundary setup of a forecast computation.
    lowerBoundary :: Prelude.Maybe Prelude.Double,
    -- | The name of a computation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The periods backward setup of a forecast computation.
    periodsBackward :: Prelude.Maybe Prelude.Natural,
    -- | The periods forward setup of a forecast computation.
    periodsForward :: Prelude.Maybe Prelude.Natural,
    -- | The prediction interval setup of a forecast computation.
    predictionInterval :: Prelude.Maybe Prelude.Natural,
    -- | The seasonality setup of a forecast computation. Choose one of the
    -- following options:
    --
    -- -   @AUTOMATIC@
    --
    -- -   @CUSTOM@: Checks the custom seasonality value.
    seasonality :: Prelude.Maybe ForecastComputationSeasonality,
    -- | The upper boundary setup of a forecast computation.
    upperBoundary :: Prelude.Maybe Prelude.Double,
    -- | The value field that is used in a computation.
    value :: Prelude.Maybe MeasureField,
    -- | The ID for a computation.
    computationId :: Prelude.Text,
    -- | The time field that is used in a computation.
    time :: DimensionField
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForecastComputation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customSeasonalityValue', 'forecastComputation_customSeasonalityValue' - The custom seasonality value setup of a forecast computation.
--
-- 'lowerBoundary', 'forecastComputation_lowerBoundary' - The lower boundary setup of a forecast computation.
--
-- 'name', 'forecastComputation_name' - The name of a computation.
--
-- 'periodsBackward', 'forecastComputation_periodsBackward' - The periods backward setup of a forecast computation.
--
-- 'periodsForward', 'forecastComputation_periodsForward' - The periods forward setup of a forecast computation.
--
-- 'predictionInterval', 'forecastComputation_predictionInterval' - The prediction interval setup of a forecast computation.
--
-- 'seasonality', 'forecastComputation_seasonality' - The seasonality setup of a forecast computation. Choose one of the
-- following options:
--
-- -   @AUTOMATIC@
--
-- -   @CUSTOM@: Checks the custom seasonality value.
--
-- 'upperBoundary', 'forecastComputation_upperBoundary' - The upper boundary setup of a forecast computation.
--
-- 'value', 'forecastComputation_value' - The value field that is used in a computation.
--
-- 'computationId', 'forecastComputation_computationId' - The ID for a computation.
--
-- 'time', 'forecastComputation_time' - The time field that is used in a computation.
newForecastComputation ::
  -- | 'computationId'
  Prelude.Text ->
  -- | 'time'
  DimensionField ->
  ForecastComputation
newForecastComputation pComputationId_ pTime_ =
  ForecastComputation'
    { customSeasonalityValue =
        Prelude.Nothing,
      lowerBoundary = Prelude.Nothing,
      name = Prelude.Nothing,
      periodsBackward = Prelude.Nothing,
      periodsForward = Prelude.Nothing,
      predictionInterval = Prelude.Nothing,
      seasonality = Prelude.Nothing,
      upperBoundary = Prelude.Nothing,
      value = Prelude.Nothing,
      computationId = pComputationId_,
      time = pTime_
    }

-- | The custom seasonality value setup of a forecast computation.
forecastComputation_customSeasonalityValue :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Natural)
forecastComputation_customSeasonalityValue = Lens.lens (\ForecastComputation' {customSeasonalityValue} -> customSeasonalityValue) (\s@ForecastComputation' {} a -> s {customSeasonalityValue = a} :: ForecastComputation)

-- | The lower boundary setup of a forecast computation.
forecastComputation_lowerBoundary :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Double)
forecastComputation_lowerBoundary = Lens.lens (\ForecastComputation' {lowerBoundary} -> lowerBoundary) (\s@ForecastComputation' {} a -> s {lowerBoundary = a} :: ForecastComputation)

-- | The name of a computation.
forecastComputation_name :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Text)
forecastComputation_name = Lens.lens (\ForecastComputation' {name} -> name) (\s@ForecastComputation' {} a -> s {name = a} :: ForecastComputation)

-- | The periods backward setup of a forecast computation.
forecastComputation_periodsBackward :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Natural)
forecastComputation_periodsBackward = Lens.lens (\ForecastComputation' {periodsBackward} -> periodsBackward) (\s@ForecastComputation' {} a -> s {periodsBackward = a} :: ForecastComputation)

-- | The periods forward setup of a forecast computation.
forecastComputation_periodsForward :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Natural)
forecastComputation_periodsForward = Lens.lens (\ForecastComputation' {periodsForward} -> periodsForward) (\s@ForecastComputation' {} a -> s {periodsForward = a} :: ForecastComputation)

-- | The prediction interval setup of a forecast computation.
forecastComputation_predictionInterval :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Natural)
forecastComputation_predictionInterval = Lens.lens (\ForecastComputation' {predictionInterval} -> predictionInterval) (\s@ForecastComputation' {} a -> s {predictionInterval = a} :: ForecastComputation)

-- | The seasonality setup of a forecast computation. Choose one of the
-- following options:
--
-- -   @AUTOMATIC@
--
-- -   @CUSTOM@: Checks the custom seasonality value.
forecastComputation_seasonality :: Lens.Lens' ForecastComputation (Prelude.Maybe ForecastComputationSeasonality)
forecastComputation_seasonality = Lens.lens (\ForecastComputation' {seasonality} -> seasonality) (\s@ForecastComputation' {} a -> s {seasonality = a} :: ForecastComputation)

-- | The upper boundary setup of a forecast computation.
forecastComputation_upperBoundary :: Lens.Lens' ForecastComputation (Prelude.Maybe Prelude.Double)
forecastComputation_upperBoundary = Lens.lens (\ForecastComputation' {upperBoundary} -> upperBoundary) (\s@ForecastComputation' {} a -> s {upperBoundary = a} :: ForecastComputation)

-- | The value field that is used in a computation.
forecastComputation_value :: Lens.Lens' ForecastComputation (Prelude.Maybe MeasureField)
forecastComputation_value = Lens.lens (\ForecastComputation' {value} -> value) (\s@ForecastComputation' {} a -> s {value = a} :: ForecastComputation)

-- | The ID for a computation.
forecastComputation_computationId :: Lens.Lens' ForecastComputation Prelude.Text
forecastComputation_computationId = Lens.lens (\ForecastComputation' {computationId} -> computationId) (\s@ForecastComputation' {} a -> s {computationId = a} :: ForecastComputation)

-- | The time field that is used in a computation.
forecastComputation_time :: Lens.Lens' ForecastComputation DimensionField
forecastComputation_time = Lens.lens (\ForecastComputation' {time} -> time) (\s@ForecastComputation' {} a -> s {time = a} :: ForecastComputation)

instance Data.FromJSON ForecastComputation where
  parseJSON =
    Data.withObject
      "ForecastComputation"
      ( \x ->
          ForecastComputation'
            Prelude.<$> (x Data..:? "CustomSeasonalityValue")
            Prelude.<*> (x Data..:? "LowerBoundary")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "PeriodsBackward")
            Prelude.<*> (x Data..:? "PeriodsForward")
            Prelude.<*> (x Data..:? "PredictionInterval")
            Prelude.<*> (x Data..:? "Seasonality")
            Prelude.<*> (x Data..:? "UpperBoundary")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..: "ComputationId")
            Prelude.<*> (x Data..: "Time")
      )

instance Prelude.Hashable ForecastComputation where
  hashWithSalt _salt ForecastComputation' {..} =
    _salt
      `Prelude.hashWithSalt` customSeasonalityValue
      `Prelude.hashWithSalt` lowerBoundary
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` periodsBackward
      `Prelude.hashWithSalt` periodsForward
      `Prelude.hashWithSalt` predictionInterval
      `Prelude.hashWithSalt` seasonality
      `Prelude.hashWithSalt` upperBoundary
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` computationId
      `Prelude.hashWithSalt` time

instance Prelude.NFData ForecastComputation where
  rnf ForecastComputation' {..} =
    Prelude.rnf customSeasonalityValue
      `Prelude.seq` Prelude.rnf lowerBoundary
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf periodsBackward
      `Prelude.seq` Prelude.rnf periodsForward
      `Prelude.seq` Prelude.rnf predictionInterval
      `Prelude.seq` Prelude.rnf seasonality
      `Prelude.seq` Prelude.rnf upperBoundary
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf computationId
      `Prelude.seq` Prelude.rnf time

instance Data.ToJSON ForecastComputation where
  toJSON ForecastComputation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CustomSeasonalityValue" Data..=)
              Prelude.<$> customSeasonalityValue,
            ("LowerBoundary" Data..=) Prelude.<$> lowerBoundary,
            ("Name" Data..=) Prelude.<$> name,
            ("PeriodsBackward" Data..=)
              Prelude.<$> periodsBackward,
            ("PeriodsForward" Data..=)
              Prelude.<$> periodsForward,
            ("PredictionInterval" Data..=)
              Prelude.<$> predictionInterval,
            ("Seasonality" Data..=) Prelude.<$> seasonality,
            ("UpperBoundary" Data..=) Prelude.<$> upperBoundary,
            ("Value" Data..=) Prelude.<$> value,
            Prelude.Just ("ComputationId" Data..= computationId),
            Prelude.Just ("Time" Data..= time)
          ]
      )
