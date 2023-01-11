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
-- Module      : Amazonka.QuickSight.Types.Computation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Computation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ForecastComputation
import Amazonka.QuickSight.Types.GrowthRateComputation
import Amazonka.QuickSight.Types.MaximumMinimumComputation
import Amazonka.QuickSight.Types.MetricComparisonComputation
import Amazonka.QuickSight.Types.PeriodOverPeriodComputation
import Amazonka.QuickSight.Types.PeriodToDateComputation
import Amazonka.QuickSight.Types.TopBottomMoversComputation
import Amazonka.QuickSight.Types.TopBottomRankedComputation
import Amazonka.QuickSight.Types.TotalAggregationComputation
import Amazonka.QuickSight.Types.UniqueValuesComputation

-- | The computation union that is used in an insight visual.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newComputation' smart constructor.
data Computation = Computation'
  { -- | The forecast computation configuration.
    forecast :: Prelude.Maybe ForecastComputation,
    -- | The growth rate computation configuration.
    growthRate :: Prelude.Maybe GrowthRateComputation,
    -- | The maximum and minimum computation configuration.
    maximumMinimum :: Prelude.Maybe MaximumMinimumComputation,
    -- | The metric comparison computation configuration.
    metricComparison :: Prelude.Maybe MetricComparisonComputation,
    -- | The period over period computation configuration.
    periodOverPeriod :: Prelude.Maybe PeriodOverPeriodComputation,
    -- | The period to @DataSetIdentifier@ computation configuration.
    periodToDate :: Prelude.Maybe PeriodToDateComputation,
    -- | The top movers and bottom movers computation configuration.
    topBottomMovers :: Prelude.Maybe TopBottomMoversComputation,
    -- | The top ranked and bottom ranked computation configuration.
    topBottomRanked :: Prelude.Maybe TopBottomRankedComputation,
    -- | The total aggregation computation configuration.
    totalAggregation :: Prelude.Maybe TotalAggregationComputation,
    -- | The unique values computation configuration.
    uniqueValues :: Prelude.Maybe UniqueValuesComputation
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Computation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forecast', 'computation_forecast' - The forecast computation configuration.
--
-- 'growthRate', 'computation_growthRate' - The growth rate computation configuration.
--
-- 'maximumMinimum', 'computation_maximumMinimum' - The maximum and minimum computation configuration.
--
-- 'metricComparison', 'computation_metricComparison' - The metric comparison computation configuration.
--
-- 'periodOverPeriod', 'computation_periodOverPeriod' - The period over period computation configuration.
--
-- 'periodToDate', 'computation_periodToDate' - The period to @DataSetIdentifier@ computation configuration.
--
-- 'topBottomMovers', 'computation_topBottomMovers' - The top movers and bottom movers computation configuration.
--
-- 'topBottomRanked', 'computation_topBottomRanked' - The top ranked and bottom ranked computation configuration.
--
-- 'totalAggregation', 'computation_totalAggregation' - The total aggregation computation configuration.
--
-- 'uniqueValues', 'computation_uniqueValues' - The unique values computation configuration.
newComputation ::
  Computation
newComputation =
  Computation'
    { forecast = Prelude.Nothing,
      growthRate = Prelude.Nothing,
      maximumMinimum = Prelude.Nothing,
      metricComparison = Prelude.Nothing,
      periodOverPeriod = Prelude.Nothing,
      periodToDate = Prelude.Nothing,
      topBottomMovers = Prelude.Nothing,
      topBottomRanked = Prelude.Nothing,
      totalAggregation = Prelude.Nothing,
      uniqueValues = Prelude.Nothing
    }

-- | The forecast computation configuration.
computation_forecast :: Lens.Lens' Computation (Prelude.Maybe ForecastComputation)
computation_forecast = Lens.lens (\Computation' {forecast} -> forecast) (\s@Computation' {} a -> s {forecast = a} :: Computation)

-- | The growth rate computation configuration.
computation_growthRate :: Lens.Lens' Computation (Prelude.Maybe GrowthRateComputation)
computation_growthRate = Lens.lens (\Computation' {growthRate} -> growthRate) (\s@Computation' {} a -> s {growthRate = a} :: Computation)

-- | The maximum and minimum computation configuration.
computation_maximumMinimum :: Lens.Lens' Computation (Prelude.Maybe MaximumMinimumComputation)
computation_maximumMinimum = Lens.lens (\Computation' {maximumMinimum} -> maximumMinimum) (\s@Computation' {} a -> s {maximumMinimum = a} :: Computation)

-- | The metric comparison computation configuration.
computation_metricComparison :: Lens.Lens' Computation (Prelude.Maybe MetricComparisonComputation)
computation_metricComparison = Lens.lens (\Computation' {metricComparison} -> metricComparison) (\s@Computation' {} a -> s {metricComparison = a} :: Computation)

-- | The period over period computation configuration.
computation_periodOverPeriod :: Lens.Lens' Computation (Prelude.Maybe PeriodOverPeriodComputation)
computation_periodOverPeriod = Lens.lens (\Computation' {periodOverPeriod} -> periodOverPeriod) (\s@Computation' {} a -> s {periodOverPeriod = a} :: Computation)

-- | The period to @DataSetIdentifier@ computation configuration.
computation_periodToDate :: Lens.Lens' Computation (Prelude.Maybe PeriodToDateComputation)
computation_periodToDate = Lens.lens (\Computation' {periodToDate} -> periodToDate) (\s@Computation' {} a -> s {periodToDate = a} :: Computation)

-- | The top movers and bottom movers computation configuration.
computation_topBottomMovers :: Lens.Lens' Computation (Prelude.Maybe TopBottomMoversComputation)
computation_topBottomMovers = Lens.lens (\Computation' {topBottomMovers} -> topBottomMovers) (\s@Computation' {} a -> s {topBottomMovers = a} :: Computation)

-- | The top ranked and bottom ranked computation configuration.
computation_topBottomRanked :: Lens.Lens' Computation (Prelude.Maybe TopBottomRankedComputation)
computation_topBottomRanked = Lens.lens (\Computation' {topBottomRanked} -> topBottomRanked) (\s@Computation' {} a -> s {topBottomRanked = a} :: Computation)

-- | The total aggregation computation configuration.
computation_totalAggregation :: Lens.Lens' Computation (Prelude.Maybe TotalAggregationComputation)
computation_totalAggregation = Lens.lens (\Computation' {totalAggregation} -> totalAggregation) (\s@Computation' {} a -> s {totalAggregation = a} :: Computation)

-- | The unique values computation configuration.
computation_uniqueValues :: Lens.Lens' Computation (Prelude.Maybe UniqueValuesComputation)
computation_uniqueValues = Lens.lens (\Computation' {uniqueValues} -> uniqueValues) (\s@Computation' {} a -> s {uniqueValues = a} :: Computation)

instance Data.FromJSON Computation where
  parseJSON =
    Data.withObject
      "Computation"
      ( \x ->
          Computation'
            Prelude.<$> (x Data..:? "Forecast")
            Prelude.<*> (x Data..:? "GrowthRate")
            Prelude.<*> (x Data..:? "MaximumMinimum")
            Prelude.<*> (x Data..:? "MetricComparison")
            Prelude.<*> (x Data..:? "PeriodOverPeriod")
            Prelude.<*> (x Data..:? "PeriodToDate")
            Prelude.<*> (x Data..:? "TopBottomMovers")
            Prelude.<*> (x Data..:? "TopBottomRanked")
            Prelude.<*> (x Data..:? "TotalAggregation")
            Prelude.<*> (x Data..:? "UniqueValues")
      )

instance Prelude.Hashable Computation where
  hashWithSalt _salt Computation' {..} =
    _salt `Prelude.hashWithSalt` forecast
      `Prelude.hashWithSalt` growthRate
      `Prelude.hashWithSalt` maximumMinimum
      `Prelude.hashWithSalt` metricComparison
      `Prelude.hashWithSalt` periodOverPeriod
      `Prelude.hashWithSalt` periodToDate
      `Prelude.hashWithSalt` topBottomMovers
      `Prelude.hashWithSalt` topBottomRanked
      `Prelude.hashWithSalt` totalAggregation
      `Prelude.hashWithSalt` uniqueValues

instance Prelude.NFData Computation where
  rnf Computation' {..} =
    Prelude.rnf forecast
      `Prelude.seq` Prelude.rnf growthRate
      `Prelude.seq` Prelude.rnf maximumMinimum
      `Prelude.seq` Prelude.rnf metricComparison
      `Prelude.seq` Prelude.rnf periodOverPeriod
      `Prelude.seq` Prelude.rnf periodToDate
      `Prelude.seq` Prelude.rnf topBottomMovers
      `Prelude.seq` Prelude.rnf topBottomRanked
      `Prelude.seq` Prelude.rnf totalAggregation
      `Prelude.seq` Prelude.rnf uniqueValues

instance Data.ToJSON Computation where
  toJSON Computation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Forecast" Data..=) Prelude.<$> forecast,
            ("GrowthRate" Data..=) Prelude.<$> growthRate,
            ("MaximumMinimum" Data..=)
              Prelude.<$> maximumMinimum,
            ("MetricComparison" Data..=)
              Prelude.<$> metricComparison,
            ("PeriodOverPeriod" Data..=)
              Prelude.<$> periodOverPeriod,
            ("PeriodToDate" Data..=) Prelude.<$> periodToDate,
            ("TopBottomMovers" Data..=)
              Prelude.<$> topBottomMovers,
            ("TopBottomRanked" Data..=)
              Prelude.<$> topBottomRanked,
            ("TotalAggregation" Data..=)
              Prelude.<$> totalAggregation,
            ("UniqueValues" Data..=) Prelude.<$> uniqueValues
          ]
      )
