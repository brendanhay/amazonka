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
-- Module      : Amazonka.QuickSight.Types.NumericalAggregationFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.NumericalAggregationFunction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.PercentileAggregation
import Amazonka.QuickSight.Types.SimpleNumericalAggregationFunction

-- | Aggregation for numerical values.
--
-- /See:/ 'newNumericalAggregationFunction' smart constructor.
data NumericalAggregationFunction = NumericalAggregationFunction'
  { -- | An aggregation based on the percentile of values in a dimension or
    -- measure.
    percentileAggregation :: Prelude.Maybe PercentileAggregation,
    -- | Built-in aggregation functions for numerical values.
    --
    -- -   @SUM@: The sum of a dimension or measure.
    --
    -- -   @AVERAGE@: The average of a dimension or measure.
    --
    -- -   @MIN@: The minimum value of a dimension or measure.
    --
    -- -   @MAX@: The maximum value of a dimension or measure.
    --
    -- -   @COUNT@: The count of a dimension or measure.
    --
    -- -   @DISTINCT_COUNT@: The count of distinct values in a dimension or
    --     measure.
    --
    -- -   @VAR@: The variance of a dimension or measure.
    --
    -- -   @VARP@: The partitioned variance of a dimension or measure.
    --
    -- -   @STDEV@: The standard deviation of a dimension or measure.
    --
    -- -   @STDEVP@: The partitioned standard deviation of a dimension or
    --     measure.
    --
    -- -   @MEDIAN@: The median value of a dimension or measure.
    simpleNumericalAggregation :: Prelude.Maybe SimpleNumericalAggregationFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NumericalAggregationFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'percentileAggregation', 'numericalAggregationFunction_percentileAggregation' - An aggregation based on the percentile of values in a dimension or
-- measure.
--
-- 'simpleNumericalAggregation', 'numericalAggregationFunction_simpleNumericalAggregation' - Built-in aggregation functions for numerical values.
--
-- -   @SUM@: The sum of a dimension or measure.
--
-- -   @AVERAGE@: The average of a dimension or measure.
--
-- -   @MIN@: The minimum value of a dimension or measure.
--
-- -   @MAX@: The maximum value of a dimension or measure.
--
-- -   @COUNT@: The count of a dimension or measure.
--
-- -   @DISTINCT_COUNT@: The count of distinct values in a dimension or
--     measure.
--
-- -   @VAR@: The variance of a dimension or measure.
--
-- -   @VARP@: The partitioned variance of a dimension or measure.
--
-- -   @STDEV@: The standard deviation of a dimension or measure.
--
-- -   @STDEVP@: The partitioned standard deviation of a dimension or
--     measure.
--
-- -   @MEDIAN@: The median value of a dimension or measure.
newNumericalAggregationFunction ::
  NumericalAggregationFunction
newNumericalAggregationFunction =
  NumericalAggregationFunction'
    { percentileAggregation =
        Prelude.Nothing,
      simpleNumericalAggregation = Prelude.Nothing
    }

-- | An aggregation based on the percentile of values in a dimension or
-- measure.
numericalAggregationFunction_percentileAggregation :: Lens.Lens' NumericalAggregationFunction (Prelude.Maybe PercentileAggregation)
numericalAggregationFunction_percentileAggregation = Lens.lens (\NumericalAggregationFunction' {percentileAggregation} -> percentileAggregation) (\s@NumericalAggregationFunction' {} a -> s {percentileAggregation = a} :: NumericalAggregationFunction)

-- | Built-in aggregation functions for numerical values.
--
-- -   @SUM@: The sum of a dimension or measure.
--
-- -   @AVERAGE@: The average of a dimension or measure.
--
-- -   @MIN@: The minimum value of a dimension or measure.
--
-- -   @MAX@: The maximum value of a dimension or measure.
--
-- -   @COUNT@: The count of a dimension or measure.
--
-- -   @DISTINCT_COUNT@: The count of distinct values in a dimension or
--     measure.
--
-- -   @VAR@: The variance of a dimension or measure.
--
-- -   @VARP@: The partitioned variance of a dimension or measure.
--
-- -   @STDEV@: The standard deviation of a dimension or measure.
--
-- -   @STDEVP@: The partitioned standard deviation of a dimension or
--     measure.
--
-- -   @MEDIAN@: The median value of a dimension or measure.
numericalAggregationFunction_simpleNumericalAggregation :: Lens.Lens' NumericalAggregationFunction (Prelude.Maybe SimpleNumericalAggregationFunction)
numericalAggregationFunction_simpleNumericalAggregation = Lens.lens (\NumericalAggregationFunction' {simpleNumericalAggregation} -> simpleNumericalAggregation) (\s@NumericalAggregationFunction' {} a -> s {simpleNumericalAggregation = a} :: NumericalAggregationFunction)

instance Data.FromJSON NumericalAggregationFunction where
  parseJSON =
    Data.withObject
      "NumericalAggregationFunction"
      ( \x ->
          NumericalAggregationFunction'
            Prelude.<$> (x Data..:? "PercentileAggregation")
            Prelude.<*> (x Data..:? "SimpleNumericalAggregation")
      )

instance
  Prelude.Hashable
    NumericalAggregationFunction
  where
  hashWithSalt _salt NumericalAggregationFunction' {..} =
    _salt `Prelude.hashWithSalt` percentileAggregation
      `Prelude.hashWithSalt` simpleNumericalAggregation

instance Prelude.NFData NumericalAggregationFunction where
  rnf NumericalAggregationFunction' {..} =
    Prelude.rnf percentileAggregation
      `Prelude.seq` Prelude.rnf simpleNumericalAggregation

instance Data.ToJSON NumericalAggregationFunction where
  toJSON NumericalAggregationFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("PercentileAggregation" Data..=)
              Prelude.<$> percentileAggregation,
            ("SimpleNumericalAggregation" Data..=)
              Prelude.<$> simpleNumericalAggregation
          ]
      )
