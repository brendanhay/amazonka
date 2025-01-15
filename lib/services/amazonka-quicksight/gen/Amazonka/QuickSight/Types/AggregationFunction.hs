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
-- Module      : Amazonka.QuickSight.Types.AggregationFunction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AggregationFunction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.CategoricalAggregationFunction
import Amazonka.QuickSight.Types.DateAggregationFunction
import Amazonka.QuickSight.Types.NumericalAggregationFunction

-- | An aggregation function aggregates values from a dimension or measure.
--
-- This is a union type structure. For this structure to be valid, only one
-- of the attributes can be defined.
--
-- /See:/ 'newAggregationFunction' smart constructor.
data AggregationFunction = AggregationFunction'
  { -- | Aggregation for categorical values.
    --
    -- -   @COUNT@: Aggregate by the total number of values, including
    --     duplicates.
    --
    -- -   @DISTINCT_COUNT@: Aggregate by the total number of distinct values.
    categoricalAggregationFunction :: Prelude.Maybe CategoricalAggregationFunction,
    -- | Aggregation for date values.
    --
    -- -   @COUNT@: Aggregate by the total number of values, including
    --     duplicates.
    --
    -- -   @DISTINCT_COUNT@: Aggregate by the total number of distinct values.
    --
    -- -   @MIN@: Select the smallest date value.
    --
    -- -   @MAX@: Select the largest date value.
    dateAggregationFunction :: Prelude.Maybe DateAggregationFunction,
    -- | Aggregation for numerical values.
    numericalAggregationFunction :: Prelude.Maybe NumericalAggregationFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregationFunction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalAggregationFunction', 'aggregationFunction_categoricalAggregationFunction' - Aggregation for categorical values.
--
-- -   @COUNT@: Aggregate by the total number of values, including
--     duplicates.
--
-- -   @DISTINCT_COUNT@: Aggregate by the total number of distinct values.
--
-- 'dateAggregationFunction', 'aggregationFunction_dateAggregationFunction' - Aggregation for date values.
--
-- -   @COUNT@: Aggregate by the total number of values, including
--     duplicates.
--
-- -   @DISTINCT_COUNT@: Aggregate by the total number of distinct values.
--
-- -   @MIN@: Select the smallest date value.
--
-- -   @MAX@: Select the largest date value.
--
-- 'numericalAggregationFunction', 'aggregationFunction_numericalAggregationFunction' - Aggregation for numerical values.
newAggregationFunction ::
  AggregationFunction
newAggregationFunction =
  AggregationFunction'
    { categoricalAggregationFunction =
        Prelude.Nothing,
      dateAggregationFunction = Prelude.Nothing,
      numericalAggregationFunction = Prelude.Nothing
    }

-- | Aggregation for categorical values.
--
-- -   @COUNT@: Aggregate by the total number of values, including
--     duplicates.
--
-- -   @DISTINCT_COUNT@: Aggregate by the total number of distinct values.
aggregationFunction_categoricalAggregationFunction :: Lens.Lens' AggregationFunction (Prelude.Maybe CategoricalAggregationFunction)
aggregationFunction_categoricalAggregationFunction = Lens.lens (\AggregationFunction' {categoricalAggregationFunction} -> categoricalAggregationFunction) (\s@AggregationFunction' {} a -> s {categoricalAggregationFunction = a} :: AggregationFunction)

-- | Aggregation for date values.
--
-- -   @COUNT@: Aggregate by the total number of values, including
--     duplicates.
--
-- -   @DISTINCT_COUNT@: Aggregate by the total number of distinct values.
--
-- -   @MIN@: Select the smallest date value.
--
-- -   @MAX@: Select the largest date value.
aggregationFunction_dateAggregationFunction :: Lens.Lens' AggregationFunction (Prelude.Maybe DateAggregationFunction)
aggregationFunction_dateAggregationFunction = Lens.lens (\AggregationFunction' {dateAggregationFunction} -> dateAggregationFunction) (\s@AggregationFunction' {} a -> s {dateAggregationFunction = a} :: AggregationFunction)

-- | Aggregation for numerical values.
aggregationFunction_numericalAggregationFunction :: Lens.Lens' AggregationFunction (Prelude.Maybe NumericalAggregationFunction)
aggregationFunction_numericalAggregationFunction = Lens.lens (\AggregationFunction' {numericalAggregationFunction} -> numericalAggregationFunction) (\s@AggregationFunction' {} a -> s {numericalAggregationFunction = a} :: AggregationFunction)

instance Data.FromJSON AggregationFunction where
  parseJSON =
    Data.withObject
      "AggregationFunction"
      ( \x ->
          AggregationFunction'
            Prelude.<$> (x Data..:? "CategoricalAggregationFunction")
            Prelude.<*> (x Data..:? "DateAggregationFunction")
            Prelude.<*> (x Data..:? "NumericalAggregationFunction")
      )

instance Prelude.Hashable AggregationFunction where
  hashWithSalt _salt AggregationFunction' {..} =
    _salt
      `Prelude.hashWithSalt` categoricalAggregationFunction
      `Prelude.hashWithSalt` dateAggregationFunction
      `Prelude.hashWithSalt` numericalAggregationFunction

instance Prelude.NFData AggregationFunction where
  rnf AggregationFunction' {..} =
    Prelude.rnf categoricalAggregationFunction `Prelude.seq`
      Prelude.rnf dateAggregationFunction `Prelude.seq`
        Prelude.rnf numericalAggregationFunction

instance Data.ToJSON AggregationFunction where
  toJSON AggregationFunction' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CategoricalAggregationFunction" Data..=)
              Prelude.<$> categoricalAggregationFunction,
            ("DateAggregationFunction" Data..=)
              Prelude.<$> dateAggregationFunction,
            ("NumericalAggregationFunction" Data..=)
              Prelude.<$> numericalAggregationFunction
          ]
      )
