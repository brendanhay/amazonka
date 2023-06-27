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
-- Module      : Amazonka.CleanRooms.Types.AnalysisRuleAggregation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AnalysisRuleAggregation where

import Amazonka.CleanRooms.Types.AggregateColumn
import Amazonka.CleanRooms.Types.AggregationConstraint
import Amazonka.CleanRooms.Types.JoinRequiredOption
import Amazonka.CleanRooms.Types.ScalarFunctions
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enables query structure and specified queries that product aggregate
-- statistics.
--
-- /See:/ 'newAnalysisRuleAggregation' smart constructor.
data AnalysisRuleAggregation = AnalysisRuleAggregation'
  { -- | Control that requires member who runs query to do a join with their
    -- configured table and\/or other configured table in query
    joinRequired :: Prelude.Maybe JoinRequiredOption,
    -- | The columns that query runners are allowed to use in aggregation
    -- queries.
    aggregateColumns :: Prelude.NonEmpty AggregateColumn,
    -- | Columns in configured table that can be used in join statements and\/or
    -- as aggregate columns. They can never be outputted directly.
    joinColumns :: [Prelude.Text],
    -- | The columns that query runners are allowed to select, group by, or
    -- filter by.
    dimensionColumns :: [Prelude.Text],
    -- | Set of scalar functions that are allowed to be used on dimension columns
    -- and the output of aggregation of metrics.
    scalarFunctions :: [ScalarFunctions],
    -- | Columns that must meet a specific threshold value (after an aggregation
    -- function is applied to it) for each output row to be returned.
    outputConstraints :: Prelude.NonEmpty AggregationConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisRuleAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'joinRequired', 'analysisRuleAggregation_joinRequired' - Control that requires member who runs query to do a join with their
-- configured table and\/or other configured table in query
--
-- 'aggregateColumns', 'analysisRuleAggregation_aggregateColumns' - The columns that query runners are allowed to use in aggregation
-- queries.
--
-- 'joinColumns', 'analysisRuleAggregation_joinColumns' - Columns in configured table that can be used in join statements and\/or
-- as aggregate columns. They can never be outputted directly.
--
-- 'dimensionColumns', 'analysisRuleAggregation_dimensionColumns' - The columns that query runners are allowed to select, group by, or
-- filter by.
--
-- 'scalarFunctions', 'analysisRuleAggregation_scalarFunctions' - Set of scalar functions that are allowed to be used on dimension columns
-- and the output of aggregation of metrics.
--
-- 'outputConstraints', 'analysisRuleAggregation_outputConstraints' - Columns that must meet a specific threshold value (after an aggregation
-- function is applied to it) for each output row to be returned.
newAnalysisRuleAggregation ::
  -- | 'aggregateColumns'
  Prelude.NonEmpty AggregateColumn ->
  -- | 'outputConstraints'
  Prelude.NonEmpty AggregationConstraint ->
  AnalysisRuleAggregation
newAnalysisRuleAggregation
  pAggregateColumns_
  pOutputConstraints_ =
    AnalysisRuleAggregation'
      { joinRequired =
          Prelude.Nothing,
        aggregateColumns =
          Lens.coerced Lens.# pAggregateColumns_,
        joinColumns = Prelude.mempty,
        dimensionColumns = Prelude.mempty,
        scalarFunctions = Prelude.mempty,
        outputConstraints =
          Lens.coerced Lens.# pOutputConstraints_
      }

-- | Control that requires member who runs query to do a join with their
-- configured table and\/or other configured table in query
analysisRuleAggregation_joinRequired :: Lens.Lens' AnalysisRuleAggregation (Prelude.Maybe JoinRequiredOption)
analysisRuleAggregation_joinRequired = Lens.lens (\AnalysisRuleAggregation' {joinRequired} -> joinRequired) (\s@AnalysisRuleAggregation' {} a -> s {joinRequired = a} :: AnalysisRuleAggregation)

-- | The columns that query runners are allowed to use in aggregation
-- queries.
analysisRuleAggregation_aggregateColumns :: Lens.Lens' AnalysisRuleAggregation (Prelude.NonEmpty AggregateColumn)
analysisRuleAggregation_aggregateColumns = Lens.lens (\AnalysisRuleAggregation' {aggregateColumns} -> aggregateColumns) (\s@AnalysisRuleAggregation' {} a -> s {aggregateColumns = a} :: AnalysisRuleAggregation) Prelude.. Lens.coerced

-- | Columns in configured table that can be used in join statements and\/or
-- as aggregate columns. They can never be outputted directly.
analysisRuleAggregation_joinColumns :: Lens.Lens' AnalysisRuleAggregation [Prelude.Text]
analysisRuleAggregation_joinColumns = Lens.lens (\AnalysisRuleAggregation' {joinColumns} -> joinColumns) (\s@AnalysisRuleAggregation' {} a -> s {joinColumns = a} :: AnalysisRuleAggregation) Prelude.. Lens.coerced

-- | The columns that query runners are allowed to select, group by, or
-- filter by.
analysisRuleAggregation_dimensionColumns :: Lens.Lens' AnalysisRuleAggregation [Prelude.Text]
analysisRuleAggregation_dimensionColumns = Lens.lens (\AnalysisRuleAggregation' {dimensionColumns} -> dimensionColumns) (\s@AnalysisRuleAggregation' {} a -> s {dimensionColumns = a} :: AnalysisRuleAggregation) Prelude.. Lens.coerced

-- | Set of scalar functions that are allowed to be used on dimension columns
-- and the output of aggregation of metrics.
analysisRuleAggregation_scalarFunctions :: Lens.Lens' AnalysisRuleAggregation [ScalarFunctions]
analysisRuleAggregation_scalarFunctions = Lens.lens (\AnalysisRuleAggregation' {scalarFunctions} -> scalarFunctions) (\s@AnalysisRuleAggregation' {} a -> s {scalarFunctions = a} :: AnalysisRuleAggregation) Prelude.. Lens.coerced

-- | Columns that must meet a specific threshold value (after an aggregation
-- function is applied to it) for each output row to be returned.
analysisRuleAggregation_outputConstraints :: Lens.Lens' AnalysisRuleAggregation (Prelude.NonEmpty AggregationConstraint)
analysisRuleAggregation_outputConstraints = Lens.lens (\AnalysisRuleAggregation' {outputConstraints} -> outputConstraints) (\s@AnalysisRuleAggregation' {} a -> s {outputConstraints = a} :: AnalysisRuleAggregation) Prelude.. Lens.coerced

instance Data.FromJSON AnalysisRuleAggregation where
  parseJSON =
    Data.withObject
      "AnalysisRuleAggregation"
      ( \x ->
          AnalysisRuleAggregation'
            Prelude.<$> (x Data..:? "joinRequired")
            Prelude.<*> (x Data..: "aggregateColumns")
            Prelude.<*> (x Data..:? "joinColumns" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "dimensionColumns"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "scalarFunctions"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "outputConstraints")
      )

instance Prelude.Hashable AnalysisRuleAggregation where
  hashWithSalt _salt AnalysisRuleAggregation' {..} =
    _salt
      `Prelude.hashWithSalt` joinRequired
      `Prelude.hashWithSalt` aggregateColumns
      `Prelude.hashWithSalt` joinColumns
      `Prelude.hashWithSalt` dimensionColumns
      `Prelude.hashWithSalt` scalarFunctions
      `Prelude.hashWithSalt` outputConstraints

instance Prelude.NFData AnalysisRuleAggregation where
  rnf AnalysisRuleAggregation' {..} =
    Prelude.rnf joinRequired
      `Prelude.seq` Prelude.rnf aggregateColumns
      `Prelude.seq` Prelude.rnf joinColumns
      `Prelude.seq` Prelude.rnf dimensionColumns
      `Prelude.seq` Prelude.rnf scalarFunctions
      `Prelude.seq` Prelude.rnf outputConstraints

instance Data.ToJSON AnalysisRuleAggregation where
  toJSON AnalysisRuleAggregation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("joinRequired" Data..=) Prelude.<$> joinRequired,
            Prelude.Just
              ("aggregateColumns" Data..= aggregateColumns),
            Prelude.Just ("joinColumns" Data..= joinColumns),
            Prelude.Just
              ("dimensionColumns" Data..= dimensionColumns),
            Prelude.Just
              ("scalarFunctions" Data..= scalarFunctions),
            Prelude.Just
              ("outputConstraints" Data..= outputConstraints)
          ]
      )
