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
-- Module      : Amazonka.QuickSight.Types.AggregationSortConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AggregationSortConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.SortDirection

-- | The configuration options to sort aggregated values.
--
-- /See:/ 'newAggregationSortConfiguration' smart constructor.
data AggregationSortConfiguration = AggregationSortConfiguration'
  { -- | The column that determines the sort order of aggregated values.
    column :: ColumnIdentifier,
    -- | The sort direction of values.
    --
    -- -   @ASC@: Sort in ascending order.
    --
    -- -   @DESC@: Sort in descending order.
    sortDirection :: SortDirection,
    -- | The function that aggregates the values in @Column@.
    aggregationFunction :: AggregationFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregationSortConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'aggregationSortConfiguration_column' - The column that determines the sort order of aggregated values.
--
-- 'sortDirection', 'aggregationSortConfiguration_sortDirection' - The sort direction of values.
--
-- -   @ASC@: Sort in ascending order.
--
-- -   @DESC@: Sort in descending order.
--
-- 'aggregationFunction', 'aggregationSortConfiguration_aggregationFunction' - The function that aggregates the values in @Column@.
newAggregationSortConfiguration ::
  -- | 'column'
  ColumnIdentifier ->
  -- | 'sortDirection'
  SortDirection ->
  -- | 'aggregationFunction'
  AggregationFunction ->
  AggregationSortConfiguration
newAggregationSortConfiguration
  pColumn_
  pSortDirection_
  pAggregationFunction_ =
    AggregationSortConfiguration'
      { column = pColumn_,
        sortDirection = pSortDirection_,
        aggregationFunction = pAggregationFunction_
      }

-- | The column that determines the sort order of aggregated values.
aggregationSortConfiguration_column :: Lens.Lens' AggregationSortConfiguration ColumnIdentifier
aggregationSortConfiguration_column = Lens.lens (\AggregationSortConfiguration' {column} -> column) (\s@AggregationSortConfiguration' {} a -> s {column = a} :: AggregationSortConfiguration)

-- | The sort direction of values.
--
-- -   @ASC@: Sort in ascending order.
--
-- -   @DESC@: Sort in descending order.
aggregationSortConfiguration_sortDirection :: Lens.Lens' AggregationSortConfiguration SortDirection
aggregationSortConfiguration_sortDirection = Lens.lens (\AggregationSortConfiguration' {sortDirection} -> sortDirection) (\s@AggregationSortConfiguration' {} a -> s {sortDirection = a} :: AggregationSortConfiguration)

-- | The function that aggregates the values in @Column@.
aggregationSortConfiguration_aggregationFunction :: Lens.Lens' AggregationSortConfiguration AggregationFunction
aggregationSortConfiguration_aggregationFunction = Lens.lens (\AggregationSortConfiguration' {aggregationFunction} -> aggregationFunction) (\s@AggregationSortConfiguration' {} a -> s {aggregationFunction = a} :: AggregationSortConfiguration)

instance Data.FromJSON AggregationSortConfiguration where
  parseJSON =
    Data.withObject
      "AggregationSortConfiguration"
      ( \x ->
          AggregationSortConfiguration'
            Prelude.<$> (x Data..: "Column")
            Prelude.<*> (x Data..: "SortDirection")
            Prelude.<*> (x Data..: "AggregationFunction")
      )

instance
  Prelude.Hashable
    AggregationSortConfiguration
  where
  hashWithSalt _salt AggregationSortConfiguration' {..} =
    _salt `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` sortDirection
      `Prelude.hashWithSalt` aggregationFunction

instance Prelude.NFData AggregationSortConfiguration where
  rnf AggregationSortConfiguration' {..} =
    Prelude.rnf column
      `Prelude.seq` Prelude.rnf sortDirection
      `Prelude.seq` Prelude.rnf aggregationFunction

instance Data.ToJSON AggregationSortConfiguration where
  toJSON AggregationSortConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just ("SortDirection" Data..= sortDirection),
            Prelude.Just
              ("AggregationFunction" Data..= aggregationFunction)
          ]
      )
