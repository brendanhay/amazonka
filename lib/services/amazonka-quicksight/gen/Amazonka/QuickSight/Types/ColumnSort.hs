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
-- Module      : Amazonka.QuickSight.Types.ColumnSort
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnSort where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AggregationFunction
import Amazonka.QuickSight.Types.ColumnIdentifier
import Amazonka.QuickSight.Types.SortDirection

-- | The sort configuration for a column that is not used in a field well.
--
-- /See:/ 'newColumnSort' smart constructor.
data ColumnSort = ColumnSort'
  { -- | The aggregation function that is defined in the column sort.
    aggregationFunction :: Prelude.Maybe AggregationFunction,
    sortBy :: ColumnIdentifier,
    -- | The sort direction.
    direction :: SortDirection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnSort' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationFunction', 'columnSort_aggregationFunction' - The aggregation function that is defined in the column sort.
--
-- 'sortBy', 'columnSort_sortBy' - Undocumented member.
--
-- 'direction', 'columnSort_direction' - The sort direction.
newColumnSort ::
  -- | 'sortBy'
  ColumnIdentifier ->
  -- | 'direction'
  SortDirection ->
  ColumnSort
newColumnSort pSortBy_ pDirection_ =
  ColumnSort'
    { aggregationFunction = Prelude.Nothing,
      sortBy = pSortBy_,
      direction = pDirection_
    }

-- | The aggregation function that is defined in the column sort.
columnSort_aggregationFunction :: Lens.Lens' ColumnSort (Prelude.Maybe AggregationFunction)
columnSort_aggregationFunction = Lens.lens (\ColumnSort' {aggregationFunction} -> aggregationFunction) (\s@ColumnSort' {} a -> s {aggregationFunction = a} :: ColumnSort)

-- | Undocumented member.
columnSort_sortBy :: Lens.Lens' ColumnSort ColumnIdentifier
columnSort_sortBy = Lens.lens (\ColumnSort' {sortBy} -> sortBy) (\s@ColumnSort' {} a -> s {sortBy = a} :: ColumnSort)

-- | The sort direction.
columnSort_direction :: Lens.Lens' ColumnSort SortDirection
columnSort_direction = Lens.lens (\ColumnSort' {direction} -> direction) (\s@ColumnSort' {} a -> s {direction = a} :: ColumnSort)

instance Data.FromJSON ColumnSort where
  parseJSON =
    Data.withObject
      "ColumnSort"
      ( \x ->
          ColumnSort'
            Prelude.<$> (x Data..:? "AggregationFunction")
            Prelude.<*> (x Data..: "SortBy")
            Prelude.<*> (x Data..: "Direction")
      )

instance Prelude.Hashable ColumnSort where
  hashWithSalt _salt ColumnSort' {..} =
    _salt
      `Prelude.hashWithSalt` aggregationFunction
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` direction

instance Prelude.NFData ColumnSort where
  rnf ColumnSort' {..} =
    Prelude.rnf aggregationFunction `Prelude.seq`
      Prelude.rnf sortBy `Prelude.seq`
        Prelude.rnf direction

instance Data.ToJSON ColumnSort where
  toJSON ColumnSort' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AggregationFunction" Data..=)
              Prelude.<$> aggregationFunction,
            Prelude.Just ("SortBy" Data..= sortBy),
            Prelude.Just ("Direction" Data..= direction)
          ]
      )
