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
-- Module      : Amazonka.CleanRooms.Types.AggregationConstraint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AggregationConstraint where

import Amazonka.CleanRooms.Types.AggregationType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Constraint on query output removing output rows that do not meet a
-- minimum number of distinct values of a specified column.
--
-- /See:/ 'newAggregationConstraint' smart constructor.
data AggregationConstraint = AggregationConstraint'
  { -- | Column in aggregation constraint for which there must be a minimum
    -- number of distinct values in an output row for it to be in the query
    -- output.
    columnName :: Prelude.Text,
    -- | The minimum number of distinct values that an output row must be an
    -- aggregation of. Minimum threshold of distinct values for a specified
    -- column that must exist in an output row for it to be in the query
    -- output.
    minimum :: Prelude.Natural,
    -- | The type of aggregation the constraint allows. The only valid value is
    -- currently \`COUNT_DISTINCT\`.
    type' :: AggregationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregationConstraint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'aggregationConstraint_columnName' - Column in aggregation constraint for which there must be a minimum
-- number of distinct values in an output row for it to be in the query
-- output.
--
-- 'minimum', 'aggregationConstraint_minimum' - The minimum number of distinct values that an output row must be an
-- aggregation of. Minimum threshold of distinct values for a specified
-- column that must exist in an output row for it to be in the query
-- output.
--
-- 'type'', 'aggregationConstraint_type' - The type of aggregation the constraint allows. The only valid value is
-- currently \`COUNT_DISTINCT\`.
newAggregationConstraint ::
  -- | 'columnName'
  Prelude.Text ->
  -- | 'minimum'
  Prelude.Natural ->
  -- | 'type''
  AggregationType ->
  AggregationConstraint
newAggregationConstraint
  pColumnName_
  pMinimum_
  pType_ =
    AggregationConstraint'
      { columnName = pColumnName_,
        minimum = pMinimum_,
        type' = pType_
      }

-- | Column in aggregation constraint for which there must be a minimum
-- number of distinct values in an output row for it to be in the query
-- output.
aggregationConstraint_columnName :: Lens.Lens' AggregationConstraint Prelude.Text
aggregationConstraint_columnName = Lens.lens (\AggregationConstraint' {columnName} -> columnName) (\s@AggregationConstraint' {} a -> s {columnName = a} :: AggregationConstraint)

-- | The minimum number of distinct values that an output row must be an
-- aggregation of. Minimum threshold of distinct values for a specified
-- column that must exist in an output row for it to be in the query
-- output.
aggregationConstraint_minimum :: Lens.Lens' AggregationConstraint Prelude.Natural
aggregationConstraint_minimum = Lens.lens (\AggregationConstraint' {minimum} -> minimum) (\s@AggregationConstraint' {} a -> s {minimum = a} :: AggregationConstraint)

-- | The type of aggregation the constraint allows. The only valid value is
-- currently \`COUNT_DISTINCT\`.
aggregationConstraint_type :: Lens.Lens' AggregationConstraint AggregationType
aggregationConstraint_type = Lens.lens (\AggregationConstraint' {type'} -> type') (\s@AggregationConstraint' {} a -> s {type' = a} :: AggregationConstraint)

instance Data.FromJSON AggregationConstraint where
  parseJSON =
    Data.withObject
      "AggregationConstraint"
      ( \x ->
          AggregationConstraint'
            Prelude.<$> (x Data..: "columnName")
            Prelude.<*> (x Data..: "minimum")
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable AggregationConstraint where
  hashWithSalt _salt AggregationConstraint' {..} =
    _salt
      `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` type'

instance Prelude.NFData AggregationConstraint where
  rnf AggregationConstraint' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON AggregationConstraint where
  toJSON AggregationConstraint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("columnName" Data..= columnName),
            Prelude.Just ("minimum" Data..= minimum),
            Prelude.Just ("type" Data..= type')
          ]
      )
