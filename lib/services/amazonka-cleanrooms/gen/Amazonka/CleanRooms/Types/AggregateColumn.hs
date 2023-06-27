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
-- Module      : Amazonka.CleanRooms.Types.AggregateColumn
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CleanRooms.Types.AggregateColumn where

import Amazonka.CleanRooms.Types.AggregateFunctionName
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Column in configured table that can be used in aggregate function in
-- query.
--
-- /See:/ 'newAggregateColumn' smart constructor.
data AggregateColumn = AggregateColumn'
  { -- | Column names in configured table of aggregate columns.
    columnNames :: Prelude.NonEmpty Prelude.Text,
    -- | Aggregation function that can be applied to aggregate column in query.
    function :: AggregateFunctionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateColumn' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnNames', 'aggregateColumn_columnNames' - Column names in configured table of aggregate columns.
--
-- 'function', 'aggregateColumn_function' - Aggregation function that can be applied to aggregate column in query.
newAggregateColumn ::
  -- | 'columnNames'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'function'
  AggregateFunctionName ->
  AggregateColumn
newAggregateColumn pColumnNames_ pFunction_ =
  AggregateColumn'
    { columnNames =
        Lens.coerced Lens.# pColumnNames_,
      function = pFunction_
    }

-- | Column names in configured table of aggregate columns.
aggregateColumn_columnNames :: Lens.Lens' AggregateColumn (Prelude.NonEmpty Prelude.Text)
aggregateColumn_columnNames = Lens.lens (\AggregateColumn' {columnNames} -> columnNames) (\s@AggregateColumn' {} a -> s {columnNames = a} :: AggregateColumn) Prelude.. Lens.coerced

-- | Aggregation function that can be applied to aggregate column in query.
aggregateColumn_function :: Lens.Lens' AggregateColumn AggregateFunctionName
aggregateColumn_function = Lens.lens (\AggregateColumn' {function} -> function) (\s@AggregateColumn' {} a -> s {function = a} :: AggregateColumn)

instance Data.FromJSON AggregateColumn where
  parseJSON =
    Data.withObject
      "AggregateColumn"
      ( \x ->
          AggregateColumn'
            Prelude.<$> (x Data..: "columnNames")
            Prelude.<*> (x Data..: "function")
      )

instance Prelude.Hashable AggregateColumn where
  hashWithSalt _salt AggregateColumn' {..} =
    _salt
      `Prelude.hashWithSalt` columnNames
      `Prelude.hashWithSalt` function

instance Prelude.NFData AggregateColumn where
  rnf AggregateColumn' {..} =
    Prelude.rnf columnNames
      `Prelude.seq` Prelude.rnf function

instance Data.ToJSON AggregateColumn where
  toJSON AggregateColumn' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("columnNames" Data..= columnNames),
            Prelude.Just ("function" Data..= function)
          ]
      )
