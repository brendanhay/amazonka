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
-- Module      : Amazonka.LexV2Models.Types.AggregatedUtterancesFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.AggregatedUtterancesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.AggregatedUtterancesFilterName
import Amazonka.LexV2Models.Types.AggregatedUtterancesFilterOperator
import qualified Amazonka.Prelude as Prelude

-- | Filters responses returned by the @ListAggregatedUtterances@ operation.
--
-- /See:/ 'newAggregatedUtterancesFilter' smart constructor.
data AggregatedUtterancesFilter = AggregatedUtterancesFilter'
  { -- | The name of the field to filter the utterance list.
    name :: AggregatedUtterancesFilterName,
    -- | The value to use for filtering the list of bots.
    values :: Prelude.NonEmpty Prelude.Text,
    -- | The operator to use for the filter. Specify @EQ@ when the
    -- @ListAggregatedUtterances@ operation should return only utterances that
    -- equal the specified value. Specify @CO@ when the
    -- @ListAggregatedUtterances@ operation should return utterances that
    -- contain the specified value.
    operator :: AggregatedUtterancesFilterOperator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregatedUtterancesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'aggregatedUtterancesFilter_name' - The name of the field to filter the utterance list.
--
-- 'values', 'aggregatedUtterancesFilter_values' - The value to use for filtering the list of bots.
--
-- 'operator', 'aggregatedUtterancesFilter_operator' - The operator to use for the filter. Specify @EQ@ when the
-- @ListAggregatedUtterances@ operation should return only utterances that
-- equal the specified value. Specify @CO@ when the
-- @ListAggregatedUtterances@ operation should return utterances that
-- contain the specified value.
newAggregatedUtterancesFilter ::
  -- | 'name'
  AggregatedUtterancesFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'operator'
  AggregatedUtterancesFilterOperator ->
  AggregatedUtterancesFilter
newAggregatedUtterancesFilter
  pName_
  pValues_
  pOperator_ =
    AggregatedUtterancesFilter'
      { name = pName_,
        values = Lens.coerced Lens.# pValues_,
        operator = pOperator_
      }

-- | The name of the field to filter the utterance list.
aggregatedUtterancesFilter_name :: Lens.Lens' AggregatedUtterancesFilter AggregatedUtterancesFilterName
aggregatedUtterancesFilter_name = Lens.lens (\AggregatedUtterancesFilter' {name} -> name) (\s@AggregatedUtterancesFilter' {} a -> s {name = a} :: AggregatedUtterancesFilter)

-- | The value to use for filtering the list of bots.
aggregatedUtterancesFilter_values :: Lens.Lens' AggregatedUtterancesFilter (Prelude.NonEmpty Prelude.Text)
aggregatedUtterancesFilter_values = Lens.lens (\AggregatedUtterancesFilter' {values} -> values) (\s@AggregatedUtterancesFilter' {} a -> s {values = a} :: AggregatedUtterancesFilter) Prelude.. Lens.coerced

-- | The operator to use for the filter. Specify @EQ@ when the
-- @ListAggregatedUtterances@ operation should return only utterances that
-- equal the specified value. Specify @CO@ when the
-- @ListAggregatedUtterances@ operation should return utterances that
-- contain the specified value.
aggregatedUtterancesFilter_operator :: Lens.Lens' AggregatedUtterancesFilter AggregatedUtterancesFilterOperator
aggregatedUtterancesFilter_operator = Lens.lens (\AggregatedUtterancesFilter' {operator} -> operator) (\s@AggregatedUtterancesFilter' {} a -> s {operator = a} :: AggregatedUtterancesFilter)

instance Prelude.Hashable AggregatedUtterancesFilter where
  hashWithSalt _salt AggregatedUtterancesFilter' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` operator

instance Prelude.NFData AggregatedUtterancesFilter where
  rnf AggregatedUtterancesFilter' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf operator

instance Data.ToJSON AggregatedUtterancesFilter where
  toJSON AggregatedUtterancesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("values" Data..= values),
            Prelude.Just ("operator" Data..= operator)
          ]
      )
