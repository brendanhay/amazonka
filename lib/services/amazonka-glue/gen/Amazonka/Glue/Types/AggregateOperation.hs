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
-- Module      : Amazonka.Glue.Types.AggregateOperation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.AggregateOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.AggFunction
import qualified Amazonka.Prelude as Prelude

-- | Specifies the set of parameters needed to perform aggregation in the
-- aggregate transform.
--
-- /See:/ 'newAggregateOperation' smart constructor.
data AggregateOperation = AggregateOperation'
  { -- | Specifies the column on the data set on which the aggregation function
    -- will be applied.
    column :: [Prelude.Text],
    -- | Specifies the aggregation function to apply.
    --
    -- Possible aggregation functions include: avg countDistinct, count, first,
    -- last, kurtosis, max, min, skewness, stddev_samp, stddev_pop, sum,
    -- sumDistinct, var_samp, var_pop
    aggFunc :: AggFunction
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'column', 'aggregateOperation_column' - Specifies the column on the data set on which the aggregation function
-- will be applied.
--
-- 'aggFunc', 'aggregateOperation_aggFunc' - Specifies the aggregation function to apply.
--
-- Possible aggregation functions include: avg countDistinct, count, first,
-- last, kurtosis, max, min, skewness, stddev_samp, stddev_pop, sum,
-- sumDistinct, var_samp, var_pop
newAggregateOperation ::
  -- | 'aggFunc'
  AggFunction ->
  AggregateOperation
newAggregateOperation pAggFunc_ =
  AggregateOperation'
    { column = Prelude.mempty,
      aggFunc = pAggFunc_
    }

-- | Specifies the column on the data set on which the aggregation function
-- will be applied.
aggregateOperation_column :: Lens.Lens' AggregateOperation [Prelude.Text]
aggregateOperation_column = Lens.lens (\AggregateOperation' {column} -> column) (\s@AggregateOperation' {} a -> s {column = a} :: AggregateOperation) Prelude.. Lens.coerced

-- | Specifies the aggregation function to apply.
--
-- Possible aggregation functions include: avg countDistinct, count, first,
-- last, kurtosis, max, min, skewness, stddev_samp, stddev_pop, sum,
-- sumDistinct, var_samp, var_pop
aggregateOperation_aggFunc :: Lens.Lens' AggregateOperation AggFunction
aggregateOperation_aggFunc = Lens.lens (\AggregateOperation' {aggFunc} -> aggFunc) (\s@AggregateOperation' {} a -> s {aggFunc = a} :: AggregateOperation)

instance Data.FromJSON AggregateOperation where
  parseJSON =
    Data.withObject
      "AggregateOperation"
      ( \x ->
          AggregateOperation'
            Prelude.<$> (x Data..:? "Column" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "AggFunc")
      )

instance Prelude.Hashable AggregateOperation where
  hashWithSalt _salt AggregateOperation' {..} =
    _salt `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` aggFunc

instance Prelude.NFData AggregateOperation where
  rnf AggregateOperation' {..} =
    Prelude.rnf column
      `Prelude.seq` Prelude.rnf aggFunc

instance Data.ToJSON AggregateOperation where
  toJSON AggregateOperation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Column" Data..= column),
            Prelude.Just ("AggFunc" Data..= aggFunc)
          ]
      )
