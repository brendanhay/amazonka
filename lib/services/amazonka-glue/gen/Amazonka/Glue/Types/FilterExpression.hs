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
-- Module      : Amazonka.Glue.Types.FilterExpression
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.FilterExpression where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.FilterOperation
import Amazonka.Glue.Types.FilterValue
import qualified Amazonka.Prelude as Prelude

-- | Specifies a filter expression.
--
-- /See:/ 'newFilterExpression' smart constructor.
data FilterExpression = FilterExpression'
  { -- | Whether the expression is to be negated.
    negated :: Prelude.Maybe Prelude.Bool,
    -- | The type of operation to perform in the expression.
    operation :: FilterOperation,
    -- | A list of filter values.
    values :: [FilterValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterExpression' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'negated', 'filterExpression_negated' - Whether the expression is to be negated.
--
-- 'operation', 'filterExpression_operation' - The type of operation to perform in the expression.
--
-- 'values', 'filterExpression_values' - A list of filter values.
newFilterExpression ::
  -- | 'operation'
  FilterOperation ->
  FilterExpression
newFilterExpression pOperation_ =
  FilterExpression'
    { negated = Prelude.Nothing,
      operation = pOperation_,
      values = Prelude.mempty
    }

-- | Whether the expression is to be negated.
filterExpression_negated :: Lens.Lens' FilterExpression (Prelude.Maybe Prelude.Bool)
filterExpression_negated = Lens.lens (\FilterExpression' {negated} -> negated) (\s@FilterExpression' {} a -> s {negated = a} :: FilterExpression)

-- | The type of operation to perform in the expression.
filterExpression_operation :: Lens.Lens' FilterExpression FilterOperation
filterExpression_operation = Lens.lens (\FilterExpression' {operation} -> operation) (\s@FilterExpression' {} a -> s {operation = a} :: FilterExpression)

-- | A list of filter values.
filterExpression_values :: Lens.Lens' FilterExpression [FilterValue]
filterExpression_values = Lens.lens (\FilterExpression' {values} -> values) (\s@FilterExpression' {} a -> s {values = a} :: FilterExpression) Prelude.. Lens.coerced

instance Data.FromJSON FilterExpression where
  parseJSON =
    Data.withObject
      "FilterExpression"
      ( \x ->
          FilterExpression'
            Prelude.<$> (x Data..:? "Negated")
            Prelude.<*> (x Data..: "Operation")
            Prelude.<*> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FilterExpression where
  hashWithSalt _salt FilterExpression' {..} =
    _salt
      `Prelude.hashWithSalt` negated
      `Prelude.hashWithSalt` operation
      `Prelude.hashWithSalt` values

instance Prelude.NFData FilterExpression where
  rnf FilterExpression' {..} =
    Prelude.rnf negated
      `Prelude.seq` Prelude.rnf operation
      `Prelude.seq` Prelude.rnf values

instance Data.ToJSON FilterExpression where
  toJSON FilterExpression' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Negated" Data..=) Prelude.<$> negated,
            Prelude.Just ("Operation" Data..= operation),
            Prelude.Just ("Values" Data..= values)
          ]
      )
