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
-- Module      : Amazonka.AmplifyUiBuilder.Types.Predicate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.Predicate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Stores information for generating Amplify DataStore queries. Use a
-- @Predicate@ to retrieve a subset of the data in a collection.
--
-- /See:/ 'newPredicate' smart constructor.
data Predicate = Predicate'
  { -- | The value to use when performing the evaluation.
    operand :: Prelude.Maybe Prelude.Text,
    -- | The field to query.
    field :: Prelude.Maybe Prelude.Text,
    -- | A list of predicates to combine logically.
    or :: Prelude.Maybe [Predicate],
    -- | The operator to use to perform the evaluation.
    operator :: Prelude.Maybe Prelude.Text,
    -- | A list of predicates to combine logically.
    and :: Prelude.Maybe [Predicate]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Predicate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operand', 'predicate_operand' - The value to use when performing the evaluation.
--
-- 'field', 'predicate_field' - The field to query.
--
-- 'or', 'predicate_or' - A list of predicates to combine logically.
--
-- 'operator', 'predicate_operator' - The operator to use to perform the evaluation.
--
-- 'and', 'predicate_and' - A list of predicates to combine logically.
newPredicate ::
  Predicate
newPredicate =
  Predicate'
    { operand = Prelude.Nothing,
      field = Prelude.Nothing,
      or = Prelude.Nothing,
      operator = Prelude.Nothing,
      and = Prelude.Nothing
    }

-- | The value to use when performing the evaluation.
predicate_operand :: Lens.Lens' Predicate (Prelude.Maybe Prelude.Text)
predicate_operand = Lens.lens (\Predicate' {operand} -> operand) (\s@Predicate' {} a -> s {operand = a} :: Predicate)

-- | The field to query.
predicate_field :: Lens.Lens' Predicate (Prelude.Maybe Prelude.Text)
predicate_field = Lens.lens (\Predicate' {field} -> field) (\s@Predicate' {} a -> s {field = a} :: Predicate)

-- | A list of predicates to combine logically.
predicate_or :: Lens.Lens' Predicate (Prelude.Maybe [Predicate])
predicate_or = Lens.lens (\Predicate' {or} -> or) (\s@Predicate' {} a -> s {or = a} :: Predicate) Prelude.. Lens.mapping Lens.coerced

-- | The operator to use to perform the evaluation.
predicate_operator :: Lens.Lens' Predicate (Prelude.Maybe Prelude.Text)
predicate_operator = Lens.lens (\Predicate' {operator} -> operator) (\s@Predicate' {} a -> s {operator = a} :: Predicate)

-- | A list of predicates to combine logically.
predicate_and :: Lens.Lens' Predicate (Prelude.Maybe [Predicate])
predicate_and = Lens.lens (\Predicate' {and} -> and) (\s@Predicate' {} a -> s {and = a} :: Predicate) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Predicate where
  parseJSON =
    Core.withObject
      "Predicate"
      ( \x ->
          Predicate'
            Prelude.<$> (x Core..:? "operand")
            Prelude.<*> (x Core..:? "field")
            Prelude.<*> (x Core..:? "or" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "operator")
            Prelude.<*> (x Core..:? "and" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Predicate where
  hashWithSalt _salt Predicate' {..} =
    _salt `Prelude.hashWithSalt` operand
      `Prelude.hashWithSalt` field
      `Prelude.hashWithSalt` or
      `Prelude.hashWithSalt` operator
      `Prelude.hashWithSalt` and

instance Prelude.NFData Predicate where
  rnf Predicate' {..} =
    Prelude.rnf operand
      `Prelude.seq` Prelude.rnf field
      `Prelude.seq` Prelude.rnf or
      `Prelude.seq` Prelude.rnf operator
      `Prelude.seq` Prelude.rnf and

instance Core.ToJSON Predicate where
  toJSON Predicate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("operand" Core..=) Prelude.<$> operand,
            ("field" Core..=) Prelude.<$> field,
            ("or" Core..=) Prelude.<$> or,
            ("operator" Core..=) Prelude.<$> operator,
            ("and" Core..=) Prelude.<$> and
          ]
      )
