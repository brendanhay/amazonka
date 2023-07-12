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
-- Module      : Amazonka.HoneyCode.Types.Filter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.Filter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a filter formula along with the id of the
-- context row under which the filter function needs to evaluate.
--
-- /See:/ 'newFilter' smart constructor.
data Filter = Filter'
  { -- | The optional contextRowId attribute can be used to specify the row id of
    -- the context row if the filter formula contains unqualified references to
    -- table columns and needs a context row to evaluate them successfully.
    contextRowId :: Prelude.Maybe Prelude.Text,
    -- | A formula representing a filter function that returns zero or more
    -- matching rows from a table. Valid formulas in this field return a list
    -- of rows from a table. The most common ways of writing a formula to
    -- return a list of rows are to use the FindRow() or Filter() functions.
    -- Any other formula that returns zero or more rows is also acceptable. For
    -- example, you can use a formula that points to a cell that contains a
    -- filter function.
    formula :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Filter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextRowId', 'filter_contextRowId' - The optional contextRowId attribute can be used to specify the row id of
-- the context row if the filter formula contains unqualified references to
-- table columns and needs a context row to evaluate them successfully.
--
-- 'formula', 'filter_formula' - A formula representing a filter function that returns zero or more
-- matching rows from a table. Valid formulas in this field return a list
-- of rows from a table. The most common ways of writing a formula to
-- return a list of rows are to use the FindRow() or Filter() functions.
-- Any other formula that returns zero or more rows is also acceptable. For
-- example, you can use a formula that points to a cell that contains a
-- filter function.
newFilter ::
  -- | 'formula'
  Prelude.Text ->
  Filter
newFilter pFormula_ =
  Filter'
    { contextRowId = Prelude.Nothing,
      formula = Data._Sensitive Lens.# pFormula_
    }

-- | The optional contextRowId attribute can be used to specify the row id of
-- the context row if the filter formula contains unqualified references to
-- table columns and needs a context row to evaluate them successfully.
filter_contextRowId :: Lens.Lens' Filter (Prelude.Maybe Prelude.Text)
filter_contextRowId = Lens.lens (\Filter' {contextRowId} -> contextRowId) (\s@Filter' {} a -> s {contextRowId = a} :: Filter)

-- | A formula representing a filter function that returns zero or more
-- matching rows from a table. Valid formulas in this field return a list
-- of rows from a table. The most common ways of writing a formula to
-- return a list of rows are to use the FindRow() or Filter() functions.
-- Any other formula that returns zero or more rows is also acceptable. For
-- example, you can use a formula that points to a cell that contains a
-- filter function.
filter_formula :: Lens.Lens' Filter Prelude.Text
filter_formula = Lens.lens (\Filter' {formula} -> formula) (\s@Filter' {} a -> s {formula = a} :: Filter) Prelude.. Data._Sensitive

instance Prelude.Hashable Filter where
  hashWithSalt _salt Filter' {..} =
    _salt
      `Prelude.hashWithSalt` contextRowId
      `Prelude.hashWithSalt` formula

instance Prelude.NFData Filter where
  rnf Filter' {..} =
    Prelude.rnf contextRowId
      `Prelude.seq` Prelude.rnf formula

instance Data.ToJSON Filter where
  toJSON Filter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("contextRowId" Data..=) Prelude.<$> contextRowId,
            Prelude.Just ("formula" Data..= formula)
          ]
      )
