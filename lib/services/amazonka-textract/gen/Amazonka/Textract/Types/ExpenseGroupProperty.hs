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
-- Module      : Amazonka.Textract.Types.ExpenseGroupProperty
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseGroupProperty where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Shows the group that a certain key belongs to. This helps differentiate
-- between names and addresses for different organizations, that can be
-- hard to determine via JSON response.
--
-- /See:/ 'newExpenseGroupProperty' smart constructor.
data ExpenseGroupProperty = ExpenseGroupProperty'
  { -- | Provides a group Id number, which will be the same for each in the
    -- group.
    id :: Prelude.Maybe Prelude.Text,
    -- | Informs you on whether the expense group is a name or an address.
    types :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpenseGroupProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'expenseGroupProperty_id' - Provides a group Id number, which will be the same for each in the
-- group.
--
-- 'types', 'expenseGroupProperty_types' - Informs you on whether the expense group is a name or an address.
newExpenseGroupProperty ::
  ExpenseGroupProperty
newExpenseGroupProperty =
  ExpenseGroupProperty'
    { id = Prelude.Nothing,
      types = Prelude.Nothing
    }

-- | Provides a group Id number, which will be the same for each in the
-- group.
expenseGroupProperty_id :: Lens.Lens' ExpenseGroupProperty (Prelude.Maybe Prelude.Text)
expenseGroupProperty_id = Lens.lens (\ExpenseGroupProperty' {id} -> id) (\s@ExpenseGroupProperty' {} a -> s {id = a} :: ExpenseGroupProperty)

-- | Informs you on whether the expense group is a name or an address.
expenseGroupProperty_types :: Lens.Lens' ExpenseGroupProperty (Prelude.Maybe [Prelude.Text])
expenseGroupProperty_types = Lens.lens (\ExpenseGroupProperty' {types} -> types) (\s@ExpenseGroupProperty' {} a -> s {types = a} :: ExpenseGroupProperty) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExpenseGroupProperty where
  parseJSON =
    Data.withObject
      "ExpenseGroupProperty"
      ( \x ->
          ExpenseGroupProperty'
            Prelude.<$> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Types" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExpenseGroupProperty where
  hashWithSalt _salt ExpenseGroupProperty' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` types

instance Prelude.NFData ExpenseGroupProperty where
  rnf ExpenseGroupProperty' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf types
