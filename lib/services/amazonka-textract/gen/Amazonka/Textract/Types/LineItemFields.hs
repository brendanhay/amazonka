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
-- Module      : Amazonka.Textract.Types.LineItemFields
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.LineItemFields where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.ExpenseField

-- | A structure that holds information about the different lines found in a
-- document\'s tables.
--
-- /See:/ 'newLineItemFields' smart constructor.
data LineItemFields = LineItemFields'
  { -- | ExpenseFields used to show information from detected lines on a table.
    lineItemExpenseFields :: Prelude.Maybe [ExpenseField]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineItemFields' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineItemExpenseFields', 'lineItemFields_lineItemExpenseFields' - ExpenseFields used to show information from detected lines on a table.
newLineItemFields ::
  LineItemFields
newLineItemFields =
  LineItemFields'
    { lineItemExpenseFields =
        Prelude.Nothing
    }

-- | ExpenseFields used to show information from detected lines on a table.
lineItemFields_lineItemExpenseFields :: Lens.Lens' LineItemFields (Prelude.Maybe [ExpenseField])
lineItemFields_lineItemExpenseFields = Lens.lens (\LineItemFields' {lineItemExpenseFields} -> lineItemExpenseFields) (\s@LineItemFields' {} a -> s {lineItemExpenseFields = a} :: LineItemFields) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LineItemFields where
  parseJSON =
    Data.withObject
      "LineItemFields"
      ( \x ->
          LineItemFields'
            Prelude.<$> ( x Data..:? "LineItemExpenseFields"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LineItemFields where
  hashWithSalt _salt LineItemFields' {..} =
    _salt `Prelude.hashWithSalt` lineItemExpenseFields

instance Prelude.NFData LineItemFields where
  rnf LineItemFields' {..} =
    Prelude.rnf lineItemExpenseFields
