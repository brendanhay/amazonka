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
-- Module      : Amazonka.Textract.Types.ExpenseField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.ExpenseCurrency
import Amazonka.Textract.Types.ExpenseDetection
import Amazonka.Textract.Types.ExpenseGroupProperty
import Amazonka.Textract.Types.ExpenseType

-- | Breakdown of detected information, seperated into the catagories Type,
-- LabelDetection, and ValueDetection
--
-- /See:/ 'newExpenseField' smart constructor.
data ExpenseField = ExpenseField'
  { -- | Shows the kind of currency, both the code and confidence associated with
    -- any monatary value detected.
    currency :: Prelude.Maybe ExpenseCurrency,
    -- | Shows which group a response object belongs to, such as whether an
    -- address line belongs to the vendor\'s address or the recipent\'s
    -- address.
    groupProperties :: Prelude.Maybe [ExpenseGroupProperty],
    -- | The explicitly stated label of a detected element.
    labelDetection :: Prelude.Maybe ExpenseDetection,
    -- | The page number the value was detected on.
    pageNumber :: Prelude.Maybe Prelude.Natural,
    -- | The implied label of a detected element. Present alongside
    -- LabelDetection for explicit elements.
    type' :: Prelude.Maybe ExpenseType,
    -- | The value of a detected element. Present in explicit and implicit
    -- elements.
    valueDetection :: Prelude.Maybe ExpenseDetection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpenseField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currency', 'expenseField_currency' - Shows the kind of currency, both the code and confidence associated with
-- any monatary value detected.
--
-- 'groupProperties', 'expenseField_groupProperties' - Shows which group a response object belongs to, such as whether an
-- address line belongs to the vendor\'s address or the recipent\'s
-- address.
--
-- 'labelDetection', 'expenseField_labelDetection' - The explicitly stated label of a detected element.
--
-- 'pageNumber', 'expenseField_pageNumber' - The page number the value was detected on.
--
-- 'type'', 'expenseField_type' - The implied label of a detected element. Present alongside
-- LabelDetection for explicit elements.
--
-- 'valueDetection', 'expenseField_valueDetection' - The value of a detected element. Present in explicit and implicit
-- elements.
newExpenseField ::
  ExpenseField
newExpenseField =
  ExpenseField'
    { currency = Prelude.Nothing,
      groupProperties = Prelude.Nothing,
      labelDetection = Prelude.Nothing,
      pageNumber = Prelude.Nothing,
      type' = Prelude.Nothing,
      valueDetection = Prelude.Nothing
    }

-- | Shows the kind of currency, both the code and confidence associated with
-- any monatary value detected.
expenseField_currency :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseCurrency)
expenseField_currency = Lens.lens (\ExpenseField' {currency} -> currency) (\s@ExpenseField' {} a -> s {currency = a} :: ExpenseField)

-- | Shows which group a response object belongs to, such as whether an
-- address line belongs to the vendor\'s address or the recipent\'s
-- address.
expenseField_groupProperties :: Lens.Lens' ExpenseField (Prelude.Maybe [ExpenseGroupProperty])
expenseField_groupProperties = Lens.lens (\ExpenseField' {groupProperties} -> groupProperties) (\s@ExpenseField' {} a -> s {groupProperties = a} :: ExpenseField) Prelude.. Lens.mapping Lens.coerced

-- | The explicitly stated label of a detected element.
expenseField_labelDetection :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseDetection)
expenseField_labelDetection = Lens.lens (\ExpenseField' {labelDetection} -> labelDetection) (\s@ExpenseField' {} a -> s {labelDetection = a} :: ExpenseField)

-- | The page number the value was detected on.
expenseField_pageNumber :: Lens.Lens' ExpenseField (Prelude.Maybe Prelude.Natural)
expenseField_pageNumber = Lens.lens (\ExpenseField' {pageNumber} -> pageNumber) (\s@ExpenseField' {} a -> s {pageNumber = a} :: ExpenseField)

-- | The implied label of a detected element. Present alongside
-- LabelDetection for explicit elements.
expenseField_type :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseType)
expenseField_type = Lens.lens (\ExpenseField' {type'} -> type') (\s@ExpenseField' {} a -> s {type' = a} :: ExpenseField)

-- | The value of a detected element. Present in explicit and implicit
-- elements.
expenseField_valueDetection :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseDetection)
expenseField_valueDetection = Lens.lens (\ExpenseField' {valueDetection} -> valueDetection) (\s@ExpenseField' {} a -> s {valueDetection = a} :: ExpenseField)

instance Data.FromJSON ExpenseField where
  parseJSON =
    Data.withObject
      "ExpenseField"
      ( \x ->
          ExpenseField'
            Prelude.<$> (x Data..:? "Currency")
            Prelude.<*> ( x
                            Data..:? "GroupProperties"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "LabelDetection")
            Prelude.<*> (x Data..:? "PageNumber")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "ValueDetection")
      )

instance Prelude.Hashable ExpenseField where
  hashWithSalt _salt ExpenseField' {..} =
    _salt
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` groupProperties
      `Prelude.hashWithSalt` labelDetection
      `Prelude.hashWithSalt` pageNumber
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` valueDetection

instance Prelude.NFData ExpenseField where
  rnf ExpenseField' {..} =
    Prelude.rnf currency `Prelude.seq`
      Prelude.rnf groupProperties `Prelude.seq`
        Prelude.rnf labelDetection `Prelude.seq`
          Prelude.rnf pageNumber `Prelude.seq`
            Prelude.rnf type' `Prelude.seq`
              Prelude.rnf valueDetection
