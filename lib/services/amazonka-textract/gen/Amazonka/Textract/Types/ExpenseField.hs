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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseField where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.ExpenseDetection
import Amazonka.Textract.Types.ExpenseType

-- | Breakdown of detected information, seperated into the catagories Type,
-- LabelDetection, and ValueDetection
--
-- /See:/ 'newExpenseField' smart constructor.
data ExpenseField = ExpenseField'
  { -- | The implied label of a detected element. Present alongside
    -- LabelDetection for explicit elements.
    type' :: Prelude.Maybe ExpenseType,
    -- | The page number the value was detected on.
    pageNumber :: Prelude.Maybe Prelude.Natural,
    -- | The explicitly stated label of a detected element.
    labelDetection :: Prelude.Maybe ExpenseDetection,
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
-- 'type'', 'expenseField_type' - The implied label of a detected element. Present alongside
-- LabelDetection for explicit elements.
--
-- 'pageNumber', 'expenseField_pageNumber' - The page number the value was detected on.
--
-- 'labelDetection', 'expenseField_labelDetection' - The explicitly stated label of a detected element.
--
-- 'valueDetection', 'expenseField_valueDetection' - The value of a detected element. Present in explicit and implicit
-- elements.
newExpenseField ::
  ExpenseField
newExpenseField =
  ExpenseField'
    { type' = Prelude.Nothing,
      pageNumber = Prelude.Nothing,
      labelDetection = Prelude.Nothing,
      valueDetection = Prelude.Nothing
    }

-- | The implied label of a detected element. Present alongside
-- LabelDetection for explicit elements.
expenseField_type :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseType)
expenseField_type = Lens.lens (\ExpenseField' {type'} -> type') (\s@ExpenseField' {} a -> s {type' = a} :: ExpenseField)

-- | The page number the value was detected on.
expenseField_pageNumber :: Lens.Lens' ExpenseField (Prelude.Maybe Prelude.Natural)
expenseField_pageNumber = Lens.lens (\ExpenseField' {pageNumber} -> pageNumber) (\s@ExpenseField' {} a -> s {pageNumber = a} :: ExpenseField)

-- | The explicitly stated label of a detected element.
expenseField_labelDetection :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseDetection)
expenseField_labelDetection = Lens.lens (\ExpenseField' {labelDetection} -> labelDetection) (\s@ExpenseField' {} a -> s {labelDetection = a} :: ExpenseField)

-- | The value of a detected element. Present in explicit and implicit
-- elements.
expenseField_valueDetection :: Lens.Lens' ExpenseField (Prelude.Maybe ExpenseDetection)
expenseField_valueDetection = Lens.lens (\ExpenseField' {valueDetection} -> valueDetection) (\s@ExpenseField' {} a -> s {valueDetection = a} :: ExpenseField)

instance Core.FromJSON ExpenseField where
  parseJSON =
    Core.withObject
      "ExpenseField"
      ( \x ->
          ExpenseField'
            Prelude.<$> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "PageNumber")
            Prelude.<*> (x Core..:? "LabelDetection")
            Prelude.<*> (x Core..:? "ValueDetection")
      )

instance Prelude.Hashable ExpenseField where
  hashWithSalt _salt ExpenseField' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` pageNumber
      `Prelude.hashWithSalt` labelDetection
      `Prelude.hashWithSalt` valueDetection

instance Prelude.NFData ExpenseField where
  rnf ExpenseField' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf pageNumber
      `Prelude.seq` Prelude.rnf labelDetection
      `Prelude.seq` Prelude.rnf valueDetection
