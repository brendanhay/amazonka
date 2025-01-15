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
-- Module      : Amazonka.Textract.Types.ExpenseType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object used to store information about the Type detected by Amazon
-- Textract.
--
-- /See:/ 'newExpenseType' smart constructor.
data ExpenseType = ExpenseType'
  { -- | The confidence of accuracy, as a percentage.
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The word or line of text detected by Amazon Textract.
    text :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpenseType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'confidence', 'expenseType_confidence' - The confidence of accuracy, as a percentage.
--
-- 'text', 'expenseType_text' - The word or line of text detected by Amazon Textract.
newExpenseType ::
  ExpenseType
newExpenseType =
  ExpenseType'
    { confidence = Prelude.Nothing,
      text = Prelude.Nothing
    }

-- | The confidence of accuracy, as a percentage.
expenseType_confidence :: Lens.Lens' ExpenseType (Prelude.Maybe Prelude.Double)
expenseType_confidence = Lens.lens (\ExpenseType' {confidence} -> confidence) (\s@ExpenseType' {} a -> s {confidence = a} :: ExpenseType)

-- | The word or line of text detected by Amazon Textract.
expenseType_text :: Lens.Lens' ExpenseType (Prelude.Maybe Prelude.Text)
expenseType_text = Lens.lens (\ExpenseType' {text} -> text) (\s@ExpenseType' {} a -> s {text = a} :: ExpenseType)

instance Data.FromJSON ExpenseType where
  parseJSON =
    Data.withObject
      "ExpenseType"
      ( \x ->
          ExpenseType'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Text")
      )

instance Prelude.Hashable ExpenseType where
  hashWithSalt _salt ExpenseType' {..} =
    _salt
      `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` text

instance Prelude.NFData ExpenseType where
  rnf ExpenseType' {..} =
    Prelude.rnf confidence `Prelude.seq`
      Prelude.rnf text
