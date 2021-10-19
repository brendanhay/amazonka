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
-- Module      : Network.AWS.Textract.Types.ExpenseType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Textract.Types.ExpenseType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object used to store information about the Type detected by Amazon
-- Textract.
--
-- /See:/ 'newExpenseType' smart constructor.
data ExpenseType = ExpenseType'
  { -- | The word or line of text detected by Amazon Textract.
    text :: Prelude.Maybe Prelude.Text,
    -- | The confidence of accuracy, as a percentage.
    confidence :: Prelude.Maybe Prelude.Double
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
-- 'text', 'expenseType_text' - The word or line of text detected by Amazon Textract.
--
-- 'confidence', 'expenseType_confidence' - The confidence of accuracy, as a percentage.
newExpenseType ::
  ExpenseType
newExpenseType =
  ExpenseType'
    { text = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | The word or line of text detected by Amazon Textract.
expenseType_text :: Lens.Lens' ExpenseType (Prelude.Maybe Prelude.Text)
expenseType_text = Lens.lens (\ExpenseType' {text} -> text) (\s@ExpenseType' {} a -> s {text = a} :: ExpenseType)

-- | The confidence of accuracy, as a percentage.
expenseType_confidence :: Lens.Lens' ExpenseType (Prelude.Maybe Prelude.Double)
expenseType_confidence = Lens.lens (\ExpenseType' {confidence} -> confidence) (\s@ExpenseType' {} a -> s {confidence = a} :: ExpenseType)

instance Core.FromJSON ExpenseType where
  parseJSON =
    Core.withObject
      "ExpenseType"
      ( \x ->
          ExpenseType'
            Prelude.<$> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable ExpenseType

instance Prelude.NFData ExpenseType
