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
-- Module      : Network.AWS.Textract.Types.ExpenseDocument
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Textract.Types.ExpenseDocument where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Textract.Types.ExpenseField
import Network.AWS.Textract.Types.LineItemGroup

-- | The structure holding all the information returned by AnalyzeExpense
--
-- /See:/ 'newExpenseDocument' smart constructor.
data ExpenseDocument = ExpenseDocument'
  { -- | Information detected on each table of a document, seperated into
    -- @LineItems@.
    lineItemGroups :: Prelude.Maybe [LineItemGroup],
    -- | Denotes which invoice or receipt in the document the information is
    -- coming from. First document will be 1, the second 2, and so on.
    expenseIndex :: Prelude.Maybe Prelude.Natural,
    -- | Any information found outside of a table by Amazon Textract.
    summaryFields :: Prelude.Maybe [ExpenseField]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpenseDocument' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lineItemGroups', 'expenseDocument_lineItemGroups' - Information detected on each table of a document, seperated into
-- @LineItems@.
--
-- 'expenseIndex', 'expenseDocument_expenseIndex' - Denotes which invoice or receipt in the document the information is
-- coming from. First document will be 1, the second 2, and so on.
--
-- 'summaryFields', 'expenseDocument_summaryFields' - Any information found outside of a table by Amazon Textract.
newExpenseDocument ::
  ExpenseDocument
newExpenseDocument =
  ExpenseDocument'
    { lineItemGroups = Prelude.Nothing,
      expenseIndex = Prelude.Nothing,
      summaryFields = Prelude.Nothing
    }

-- | Information detected on each table of a document, seperated into
-- @LineItems@.
expenseDocument_lineItemGroups :: Lens.Lens' ExpenseDocument (Prelude.Maybe [LineItemGroup])
expenseDocument_lineItemGroups = Lens.lens (\ExpenseDocument' {lineItemGroups} -> lineItemGroups) (\s@ExpenseDocument' {} a -> s {lineItemGroups = a} :: ExpenseDocument) Prelude.. Lens.mapping Lens._Coerce

-- | Denotes which invoice or receipt in the document the information is
-- coming from. First document will be 1, the second 2, and so on.
expenseDocument_expenseIndex :: Lens.Lens' ExpenseDocument (Prelude.Maybe Prelude.Natural)
expenseDocument_expenseIndex = Lens.lens (\ExpenseDocument' {expenseIndex} -> expenseIndex) (\s@ExpenseDocument' {} a -> s {expenseIndex = a} :: ExpenseDocument)

-- | Any information found outside of a table by Amazon Textract.
expenseDocument_summaryFields :: Lens.Lens' ExpenseDocument (Prelude.Maybe [ExpenseField])
expenseDocument_summaryFields = Lens.lens (\ExpenseDocument' {summaryFields} -> summaryFields) (\s@ExpenseDocument' {} a -> s {summaryFields = a} :: ExpenseDocument) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON ExpenseDocument where
  parseJSON =
    Core.withObject
      "ExpenseDocument"
      ( \x ->
          ExpenseDocument'
            Prelude.<$> (x Core..:? "LineItemGroups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ExpenseIndex")
            Prelude.<*> (x Core..:? "SummaryFields" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ExpenseDocument

instance Prelude.NFData ExpenseDocument
