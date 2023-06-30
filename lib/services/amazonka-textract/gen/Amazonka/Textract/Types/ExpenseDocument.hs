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
-- Module      : Amazonka.Textract.Types.ExpenseDocument
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseDocument where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Block
import Amazonka.Textract.Types.ExpenseField
import Amazonka.Textract.Types.LineItemGroup

-- | The structure holding all the information returned by AnalyzeExpense
--
-- /See:/ 'newExpenseDocument' smart constructor.
data ExpenseDocument = ExpenseDocument'
  { -- | This is a block object, the same as reported when DetectDocumentText is
    -- run on a document. It provides word level recognition of text.
    blocks :: Prelude.Maybe [Block],
    -- | Denotes which invoice or receipt in the document the information is
    -- coming from. First document will be 1, the second 2, and so on.
    expenseIndex :: Prelude.Maybe Prelude.Natural,
    -- | Information detected on each table of a document, seperated into
    -- @LineItems@.
    lineItemGroups :: Prelude.Maybe [LineItemGroup],
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
-- 'blocks', 'expenseDocument_blocks' - This is a block object, the same as reported when DetectDocumentText is
-- run on a document. It provides word level recognition of text.
--
-- 'expenseIndex', 'expenseDocument_expenseIndex' - Denotes which invoice or receipt in the document the information is
-- coming from. First document will be 1, the second 2, and so on.
--
-- 'lineItemGroups', 'expenseDocument_lineItemGroups' - Information detected on each table of a document, seperated into
-- @LineItems@.
--
-- 'summaryFields', 'expenseDocument_summaryFields' - Any information found outside of a table by Amazon Textract.
newExpenseDocument ::
  ExpenseDocument
newExpenseDocument =
  ExpenseDocument'
    { blocks = Prelude.Nothing,
      expenseIndex = Prelude.Nothing,
      lineItemGroups = Prelude.Nothing,
      summaryFields = Prelude.Nothing
    }

-- | This is a block object, the same as reported when DetectDocumentText is
-- run on a document. It provides word level recognition of text.
expenseDocument_blocks :: Lens.Lens' ExpenseDocument (Prelude.Maybe [Block])
expenseDocument_blocks = Lens.lens (\ExpenseDocument' {blocks} -> blocks) (\s@ExpenseDocument' {} a -> s {blocks = a} :: ExpenseDocument) Prelude.. Lens.mapping Lens.coerced

-- | Denotes which invoice or receipt in the document the information is
-- coming from. First document will be 1, the second 2, and so on.
expenseDocument_expenseIndex :: Lens.Lens' ExpenseDocument (Prelude.Maybe Prelude.Natural)
expenseDocument_expenseIndex = Lens.lens (\ExpenseDocument' {expenseIndex} -> expenseIndex) (\s@ExpenseDocument' {} a -> s {expenseIndex = a} :: ExpenseDocument)

-- | Information detected on each table of a document, seperated into
-- @LineItems@.
expenseDocument_lineItemGroups :: Lens.Lens' ExpenseDocument (Prelude.Maybe [LineItemGroup])
expenseDocument_lineItemGroups = Lens.lens (\ExpenseDocument' {lineItemGroups} -> lineItemGroups) (\s@ExpenseDocument' {} a -> s {lineItemGroups = a} :: ExpenseDocument) Prelude.. Lens.mapping Lens.coerced

-- | Any information found outside of a table by Amazon Textract.
expenseDocument_summaryFields :: Lens.Lens' ExpenseDocument (Prelude.Maybe [ExpenseField])
expenseDocument_summaryFields = Lens.lens (\ExpenseDocument' {summaryFields} -> summaryFields) (\s@ExpenseDocument' {} a -> s {summaryFields = a} :: ExpenseDocument) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON ExpenseDocument where
  parseJSON =
    Data.withObject
      "ExpenseDocument"
      ( \x ->
          ExpenseDocument'
            Prelude.<$> (x Data..:? "Blocks" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ExpenseIndex")
            Prelude.<*> (x Data..:? "LineItemGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SummaryFields" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable ExpenseDocument where
  hashWithSalt _salt ExpenseDocument' {..} =
    _salt
      `Prelude.hashWithSalt` blocks
      `Prelude.hashWithSalt` expenseIndex
      `Prelude.hashWithSalt` lineItemGroups
      `Prelude.hashWithSalt` summaryFields

instance Prelude.NFData ExpenseDocument where
  rnf ExpenseDocument' {..} =
    Prelude.rnf blocks
      `Prelude.seq` Prelude.rnf expenseIndex
      `Prelude.seq` Prelude.rnf lineItemGroups
      `Prelude.seq` Prelude.rnf summaryFields
