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
-- Module      : Amazonka.Textract.Types.ExpenseDetection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Geometry

-- | An object used to store information about the Value or Label detected by
-- Amazon Textract.
--
-- /See:/ 'newExpenseDetection' smart constructor.
data ExpenseDetection = ExpenseDetection'
  { -- | The word or line of text recognized by Amazon Textract
    text :: Prelude.Maybe Prelude.Text,
    -- | The confidence in detection, as a percentage
    confidence :: Prelude.Maybe Prelude.Double,
    geometry :: Prelude.Maybe Geometry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpenseDetection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'expenseDetection_text' - The word or line of text recognized by Amazon Textract
--
-- 'confidence', 'expenseDetection_confidence' - The confidence in detection, as a percentage
--
-- 'geometry', 'expenseDetection_geometry' - Undocumented member.
newExpenseDetection ::
  ExpenseDetection
newExpenseDetection =
  ExpenseDetection'
    { text = Prelude.Nothing,
      confidence = Prelude.Nothing,
      geometry = Prelude.Nothing
    }

-- | The word or line of text recognized by Amazon Textract
expenseDetection_text :: Lens.Lens' ExpenseDetection (Prelude.Maybe Prelude.Text)
expenseDetection_text = Lens.lens (\ExpenseDetection' {text} -> text) (\s@ExpenseDetection' {} a -> s {text = a} :: ExpenseDetection)

-- | The confidence in detection, as a percentage
expenseDetection_confidence :: Lens.Lens' ExpenseDetection (Prelude.Maybe Prelude.Double)
expenseDetection_confidence = Lens.lens (\ExpenseDetection' {confidence} -> confidence) (\s@ExpenseDetection' {} a -> s {confidence = a} :: ExpenseDetection)

-- | Undocumented member.
expenseDetection_geometry :: Lens.Lens' ExpenseDetection (Prelude.Maybe Geometry)
expenseDetection_geometry = Lens.lens (\ExpenseDetection' {geometry} -> geometry) (\s@ExpenseDetection' {} a -> s {geometry = a} :: ExpenseDetection)

instance Core.FromJSON ExpenseDetection where
  parseJSON =
    Core.withObject
      "ExpenseDetection"
      ( \x ->
          ExpenseDetection'
            Prelude.<$> (x Core..:? "Text")
            Prelude.<*> (x Core..:? "Confidence")
            Prelude.<*> (x Core..:? "Geometry")
      )

instance Prelude.Hashable ExpenseDetection

instance Prelude.NFData ExpenseDetection
