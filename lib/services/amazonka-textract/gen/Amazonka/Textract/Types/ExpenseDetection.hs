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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseDetection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Textract.Types.Geometry

-- | An object used to store information about the Value or Label detected by
-- Amazon Textract.
--
-- /See:/ 'newExpenseDetection' smart constructor.
data ExpenseDetection = ExpenseDetection'
  { -- | The confidence in detection, as a percentage
    confidence :: Prelude.Maybe Prelude.Double,
    -- | The word or line of text recognized by Amazon Textract
    text :: Prelude.Maybe Prelude.Text,
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
-- 'confidence', 'expenseDetection_confidence' - The confidence in detection, as a percentage
--
-- 'text', 'expenseDetection_text' - The word or line of text recognized by Amazon Textract
--
-- 'geometry', 'expenseDetection_geometry' - Undocumented member.
newExpenseDetection ::
  ExpenseDetection
newExpenseDetection =
  ExpenseDetection'
    { confidence = Prelude.Nothing,
      text = Prelude.Nothing,
      geometry = Prelude.Nothing
    }

-- | The confidence in detection, as a percentage
expenseDetection_confidence :: Lens.Lens' ExpenseDetection (Prelude.Maybe Prelude.Double)
expenseDetection_confidence = Lens.lens (\ExpenseDetection' {confidence} -> confidence) (\s@ExpenseDetection' {} a -> s {confidence = a} :: ExpenseDetection)

-- | The word or line of text recognized by Amazon Textract
expenseDetection_text :: Lens.Lens' ExpenseDetection (Prelude.Maybe Prelude.Text)
expenseDetection_text = Lens.lens (\ExpenseDetection' {text} -> text) (\s@ExpenseDetection' {} a -> s {text = a} :: ExpenseDetection)

-- | Undocumented member.
expenseDetection_geometry :: Lens.Lens' ExpenseDetection (Prelude.Maybe Geometry)
expenseDetection_geometry = Lens.lens (\ExpenseDetection' {geometry} -> geometry) (\s@ExpenseDetection' {} a -> s {geometry = a} :: ExpenseDetection)

instance Data.FromJSON ExpenseDetection where
  parseJSON =
    Data.withObject
      "ExpenseDetection"
      ( \x ->
          ExpenseDetection'
            Prelude.<$> (x Data..:? "Confidence")
            Prelude.<*> (x Data..:? "Text")
            Prelude.<*> (x Data..:? "Geometry")
      )

instance Prelude.Hashable ExpenseDetection where
  hashWithSalt _salt ExpenseDetection' {..} =
    _salt `Prelude.hashWithSalt` confidence
      `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` geometry

instance Prelude.NFData ExpenseDetection where
  rnf ExpenseDetection' {..} =
    Prelude.rnf confidence
      `Prelude.seq` Prelude.rnf text
      `Prelude.seq` Prelude.rnf geometry
