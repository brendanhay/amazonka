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
-- Module      : Amazonka.Textract.Types.ExpenseCurrency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Textract.Types.ExpenseCurrency where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns the kind of currency detected.
--
-- /See:/ 'newExpenseCurrency' smart constructor.
data ExpenseCurrency = ExpenseCurrency'
  { -- | Currency code for detected currency. the current supported codes are:
    --
    -- -   USD
    --
    -- -   EUR
    --
    -- -   GBP
    --
    -- -   CAD
    --
    -- -   INR
    --
    -- -   JPY
    --
    -- -   CHF
    --
    -- -   AUD
    --
    -- -   CNY
    --
    -- -   BZR
    --
    -- -   SEK
    --
    -- -   HKD
    code :: Prelude.Maybe Prelude.Text,
    -- | Percentage confideence in the detected currency.
    confidence :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExpenseCurrency' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'expenseCurrency_code' - Currency code for detected currency. the current supported codes are:
--
-- -   USD
--
-- -   EUR
--
-- -   GBP
--
-- -   CAD
--
-- -   INR
--
-- -   JPY
--
-- -   CHF
--
-- -   AUD
--
-- -   CNY
--
-- -   BZR
--
-- -   SEK
--
-- -   HKD
--
-- 'confidence', 'expenseCurrency_confidence' - Percentage confideence in the detected currency.
newExpenseCurrency ::
  ExpenseCurrency
newExpenseCurrency =
  ExpenseCurrency'
    { code = Prelude.Nothing,
      confidence = Prelude.Nothing
    }

-- | Currency code for detected currency. the current supported codes are:
--
-- -   USD
--
-- -   EUR
--
-- -   GBP
--
-- -   CAD
--
-- -   INR
--
-- -   JPY
--
-- -   CHF
--
-- -   AUD
--
-- -   CNY
--
-- -   BZR
--
-- -   SEK
--
-- -   HKD
expenseCurrency_code :: Lens.Lens' ExpenseCurrency (Prelude.Maybe Prelude.Text)
expenseCurrency_code = Lens.lens (\ExpenseCurrency' {code} -> code) (\s@ExpenseCurrency' {} a -> s {code = a} :: ExpenseCurrency)

-- | Percentage confideence in the detected currency.
expenseCurrency_confidence :: Lens.Lens' ExpenseCurrency (Prelude.Maybe Prelude.Double)
expenseCurrency_confidence = Lens.lens (\ExpenseCurrency' {confidence} -> confidence) (\s@ExpenseCurrency' {} a -> s {confidence = a} :: ExpenseCurrency)

instance Core.FromJSON ExpenseCurrency where
  parseJSON =
    Core.withObject
      "ExpenseCurrency"
      ( \x ->
          ExpenseCurrency'
            Prelude.<$> (x Core..:? "Code")
            Prelude.<*> (x Core..:? "Confidence")
      )

instance Prelude.Hashable ExpenseCurrency where
  hashWithSalt _salt ExpenseCurrency' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` confidence

instance Prelude.NFData ExpenseCurrency where
  rnf ExpenseCurrency' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf confidence
