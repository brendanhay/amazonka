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
-- Module      : Amazonka.Budgets.Types.Spend
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Budgets.Types.Spend where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The amount of cost or usage that\'s measured for a budget.
--
-- For example, a @Spend@ for @3 GB@ of S3 usage has the following
-- parameters:
--
-- -   An @Amount@ of @3@
--
-- -   A @unit@ of @GB@
--
-- /See:/ 'newSpend' smart constructor.
data Spend = Spend'
  { -- | The cost or usage amount that\'s associated with a budget forecast,
    -- actual spend, or budget threshold.
    amount :: Prelude.Text,
    -- | The unit of measurement that\'s used for the budget forecast, actual
    -- spend, or budget threshold, such as USD or GBP.
    unit :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Spend' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'spend_amount' - The cost or usage amount that\'s associated with a budget forecast,
-- actual spend, or budget threshold.
--
-- 'unit', 'spend_unit' - The unit of measurement that\'s used for the budget forecast, actual
-- spend, or budget threshold, such as USD or GBP.
newSpend ::
  -- | 'amount'
  Prelude.Text ->
  -- | 'unit'
  Prelude.Text ->
  Spend
newSpend pAmount_ pUnit_ =
  Spend' {amount = pAmount_, unit = pUnit_}

-- | The cost or usage amount that\'s associated with a budget forecast,
-- actual spend, or budget threshold.
spend_amount :: Lens.Lens' Spend Prelude.Text
spend_amount = Lens.lens (\Spend' {amount} -> amount) (\s@Spend' {} a -> s {amount = a} :: Spend)

-- | The unit of measurement that\'s used for the budget forecast, actual
-- spend, or budget threshold, such as USD or GBP.
spend_unit :: Lens.Lens' Spend Prelude.Text
spend_unit = Lens.lens (\Spend' {unit} -> unit) (\s@Spend' {} a -> s {unit = a} :: Spend)

instance Data.FromJSON Spend where
  parseJSON =
    Data.withObject
      "Spend"
      ( \x ->
          Spend'
            Prelude.<$> (x Data..: "Amount") Prelude.<*> (x Data..: "Unit")
      )

instance Prelude.Hashable Spend where
  hashWithSalt _salt Spend' {..} =
    _salt `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` unit

instance Prelude.NFData Spend where
  rnf Spend' {..} =
    Prelude.rnf amount `Prelude.seq` Prelude.rnf unit

instance Data.ToJSON Spend where
  toJSON Spend' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Amount" Data..= amount),
            Prelude.Just ("Unit" Data..= unit)
          ]
      )
