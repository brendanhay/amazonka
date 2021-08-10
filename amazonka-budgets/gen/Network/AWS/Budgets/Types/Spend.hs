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
-- Module      : Network.AWS.Budgets.Types.Spend
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.Spend where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The amount of cost or usage that is measured for a budget.
--
-- For example, a @Spend@ for @3 GB@ of S3 usage would have the following
-- parameters:
--
-- -   An @Amount@ of @3@
--
-- -   A @unit@ of @GB@
--
-- /See:/ 'newSpend' smart constructor.
data Spend = Spend'
  { -- | The cost or usage amount that is associated with a budget forecast,
    -- actual spend, or budget threshold.
    amount :: Prelude.Text,
    -- | The unit of measurement that is used for the budget forecast, actual
    -- spend, or budget threshold, such as dollars or GB.
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
-- 'amount', 'spend_amount' - The cost or usage amount that is associated with a budget forecast,
-- actual spend, or budget threshold.
--
-- 'unit', 'spend_unit' - The unit of measurement that is used for the budget forecast, actual
-- spend, or budget threshold, such as dollars or GB.
newSpend ::
  -- | 'amount'
  Prelude.Text ->
  -- | 'unit'
  Prelude.Text ->
  Spend
newSpend pAmount_ pUnit_ =
  Spend' {amount = pAmount_, unit = pUnit_}

-- | The cost or usage amount that is associated with a budget forecast,
-- actual spend, or budget threshold.
spend_amount :: Lens.Lens' Spend Prelude.Text
spend_amount = Lens.lens (\Spend' {amount} -> amount) (\s@Spend' {} a -> s {amount = a} :: Spend)

-- | The unit of measurement that is used for the budget forecast, actual
-- spend, or budget threshold, such as dollars or GB.
spend_unit :: Lens.Lens' Spend Prelude.Text
spend_unit = Lens.lens (\Spend' {unit} -> unit) (\s@Spend' {} a -> s {unit = a} :: Spend)

instance Core.FromJSON Spend where
  parseJSON =
    Core.withObject
      "Spend"
      ( \x ->
          Spend'
            Prelude.<$> (x Core..: "Amount") Prelude.<*> (x Core..: "Unit")
      )

instance Prelude.Hashable Spend

instance Prelude.NFData Spend

instance Core.ToJSON Spend where
  toJSON Spend' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Amount" Core..= amount),
            Prelude.Just ("Unit" Core..= unit)
          ]
      )
