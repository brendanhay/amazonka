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
-- Module      : Amazonka.ResilienceHub.Types.Cost
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.Cost where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ResilienceHub.Types.CostFrequency

-- | Defines a cost object.
--
-- /See:/ 'newCost' smart constructor.
data Cost = Cost'
  { -- | The cost amount.
    amount :: Prelude.Double,
    -- | The cost currency, for example @USD@.
    currency :: Prelude.Text,
    -- | The cost frequency.
    frequency :: CostFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cost' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'cost_amount' - The cost amount.
--
-- 'currency', 'cost_currency' - The cost currency, for example @USD@.
--
-- 'frequency', 'cost_frequency' - The cost frequency.
newCost ::
  -- | 'amount'
  Prelude.Double ->
  -- | 'currency'
  Prelude.Text ->
  -- | 'frequency'
  CostFrequency ->
  Cost
newCost pAmount_ pCurrency_ pFrequency_ =
  Cost'
    { amount = pAmount_,
      currency = pCurrency_,
      frequency = pFrequency_
    }

-- | The cost amount.
cost_amount :: Lens.Lens' Cost Prelude.Double
cost_amount = Lens.lens (\Cost' {amount} -> amount) (\s@Cost' {} a -> s {amount = a} :: Cost)

-- | The cost currency, for example @USD@.
cost_currency :: Lens.Lens' Cost Prelude.Text
cost_currency = Lens.lens (\Cost' {currency} -> currency) (\s@Cost' {} a -> s {currency = a} :: Cost)

-- | The cost frequency.
cost_frequency :: Lens.Lens' Cost CostFrequency
cost_frequency = Lens.lens (\Cost' {frequency} -> frequency) (\s@Cost' {} a -> s {frequency = a} :: Cost)

instance Data.FromJSON Cost where
  parseJSON =
    Data.withObject
      "Cost"
      ( \x ->
          Cost'
            Prelude.<$> (x Data..: "amount")
            Prelude.<*> (x Data..: "currency")
            Prelude.<*> (x Data..: "frequency")
      )

instance Prelude.Hashable Cost where
  hashWithSalt _salt Cost' {..} =
    _salt
      `Prelude.hashWithSalt` amount
      `Prelude.hashWithSalt` currency
      `Prelude.hashWithSalt` frequency

instance Prelude.NFData Cost where
  rnf Cost' {..} =
    Prelude.rnf amount `Prelude.seq`
      Prelude.rnf currency `Prelude.seq`
        Prelude.rnf frequency
