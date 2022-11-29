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
-- Module      : Amazonka.DeviceFarm.Types.RecurringCharge
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.RecurringCharge where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DeviceFarm.Types.MonetaryAmount
import Amazonka.DeviceFarm.Types.RecurringChargeFrequency
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether charges for devices are recurring.
--
-- /See:/ 'newRecurringCharge' smart constructor.
data RecurringCharge = RecurringCharge'
  { -- | The frequency in which charges recur.
    frequency :: Prelude.Maybe RecurringChargeFrequency,
    -- | The cost of the recurring charge.
    cost :: Prelude.Maybe MonetaryAmount
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecurringCharge' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frequency', 'recurringCharge_frequency' - The frequency in which charges recur.
--
-- 'cost', 'recurringCharge_cost' - The cost of the recurring charge.
newRecurringCharge ::
  RecurringCharge
newRecurringCharge =
  RecurringCharge'
    { frequency = Prelude.Nothing,
      cost = Prelude.Nothing
    }

-- | The frequency in which charges recur.
recurringCharge_frequency :: Lens.Lens' RecurringCharge (Prelude.Maybe RecurringChargeFrequency)
recurringCharge_frequency = Lens.lens (\RecurringCharge' {frequency} -> frequency) (\s@RecurringCharge' {} a -> s {frequency = a} :: RecurringCharge)

-- | The cost of the recurring charge.
recurringCharge_cost :: Lens.Lens' RecurringCharge (Prelude.Maybe MonetaryAmount)
recurringCharge_cost = Lens.lens (\RecurringCharge' {cost} -> cost) (\s@RecurringCharge' {} a -> s {cost = a} :: RecurringCharge)

instance Core.FromJSON RecurringCharge where
  parseJSON =
    Core.withObject
      "RecurringCharge"
      ( \x ->
          RecurringCharge'
            Prelude.<$> (x Core..:? "frequency")
            Prelude.<*> (x Core..:? "cost")
      )

instance Prelude.Hashable RecurringCharge where
  hashWithSalt _salt RecurringCharge' {..} =
    _salt `Prelude.hashWithSalt` frequency
      `Prelude.hashWithSalt` cost

instance Prelude.NFData RecurringCharge where
  rnf RecurringCharge' {..} =
    Prelude.rnf frequency
      `Prelude.seq` Prelude.rnf cost
